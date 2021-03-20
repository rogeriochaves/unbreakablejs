module AstParser exposing (parse)

import Dict
import Parser exposing (..)
import Parser.Expression exposing (..)
import Parser.Extras exposing (..)
import Set
import Types exposing (..)


digits : Parser Expression
digits =
    number
        { int = Just (toFloat >> Number >> Value >> Untracked)
        , hex = Nothing
        , octal = Nothing
        , binary = Nothing
        , float = Just (Number >> Value >> Untracked)
        }


identifier : Parser String
identifier =
    scalarIdentifier


vectorIdentifier : Parser String
vectorIdentifier =
    succeed identity
        |. oneOf [ symbol "\\vec", symbol "\\mathbf" ]
        |= braces scalarIdentifier


scalarIdentifier : Parser String
scalarIdentifier =
    let
        decorators =
            succeed ()
                |. oneOf [ symbol "\\tilde", symbol "\\bar" ]
                |. braces names

        names =
            oneOf
                (lowercaseGreek
                    ++ [ succeed ()
                            |. chompIf (\c -> Char.isLower c && Char.isAlphaNum c)
                       , succeed ()
                            |. symbol "\\operatorname"
                            |. braces
                                (variable
                                    { start = Char.isAlphaNum
                                    , inner = \c -> Char.isAlphaNum c
                                    , reserved = Set.empty
                                    }
                                )
                       ]
                )
    in
    oneOf
        [ decorators
        , names
        ]
        |> getChompedString


lowercaseGreek : List (Parser ())
lowercaseGreek =
    List.map symbol
        [ "\\alpha"
        , "\\beta"
        , "\\gamma"
        , "\\delta"
        , "\\epsilon"
        , "\\varepsilon"
        , "\\zeta"
        , "\\eta"
        , "\\theta"
        , "\\vartheta"
        , "\\iota"
        , "\\kappa"
        , "\\lambda"
        , "\\mu"
        , "\\nu"
        , "\\xi"
        , "\\pi"
        , "\\rho"
        , "\\sigma"
        , "\\tau"
        , "\\upsilon"
        , "\\phi"
        , "\\chi"
        , "\\psi"
        , "\\omega"
        ]


symbolIdentifier : Parser String
symbolIdentifier =
    variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.fromList []
        }


tracked : String -> ( Int, Int ) -> UntrackedExp -> Expression
tracked filename ( row, col ) =
    Tracked { line = row, column = col, filename = filename }


functionCall : String -> Parser Expression
functionCall filename =
    succeed
        (\posIdentifier name posApplication ->
            tracked filename posApplication << Application (tracked filename posIdentifier (Variable name))
        )
        |= getPosition
        |= backtrackable scalarIdentifier
        |= getPosition
        |= backtrackable
            (sequence
                { start = "("
                , separator = ","
                , end = ")"
                , spaces = spaces
                , item = expression filename
                , trailing = Forbidden
                }
            )


infixOperator : String -> Reserved -> Parser ( Int, Int ) -> Assoc -> Operator Expression
infixOperator filename operation opParser assoc =
    let
        binaryOp =
            succeed (\pos expr1 expr2 -> tracked filename pos (doubleArity operation expr1 expr2))
                |= opParser
                |. spaces
    in
    Infix binaryOp assoc


operators : String -> OperatorTable Expression
operators filename =
    let
        symb : String -> Parser ( Int, Int )
        symb sign =
            succeed identity
                |. backtrackable spaces
                |= getPosition
                |. symbol sign
    in
    -- [ [ prefixOperator (SingleArity Negation) (symbol "-") ]
    -- , [ infixOperator Exponentiation (symb "^") AssocLeft ]
    -- , [ infixOperator Multiplication (symb "*") AssocLeft, infixOperator Division (symb "/") AssocLeft ]
    -- , [ infixOperator Modulo (symb "\\mod") AssocLeft, infixOperator EuclideanDivision (symb "\\div") AssocLeft ]
    [ [ infixOperator filename Addition (symb "+") AssocLeft
      , infixOperator filename Subtraction (symb "-") AssocLeft
      ]
    ]


assignment : String -> Parser Expression
assignment filename =
    succeed (\name pos -> tracked filename pos << singleArity (Assignment name))
        |= backtrackable identifier
        |. backtrackable spaces
        |= getPosition
        |. symbol "="
        |. spaces
        |= expression filename


functionDeclaration : String -> Parser Expression
functionDeclaration filename =
    succeed
        (\name pos param body ->
            tracked filename
                pos
                (ReservedApplication (Assignment name)
                    [ Untracked (Value (Abstraction param body)) ]
                )
        )
        |= backtrackable identifier
        |. backtrackable spaces
        |= getPosition
        |. backtrackable (symbol "=")
        |. backtrackable spaces
        |= backtrackable
            (sequence
                { start = "("
                , separator = ","
                , end = ")"
                , spaces = spaces
                , item = identifier
                , trailing = Forbidden
                }
            )
        |. backtrackable spaces
        |. backtrackable (symbol "=>")
        |. spaces
        |= expression filename


singleArity : Reserved -> Expression -> UntrackedExp
singleArity fn expr =
    ReservedApplication fn [ expr ]


doubleArity : Reserved -> Expression -> Expression -> UntrackedExp
doubleArity fn expr1 expr2 =
    ReservedApplication fn [ expr1, expr2 ]



-- mapFunctionDeclaration : Parser Expression
-- mapFunctionDeclaration =
--     succeed (\name param idx body -> SingleArity (Assignment (ScalarIdentifier name)) (MapAbstraction param idx body))
--         |= backtrackable scalarIdentifier
--         |= backtrackable (parens vectorIdentifier)
--         |. backtrackable (symbol "_")
--         |= braces scalarIdentifier
--         |. spaces
--         |. symbol "="
--         |. spaces
--         |= expression
-- index : Expression -> Parser Expression
-- index expr =
--     succeed (DoubleArity Index expr)
--         |. backtrackable (symbol "_")
--         |= backtrackable (braces (lazy <| \_ -> expression))
-- exponentiation : Expression -> Parser Expression
-- exponentiation expr =
--     succeed (DoubleArity Exponentiation expr)
--         |. backtrackable spaces
--         |. backtrackable (symbol "^")
--         |. backtrackable spaces
--         |= backtrackable (braces (lazy <| \_ -> expression))
-- factorial : Expression -> Parser Expression
-- factorial expr =
--     succeed (SingleArity Factorial expr)
--         |. backtrackable (symbol "!")


program : String -> Parser Types.Program
program filename =
    loop [] (programLoop filename)


statementBreak : Parser ()
statementBreak =
    succeed ()
        |. chompWhile (\c -> c == ' ')
        |. chompIf (\c -> c == '\n')
        |. spaces


programLoop : String -> List Expression -> Parser (Step (List Expression) (List Expression))
programLoop filename expressions =
    let
        appendExpr expr =
            case List.head expressions of
                Just (Tracked _ (Block items)) ->
                    Loop (Untracked (Block (items ++ [ expr ])) :: List.drop 1 expressions)

                _ ->
                    Loop (expr :: expressions)
    in
    oneOf
        [ succeed (Done (List.reverse expressions))
            |. symbol "EOF"
        , succeed appendExpr
            |= expression_ filename True
            |. statementBreak
        ]


expression : String -> Parser Expression
expression filename =
    expression_ filename False


expression_ : String -> Bool -> Parser Expression
expression_ filename withDeclarations =
    buildExpressionParser (operators filename)
        (lazy <|
            \_ ->
                expressionParsers filename withDeclarations
                    |> andThen
                        (\expr ->
                            oneOf
                                [ -- index expr
                                  -- , exponentiation expr
                                  -- , factorial expr
                                  succeed expr
                                ]
                        )
        )


expressionParsers : String -> Bool -> Parser Expression
expressionParsers filename withDeclarations =
    let
        declarations =
            [ functionDeclaration filename
            , assignment filename
            ]

        -- [ mapFunctionDeclaration
        -- , functionDeclaration
        -- , assignment
        -- ]
        expressions =
            [ block filename
            , backtrackable <| parens <| lazy (\_ -> expression filename)
            , functionCall filename
            , atoms filename
            , vectors filename
            ]
    in
    if withDeclarations then
        oneOf (declarations ++ expressions)

    else
        oneOf expressions


block : String -> Parser Expression
block filename =
    let
        expressionLine =
            oneOf
                [ succeed identity
                    |= backtrackable (expression filename)
                    |. backtrackable statementBreak
                , succeed identity
                    |= expression filename
                ]
    in
    succeed (\list -> Untracked <| Block list)
        |= backtrackable (braces (many expressionLine))


atoms : String -> Parser Expression
atoms filename =
    oneOf
        [ succeed (\pos name -> tracked filename pos (Variable name))
            |= getPosition
            |= identifier
        , digits
        ]


vectors : String -> Parser Expression
vectors filename =
    succeed (Vector >> Value >> Untracked)
        |= sequence
            { start = "("
            , separator = ","
            , end = ")"
            , spaces = spaces
            , item = expression filename
            , trailing = Forbidden
            }


parse : String -> String -> Result Error Types.Program
parse filename content =
    run (program filename) (content ++ "\nEOF")
