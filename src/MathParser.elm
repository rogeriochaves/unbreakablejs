module MathParser exposing (parse)

import Dict
import Parser exposing (..)
import Parser.Expression exposing (..)
import Parser.Extras exposing (..)
import Set
import Types exposing (..)


digits : Parser Expression
digits =
    number
        { int = Just (toFloat >> Number)
        , hex = Nothing
        , octal = Nothing
        , binary = Nothing
        , float = Just Number
        }


identifier : Parser Identifier
identifier =
    oneOf
        [ map ScalarIdentifier scalarIdentifier
        , map VectorIdentifier vectorIdentifier
        ]


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


functionCall : Parser Expression
functionCall =
    succeed (SingleArity << Application << Variable << ScalarIdentifier)
        |= backtrackable scalarIdentifier
        |= backtrackable (parens expression)


cardinality : Parser Expression
cardinality =
    succeed (SingleArity Cardinality)
        |. backtrackable (symbol "|")
        |= backtrackable expression
        |. symbol "|"


operators : OperatorTable Expression
operators =
    let
        infixOp op =
            infixOperator (DoubleArity op)

        symb sign =
            succeed identity
                |. backtrackable spaces
                |= symbol sign
    in
    [ [ prefixOperator (SingleArity Negation) (symbol "-") ]
    , [ infixOp Exponentiation (symb "^") AssocLeft ]
    , [ infixOp Multiplication (symb "*") AssocLeft, infixOp Division (symb "/") AssocLeft ]
    , [ infixOp Modulo (symb "\\mod") AssocLeft, infixOp EuclideanDivision (symb "\\div") AssocLeft ]
    , [ infixOp Addition (symb "+") AssocLeft, infixOp Subtraction (symb "-") AssocLeft ]
    ]


assignment : Parser Expression
assignment =
    succeed (SingleArity << Assignment)
        |= backtrackable identifier
        |. backtrackable spaces
        |. symbol "="
        |. spaces
        |= expression


functionDeclaration : Parser Expression
functionDeclaration =
    succeed (\name param body -> SingleArity (Assignment (ScalarIdentifier name)) (Abstraction param body))
        |= backtrackable scalarIdentifier
        |= backtrackable (parens identifier)
        |. backtrackable spaces
        |. backtrackable (symbol "=")
        |. spaces
        |= expression


mapFunctionDeclaration : Parser Expression
mapFunctionDeclaration =
    succeed (\name param idx body -> SingleArity (Assignment (ScalarIdentifier name)) (MapAbstraction param idx body))
        |= backtrackable scalarIdentifier
        |= backtrackable (parens vectorIdentifier)
        |. backtrackable (symbol "_")
        |= braces scalarIdentifier
        |. spaces
        |. symbol "="
        |. spaces
        |= expression


index : Expression -> Parser Expression
index expr =
    succeed (DoubleArity Index expr)
        |. backtrackable (symbol "_")
        |= backtrackable (braces (lazy <| \_ -> expression))


exponentiation : Expression -> Parser Expression
exponentiation expr =
    succeed (DoubleArity Exponentiation expr)
        |. backtrackable spaces
        |. backtrackable (symbol "^")
        |. backtrackable spaces
        |= backtrackable (braces (lazy <| \_ -> expression))


factorial : Expression -> Parser Expression
factorial expr =
    succeed (SingleArity Factorial expr)
        |. backtrackable (symbol "!")


program : Parser Types.Program
program =
    loop [] programLoop


programLoop : List Expression -> Parser (Step (List Expression) (List Expression))
programLoop expressions =
    let
        appendExpr expr =
            case List.head expressions of
                Just (Block name items) ->
                    Loop (Block name (items ++ [ expr ]) :: List.drop 1 expressions)

                _ ->
                    Loop (expr :: expressions)

        statementBreak =
            succeed ()
                |. chompWhile (\c -> c == ' ')
                |. chompIf (\c -> c == '\n')
                |. spaces
    in
    oneOf
        [ succeed (Done (List.reverse expressions))
            |. symbol "EOF"
        , succeed (\name -> Loop (Block name [] :: expressions))
            |= backtrackable (getChompedString (chompWhile (\c -> c /= ':' && c /= '\n')))
            |. symbol ":"
            |. statementBreak
        , succeed appendExpr
            |= expression_ True
            |. statementBreak
        ]


expression : Parser Expression
expression =
    expression_ False


expression_ : Bool -> Parser Expression
expression_ withDeclarations =
    buildExpressionParser operators
        (lazy <|
            \_ ->
                expressionParsers withDeclarations
                    |> andThen
                        (\expr ->
                            oneOf
                                [ index expr
                                , exponentiation expr
                                , factorial expr
                                , succeed expr
                                ]
                        )
        )


expressionParsers : Bool -> Parser Expression
expressionParsers withDeclarations =
    let
        declarations =
            [ mapFunctionDeclaration
            , functionDeclaration
            , assignment
            ]

        expressions =
            [ backtrackable <| parens <| lazy (\_ -> expression)
            , functionCall
            , atoms
            , vectors
            , symbolicFunction
            , cardinality
            ]
    in
    if withDeclarations then
        oneOf (declarations ++ expressions)

    else
        oneOf expressions


atoms : Parser Expression
atoms =
    oneOf
        [ map Variable identifier
        , digits
        ]


vectors : Parser Expression
vectors =
    succeed Vector
        |= sequence
            { start = "("
            , separator = ","
            , end = ")"
            , spaces = spaces
            , item = expression
            , trailing = Forbidden
            }


symbolicFunction : Parser Expression
symbolicFunction =
    let
        findSymbols name =
            ( Dict.get name singleAritySymbolsMap
            , Dict.get name doubleAritySymbolsMap
            , Dict.get name tripleAritySymbolsMap
            )

        matchArities name =
            case findSymbols name of
                ( Just symbol, _, _ ) ->
                    succeed (SingleArity symbol)
                        |= braces expression

                ( Nothing, Just symbol, _ ) ->
                    succeed (DoubleArity symbol)
                        |= braces expression
                        |= braces expression

                ( Nothing, Nothing, Just "sum_" ) ->
                    let
                        scalarAssignment =
                            succeed (TripleArity << Sum_)
                                |= backtrackable scalarIdentifier
                                |. backtrackable spaces
                                |. symbol "="
                                |. spaces
                                |= expression
                    in
                    succeed identity
                        |= braces scalarAssignment
                        |. Parser.symbol "^"
                        |= braces expression
                        |. spaces
                        |= expression

                ( Nothing, Nothing, _ ) ->
                    problem ("could not find symbol " ++ name)
    in
    succeed identity
        |. symbol "\\"
        |= (symbolIdentifier |> andThen matchArities)


parse : String -> Result Error Types.Program
parse string =
    run program (string ++ "\nEOF")
