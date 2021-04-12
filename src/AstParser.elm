module AstParser exposing (parse)

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
    variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.fromList []
        }



-- vectorIdentifier : Parser String
-- vectorIdentifier =
--     succeed identity
--         |. oneOf [ symbol "\\vec", symbol "\\mathbf" ]
--         |= braces scalarIdentifier
-- scalarIdentifier : Parser String
-- scalarIdentifier =
--     let
--         decorators =
--             succeed ()
--                 |. oneOf [ symbol "\\tilde", symbol "\\bar" ]
--                 |. braces names
--         names =
--             oneOf
--                 (lowercaseGreek
--                     ++ [ succeed ()
--                             |. chompIf (\c -> Char.isLower c && Char.isAlphaNum c)
--                        , succeed ()
--                             |. symbol "\\operatorname"
--                             |. braces
--                                 (variable
--                                     { start = Char.isAlphaNum
--                                     , inner = \c -> Char.isAlphaNum c
--                                     , reserved = Set.empty
--                                     }
--                                 )
--                        ]
--                 )
--     in
--     oneOf
--         [ decorators
--         , names
--         ]
--         |> getChompedString


tracked : String -> ( Int, Int ) -> UntrackedExp -> Expression
tracked filename ( row, col ) =
    Tracked { line = row, column = col, filename = filename }


ifCondition : String -> Parser Expression
ifCondition filename =
    succeed
        (\pos condition expr ->
            tracked filename pos (IfCondition condition expr)
        )
        |= getPosition
        |. backtrackable (symbol "if")
        |. spaces
        |= parens (lazy (\_ -> expression filename))
        |. spaces
        |= lazy (\_ -> expression filename)


while : String -> Parser Expression
while filename =
    succeed
        (\pos condition expr ->
            tracked filename pos (While condition expr)
        )
        |= getPosition
        |. backtrackable (symbol "while")
        |. spaces
        |= parens (lazy (\_ -> expression filename))
        |. spaces
        |= lazy (\_ -> expression filename)


functionCall : Expression -> String -> Parser Expression
functionCall expr filename =
    succeed
        (\pos args ->
            tracked filename pos (Application expr args)
        )
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


infixOperator : String -> Operation2 -> Parser ( Int, Int ) -> Assoc -> Operator Expression
infixOperator filename operation opParser assoc =
    let
        binaryOp =
            succeed (\pos expr1 expr2 -> tracked filename pos (Operation2 operation expr1 expr2))
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
    , [ infixOperator filename SoftEquality (symb "==") AssocLeft
      , infixOperator filename GreaterThan (symb ">") AssocLeft
      , infixOperator filename SmallerThan (symb "<") AssocLeft
      ]
    ]


members : Expression -> String -> Parser Expression
members expr filename =
    succeed
        (\pos key ->
            tracked filename pos (Operation2 Member expr key)
        )
        |= getPosition
        |. symbol "["
        |= expression filename
        |. symbol "]"


assignment : String -> Parser Expression
assignment filename =
    oneOf
        [ succeed (\name pos -> tracked filename pos << Operation (LetAssignment name))
            |. backtrackable (symbol "let ")
        , succeed (\name pos -> tracked filename pos << Operation (Assignment name))
        ]
        |= backtrackable identifier
        |. backtrackable spaces
        |= getPosition
        |. backtrackable (symbol "=")
        |. spaces
        |= lazy (\_ -> expression filename)


functionDeclaration : String -> Parser Expression
functionDeclaration filename =
    succeed
        (\pos name params body ->
            tracked filename
                pos
                (Operation (LetAssignment name)
                    (tracked filename pos (Value (Abstraction params body)))
                )
        )
        |= getPosition
        |. backtrackable (symbol "function")
        |. spaces
        |= identifier
        |. spaces
        |= sequence
            { start = "("
            , separator = ","
            , end = ")"
            , spaces = spaces
            , item = identifier
            , trailing = Forbidden
            }
        |. spaces
        |= lazy (\_ -> block filename True)


abstraction : String -> Parser Expression
abstraction filename =
    succeed
        (\pos params body ->
            tracked filename pos (Value (Abstraction params body))
        )
        |= getPosition
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
        |= lazy (\_ -> expression_ filename True True)


program : String -> Parser Types.Program
program filename =
    loop [] (programLoop filename)


statementBreak : Parser ()
statementBreak =
    succeed ()
        |. chompWhile (\c -> c == ' ')
        |. chompIf (\c -> c == '\n' || c == ';')
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
            |= expression_ filename True False
            |. statementBreak
        ]


expression : String -> Parser Expression
expression filename =
    expression_ filename False False


expression_ : String -> Bool -> Bool -> Parser Expression
expression_ filename withDeclarations withReturn =
    let
        declarations =
            if withDeclarations then
                [ functionDeclaration filename
                , assignment filename
                , ifCondition filename
                , while filename
                ]

            else
                []

        expressionParser =
            buildExpressionParser (operators filename)
                (lazy <|
                    \_ ->
                        expressionParsers filename withReturn
                 -- |> andThen
                 --     (\expr ->
                 --         oneOf
                 --             [ -- index expr
                 --               -- , exponentiation expr
                 --               -- , factorial expr
                 --               members expr filename
                 --             , succeed expr
                 --             ]
                 --     )
                )
    in
    oneOf (declarations ++ [ expressionParser ])


expressionParsers : String -> Bool -> Parser Expression
expressionParsers filename withReturn =
    let
        return_ =
            if withReturn then
                [ return filename ]

            else
                [ return filename
                    |. problem "return can only be used inside a function body"
                ]

        expressions =
            [ block filename withReturn
            , abstraction filename
            , backtrackable <| parens <| lazy (\_ -> expression filename)
            , increment filename
            , decrement filename
            , atoms filename
            ]
    in
    oneOf (return_ ++ expressions)
        |> andThen (postfixOperators filename)


increment : String -> Parser Expression
increment filename =
    succeed (\posVar name posInc -> tracked filename posInc (Operation (Increment name) (tracked filename posVar (Variable name))))
        |= getPosition
        |= backtrackable identifier
        |= getPosition
        |. backtrackable (symbol "++")


decrement : String -> Parser Expression
decrement filename =
    succeed (\posVar name posInc -> tracked filename posInc (Operation (Decrement name) (tracked filename posVar (Variable name))))
        |= getPosition
        |= backtrackable identifier
        |= getPosition
        |. backtrackable (symbol "--")


postfixOperators : String -> Expression -> Parser Expression
postfixOperators filename expr =
    oneOf
        [ members expr filename
            |> andThen (postfixOperators filename)
        , functionCall expr filename
            |> andThen (postfixOperators filename)
        , succeed expr
        ]


block : String -> Bool -> Parser Expression
block filename withReturn =
    let
        expressionLine =
            oneOf
                [ succeed identity
                    |= backtrackable (expression_ filename True withReturn)
                    |. backtrackable statementBreak
                , succeed identity
                    |= expression_ filename True withReturn
                ]
    in
    succeed (\list pos -> tracked filename pos <| Block list)
        |= backtrackable (braces (many expressionLine))
        |= getPosition


return : String -> Parser Expression
return filename =
    succeed (\pos expr -> tracked filename pos (Return expr))
        |= getPosition
        |. backtrackable (symbol "return")
        |. spaces
        |= expression filename


atoms : String -> Parser Expression
atoms filename =
    oneOf
        [ booleans
        , undefined filename
        , succeed (\pos name -> tracked filename pos (Variable name))
            |= getPosition
            |= identifier
        , digits
        , arrays filename
        , strings
        ]


booleans : Parser Expression
booleans =
    oneOf
        [ succeed (Untracked (Value (Boolean True)))
            |. backtrackable (symbol "true")
        , succeed (Untracked (Value (Boolean False)))
            |. backtrackable (symbol "false")
        ]


strings : Parser Expression
strings =
    oneOf
        [ succeed (Untracked << Value << String)
            |. symbol "\""
            |= (getChompedString <| chompWhile (\c -> c /= '"' && c /= '\n'))
            |. symbol "\""
        , succeed (Untracked << Value << String)
            |. symbol "'"
            |= (getChompedString <| chompWhile (\c -> c /= '\'' && c /= '\n'))
            |. symbol "'"
        ]


undefined : String -> Parser Expression
undefined filename =
    succeed
        (\( row, col ) ->
            Untracked
                (Value
                    (Undefined
                        [ { line = row
                          , column = col
                          , filename = filename
                          , reason = ExplicitUndefined
                          }
                        ]
                    )
                )
        )
        |= getPosition
        |. backtrackable (symbol "undefined")


arrays : String -> Parser Expression
arrays filename =
    succeed (\pos items -> tracked filename pos (ArrayExpression items))
        |= getPosition
        |= sequence
            { start = "["
            , separator = ","
            , end = "]"
            , spaces = spaces
            , item = expression filename
            , trailing = Forbidden
            }


parse : String -> String -> Result Error Types.Program
parse filename content =
    run (program filename) (content ++ "\nEOF")
