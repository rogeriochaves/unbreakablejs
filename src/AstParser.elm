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
    variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.fromList []
        }


tracked : String -> ( Int, Int ) -> UntrackedExp -> Expression
tracked filename ( row, col ) =
    Tracked { line = row, column = col, filename = filename }


type alias Tracker =
    ( Int, Int ) -> UntrackedExp -> Expression


ifCondition : Tracker -> Bool -> Parser Expression
ifCondition track withReturn =
    succeed
        (\pos condition expr ->
            track pos (IfCondition condition expr)
        )
        |= getPosition
        |. backtrackable (symbol "if")
        |. spaces
        |= parens (lazy (\_ -> expression_ track True withReturn))
        |. spaces
        |= lazy (\_ -> expression_ track True withReturn)


ifElseCondition : Tracker -> Bool -> Parser Expression
ifElseCondition track withReturn =
    succeed
        (\pos condition exprIfTrue exprIfFalse ->
            track pos (IfElseCondition condition exprIfTrue exprIfFalse)
        )
        |= getPosition
        |. backtrackable (symbol "if")
        |. backtrackable spaces
        |= backtrackable (parens (lazy (\_ -> expression_ track True withReturn)))
        |. backtrackable spaces
        |= backtrackable (lazy (\_ -> block track withReturn))
        |. backtrackable spaces
        |. backtrackable (symbol "else")
        |. spaces
        |= lazy (\_ -> expression_ track True withReturn)


while : Tracker -> Parser Expression
while track =
    succeed
        (\pos condition expr ->
            track pos (While condition expr)
        )
        |= getPosition
        |. backtrackable (symbol "while")
        |. spaces
        |= parens (lazy (\_ -> expression track))
        |. spaces
        |= lazy (\_ -> expression track)


forLoop : Tracker -> Parser Expression
forLoop track =
    succeed
        (\pos ( assignment_, condition, increment_ ) expr ->
            track pos (ForLoop assignment_ condition increment_ expr)
        )
        |= getPosition
        |. backtrackable (symbol "for")
        |. spaces
        |= parens
            (succeed
                (\assignment_ condition increment_ -> ( assignment_, condition, increment_ ))
                |. spaces
                |= lazy (\_ -> expression_ track True False)
                |. spaces
                |. symbol ";"
                |. spaces
                |= lazy (\_ -> expression_ track True False)
                |. spaces
                |. symbol ";"
                |. spaces
                |= lazy (\_ -> expression_ track True False)
                |. spaces
            )
        |. spaces
        |= lazy (\_ -> expression track)


functionCall : Tracker -> Expression -> Parser Expression
functionCall track expr =
    succeed
        (\pos args ->
            track pos (Application expr args)
        )
        |= getPosition
        |= backtrackable
            (sequence
                { start = "("
                , separator = ","
                , end = ")"
                , spaces = spaces
                , item = expression track
                , trailing = Forbidden
                }
            )


infixOperator : Tracker -> Operation2 -> Parser ( Int, Int ) -> Assoc -> Operator Expression
infixOperator track operation opParser assoc =
    let
        binaryOp =
            succeed (\pos expr1 expr2 -> track pos (Operation2 operation expr1 expr2))
                |= opParser
                |. spaces
    in
    Infix binaryOp assoc


operators : Tracker -> OperatorTable Expression
operators track =
    let
        symb : String -> Parser ( Int, Int )
        symb sign =
            succeed identity
                |. backtrackable spaces
                |= getPosition
                |. symbol sign
    in
    [ -- , [ infixOperator Exponentiation (symb "^") AssocLeft ]
      -- , [ infixOperator Multiplication (symb "*") AssocLeft, infixOperator Division (symb "/") AssocLeft ]
      -- , [ infixOperator Modulo (symb "\\mod") AssocLeft, infixOperator EuclideanDivision (symb "\\div") AssocLeft ]
      [ Prefix
            (succeed (\pos expr -> track pos (Operation Negative expr))
                |= getPosition
                |. symbol "-"
            )
      ]
    , [ infixOperator track Exponentiation (symb "**") AssocLeft ]
    , [ infixOperator track Multiplication (symb "*") AssocLeft
      , infixOperator track Division (symb "/") AssocLeft
      ]
    , [ infixOperator track Remainder (symb "%") AssocLeft ]
    , [ infixOperator track Addition (symb "+") AssocLeft
      , infixOperator track Subtraction (symb "-") AssocLeft
      ]
    , [ infixOperator track HardEquality (symb "===") AssocLeft
      , infixOperator track SoftEquality (symb "==") AssocLeft
      , infixOperator track HardNotEquality (symb "!==") AssocLeft
      , infixOperator track SoftNotEquality (symb "!=") AssocLeft
      , infixOperator track GreaterOrEqualThan (symb ">=") AssocLeft
      , infixOperator track SmallerOrEqualThan (symb "<=") AssocLeft
      , infixOperator track GreaterThan (symb ">") AssocLeft
      , infixOperator track SmallerThan (symb "<") AssocLeft
      ]
    , [ infixOperator track And (symb "&&") AssocLeft
      , infixOperator track Or (symb "||") AssocLeft
      ]
    ]


members : Tracker -> Expression -> Parser Expression
members track expr =
    succeed
        (\pos key ->
            track pos (Operation2 Member expr key)
        )
        |= getPosition
        |. symbol "["
        |= expression track
        |. symbol "]"


dot : Tracker -> Expression -> Parser Expression
dot track expr =
    succeed
        (\pos key ->
            track pos (Operation2 Member expr (Untracked (Value (String key))))
        )
        |= getPosition
        |. symbol "."
        |= identifier


assignment : Tracker -> Parser Expression
assignment track =
    oneOf
        [ succeed (\name pos -> track pos << Operation (LetAssignment name))
            |. backtrackable (symbol "let ")
        , succeed (\name pos -> track pos << Operation (Assignment name))
        ]
        |= backtrackable identifier
        |. backtrackable spaces
        |= getPosition
        |. backtrackable (symbol "=")
        |. spaces
        |= lazy (\_ -> expression track)


operationAssignment : Tracker -> Parser Expression
operationAssignment track =
    let
        symb operation str =
            succeed operation
                |. symbol str
    in
    succeed
        (\posVar name posSign operation posEqual expr ->
            track posEqual
                (Operation (Assignment name)
                    (track posSign
                        (Operation2 operation
                            (track posVar (Variable name))
                            expr
                        )
                    )
                )
        )
        |= getPosition
        |= backtrackable identifier
        |. backtrackable spaces
        |= getPosition
        |= backtrackable
            (oneOf
                [ symb Addition "+"
                , symb Subtraction "-"
                , symb Division "/"
                , symb Exponentiation "**"
                , symb Multiplication "*"
                , symb Remainder "%"
                ]
            )
        |= getPosition
        |. backtrackable (symbol "=")
        |. spaces
        |= lazy (\_ -> expression track)


functionDeclaration : Tracker -> Parser Expression
functionDeclaration track =
    succeed
        (\pos name params body ->
            track pos
                (Operation (LetAssignment name)
                    (track pos (Value (Abstraction params body)))
                )
        )
        |= getPosition
        |. backtrackable (symbol "function")
        |. backtrackable spaces
        |= backtrackable identifier
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
        |= lazy (\_ -> block track True)


abstraction : Tracker -> Parser Expression
abstraction track =
    succeed
        (\pos params body ->
            track pos (Value (Abstraction params body))
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
        |= lazy (\_ -> expression_ track True True)


anonymousFunction : Tracker -> Parser Expression
anonymousFunction track =
    succeed
        (\pos params body ->
            track pos (Value (Abstraction params body))
        )
        |= getPosition
        |. backtrackable (symbol "function")
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
        |. spaces
        |= lazy (\_ -> block track True)


program : Tracker -> Parser Types.Program
program track =
    loop [] (programLoop track)


statementBreak : Parser ()
statementBreak =
    succeed ()
        |. chompWhile (\c -> c == ' ')
        |. chompIf (\c -> c == '\n' || c == ';')
        |. spaces


programLoop : Tracker -> List Expression -> Parser (Step (List Expression) (List Expression))
programLoop track expressions =
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
            |. spaces
            |= expression_ track True False
            |. statementBreak
        ]


expression : Tracker -> Parser Expression
expression track =
    expression_ track False False


expression_ : Tracker -> Bool -> Bool -> Parser Expression
expression_ track withDeclarations withReturn =
    let
        declarations =
            if withDeclarations then
                [ functionDeclaration track
                , assignment track
                , operationAssignment track
                , ifElseCondition track withReturn
                , ifCondition track withReturn
                , while track
                , forLoop track
                ]

            else
                []

        expressionParser =
            buildExpressionParser (operators track)
                (lazy (\_ -> expressionParsers track withReturn))
    in
    oneOf (declarations ++ [ expressionParser ])


expressionParsers : Tracker -> Bool -> Parser Expression
expressionParsers track withReturn =
    let
        return_ =
            if withReturn then
                [ return track ]

            else
                [ return track
                    |. problem "return can only be used inside a function body"
                ]

        expressions =
            [ objects track
            , block track withReturn
            , abstraction track
            , anonymousFunction track
            , backtrackable <| parens <| lazy (\_ -> expression track)
            , not_ track
            , increment track
            , decrement track
            , atoms track
            ]
    in
    oneOf (return_ ++ expressions)
        |> andThen (postfixOperators track)


not_ : Tracker -> Parser Expression
not_ track =
    succeed (\pos expr -> track pos (Operation Not expr))
        |= getPosition
        |. symbol "!"
        |. spaces
        |= lazy (\_ -> expression track)


increment : Tracker -> Parser Expression
increment track =
    succeed (\posVar name posInc -> track posInc (Operation (Increment name) (track posVar (Variable name))))
        |= getPosition
        |= backtrackable identifier
        |= getPosition
        |. backtrackable (symbol "++")


decrement : Tracker -> Parser Expression
decrement track =
    succeed (\posVar name posInc -> track posInc (Operation (Decrement name) (track posVar (Variable name))))
        |= getPosition
        |= backtrackable identifier
        |= getPosition
        |. backtrackable (symbol "--")


postfixOperators : Tracker -> Expression -> Parser Expression
postfixOperators track expr =
    oneOf
        [ members track expr
            |> andThen (postfixOperators track)
        , dot track expr
            |> andThen (postfixOperators track)
        , functionCall track expr
            |> andThen (postfixOperators track)
        , succeed expr
        ]


block : Tracker -> Bool -> Parser Expression
block track withReturn =
    let
        expressionLine =
            oneOf
                [ succeed identity
                    |= backtrackable (expression_ track True withReturn)
                    |. backtrackable statementBreak
                , succeed identity
                    |= expression_ track True withReturn
                ]
    in
    succeed (\list pos -> track pos <| Block list)
        |= backtrackable (braces (many expressionLine))
        |= getPosition


return : Tracker -> Parser Expression
return track =
    succeed (\pos expr -> track pos (Return expr))
        |= getPosition
        |. backtrackable (symbol "return")
        |. spaces
        |= expression track


atoms : Tracker -> Parser Expression
atoms track =
    oneOf
        [ booleans
        , undefined track
        , succeed (\pos name -> track pos (Variable name))
            |= getPosition
            |= identifier
        , digits
        , arrays track
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


undefined : Tracker -> Parser Expression
undefined track =
    succeed
        (\( row, col ) ->
            let
                info =
                    case track ( row, col ) (Value (Undefined [])) of
                        Tracked trackInfo _ ->
                            trackInfo

                        _ ->
                            { line = row, column = col, filename = "" }
            in
            Untracked
                (Value
                    (Undefined
                        [ { line = row
                          , column = col
                          , filename = info.filename
                          , reason = ExplicitUndefined
                          }
                        ]
                    )
                )
        )
        |= getPosition
        |. backtrackable (symbol "undefined")


arrays : Tracker -> Parser Expression
arrays track =
    succeed (\pos items -> track pos (ArrayExpression items))
        |= getPosition
        |= sequence
            { start = "["
            , separator = ","
            , end = "]"
            , spaces = spaces
            , item = expression track
            , trailing = Forbidden
            }


objects : Tracker -> Parser Expression
objects track =
    let
        objectItem : Parser ( String, Expression )
        objectItem =
            succeed (\key value -> ( key, value ))
                |. spaces
                |= oneOf
                    [ identifier
                    , between (symbol "'") (symbol "'") (getChompedString <| chompWhile (\c -> c /= '\'' && c /= '\n'))
                    , between (symbol "\"") (symbol "\"") (getChompedString <| chompWhile (\c -> c /= '"' && c /= '\n'))
                    , number
                        { int = Just String.fromInt
                        , hex = Nothing
                        , octal = Nothing
                        , binary = Nothing
                        , float = Just String.fromFloat
                        }
                    ]
                |. spaces
                |. symbol ":"
                |. spaces
                |= expression track
                |. spaces
                |. oneOf [ symbol ",", succeed () ]
    in
    succeed (\pos dict -> track pos (ObjectExpression (Dict.fromList dict)))
        |= getPosition
        |= backtrackable (braces (many objectItem))


parse : String -> String -> Result Error Types.Program
parse filename content =
    run (program (tracked filename)) (content ++ "\nEOF")
