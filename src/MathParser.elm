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


identifier : Parser String
identifier =
    variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.fromList []
        }


functionCall : Parser Expression
functionCall =
    succeed FunctionCall
        |= backtrackable identifier
        |= backtrackable (parens expression)


operators : OperatorTable Expression
operators =
    let
        symb sign =
            succeed identity
                |. backtrackable spaces
                |= symbol sign

        infixOp op =
            infixOperator (InfixFunction op)
    in
    [ [ infixOp Exponentiation (symb "^") AssocLeft ]
    , [ infixOp Multiplication (symb "*") AssocLeft, infixOp Division (symb "/") AssocLeft ]
    , [ infixOp Addition (symb "+") AssocLeft, infixOp Subtraction (symb "-") AssocLeft ]
    ]


equation : Parser Expression
equation =
    succeed (\( id, expr ) -> Equation id expr)
        |= assignment


assignment : Parser ( String, Expression )
assignment =
    succeed (\id expr -> ( id, expr ))
        |= backtrackable identifier
        |. backtrackable spaces
        |. symbol "="
        |. spaces
        |= expression


functionDeclaration : Parser Expression
functionDeclaration =
    succeed (\name param body -> FunctionDeclaration name (FunctionSchema param body))
        |= backtrackable identifier
        |= backtrackable (parens identifier)
        |. backtrackable spaces
        |. backtrackable (symbol "=")
        |. spaces
        |= expression


program : Parser Types.Program
program =
    loop []
        (\expressions ->
            oneOf
                [ succeed (Done expressions)
                    |. symbol "EOF"
                , succeed (\expr -> Loop (expressions ++ [ expr ]))
                    |= expressionWithDeclarations
                    |. chompWhile (\c -> c == ' ')
                    |. chompIf (\c -> c == '\n')
                    |. spaces
                ]
        )


expression : Parser Expression
expression =
    buildExpressionParser operators
        (lazy <| \_ -> expressionParsers False)


expressionWithDeclarations : Parser Expression
expressionWithDeclarations =
    buildExpressionParser operators
        (lazy <| \_ -> expressionParsers True)


expressionParsers : Bool -> Parser Expression
expressionParsers withDeclarations =
    let
        declarations =
            [ functionDeclaration
            , equation
            ]

        expressions =
            [ parens <| lazy (\_ -> expression)
            , symbolicFunction
            , functionCall
            , atoms
            ]
    in
    if withDeclarations then
        oneOf (declarations ++ expressions)

    else
        oneOf expressions


atoms : Parser Expression
atoms =
    oneOf [ map Identifier identifier, digits ]


symbolicFunction : Parser Expression
symbolicFunction =
    let
        findSymbols name =
            ( Dict.get name singleAritySymbolsMap
            , Dict.get name doubleAritySymbolsMap
            , Dict.get name iteratorSymbolsMap
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

                ( Nothing, Nothing, Just symbol ) ->
                    succeed (\( id, expr ) -> Iterator symbol id expr)
                        |= braces assignment
                        |. Parser.symbol "^"
                        |= braces expression
                        |. spaces
                        |= expression

                ( Nothing, Nothing, Nothing ) ->
                    problem ("could not find symbol " ++ name)
    in
    succeed SymbolicFunction
        |. symbol "\\"
        |= (identifier |> andThen matchArities)


parse : String -> Result Error Types.Program
parse string =
    run program (string ++ "\nEOF")
