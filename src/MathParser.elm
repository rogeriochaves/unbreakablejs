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
    succeed (SingleArityApplication << NamedFunction)
        |= backtrackable identifier
        |= backtrackable (parens expression)


operators : OperatorTable Expression
operators =
    let
        infixOp op =
            infixOperator (DoubleArityApplication op)

        symb sign =
            succeed identity
                |. backtrackable spaces
                |= symbol sign
    in
    [ [ infixOp Exponentiation (symb "^") AssocLeft ]
    , [ infixOp Multiplication (symb "*") AssocLeft, infixOp Division (symb "/") AssocLeft ]
    , [ infixOp Addition (symb "+") AssocLeft, infixOp Subtraction (symb "-") AssocLeft ]
    ]


assignment : Parser Expression
assignment =
    succeed (\( id, expr ) -> SingleArityApplication (Assignment id) expr)
        |= assignmentParser


assignmentParser : Parser ( String, Expression )
assignmentParser =
    succeed (\id expr -> ( id, expr ))
        |= backtrackable identifier
        |. backtrackable spaces
        |. symbol "="
        |. spaces
        |= expression


functionDeclaration : Parser Expression
functionDeclaration =
    succeed (\name param body -> SingleArityApplication (Assignment name) (Abstraction param body))
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
            , assignment
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
    oneOf [ map Variable identifier, digits ]


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
                    succeed (SingleArityApplication symbol)
                        |= braces expression

                ( Nothing, Just symbol, _ ) ->
                    succeed (DoubleArityApplication symbol)
                        |= braces expression
                        |= braces expression

                ( Nothing, Nothing, Just "sum_" ) ->
                    succeed (\( id, expr ) -> TripleArityApplication (Sum_ id) expr)
                        |= braces assignmentParser
                        |. Parser.symbol "^"
                        |= braces expression
                        |. spaces
                        |= expression

                ( Nothing, Nothing, _ ) ->
                    problem ("could not find symbol " ++ name)
    in
    succeed identity
        |. symbol "\\"
        |= (identifier |> andThen matchArities)


parse : String -> Result Error Types.Program
parse string =
    run program (string ++ "\nEOF")
