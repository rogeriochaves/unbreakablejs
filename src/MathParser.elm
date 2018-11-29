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
        { int = Just Integer
        , hex = Nothing
        , octal = Nothing
        , binary = Nothing
        , float = Just Floating
        }


identifier : Parser String
identifier =
    variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.fromList []
        }


operators : OperatorTable Expression
operators =
    let
        symb sign =
            succeed identity
                |. backtrackable spaces
                |= symbol sign
    in
    [ [ infixOperator Exponentiation (symb "^") AssocLeft ]
    , [ infixOperator Multiplication (symb "*") AssocLeft, infixOperator Division (symb "/") AssocLeft ]
    , [ infixOperator Addition (symb "+") AssocLeft, infixOperator Subtraction (symb "-") AssocLeft ]
    ]


functionParensAndAtoms : Parser Expression
functionParensAndAtoms =
    oneOf
        [ parens <| lazy (\_ -> expression)
        , symbolicFunction
        , equation
        , atoms
        ]


equation : Parser Expression
equation =
    succeed (\( id, expr ) -> Equation (Identifier id) expr)
        |= assignment


assignment : Parser ( String, Expression )
assignment =
    succeed (\id expr -> ( id, expr ))
        |= backtrackable identifier
        |. backtrackable spaces
        |. symbol "="
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
                    |= expression
                    |. chompWhile (\c -> c == ' ')
                    |. chompIf (\c -> c == '\n')
                    |. spaces
                ]
        )


expression : Parser Expression
expression =
    buildExpressionParser operators (lazy <| \_ -> functionParensAndAtoms)


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


type alias Error =
    List DeadEnd


parse : String -> Result Error Types.Program
parse string =
    run program (string ++ "\nEOF")
