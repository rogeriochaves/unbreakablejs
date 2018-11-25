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
    [ [ infixOperator Exponentiation (symbol "^") AssocLeft ]
    , [ infixOperator Multiplication (symbol "*") AssocLeft, infixOperator Division (symbol "/") AssocLeft ]
    , [ infixOperator Addition (symbol "+") AssocLeft, infixOperator Subtraction (symbol "-") AssocLeft ]
    ]


functionParensAndAtoms : Parser Expression
functionParensAndAtoms =
    oneOf
        [ parens <| lazy (\_ -> expression)
        , symbolicFunction |. spaces
        , atoms |. spaces
        ]


expression : Parser Expression
expression =
    buildExpressionParser operators (lazy <| \_ -> functionParensAndAtoms)


atoms : Parser Expression
atoms =
    oneOf [ digits ]


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
                    succeed (Iterator symbol)
                        |= braces expression
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


parse : String -> Result Error Expression
parse string =
    run expression string
