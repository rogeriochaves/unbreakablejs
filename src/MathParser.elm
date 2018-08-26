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
        { int = Just EInt
        , hex = Nothing
        , octal = Nothing
        , binary = Nothing
        , float = Just EFloat
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
    [ [ infixOperator EExponentiation (symbol "^") AssocLeft ]
    , [ infixOperator EMul (symbol "*") AssocLeft, infixOperator EDiv (symbol "/") AssocLeft ]
    , [ infixOperator EAdd (symbol "+") AssocLeft, infixOperator ESub (symbol "-") AssocLeft ]
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
            )

        matchArities name =
            case findSymbols name of
                ( Just symbol, _ ) ->
                    succeed (SingleArity symbol)
                        |= braces expression

                ( Nothing, Just symbol ) ->
                    succeed (DoubleArity symbol)
                        |= braces expression
                        |= braces expression

                ( Nothing, Nothing ) ->
                    problem ("could not find symbol " ++ name)
    in
    succeed ESymbolicFunction
        |. symbol "\\"
        |= (identifier |> andThen matchArities)


type alias Error =
    List DeadEnd


parse : String -> Result Error Expression
parse string =
    run expression string
