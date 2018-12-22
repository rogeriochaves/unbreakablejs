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
        |. symbol "\\vec"
        |= braces scalarIdentifier


scalarIdentifier : Parser String
scalarIdentifier =
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
    [ [ infixOp Exponentiation (symb "^") AssocLeft ]
    , [ infixOp Multiplication (symb "*") AssocLeft, infixOp Division (symb "/") AssocLeft ]
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
            [ backtrackable <| parens <| lazy (\_ -> expression)
            , functionCall
            , atoms
            , vectors
            , symbolicFunction
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
        |= (scalarIdentifier |> andThen matchArities)


parse : String -> Result Error Types.Program
parse string =
    run program (string ++ "\nEOF")
