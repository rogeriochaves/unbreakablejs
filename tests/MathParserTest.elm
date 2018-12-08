module MathParserTest exposing (suite)

import Expect exposing (Expectation)
import MathParser exposing (..)
import Test exposing (..)
import Types exposing (..)


suite : Test
suite =
    describe "MathParser suite"
        [ test "read int numbers" <|
            \_ ->
                MathParser.parse "1 + 1"
                    |> Expect.equal (Ok [ InfixFunction Addition (Number 1) (Number 1) ])
        , test "read float numbers" <|
            \_ ->
                MathParser.parse "1.5 + 1.3"
                    |> Expect.equal (Ok [ InfixFunction Addition (Number 1.5) (Number 1.3) ])
        , test "read nested operations" <|
            \_ ->
                MathParser.parse "1 - (3 - 2)"
                    |> Expect.equal (Ok [ InfixFunction Subtraction (Number 1) (InfixFunction Subtraction (Number 3) (Number 2)) ])
        , test "read single-arity symbolic function" <|
            \_ ->
                MathParser.parse "\\sqrt{5}"
                    |> Expect.equal (Ok [ SymbolicFunction (SingleArity Sqrt (Number 5)) ])
        , test "read double-arity symbolic function" <|
            \_ ->
                MathParser.parse "\\frac{2}{3}"
                    |> Expect.equal (Ok [ SymbolicFunction (DoubleArity Frac (Number 2) (Number 3)) ])

        -- https://www.overleaf.com/learn/latex/Integrals,_sums_and_limits#Sums_and_products
        , test "read iterator functions" <|
            \_ ->
                MathParser.parse "\\sum_{x = 1}^{3} 5"
                    |> Expect.equal (Ok [ SymbolicFunction (Iterator Sum_ "x" (Number 1) (Number 3) (Number 5)) ])
        , test "read symbol function aplication with other expression" <|
            \_ ->
                MathParser.parse "\\sqrt{9} + 2"
                    |> Expect.equal
                        (Ok
                            [ InfixFunction Addition
                                (SymbolicFunction (SingleArity Sqrt (Number 9)))
                                (Number 2)
                            ]
                        )
        , test "read exponentiation" <|
            \_ ->
                MathParser.parse "2 ^ 5"
                    |> Expect.equal (Ok [ InfixFunction Exponentiation (Number 2) (Number 5) ])
        , describe "multiple lines"
            [ test "parses multiple expressions" <|
                \_ ->
                    MathParser.parse "1 + 1\n2 + 2"
                        |> Expect.equal
                            (Ok
                                [ InfixFunction Addition (Number 1) (Number 1)
                                , InfixFunction Addition (Number 2) (Number 2)
                                ]
                            )
            , test "breaks for weird things after the end" <|
                \_ ->
                    MathParser.parse "1 + 1\n2 + 2[1$51"
                        |> isErr
                        |> Expect.true "it should break if anything is not parseable"
            , test "only one expression per line" <|
                \_ ->
                    MathParser.parse "1 + 1 2 + 2"
                        |> isErr
                        |> Expect.true "it should break there is no line break between expressions"
            , test "allow multiple line breaks" <|
                \_ ->
                    MathParser.parse "1 + 1\n\n2 + 2\n"
                        |> Expect.equal
                            (Ok
                                [ InfixFunction Addition (Number 1) (Number 1)
                                , InfixFunction Addition (Number 2) (Number 2)
                                ]
                            )
            , test "allow empty lines and trailing spaces" <|
                \_ ->
                    MathParser.parse "1 + 1 \n \n2 + 2\n"
                        |> Expect.equal
                            (Ok
                                [ InfixFunction Addition (Number 1) (Number 1)
                                , InfixFunction Addition (Number 2) (Number 2)
                                ]
                            )
            ]
        , describe "assignments"
            [ test "parses simple assignment" <|
                \_ ->
                    MathParser.parse "x = 1 + 1"
                        |> Expect.equal
                            (Ok [ Assignment "x" (InfixFunction Addition (Number 1) (Number 1)) ])
            , test "does not allow nested assignments" <|
                \_ ->
                    MathParser.parse "x = 1 + (x = 2)"
                        |> isErr
                        |> Expect.true "nested assignments"
            , test "parses expression with variables" <|
                \_ ->
                    MathParser.parse "x + 1"
                        |> Expect.equal
                            (Ok [ InfixFunction Addition (Identifier "x") (Number 1) ])
            ]
        , describe "functions"
            [ test "parses function declaration" <|
                \_ ->
                    MathParser.parse "f(x) = x + 1"
                        |> Expect.equal
                            (Ok [ FunctionDeclaration "f" (FunctionSchema "x" (InfixFunction Addition (Identifier "x") (Number 1))) ])
            , test "does not allow nested function declarations" <|
                \_ ->
                    MathParser.parse "fn(x) = fn(y) = 1"
                        |> isErr
                        |> Expect.true "nested functions"
            , test "parses function call" <|
                \_ ->
                    MathParser.parse "f(5)"
                        |> Expect.equal
                            (Ok [ FunctionCall "f" (Number 5) ])
            ]
        ]


isErr result =
    case result of
        Ok _ ->
            False

        Err _ ->
            True
