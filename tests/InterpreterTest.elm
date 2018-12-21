module InterpreterTest exposing (suite)

import Expect exposing (Expectation)
import Interpreter exposing (..)
import MathParser exposing (..)
import Parser exposing (Problem(..))
import Return exposing (Value(..))
import Test exposing (..)
import Types exposing (..)


suite : Test
suite =
    describe "Interpreter suite"
        [ describe "parsing and executing"
            [ test "sum integer numbers" <|
                \_ ->
                    parseAndRun "1 + 1"
                        |> isEq (Expression <| Number 2)
            , test "sum float numbers" <|
                \_ ->
                    parseAndRun "1.5 + 1.3"
                        |> isEq (Expression <| Number 2.8)
            , test "execute nested expressions" <|
                \_ ->
                    parseAndRun "1 - (3 - 2)"
                        |> isEq (Expression <| Number 0)
            , test "respects math priority" <|
                \_ ->
                    parseAndRun "2 + 3 * 2"
                        |> isEq (Expression <| Number 8)
            , test "respects math priority #2" <|
                \_ ->
                    parseAndRun "2 * 3 + 2"
                        |> isEq (Expression <| Number 8)
            , test "symbol function aplication with other expression" <|
                \_ ->
                    parseAndRun "\\sqrt{9} + 2"
                        |> isEq (Expression <| Number 5)
            , test "symbol function aplication on a expression" <|
                \_ ->
                    parseAndRun "\\sqrt{7 + 2}"
                        |> isEq (Expression <| Number 3)
            , test "exponentiation" <|
                \_ ->
                    parseAndRun "2 ^ 5"
                        |> isEq (Expression <| Number 32)
            , test "respects math priority #3" <|
                \_ ->
                    parseAndRun "2 * 3 ^ 5"
                        |> isEq (Expression <| Number 486)
            ]
        , describe "symbols"
            [ test "sqrt" <|
                \_ ->
                    parseAndRun "\\sqrt{9}"
                        |> isEq (Expression <| Number 3)
            , test "frac" <|
                \_ ->
                    parseAndRun "\\frac{3}{2}"
                        |> isEq (Expression <| Number 1.5)
            , test "summation" <|
                \_ ->
                    parseAndRun "\\sum_{x=1}^{3} 5"
                        |> isEq (Expression <| Number 15)
            , test "summation using the variable" <|
                \_ ->
                    parseAndRun "\\sum_{x=1}^{3} x + 1"
                        |> isEq (Expression <| Number 9)
            , test "summation with a float upper limit should break" <|
                \_ ->
                    parseAndRun "\\sum_{x=1}^{3.9} 5"
                        |> Expect.equal
                            (Err
                                [ { row = 0
                                  , col = 0
                                  , problem = Problem "Error on sum_: cannot use 3.9 as an upper limit, it has to be an integer higher than lower limit"
                                  }
                                ]
                            )
            , test "summation with a float lower limit should break" <|
                \_ ->
                    parseAndRun "\\sum_{x=1.9}^{3} 5"
                        |> Expect.equal
                            (Err
                                [ { row = 0
                                  , col = 0
                                  , problem = Problem "Error on sum_: cannot use 1.9 as a lower limit, it has to be an integer"
                                  }
                                ]
                            )
            , test "summation with a upper limit lower than lower limit" <|
                \_ ->
                    parseAndRun "\\sum_{x=1}^{0-5} 5"
                        |> Expect.equal
                            (Err
                                [ { row = 0
                                  , col = 0
                                  , problem = Problem "Error on sum_: cannot use -5 as an upper limit, it has to be an integer higher than lower limit"
                                  }
                                ]
                            )
            , test "summation with undefined variables" <|
                \_ ->
                    parseAndRun "\\sum_{x=1}^{7} y + (1 + 1)"
                        |> isEq (Expression (TripleArity (Sum_ "x") (Number 1) (Number 7) (DoubleArity Addition (Variable (ScalarIdentifier "y")) (Number 2))))
            ]
        , test "multiple expressions" <|
            \_ ->
                parseAndRun "1 + 1\n2 + 2"
                    |> Expect.equal (Ok [ Expression <| Number 2, Expression <| Number 4 ])
        , describe "assignments" <|
            [ test "parses a simple assignment and return void" <|
                \_ ->
                    parseAndRun "x = 2 + 2"
                        |> isEq Void
            , test "saves the value to the variable" <|
                \_ ->
                    parseAndRun "x = 2 + 2\nx + 1"
                        |> Expect.equal (Ok [ Void, Expression <| Number 5 ])
            , test "returns unapplied expression if the variable is not defined" <|
                \_ ->
                    parseAndRun "x + 1"
                        |> isEq (Expression (DoubleArity Addition (Variable (ScalarIdentifier "x")) (Number 1)))
            , test "applies the parts that can be calculated" <|
                \_ ->
                    parseAndRun "x + (1 + 1)"
                        |> isEq (Expression (DoubleArity Addition (Variable (ScalarIdentifier "x")) (Number 2)))
            , test "parses assignment with undefined variables" <|
                \_ ->
                    parseAndRun "x = y + (1 + 1)"
                        |> isEq
                            (Expression
                                (SingleArity (Assignment (ScalarIdentifier "x"))
                                    (DoubleArity Addition (Variable (ScalarIdentifier "y")) (Number 2))
                                )
                            )
            ]
        , describe "functions"
            [ test "declares a simple function" <|
                \_ ->
                    parseAndRun "f(x) = x + 1\nf(5)"
                        |> Expect.equal (Ok [ Void, Expression <| Number 6 ])
            , test "return unapplied expression if function is not defined" <|
                \_ ->
                    parseAndRun "f(x)"
                        |> isEq
                            (Expression
                                (SingleArity (Application (Variable (ScalarIdentifier "f"))) (Variable (ScalarIdentifier "x")))
                            )
            , test "return unapplied expression if function is not defined, but evaluate the params" <|
                \_ ->
                    parseAndRun "f(1 + 1)"
                        |> isEq
                            (Expression
                                (SingleArity (Application (Variable (ScalarIdentifier "f"))) (Number 2))
                            )
            ]
        , describe "vectors"
            [ test "reads a vector" <|
                \_ ->
                    parseAndRun "(1, 2, 3)"
                        |> isEq (Expression <| Vector [ Number 1, Number 2, Number 3 ])
            , test "reads a vector with operations inside" <|
                \_ ->
                    parseAndRun "(1, 1 + 1, 3)"
                        |> isEq (Expression <| Vector [ Number 1, Number 2, Number 3 ])
            , test "parses a simple assignment and return void" <|
                \_ ->
                    parseAndRun "\\vec{x} = (1, 2, 3)"
                        |> isEq Void
            , test "saves the value to the variable" <|
                \_ ->
                    parseAndRun "\\vec{x} = (1, 2, 3)\n\\vec{x}"
                        |> Expect.equal (Ok [ Void, Expression <| Vector [ Number 1, Number 2, Number 3 ] ])
            , test "calls a function with vec as param" <|
                \_ ->
                    parseAndRun "\\vec{x} = (1, 2, 3)\nf(\\vec{y}) = \\vec{y}\nf(\\vec{x})"
                        |> Expect.equal
                            (Ok
                                [ Void
                                , Void
                                , Expression <|
                                    Vector [ Number 1, Number 2, Number 3 ]
                                ]
                            )
            , test "replaces variables inside vector" <|
                \_ ->
                    parseAndRun "f(x) = (1, x, 3)\nf(2)"
                        |> Expect.equal
                            (Ok
                                [ Void
                                , Expression <|
                                    Vector [ Number 1, Number 2, Number 3 ]
                                ]
                            )
            , test "calling a vector function with a scalar argument should fail" <|
                \_ ->
                    parseAndRun "f(\\vec{y}) = \\vec{y}\nf(5)"
                        |> Expect.equal
                            (Err
                                [ { row = 0
                                  , col = 0
                                  , problem = Problem "Vector expected"
                                  }
                                ]
                            )
            , test "validation of param type should also work for indirect cases" <|
                \_ ->
                    parseAndRun "f(\\vec{y}) = \\vec{y}\ng(x) = x + 1\nf(g(5))"
                        |> Expect.equal
                            (Err
                                [ { row = 0
                                  , col = 0
                                  , problem = Problem "Vector expected"
                                  }
                                ]
                            )
            , test "validation of param type should also work for indirect cases 2" <|
                \_ ->
                    parseAndRun "f(\\vec{y}) = \\vec{y}\ng(x) = (1, x, 3)\nf(g(2))"
                        |> Expect.equal
                            (Ok
                                [ Void
                                , Void
                                , Expression <|
                                    Vector [ Number 1, Number 2, Number 3 ]
                                ]
                            )
            ]
        ]


parseAndRun : String -> Result Error (List Return.Value)
parseAndRun code =
    MathParser.parse code
        |> Result.andThen Interpreter.run


isEq result =
    Expect.equal (Ok [ result ])
