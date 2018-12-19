module InterpreterTest exposing (suite)

import Expect exposing (Expectation)
import Interpreter exposing (..)
import MathParser exposing (..)
import Parser exposing (Problem(..))
import Return
import Test exposing (..)
import Types exposing (..)


suite : Test
suite =
    describe "Interpreter suite"
        [ describe "parsing and executing"
            [ test "sum integer numbers" <|
                \_ ->
                    parseAndRun "1 + 1"
                        |> isEq (Return.Num 2)
            , test "sum float numbers" <|
                \_ ->
                    parseAndRun "1.5 + 1.3"
                        |> isEq (Return.Num 2.8)
            , test "execute nested expressions" <|
                \_ ->
                    parseAndRun "1 - (3 - 2)"
                        |> isEq (Return.Num 0)
            , test "respects math priority" <|
                \_ ->
                    parseAndRun "2 + 3 * 2"
                        |> isEq (Return.Num 8)
            , test "respects math priority #2" <|
                \_ ->
                    parseAndRun "2 * 3 + 2"
                        |> isEq (Return.Num 8)
            , test "symbol function aplication with other expression" <|
                \_ ->
                    parseAndRun "\\sqrt{9} + 2"
                        |> isEq (Return.Num 5)
            , test "symbol function aplication on a expression" <|
                \_ ->
                    parseAndRun "\\sqrt{7 + 2}"
                        |> isEq (Return.Num 3)
            , test "exponentiation" <|
                \_ ->
                    parseAndRun "2 ^ 5"
                        |> isEq (Return.Num 32)
            , test "respects math priority #3" <|
                \_ ->
                    parseAndRun "2 * 3 ^ 5"
                        |> isEq (Return.Num 486)
            ]
        , describe "symbols"
            [ test "sqrt" <|
                \_ ->
                    parseAndRun "\\sqrt{9}"
                        |> isEq (Return.Num 3)
            , test "frac" <|
                \_ ->
                    parseAndRun "\\frac{3}{2}"
                        |> isEq (Return.Num 1.5)
            , test "summation" <|
                \_ ->
                    parseAndRun "\\sum_{x=1}^{3} 5"
                        |> isEq (Return.Num 15)
            , test "summation using the variable" <|
                \_ ->
                    parseAndRun "\\sum_{x=1}^{3} x + 1"
                        |> isEq (Return.Num 9)
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
                        |> isEq (Return.Expression (TripleArityApplication (Sum_ "x") (Number 1) (Number 7) (DoubleArityApplication Addition (Variable (ScalarIdentifier "y")) (Number 2))))
            ]
        , test "multiple expressions" <|
            \_ ->
                parseAndRun "1 + 1\n2 + 2"
                    |> Expect.equal (Ok [ Return.Num 2, Return.Num 4 ])
        , describe "assignments" <|
            [ test "parses a simple assignment and return void" <|
                \_ ->
                    parseAndRun "x = 2 + 2"
                        |> isEq Return.Void
            , test "saves the value to the variable" <|
                \_ ->
                    parseAndRun "x = 2 + 2\nx + 1"
                        |> Expect.equal (Ok [ Return.Void, Return.Num 5 ])
            , test "returns unapplied expression if the variable is not defined" <|
                \_ ->
                    parseAndRun "x + 1"
                        |> isEq (Return.Expression (DoubleArityApplication Addition (Variable (ScalarIdentifier "x")) (Number 1)))
            , test "applies the parts that can be calculated" <|
                \_ ->
                    parseAndRun "x + (1 + 1)"
                        |> isEq (Return.Expression (DoubleArityApplication Addition (Variable (ScalarIdentifier "x")) (Number 2)))
            , test "parses assignment with undefined variables" <|
                \_ ->
                    parseAndRun "x = y + (1 + 1)"
                        |> isEq
                            (Return.Expression
                                (SingleArityApplication (Assignment (ScalarIdentifier "x"))
                                    (DoubleArityApplication Addition (Variable (ScalarIdentifier "y")) (Number 2))
                                )
                            )
            ]
        , describe "functions"
            [ test "declares a simple function" <|
                \_ ->
                    parseAndRun "f(x) = x + 1\nf(5)"
                        |> Expect.equal (Ok [ Return.Void, Return.Num 6 ])
            , test "return unapplied expression if function is not defined" <|
                \_ ->
                    parseAndRun "f(x)"
                        |> isEq
                            (Return.Expression
                                (SingleArityApplication (Application (Variable (ScalarIdentifier "f"))) (Variable (ScalarIdentifier "x")))
                            )
            , test "return unapplied expression if function is not defined, but evaluate the params" <|
                \_ ->
                    parseAndRun "f(1 + 1)"
                        |> isEq
                            (Return.Expression
                                (SingleArityApplication (Application (Variable (ScalarIdentifier "f"))) (Number 2))
                            )
            ]
        , describe "vectors"
            [ test "reads a vector" <|
                \_ ->
                    parseAndRun "(1, 2, 3)"
                        |> isEq (Return.Vector [ Return.Num 1, Return.Num 2, Return.Num 3 ])
            , test "reads a vector with operations inside" <|
                \_ ->
                    parseAndRun "(1, 1 + 1, 3)"
                        |> isEq (Return.Vector [ Return.Num 1, Return.Num 2, Return.Num 3 ])
            ]
        ]


parseAndRun : String -> Result Error (List Return.Value)
parseAndRun code =
    MathParser.parse code
        |> Result.andThen Interpreter.run


isEq result =
    Expect.equal (Ok [ result ])
