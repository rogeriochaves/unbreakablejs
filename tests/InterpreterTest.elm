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
            , test "negation" <|
                \_ ->
                    parseAndRun "5 * -3"
                        |> isEq (Expression <| Number -15)
            , test "factorial" <|
                \_ ->
                    parseAndRun "5!"
                        |> isEq (Expression <| Number 120)
            , test "respects math priority #4" <|
                \_ ->
                    parseAndRun "-5!"
                        |> isEq (Expression <| Number -120)
            , test "factorial should break for float numbers" <|
                \_ ->
                    parseAndRun "5.1!"
                        |> isErr "Cannot calculate factorial for 5.1, only for positive integers"
            , test "factorial should break for negative numbers" <|
                \_ ->
                    parseAndRun "(-5)!"
                        |> isErr "Cannot calculate factorial for -5, only for positive integers"
            , test "factorial of 0 is 1" <|
                \_ ->
                    parseAndRun "0!"
                        |> isEq (Expression <| Number 1)
            , test "respects math priority #5" <|
                \_ ->
                    parseAndRun "2 ^ 5 * 4"
                        |> isEq (Expression <| Number 128)
            , test "evaluates modulo" <|
                \_ ->
                    parseAndRun "5 \\mod 2"
                        |> isEq (Expression <| Number 1)
            ]
        , describe "constants"
            [ test "starts with euler number" <|
                \_ ->
                    parseAndRun "e"
                        |> isEq (Expression <| Number 2.718281828459045)
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
                        |> isErr "Error on sum_: cannot use 3.9 as an upper limit, it has to be an integer higher than lower limit"
            , test "summation with a float lower limit should break" <|
                \_ ->
                    parseAndRun "\\sum_{x=1.9}^{3} 5"
                        |> isErr "Error on sum_: cannot use 1.9 as a lower limit, it has to be an integer"
            , test "summation with a upper limit lower than lower limit" <|
                \_ ->
                    parseAndRun "\\sum_{x=1}^{0-5} 5"
                        |> isErr "Error on sum_: cannot use -5 as an upper limit, it has to be an integer higher than lower limit"
            , test "summation with undefined variables" <|
                \_ ->
                    parseAndRun "\\sum_{x=1}^{7} y + (1 + 1)"
                        |> isEq (Expression (TripleArity (Sum_ "x") (Number 1) (Number 7) (DoubleArity Addition (Variable (ScalarIdentifier "y")) (Number 2))))
            ]
        , test "multiple expressions" <|
            \_ ->
                parseAndRun "1 + 1\n2 + 2"
                    |> Result.map (List.map Tuple.second)
                    |> Expect.equal (Ok [ Expression <| Number 2, Expression <| Number 4 ])
        , describe "assignments" <|
            [ test "parses a simple assignment and return void" <|
                \_ ->
                    parseAndRun "x = 2 + 2"
                        |> isEq Void
            , test "saves the value to the variable" <|
                \_ ->
                    parseAndRun "x = 2 + 2\nx + 1"
                        |> Result.map (List.map Tuple.second)
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
                        |> Result.map (List.map Tuple.second)
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
            , test "return unapplied expression if params also cannot be evaluated" <|
                \_ ->
                    parseAndRun "f(x) = x + 1\nf(1+y)"
                        |> Result.map (List.map Tuple.second)
                        |> Expect.equal
                            (Ok
                                [ Void
                                , Expression
                                    (SingleArity (Application (Variable (ScalarIdentifier "f")))
                                        (DoubleArity Addition (Number 1) (Variable (ScalarIdentifier "y")))
                                    )
                                ]
                            )
            , test "return unapplied expression if params also cannot be evaluated for vectors" <|
                \_ ->
                    parseAndRun "f(\\vec{x}) = x + 1\nf(g(1))"
                        |> Result.map (List.map Tuple.second)
                        |> Expect.equal
                            (Ok
                                [ Void
                                , Expression
                                    (SingleArity (Application (Variable (ScalarIdentifier "f")))
                                        (SingleArity (Application (Variable (ScalarIdentifier "g"))) (Number 1))
                                    )
                                ]
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
                        |> Result.map (List.map Tuple.second)
                        |> Expect.equal (Ok [ Void, Expression <| Vector [ Number 1, Number 2, Number 3 ] ])
            , test "calls a function with vec as param" <|
                \_ ->
                    parseAndRun "\\vec{x} = (1, 2, 3)\nf(\\vec{y}) = \\vec{y}\nf(\\vec{x})"
                        |> Result.map (List.map Tuple.second)
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
                        |> Result.map (List.map Tuple.second)
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
                        |> isErr "Vector expected"
            , test "validation of param type should also work for indirect cases" <|
                \_ ->
                    parseAndRun "f(\\vec{y}) = \\vec{y}\ng(x) = x + 1\nf(g(5))"
                        |> isErr "Vector expected"
            , test "validation of param type should also work for indirect cases 2" <|
                \_ ->
                    parseAndRun "f(\\vec{y}) = \\vec{y}\ng(x) = (1, x, 3)\nf(g(2))"
                        |> Result.map (List.map Tuple.second)
                        |> Expect.equal
                            (Ok
                                [ Void
                                , Void
                                , Expression <|
                                    Vector [ Number 1, Number 2, Number 3 ]
                                ]
                            )
            , test "dont assign vector to scalar variables" <|
                \_ ->
                    parseAndRun "x = (1, 2, 3)"
                        |> isErr "Cannot assign vector to scalar variables, use \\vec{x} instead"
            , test "dont assign scalar to vector variables" <|
                \_ ->
                    parseAndRun "\\vec{x} = 1 + 1"
                        |> isErr "Cannot assign scalar to vector variables"
            , test "parses assignment with undefined variables" <|
                \_ ->
                    parseAndRun "\\vec{x} = y + (1 + 1)"
                        |> isEq
                            (Expression
                                (SingleArity (Assignment (VectorIdentifier "x"))
                                    (DoubleArity Addition (Variable (ScalarIdentifier "y")) (Number 2))
                                )
                            )
            , test "gets index position from a vector, 1-based index (unfortunatly math is like that 🙁)" <|
                \_ ->
                    parseAndRun "(3, 2, 1)_{1}"
                        |> isEq (Expression (Number 3))
            , test "breaks if index is not integer" <|
                \_ ->
                    parseAndRun "(3, 2, 1)_{3/2}"
                        |> isErr "Cannot use 1.5 as an index, it has to be a positive integer"
            , test "breaks if index is out bound" <|
                \_ ->
                    parseAndRun "(3, 2, 1)_{5}"
                        |> isErr "Index 5 out of bounds"
            , test "breaks for numbers smaller than 1" <|
                \_ ->
                    parseAndRun "(3, 2, 1)_{0}"
                        |> isErr "Cannot use 0 as an index, it has to be a positive integer"
            , test "returns unapplied index for undefined variable" <|
                \_ ->
                    parseAndRun "(1, 2, 3)_{x}"
                        |> isEq
                            (Expression
                                (DoubleArity Index
                                    (Vector [ Number 1, Number 2, Number 3 ])
                                    (Variable (ScalarIdentifier "x"))
                                )
                            )
            , test "evaluates indexes from vector variables in scalar context" <|
                \_ ->
                    parseAndRun "\\vec{x} = (3, 2, 1)\nx_{1}"
                        |> Result.map (List.map Tuple.second)
                        |> Expect.equal (Ok [ Void, Expression (Number 3) ])
            , test "evaluates map function" <|
                \_ ->
                    parseAndRun "f(\\vec{x})_{i} = x_{i} + 1\nf((1,2,3))"
                        |> Result.map (List.map Tuple.second)
                        |> Expect.equal (Ok [ Void, Expression (Vector [ Number 2, Number 3, Number 4 ]) ])
            , test "evaluates a vector summation" <|
                \_ ->
                    parseAndRun "\\mathbf{x} = (1, 2, 3)\n\\sum{\\mathbf{x}}"
                        |> Result.map (List.map Tuple.second)
                        |> Expect.equal (Ok [ Void, Expression (Number 6) ])
            , test "expands a summation as far as it can" <|
                \_ ->
                    parseAndRun "\\mathbf{x} = (1, 2, y)\n\\sum{\\mathbf{x}}"
                        |> Result.map (List.map Tuple.second)
                        |> Expect.equal (Ok [ Void, Expression (DoubleArity Addition (Number 3) (Variable (ScalarIdentifier "y"))) ])
            , test "does not enter an infinite loop trying to expand undefined vars" <|
                \_ ->
                    parseAndRun "\\sum{\\mathbf{x}}"
                        |> isEq (Expression (SingleArity Summation (Variable (VectorIdentifier "x"))))
            , test "evaluates cardinality" <|
                \_ ->
                    parseAndRun "\\mathbf{a} = (x, y, z)\n|\\mathbf{a}|"
                        |> Result.map (List.map Tuple.second)
                        |> Expect.equal (Ok [ Void, Expression (Number 3) ])
            ]
        ]


parseAndRun : String -> Result Error (List LineResult)
parseAndRun code =
    MathParser.parse code
        |> Result.andThen (Interpreter.run newState)


isEq expected actual =
    actual
        |> Result.map (List.map Tuple.second)
        |> Expect.equal (Ok [ expected ])


isErr msg =
    Expect.equal
        (Err
            [ { row = 0
              , col = 0
              , problem = Problem msg
              }
            ]
        )
