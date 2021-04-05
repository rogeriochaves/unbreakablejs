module InterpreterTest exposing (suite)

import AstParser exposing (..)
import Expect
import Interpreter exposing (..)
import Parser exposing (Problem(..))
import Test exposing (..)
import Types exposing (..)


trackInfo : ( Int, Int ) -> TrackInfo
trackInfo ( line, column ) =
    { line = line, column = column, filename = "test.us" }


undefinedTrack : ( Int, Int ) -> UndefinedReason -> UndefinedTrackInfo
undefinedTrack ( line, column ) reason =
    { line = line, column = column, filename = "test.us", reason = reason }


tracked : ( Int, Int ) -> UntrackedExp -> Expression
tracked =
    Tracked << trackInfo


suite : Test
suite =
    describe "Interpreter suite"
        [ describe "parsing and executing"
            [ test "sum integer numbers" <|
                \_ ->
                    parseAndRun "1 + 1"
                        |> isEq (Number 2)
            , test "sum float numbers" <|
                \_ ->
                    parseAndRun "1.5 + 1.3"
                        |> isEq (Number 2.8)
            , test "execute nested expressions" <|
                \_ ->
                    parseAndRun "1 - (3 - 2)"
                        |> isEq (Number 0)

            --     , test "respects math priority" <|
            --         \_ ->
            --             parseAndRun "2 + 3 * 2"
            --                 |> isEq (Number 8)
            --     , test "respects math priority #2" <|
            --         \_ ->
            --             parseAndRun "2 * 3 + 2"
            --                 |> isEq (Number 8)
            --     , test "symbol function aplication with other expression" <|
            --         \_ ->
            --             parseAndRun "\\sqrt{9} + 2"
            --                 |> isEq (Number 5)
            --     , test "symbol function aplication on a expression" <|
            --         \_ ->
            --             parseAndRun "\\sqrt{7 + 2}"
            --                 |> isEq (Number 3)
            --     , test "exponentiation" <|
            --         \_ ->
            --             parseAndRun "2 ^ 5"
            --                 |> isEq (Number 32)
            --     , test "respects math priority #3" <|
            --         \_ ->
            --             parseAndRun "2 * 3 ^ 5"
            --                 |> isEq (Number 486)
            --     , test "negation" <|
            --         \_ ->
            --             parseAndRun "5 * -3"
            --                 |> isEq (Number -15)
            --     , test "factorial" <|
            --         \_ ->
            --             parseAndRun "5!"
            --                 |> isEq (Number 120)
            --     , test "respects math priority #4" <|
            --         \_ ->
            --             parseAndRun "-5!"
            --                 |> isEq (Number -120)
            --     , test "factorial should break for float numbers" <|
            --         \_ ->
            --             parseAndRun "5.1!"
            --                 |> isErr "Cannot calculate factorial for 5.1, only for positive integers"
            --     , test "factorial should break for negative numbers" <|
            --         \_ ->
            --             parseAndRun "(-5)!"
            --                 |> isErr "Cannot calculate factorial for -5, only for positive integers"
            --     , test "factorial of 0 is 1" <|
            --         \_ ->
            --             parseAndRun "0!"
            --                 |> isEq (Number 1)
            --     , test "respects math priority #5" <|
            --         \_ ->
            --             parseAndRun "2 ^ 5 * 4"
            --                 |> isEq (Number 128)
            --     , test "evaluates modulo" <|
            --         \_ ->
            --             parseAndRun "5 \\mod 2"
            --                 |> isEq (Number 1)
            --     , test "evaluates euclidean division" <|
            --         \_ ->
            --             parseAndRun "5 \\div 2"
            --                 |> isEq (Number 2)
            --     ]
            -- , describe "constants"
            --     [ test "starts with euler number" <|
            --         \_ ->
            --             parseAndRun "e"
            --                 |> isEq (Number 2.718281828459045)
            --     , test "starts with pi number" <|
            --         \_ ->
            --             parseAndRun "\\pi"
            --                 |> isEq (Number 3.141592653589793)
            --     ]
            -- , describe "symbols"
            --     [ test "sqrt" <|
            --         \_ ->
            --             parseAndRun "\\sqrt{9}"
            --                 |> isEq (Number 3)
            --     , test "frac" <|
            --         \_ ->
            --             parseAndRun "\\frac{3}{2}"
            --                 |> isEq (Number 1.5)
            --     , test "summation" <|
            --         \_ ->
            --             parseAndRun "\\sum_{x=1}^{3} 5"
            --                 |> isEq (Number 15)
            --     , test "summation using the variable" <|
            --         \_ ->
            --             parseAndRun "\\sum_{x=1}^{3} x + 1"
            --                 |> isEq (Number 9)
            --     , test "summation with a float upper limit should break" <|
            --         \_ ->
            --             parseAndRun "\\sum_{x=1}^{3.9} 5"
            --                 |> isErr "Error on sum_: cannot use 3.9 as an upper limit, it has to be an integer higher than lower limit"
            --     , test "summation with a float lower limit should break" <|
            --         \_ ->
            --             parseAndRun "\\sum_{x=1.9}^{3} 5"
            --                 |> isErr "Error on sum_: cannot use 1.9 as a lower limit, it has to be an integer"
            --     , test "summation with a upper limit lower than lower limit" <|
            --         \_ ->
            --             parseAndRun "\\sum_{x=1}^{0-5} 5"
            --                 |> isErr "Error on sum_: cannot use -5 as an upper limit, it has to be an integer higher than lower limit"
            --     , test "summation with undefined variables" <|
            --         \_ ->
            --             parseAndRun "\\sum_{x=1}^{7} y + (1 + 1)"
            --                 |> isEq (Expression (TripleArity (Sum_ "x") (Number 1) (Number 7) (DoubleArity Addition (Variable (ScalarIdentifier "y")) (Number 2))))
            --     ]
            -- , test "multiple expressions" <|
            --     \_ ->
            --         parseAndRun "1 + 1\n2 + 2"
            --             |> Result.map (List.map Tuple.second)
            --             |> Expect.equal (Ok [ Number 2, Number 4 ])
            , describe "assignments" <|
                [ test "parses a simple assignment and return the result" <|
                    \_ ->
                        parseAndRun "x = 2 + 2"
                            |> isEq (Number 4)
                , test "saves the value to the variable" <|
                    \_ ->
                        parseAndRun "x = 2 + 2\nx + 1"
                            |> isLastEq (Number 5)
                , test "interprets a let assignment" <|
                    \_ ->
                        parseAndRun "let x = 1\nx"
                            |> isLastEq (Number 1)
                , test "keeps let assignment inside scope" <|
                    \_ ->
                        parseAndRun "fn = () => { let x = 1 }\nfn()\nx"
                            |> isLastEq
                                (Undefined [ undefinedTrack ( 3, 1 ) (VariableNotDefined "x") ])
                , test "but non-let assignments modify outside scope" <|
                    \_ ->
                        parseAndRun "fn = () => { x = 1 }\nfn()\nx"
                            |> isLastEq (Number 1)
                , test "but allows mutations if defined on higher scope" <|
                    \_ ->
                        parseAndRun "let x = 1\nfn = () => { x = 2 }\nfn()\nx"
                            |> isLastEq (Number 2)
                , test "although keeping to local scope if there is shadowing" <|
                    \_ ->
                        parseAndRun "let x = 1\nfn = () => { let x = 2 }\nfn()\nx"
                            |> isLastEq (Number 1)
                , test "and reassignment without let" <|
                    \_ ->
                        parseAndRun "let x = 1\nfn = () => { let x = 2\nx = 3 }\nfn()\nx"
                            |> isLastEq (Number 1)
                , test "values can outlive the scope" <|
                    \_ ->
                        parseAndRun "let x = 1\nfn = () => { let x = 2\nx = 3\nreturn x }\nfn()"
                            |> isLastEq (Number 3)
                , test "and changed on different levels" <|
                    \_ ->
                        parseAndRun "let x = 1\nfn = () => { let x = 2\ng = () => { x = 3 }\ng()\nreturn x }\nfn()\nx"
                            |> isLastEq (Number 1)
                , test "and changed on different levels and outlive the innermost scopes" <|
                    \_ ->
                        parseAndRun "let x = 1\nfn = () => { let x = 2\ng = () => { x = 3 }\ng()\nreturn x }\nfn()"
                            |> isLastEq (Number 3)
                , test "assignments change state also when inside function arguments" <|
                    \_ ->
                        parseAndRun "x = 1\nf = () => { x = 10 }\ng(f())\nx"
                            |> isLastEq (Number 10)
                , test "assignments change state also when inside function arguments #2" <|
                    \_ ->
                        parseAndRun "x = 1\nf = () => { x = 10 }\ng = () => {}\ng(f())\nx"
                            |> isLastEq (Number 10)
                , test "assignments change state also when inside function arguments #3" <|
                    \_ ->
                        parseAndRun "x = 1\nf = () => { x = 10\nreturn 5 }\nz = f()\nx"
                            |> isLastEq (Number 10)
                , test "assignments change state also when inside function arguments #4" <|
                    \_ ->
                        parseAndRun "x = 1\nf = () => { x = 10 }\ng = () => x\ng(f())\nx"
                            |> isLastEq (Number 10)
                , test "assignments change state also when inside operation arguments" <|
                    \_ ->
                        parseAndRun "x = 1\nf = () => { x = 10; return 5 }\nf() + x\nx"
                            |> isLastEq (Number 10)
                , test "assignments change state also when inside if condition" <|
                    \_ ->
                        parseAndRun "x = 1\nf = () => { x = 10; return 5 }\nif(f()){}\nx"
                            |> isLastEq (Number 10)
                , test "assignments change state also when inside another assignment" <|
                    \_ ->
                        parseAndRun "x = 1\nf = () => { x = 10; return 5 }\ny = f()\nx"
                            |> isLastEq (Number 10)
                ]

            --     , test "returns unapplied expression if the variable is not defined" <|
            --         \_ ->
            --             parseAndRun "x + 1"
            --                 |> isEq (Expression (DoubleArity Addition (Variable (ScalarIdentifier "x")) (Number 1)))
            --     , test "applies the parts that can be calculated" <|
            --         \_ ->
            --             parseAndRun "x + (1 + 1)"
            --                 |> isEq (Expression (DoubleArity Addition (Variable (ScalarIdentifier "x")) (Number 2)))
            --     , test "parses assignment with undefined variables" <|
            --         \_ ->
            --             parseAndRun "x = y + (1 + 1)"
            --                 |> isEq
            --                     (Expression
            --                         (SingleArity (Assignment (ScalarIdentifier "x"))
            --                             (DoubleArity Addition (Variable (ScalarIdentifier "y")) (Number 2))
            --                         )
            --                     )
            --     ]
            , describe "functions"
                [ test "declares a simple function" <|
                    \_ ->
                        parseAndRun "f = (x) => x + 1\nf(5)"
                            |> Result.map (List.map .result)
                            |> Expect.equal
                                (Ok
                                    [ Abstraction [ "x" ]
                                        (tracked ( 1, 14 )
                                            (Operation2 Addition
                                                (tracked ( 1, 12 ) (Variable "x"))
                                                (Untracked (Value (Number 1)))
                                            )
                                        )
                                    , Number 6
                                    ]
                                )

                -- , test "declares a function with multiple arguments" <|
                --     \_ ->
                --         parseAndRun "f = (x, y) => x + y\nf(3, 2)"
                --             |> Result.map (List.map Tuple.second)
                --             |> Expect.equal (Ok [ Untracked <| Value Undefined, Untracked <| Value (Number 5) ])
                , test "returns undefined when missing params" <|
                    \_ ->
                        parseAndRun "f = (x, y) => x + y\nf(3)"
                            |> isLastEq
                                (Undefined
                                    [ undefinedTrack ( 2, 2 ) (MissingPositionalArgument 1 "y")
                                    , undefinedTrack ( 1, 17 ) (OperationWithUndefined "addition")
                                    ]
                                )
                , test "returns undefined if function is not defined" <|
                    \_ ->
                        parseAndRun "f(x)"
                            |> isEq (Undefined [ undefinedTrack ( 1, 1 ) (VariableNotDefined "f") ])
                , test "accumulates undefined stack" <|
                    \_ ->
                        parseAndRun "f = (x, y) => x + y\nf(g, 1)"
                            |> isLastEq
                                (Undefined
                                    [ undefinedTrack ( 2, 3 ) (VariableNotDefined "g")
                                    , undefinedTrack ( 1, 17 ) (OperationWithUndefined "addition")
                                    ]
                                )
                , test "accumulates undefined stack of functions" <|
                    \_ ->
                        parseAndRun "f = (x, y) => x + y\nf(g(3), 1)"
                            |> isLastEq
                                (Undefined
                                    [ undefinedTrack ( 2, 3 ) (VariableNotDefined "g")
                                    , undefinedTrack ( 1, 17 ) (OperationWithUndefined "addition")
                                    ]
                                )
                , test "accumulates undefined stack on second param" <|
                    \_ ->
                        parseAndRun "f = (x, y) => x + y\nf(3, g(1))"
                            |> isLastEq
                                (Undefined
                                    [ undefinedTrack ( 2, 6 ) (VariableNotDefined "g")
                                    , undefinedTrack ( 1, 17 ) (OperationWithUndefined "addition")
                                    ]
                                )
                , test "calls a curried functions" <|
                    \_ ->
                        parseAndRun "f = (x) => (y) => x + y\nf(5)(6)"
                            |> isLastEq (Number 11)
                , test "does not keep variable hanging around" <|
                    \_ ->
                        parseAndRun "f = (x) => x + 1; f(5); x"
                            |> isLastEq (Undefined [ undefinedTrack ( 1, 25 ) (VariableNotDefined "x") ])
                , test "calls a curried functions #2" <|
                    \_ ->
                        parseAndRun "sum = () => { let x = 10; let ret = (y) => x + y; x = 15; return ret }; sum()(4)"
                            |> isLastEq (Number 19)
                ]

            --     , test "return unapplied expression if function is not defined, but evaluate the params" <|
            --         \_ ->
            --             parseAndRun "f(1 + 1)"
            --                 |> isEq
            --                     (Expression
            --                         (SingleArity (Application (Variable (ScalarIdentifier "f"))) (Number 2))
            --                     )
            --     , test "return unapplied expression if params also cannot be evaluated" <|
            --         \_ ->
            --             parseAndRun "f(x) = x + 1\nf(1+y)"
            --                 |> Result.map (List.map Tuple.second)
            --                 |> Expect.equal
            --                     (Ok
            --                         [ Undefined
            --                         , Expression
            --                             (SingleArity (Application (Variable (ScalarIdentifier "f")))
            --                                 (DoubleArity Addition (Number 1) (Variable (ScalarIdentifier "y")))
            --                             )
            --                         ]
            --                     )
            --     , test "return unapplied expression if params also cannot be evaluated for vectors" <|
            --         \_ ->
            --             parseAndRun "f(\\vec{x}) = x + 1\nf(g(1))"
            --                 |> Result.map (List.map Tuple.second)
            --                 |> Expect.equal
            --                     (Ok
            --                         [ Undefined
            --                         , Expression
            --                             (SingleArity (Application (Variable (ScalarIdentifier "f")))
            --                                 (SingleArity (Application (Variable (ScalarIdentifier "g"))) (Number 1))
            --                             )
            --                         ]
            --                     )
            --     ]
            , describe "arrays"
                [ test "reads an array" <|
                    \_ ->
                        parseAndRun "[1, 2, 3]"
                            |> isEq (Array [ Number 1, Number 2, Number 3 ])
                , test "reads a position in an array" <|
                    \_ ->
                        parseAndRun "a = [1, 2, 3]; a[1]"
                            |> isLastEq (Number 2)
                , test "out of range positions return undefined" <|
                    \_ ->
                        parseAndRun "a = [1, 2, 3]; a[5]"
                            |> isLastEq (Undefined [ undefinedTrack ( 1, 17 ) (KeyNotInObject (Array [ Number 1, Number 2, Number 3 ]) (Number 5)) ])
                , test "float positions return undefined" <|
                    \_ ->
                        parseAndRun "a = [1, 2, 3]; a[5.1]"
                            |> isLastEq (Undefined [ undefinedTrack ( 1, 17 ) (KeyNotInObject (Array [ Number 1, Number 2, Number 3 ]) (Number 5.1)) ])
                , test "string positions return undefined" <|
                    \_ ->
                        parseAndRun "a = [1, 2, 3]; a['foo']"
                            |> isLastEq (Undefined [ undefinedTrack ( 1, 17 ) (KeyNotInObject (Array [ Number 1, Number 2, Number 3 ]) (String "foo")) ])
                , test "multiple out of range positions accumulate stack" <|
                    \_ ->
                        parseAndRun "a = undefined; a[4][5]"
                            |> isLastEq
                                (Undefined
                                    [ undefinedTrack ( 1, 5 ) ExplicitUndefined
                                    , undefinedTrack ( 1, 17 ) (KeyNotInObject (Undefined [ undefinedTrack ( 1, 5 ) ExplicitUndefined ]) (Number 4))
                                    , undefinedTrack ( 1, 20 )
                                        (KeyNotInObject
                                            (Undefined
                                                [ undefinedTrack ( 1, 5 ) ExplicitUndefined
                                                , undefinedTrack ( 1, 17 ) (KeyNotInObject (Undefined [ undefinedTrack ( 1, 5 ) ExplicitUndefined ]) (Number 4))
                                                ]
                                            )
                                            (Number 5)
                                        )
                                    ]
                                )
                , test "already undefined key accumulate stack" <|
                    \_ ->
                        parseAndRun "a = undefined; [1,2,3][a]"
                            |> isLastEq
                                (Undefined
                                    [ undefinedTrack ( 1, 5 ) ExplicitUndefined
                                    , undefinedTrack ( 1, 23 )
                                        (KeyNotInObject (Array [ Number 1, Number 2, Number 3 ])
                                            (Undefined
                                                [ undefinedTrack ( 1, 5 ) ExplicitUndefined ]
                                            )
                                        )
                                    ]
                                )
                , test "reads a string position" <|
                    \_ ->
                        parseAndRun "'foo'[2]"
                            |> isLastEq (String "o")
                , test "returns undefined for out of bounds string index" <|
                    \_ ->
                        parseAndRun "'foo'[3]"
                            |> isLastEq
                                (Undefined
                                    [ undefinedTrack ( 1, 6 )
                                        (KeyNotInObject (String "foo") (Number 3))
                                    ]
                                )
                ]

            --     , test "reads a vector with operations inside" <|
            --         \_ ->
            --             parseAndRun "(1, 1 + 1, 3)"
            --                 |> isEq (Array [ Number 1, Number 2, Number 3 ])
            --     , test "parses a simple assignment and return Undefined" <|
            --         \_ ->
            --             parseAndRun "\\vec{x} = (1, 2, 3)"
            --                 |> isEq (Array [ Number 1, Number 2, Number 3 ])
            --     , test "saves the value to the variable" <|
            --         \_ ->
            --             parseAndRun "\\vec{x} = (1, 2, 3)\n\\vec{x}"
            --                 |> isLastEq (Array [ Number 1, Number 2, Number 3 ])
            --     , test "calls a function with vec as param" <|
            --         \_ ->
            --             parseAndRun "\\vec{x} = (1, 2, 3)\nf(\\vec{y}) = \\vec{y}\nf(\\vec{x})"
            --                 |> isLastEq (Array [ Number 1, Number 2, Number 3 ])
            --     , test "replaces variables inside vector" <|
            --         \_ ->
            --             parseAndRun "f(x) = (1, x, 3)\nf(2)"
            --                 |> Result.map (List.map Tuple.second)
            --                 |> Expect.equal
            --                     (Ok
            --                         [ Undefined
            --                         , Expression <|
            --                             Array [ Number 1, Number 2, Number 3 ]
            --                         ]
            --                     )
            --     , test "calling a vector function with a scalar argument should fail" <|
            --         \_ ->
            --             parseAndRun "f(\\vec{y}) = \\vec{y}\nf(5)"
            --                 |> isErr "Array expected"
            --     , test "validation of param type should also work for indirect cases" <|
            --         \_ ->
            --             parseAndRun "f(\\vec{y}) = \\vec{y}\ng(x) = x + 1\nf(g(5))"
            --                 |> isErr "Array expected"
            --     , test "validation of param type should also work for indirect cases 2" <|
            --         \_ ->
            --             parseAndRun "f(\\vec{y}) = \\vec{y}\ng(x) = (1, x, 3)\nf(g(2))"
            --                 |> Result.map (List.map Tuple.second)
            --                 |> Expect.equal
            --                     (Ok
            --                         [ Undefined
            --                         , Undefined
            --                         , Expression <|
            --                             Array [ Number 1, Number 2, Number 3 ]
            --                         ]
            --                     )
            --     , test "dont assign vector to scalar variables" <|
            --         \_ ->
            --             parseAndRun "x = (1, 2, 3)"
            --                 |> isErr "Cannot assign vector to scalar variables, use \\vec{x} instead"
            --     , test "dont assign scalar to vector variables" <|
            --         \_ ->
            --             parseAndRun "\\vec{x} = 1 + 1"
            --                 |> isErr "Cannot assign scalar to vector variables"
            --     , test "parses assignment with undefined variables" <|
            --         \_ ->
            --             parseAndRun "\\vec{x} = y + (1 + 1)"
            --                 |> isEq
            --                     (Expression
            --                         (SingleArity (Assignment (VectorIdentifier "x"))
            --                             (DoubleArity Addition (Variable (ScalarIdentifier "y")) (Number 2))
            --                         )
            --                     )
            --     , test "gets index position from a vector, 1-based index (unfortunatly math is like that 🙁)" <|
            --         \_ ->
            --             parseAndRun "(3, 2, 1)_{1}"
            --                 |> isEq (Expression (Number 3))
            --     , test "breaks if index is not integer" <|
            --         \_ ->
            --             parseAndRun "(3, 2, 1)_{3/2}"
            --                 |> isErr "Cannot use 1.5 as an index, it has to be a positive integer"
            --     , test "breaks if index is out bound" <|
            --         \_ ->
            --             parseAndRun "(3, 2, 1)_{5}"
            --                 |> isErr "Index 5 out of bounds"
            --     , test "breaks for numbers smaller than 1" <|
            --         \_ ->
            --             parseAndRun "(3, 2, 1)_{0}"
            --                 |> isErr "Cannot use 0 as an index, it has to be a positive integer"
            --     , test "returns unapplied index for undefined variable" <|
            --         \_ ->
            --             parseAndRun "(1, 2, 3)_{x}"
            --                 |> isEq
            --                     (Expression
            --                         (DoubleArity Index
            --                             (Array [ Number 1, Number 2, Number 3 ])
            --                             (Variable (ScalarIdentifier "x"))
            --                         )
            --                     )
            --     , test "evaluates indexes from vector variables in scalar context" <|
            --         \_ ->
            --             parseAndRun "\\vec{x} = (3, 2, 1)\nx_{1}"
            --                 |> isLastEq (Expression (Number 3))
            --     , test "evaluates map function" <|
            --         \_ ->
            --             parseAndRun "f(\\vec{x})_{i} = x_{i} + 1\nf((1,2,3))"
            --                 |> Result.map (List.map Tuple.second)
            --                 |> Expect.equal (Ok [ Undefined, Expression (Array [ Number 2, Number 3, Number 4 ]) ])
            --     , test "evaluates a vector summation" <|
            --         \_ ->
            --             parseAndRun "\\mathbf{x} = (1, 2, 3)\n\\sum{\\mathbf{x}}"
            --                 |> isLastEq (Expression (Number 6))
            --     , test "expands a summation as far as it can" <|
            --         \_ ->
            --             parseAndRun "\\mathbf{x} = (1, 2, y)\n\\sum{\\mathbf{x}}"
            --                 |> isLastEq (Expression (DoubleArity Addition (Number 3) (Variable (ScalarIdentifier "y"))))
            --     , test "does not enter an infinite loop trying to expand undefined vars" <|
            --         \_ ->
            --             parseAndRun "\\sum{\\mathbf{x}}"
            --                 |> isEq (Expression (SingleArity Summation (Variable (VectorIdentifier "x"))))
            --     , test "evaluates cardinality" <|
            --         \_ ->
            --             parseAndRun "\\mathbf{a} = (x, y, z)\n|\\mathbf{a}|"
            --                 |> isLastEq (Expression (Number 3))
            --     ]
            , describe "blocks"
                [ test "evaluates blocks" <|
                    \_ ->
                        parseAndRun "{\nx = 1\nx + 2}"
                            |> isEq (Undefined [ undefinedTrack ( 3, 7 ) VoidReturn ])
                , test "returns the value given to return" <|
                    \_ ->
                        parseAndRun "f = (x) => { return x + 2 }\nf(1)"
                            |> isLastEq (Number 3)
                , test "stops at early return" <|
                    \_ ->
                        parseAndRun "f = (x) => { return x + 2\nreturn 5 }\nf(1)"
                            |> isLastEq (Number 3)

                -- , test "evaluates multiple blocks" <|
                --     \_ ->
                --         parseAndRun "First\\ Block:\nx = 1\nx + 2\nSecond\\ Block:\n5"
                --             |> isLastEq (Expression (Number 5))
                ]
            , describe "equality"
                [ test "evaluates number equality" <|
                    \_ ->
                        parseAndRun "1 + 1 == 2"
                            |> isLastEq (Boolean True)
                , test "evaluates false number equality" <|
                    \_ ->
                        parseAndRun "1 + 1 == 3"
                            |> isLastEq (Boolean False)
                , test "evaluates boolean equality" <|
                    \_ ->
                        parseAndRun "true == true"
                            |> isLastEq (Boolean True)
                , test "numbers are not true" <|
                    \_ ->
                        parseAndRun "5 == true"
                            |> isLastEq (Boolean False)
                , test "but they are also not false" <|
                    \_ ->
                        parseAndRun "5 == false"
                            |> isLastEq (Boolean False)
                , test "except 1, which is true" <|
                    \_ ->
                        parseAndRun "1 == true"
                            |> isLastEq (Boolean True)
                , test "and 0, which is false" <|
                    \_ ->
                        parseAndRun "0 == false"
                            |> isLastEq (Boolean True)
                , test "functions are not true" <|
                    \_ ->
                        parseAndRun "fn = () => {}\nfn == true"
                            |> isLastEq (Boolean False)
                , test "nor functions are false" <|
                    \_ ->
                        parseAndRun "fn = () => {}\nfn == false"
                            |> isLastEq (Boolean False)
                , test "undefined is not true" <|
                    \_ ->
                        parseAndRun "x == true"
                            |> isLastEq (Boolean False)
                , test "nor undefined is false" <|
                    \_ ->
                        parseAndRun "x == false"
                            |> isLastEq (Boolean False)
                , test "empty array is not true" <|
                    \_ ->
                        parseAndRun "[] == true"
                            |> isLastEq (Boolean False)
                , test "filled array is also not true" <|
                    \_ ->
                        parseAndRun "[5] == true"
                            |> isLastEq (Boolean False)
                , test "empty array is false" <|
                    \_ ->
                        parseAndRun "[] == false"
                            |> isLastEq (Boolean True)
                , test "but filled array is also not false" <|
                    \_ ->
                        parseAndRun "[5] == false"
                            |> isLastEq (Boolean False)
                , test "except array with 1 in the first position exaclty is true" <|
                    \_ ->
                        parseAndRun "[1] == true"
                            |> isLastEq (Boolean True)
                , test "and conversely, array with 0 in first position exactly is false" <|
                    \_ ->
                        parseAndRun "[0] == false"
                            |> isLastEq (Boolean True)
                , test "evaluates number smaller than" <|
                    \_ ->
                        parseAndRun "1 < 2"
                            |> isLastEq (Boolean True)
                , test "evaluates number greater than" <|
                    \_ ->
                        parseAndRun "2 > 1"
                            |> isLastEq (Boolean True)
                , test "true is greater than 0, because apparently it gets converted to 1" <|
                    \_ ->
                        parseAndRun "true > 0"
                            |> isLastEq (Boolean True)
                , test "true is smaller than 2, because apparently it gets converted to 1" <|
                    \_ ->
                        parseAndRun "true < 2"
                            |> isLastEq (Boolean True)
                , test "false is greater than -1, because apparently it gets converted to 0" <|
                    \_ ->
                        parseAndRun "false > 0 - 1"
                            |> isLastEq (Boolean True)
                , test "false is smaller than 1, because apparently it gets converted to 0" <|
                    \_ ->
                        parseAndRun "false < 1"
                            |> isLastEq (Boolean True)
                , test "undefined is not smaller than a number" <|
                    \_ ->
                        parseAndRun "undefined < 1"
                            |> isLastEq (Boolean False)
                , test "undefined is not greater than a number" <|
                    \_ ->
                        parseAndRun "undefined > 0 - 1"
                            |> isLastEq (Boolean False)
                , test "function is not smaller than a number" <|
                    \_ ->
                        parseAndRun "fn = () => {}; fn < 1"
                            |> isLastEq (Boolean False)
                , test "function is not greater than a number" <|
                    \_ ->
                        parseAndRun "fn = () => {}; fn > 0 - 1"
                            |> isLastEq (Boolean False)
                , test "takes the first value of an array when converting to a number" <|
                    \_ ->
                        parseAndRun "[5] == 5"
                            |> isLastEq (Boolean True)
                , test "takes the first value of an array when converting to a number #2" <|
                    \_ ->
                        parseAndRun "[5] > 4"
                            |> isLastEq (Boolean True)
                , test "but not when array has more than one item" <|
                    \_ ->
                        parseAndRun "[5, 1] > 4"
                            |> isLastEq (Boolean False)
                , test "which means translation further into booleans" <|
                    \_ ->
                        parseAndRun "[1] == true"
                            |> isLastEq (Boolean True)
                , test "which means translation further into booleans #2" <|
                    \_ ->
                        parseAndRun "[5] == true"
                            |> isLastEq (Boolean False)
                , test "comparisson converts string too" <|
                    \_ ->
                        parseAndRun "\"5\" == 5"
                            |> isLastEq (Boolean True)
                , test "but first it gets translated into string, so [true] becomes ['true'] so its not == true" <|
                    \_ ->
                        parseAndRun "[true] == true"
                            |> isLastEq (Boolean False)
                , test "arrays get translated into string when compared with string, many levels deep" <|
                    \_ ->
                        parseAndRun "[1,[[2]],3] == \"1,2,3\""
                            |> isLastEq (Boolean True)
                , test "everything gets translated to string when compared to string" <|
                    \_ ->
                        parseAndRun "[true] == \"true\""
                            |> isLastEq (Boolean True)
                , test "unless its a comparison with boolean" <|
                    \_ ->
                        parseAndRun "\"0\" == false"
                            |> isLastEq (Boolean True)
                ]
            , describe "if conditions"
                [ test "if returns value of the block" <|
                    \_ ->
                        parseAndRun "if (true) { 1 + 1 }"
                            |> isLastEq (Undefined [ undefinedTrack ( 1, 20 ) VoidReturn ])
                , test "if returns undefined if block was not executed and there is no else" <|
                    \_ ->
                        parseAndRun "if (false) { 1 + 1 }"
                            |> isLastEq (Undefined [ undefinedTrack ( 1, 1 ) IfWithoutElse ])
                , test "if with true boolean condition" <|
                    \_ ->
                        parseAndRun "x = 0\nif (true) { x = 1 }\nx"
                            |> isLastEq (Number 1)
                , test "if with false boolean condition" <|
                    \_ ->
                        parseAndRun "x = 0\nif (false) { x = 1 }\nx"
                            |> isLastEq (Number 0)
                , test "if with comparisson" <|
                    \_ ->
                        parseAndRun "x = 0\nif (1 == true) { x = 1 }\nx"
                            |> isLastEq (Number 1)
                , test "numbers evaluates to true (even though 5 != true)" <|
                    \_ ->
                        parseAndRun "x = 0\nif (5) { x = 1 }\nx"
                            |> isLastEq (Number 1)
                , test "except 0" <|
                    \_ ->
                        parseAndRun "x = 0\nif (0) { x = 1 }\nx"
                            |> isLastEq (Number 0)
                , test "functions are true" <|
                    \_ ->
                        parseAndRun "fn = () => {}\nx = 0\nif (fn) { x = 1 }\nx"
                            |> isLastEq (Number 1)
                , test "undefined is falsy" <|
                    \_ ->
                        parseAndRun "x = 0\nif (y) { x = 1 }\nx"
                            |> isLastEq (Number 0)
                , test "arrays are truthy" <|
                    \_ ->
                        parseAndRun "x = 0\nif ([0]) { x = 1 }\nx"
                            |> isLastEq (Number 1)
                , test "empty arrays are also truthy" <|
                    \_ ->
                        parseAndRun "x = 0\nif ([]) { x = 1 }\nx"
                            |> isLastEq (Number 1)
                , test "empty strings are also falsy" <|
                    \_ ->
                        parseAndRun "x = 0\nif (\"\") { x = 1 }\nx"
                            |> isLastEq (Number 0)
                , test "but strings with any content and truthy" <|
                    \_ ->
                        parseAndRun "x = 0\nif (\"0\") { x = 1 }\nx"
                            |> isLastEq (Number 1)
                ]
            , describe "loops"
                [ test "loops with while" <|
                    \_ ->
                        parseAndRun "let x = 0; while (x < 3) { x = x + 1 }; x"
                            |> isLastEq (Number 3)
                ]

            -- , test "use current variables state when reusing a function declared after a variable is defined outsite its scope" <|
            --     \_ ->
            --         parseAndRun "x = 1\ny(z) = z + x\nx = 2\ny(3)"
            --             |> isLastEq (Expression (Number 5))
            , describe "math types conversion"
                [ test "string concatenation" <|
                    \_ ->
                        parseAndRun "\"foo\" + \"bar\""
                            |> isLastEq (String "foobar")
                , test "sum with boolean" <|
                    \_ ->
                        parseAndRun "5 + true"
                            |> isLastEq (Number 6)
                , test "sum booleans" <|
                    \_ ->
                        parseAndRun "true + true"
                            |> isLastEq (Number 2)
                , test "concatenates number with string" <|
                    \_ ->
                        parseAndRun "5 + \"5\""
                            |> isLastEq (String "55")
                , test "converts array to string" <|
                    \_ ->
                        parseAndRun "[] + true"
                            |> isLastEq (String "true")
                , test "sum boolean with string" <|
                    \_ ->
                        parseAndRun "true + \"false\""
                            |> isLastEq (String "truefalse")
                , test "concatenates with undefined" <|
                    \_ ->
                        parseAndRun "'foo' + undefined"
                            |> isLastEq (String "fooundefined")
                , test "sum with undefineds" <|
                    \_ ->
                        parseAndRun "undefined + undefined"
                            -- TODO: NaN
                            |> isLastEq
                                (Undefined
                                    [ undefinedTrack ( 1, 1 ) ExplicitUndefined
                                    , undefinedTrack ( 1, 11 ) (OperationWithUndefined "addition")
                                    ]
                                )
                , test "subtracts string from number" <|
                    \_ ->
                        parseAndRun "5 - \"4\""
                            |> isLastEq (Number 1)
                , test "can't subtract strings" <|
                    \_ ->
                        parseAndRun "\"foo\" - \"bar\""
                            -- TODO: NaN
                            |> isLastEq
                                (Undefined
                                    [ undefinedTrack ( 1, 7 ) (OperationWithUndefined "subtraction")
                                    ]
                                )
                , test "subtracts arrays" <|
                    \_ ->
                        parseAndRun "[] - []"
                            |> isLastEq (Number 0)
                ]
            ]
        ]


parseAndRun : String -> Result Error (List ExpressionResult)
parseAndRun code =
    AstParser.parse "test.us" code
        |> Result.map (Interpreter.run emptyState >> Tuple.second)


isEq expected actual =
    actual
        |> Result.map (List.map .result)
        |> Expect.equal (Ok [ expected ])


isLastEq expected actual =
    actual
        |> Result.map (List.map .result)
        |> Result.toMaybe
        |> Maybe.withDefault []
        |> List.reverse
        |> List.head
        |> Expect.equal (Just expected)


isErr msg =
    Expect.equal
        (Err
            [ { row = 0
              , col = 0
              , problem = Problem msg
              }
            ]
        )
