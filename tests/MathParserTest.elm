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
                    |> isEq
                        (DoubleArity Addition
                            (Number 1)
                            (Number 1)
                        )
        , test "read float numbers" <|
            \_ ->
                MathParser.parse "1.5 + 1.3"
                    |> isEq
                        (DoubleArity Addition
                            (Number 1.5)
                            (Number 1.3)
                        )
        , test "read nested operations" <|
            \_ ->
                MathParser.parse "1 - (3 - 2)"
                    |> isEq
                        (DoubleArity Subtraction
                            (Number 1)
                            (DoubleArity Subtraction
                                (Number 3)
                                (Number 2)
                            )
                        )
        , test "read single-arity symbolic function" <|
            \_ ->
                MathParser.parse "\\sqrt{5}"
                    |> isEq (SingleArity Sqrt (Number 5))
        , test "read simple summation function" <|
            \_ ->
                MathParser.parse "\\sum{\\mathbf{x}}"
                    |> isEq (SingleArity Summation (Variable (VectorIdentifier "x")))
        , test "read double-arity symbolic function" <|
            \_ ->
                MathParser.parse "\\frac{2}{3}"
                    |> isEq (DoubleArity Frac (Number 2) (Number 3))

        -- https://www.overleaf.com/learn/latex/Integrals,_sums_and_limits#Sums_and_products
        , test "read iterator functions" <|
            \_ ->
                MathParser.parse "\\sum_{x = 1}^{3} 5"
                    |> isEq (TripleArity (Sum_ "x") (Number 1) (Number 3) (Number 5))
        , test "read symbol function aplication with other expression" <|
            \_ ->
                MathParser.parse "\\sqrt{9} + 2"
                    |> isEq
                        (DoubleArity Addition
                            (SingleArity Sqrt (Number 9))
                            (Number 2)
                        )
        , test "read exponentiation" <|
            \_ ->
                MathParser.parse "2 ^ 5"
                    |> isEq
                        (DoubleArity Exponentiation
                            (Number 2)
                            (Number 5)
                        )
        , test "read modulo" <|
            \_ ->
                MathParser.parse "2 \\mod 5"
                    |> isEq
                        (DoubleArity Modulo
                            (Number 2)
                            (Number 5)
                        )
        , test "read integer div" <|
            \_ ->
                MathParser.parse "5 \\div 2"
                    |> isEq
                        (DoubleArity EuclideanDivision
                            (Number 5)
                            (Number 2)
                        )
        , test "read grouped exponentiation" <|
            \_ ->
                MathParser.parse "2 ^ {5 + 1}"
                    |> isEq
                        (DoubleArity Exponentiation
                            (Number 2)
                            (DoubleArity Addition
                                (Number 5)
                                (Number 1)
                            )
                        )
        , test "parses factorial" <|
            \_ ->
                MathParser.parse "5!"
                    |> isEq (SingleArity Factorial (Number 5))
        , test "parses negation" <|
            \_ ->
                MathParser.parse "-5"
                    |> isEq (SingleArity Negation (Number 5))
        , test "parses cardinality" <|
            \_ ->
                MathParser.parse "|\\vec{x}|"
                    |> isEq (SingleArity Cardinality (Variable (VectorIdentifier "x")))
        , describe "multiple lines"
            [ test "parses multiple expressions" <|
                \_ ->
                    MathParser.parse "1 + 1\n2 + 2"
                        |> Expect.equal
                            (Ok
                                [ DoubleArity Addition (Number 1) (Number 1)
                                , DoubleArity Addition (Number 2) (Number 2)
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
                                [ DoubleArity Addition (Number 1) (Number 1)
                                , DoubleArity Addition (Number 2) (Number 2)
                                ]
                            )
            , test "allow empty lines and trailing spaces" <|
                \_ ->
                    MathParser.parse "1 + 1 \n \n2 + 2\n"
                        |> Expect.equal
                            (Ok
                                [ DoubleArity Addition (Number 1) (Number 1)
                                , DoubleArity Addition (Number 2) (Number 2)
                                ]
                            )
            ]
        , describe "assignments"
            [ test "parses simple assignment" <|
                \_ ->
                    MathParser.parse "x = 1 + 1"
                        |> isEq (SingleArity (Assignment (ScalarIdentifier "x")) (DoubleArity Addition (Number 1) (Number 1)))
            , test "allows have a variable with a bar" <|
                \_ ->
                    MathParser.parse "\\bar{x} = 2"
                        |> isEq (SingleArity (Assignment (ScalarIdentifier "\\bar{x}")) (Number 2))
            , test "allows have a variable with a tilde" <|
                \_ ->
                    MathParser.parse "\\tilde{x} = 3"
                        |> isEq (SingleArity (Assignment (ScalarIdentifier "\\tilde{x}")) (Number 3))
            , test "does not allow nested assignments" <|
                \_ ->
                    MathParser.parse "x = 1 + (x = 2)"
                        |> isErr
                        |> Expect.true "nested assignments"
            , test "parses expression with variables" <|
                \_ ->
                    MathParser.parse "x + 1"
                        |> isEq (DoubleArity Addition (Variable (ScalarIdentifier "x")) (Number 1))
            , test "parses assignment with variables" <|
                \_ ->
                    MathParser.parse "x = y + 1"
                        |> isEq (SingleArity (Assignment (ScalarIdentifier "x")) (DoubleArity Addition (Variable (ScalarIdentifier "y")) (Number 1)))
            , test "parses sigma as a variable" <|
                \_ ->
                    MathParser.parse "\\sigma + 1"
                        |> isEq (DoubleArity Addition (Variable (ScalarIdentifier "\\sigma")) (Number 1))
            , test "parses all lowercase greek letters" <|
                \_ ->
                    MathParser.parse "(\\alpha, \\beta, \\gamma, \\delta, \\epsilon, \\varepsilon, \\zeta, \\eta, \\theta, \\vartheta, \\iota, \\kappa, \\lambda, \\mu, \\nu, \\xi, \\pi, \\rho, \\sigma, \\tau, \\upsilon, \\phi, \\chi, \\psi, \\omega)"
                        |> isEq (Vector [ Variable (ScalarIdentifier "\\alpha"), Variable (ScalarIdentifier "\\beta"), Variable (ScalarIdentifier "\\gamma"), Variable (ScalarIdentifier "\\delta"), Variable (ScalarIdentifier "\\epsilon"), Variable (ScalarIdentifier "\\varepsilon"), Variable (ScalarIdentifier "\\zeta"), Variable (ScalarIdentifier "\\eta"), Variable (ScalarIdentifier "\\theta"), Variable (ScalarIdentifier "\\vartheta"), Variable (ScalarIdentifier "\\iota"), Variable (ScalarIdentifier "\\kappa"), Variable (ScalarIdentifier "\\lambda"), Variable (ScalarIdentifier "\\mu"), Variable (ScalarIdentifier "\\nu"), Variable (ScalarIdentifier "\\xi"), Variable (ScalarIdentifier "\\pi"), Variable (ScalarIdentifier "\\rho"), Variable (ScalarIdentifier "\\sigma"), Variable (ScalarIdentifier "\\tau"), Variable (ScalarIdentifier "\\upsilon"), Variable (ScalarIdentifier "\\phi"), Variable (ScalarIdentifier "\\chi"), Variable (ScalarIdentifier "\\psi"), Variable (ScalarIdentifier "\\omega") ])
            , test "allows operatornames as scalars" <|
                \_ ->
                    MathParser.parse "\\operatorname{Q2} = 5"
                        |> isEq (SingleArity (Assignment (ScalarIdentifier "\\operatorname{Q2}")) (Number 5))
            , test "allows have a greek variables with a bar" <|
                \_ ->
                    MathParser.parse "\\bar{\\sigma} = 2"
                        |> isEq (SingleArity (Assignment (ScalarIdentifier "\\bar{\\sigma}")) (Number 2))
            ]
        , describe "functions"
            [ test "parses function declaration" <|
                \_ ->
                    MathParser.parse "f(x) = x + 1"
                        |> isEq
                            (SingleArity
                                (Assignment (ScalarIdentifier "f"))
                                (Abstraction (ScalarIdentifier "x")
                                    (DoubleArity Addition (Variable (ScalarIdentifier "x")) (Number 1))
                                )
                            )
            , test "does not allow nested function declarations" <|
                \_ ->
                    MathParser.parse "fn(x) = fn(y) = 1"
                        |> isErr
                        |> Expect.true "nested functions"
            , test "parses function call" <|
                \_ ->
                    MathParser.parse "f(5)"
                        |> isEq (SingleArity (Application (Variable (ScalarIdentifier "f"))) (Number 5))
            ]
        , describe "vectors"
            [ test "parses simple vector" <|
                \_ ->
                    MathParser.parse "(1, 2, 3)"
                        |> isEq
                            (Vector [ Number 1, Number 2, Number 3 ])
            , test "parses vector with expressions inside" <|
                \_ ->
                    MathParser.parse "(x, x + 1, x + 2)"
                        |> isEq
                            (Vector
                                [ Variable (ScalarIdentifier "x")
                                , DoubleArity Addition (Variable (ScalarIdentifier "x")) (Number 1)
                                , DoubleArity Addition (Variable (ScalarIdentifier "x")) (Number 2)
                                ]
                            )
            , test "parses vector assignment" <|
                \_ ->
                    MathParser.parse "\\vec{x} = (1, 2, 3)"
                        |> isEq (SingleArity (Assignment (VectorIdentifier "x")) (Vector [ Number 1, Number 2, Number 3 ]))
            , test "vector variables can also use mathbf" <|
                \_ ->
                    MathParser.parse "\\mathbf{x} = (1, 2, 3)"
                        |> isEq (SingleArity (Assignment (VectorIdentifier "x")) (Vector [ Number 1, Number 2, Number 3 ]))
            , test "parses expression with vector variable" <|
                \_ ->
                    MathParser.parse "\\vec{x} + 1"
                        |> isEq (DoubleArity Addition (Variable (VectorIdentifier "x")) (Number 1))
            , test "parses function with vector as param" <|
                \_ ->
                    MathParser.parse "f(\\vec{x}) = \\vec{x} + 1"
                        |> isEq
                            (SingleArity
                                (Assignment (ScalarIdentifier "f"))
                                (Abstraction (VectorIdentifier "x")
                                    (DoubleArity Addition (Variable (VectorIdentifier "x")) (Number 1))
                                )
                            )
            , test "parses vector index position" <|
                \_ ->
                    MathParser.parse "x_{3}"
                        |> isEq
                            (DoubleArity Index (Variable (ScalarIdentifier "x")) (Number 3))
            , test "parses vector index position and other operation" <|
                \_ ->
                    MathParser.parse "x_{3} + 1"
                        |> isEq
                            (DoubleArity Addition (DoubleArity Index (Variable (ScalarIdentifier "x")) (Number 3)) (Number 1))
            , test "parses vector index position inside function assignment" <|
                \_ ->
                    MathParser.parse "f(\\vec{x}) = x_{3}"
                        |> isEq
                            (SingleArity
                                (Assignment (ScalarIdentifier "f"))
                                (Abstraction (VectorIdentifier "x")
                                    (DoubleArity Index (Variable (ScalarIdentifier "x")) (Number 3))
                                )
                            )
            , test "parses nested index" <|
                \_ ->
                    MathParser.parse "x_{y_{3}}"
                        |> isEq
                            (DoubleArity Index (Variable (ScalarIdentifier "x")) (DoubleArity Index (Variable (ScalarIdentifier "y")) (Number 3)))
            ]
        , describe "mapping function"
            [ test "parses mapping function declaration" <|
                \_ ->
                    MathParser.parse "f(\\vec{x})_{i} = x_{i} + 1"
                        |> isEq
                            (SingleArity
                                (Assignment (ScalarIdentifier "f"))
                                (MapAbstraction "x"
                                    "i"
                                    (DoubleArity Addition
                                        (DoubleArity Index (Variable (ScalarIdentifier "x")) (Variable (ScalarIdentifier "i")))
                                        (Number 1)
                                    )
                                )
                            )
            ]
        , describe "blocks"
            [ test "parses blocks" <|
                \_ ->
                    MathParser.parse "Test:\nx = 1\nx + 2"
                        |> isEq
                            (Block "Test"
                                [ SingleArity (Assignment (ScalarIdentifier "x")) (Number 1)
                                , DoubleArity Addition (Variable (ScalarIdentifier "x")) (Number 2)
                                ]
                            )
            , test "parses multiple blocks" <|
                \_ ->
                    MathParser.parse "First\\ Block:\nx = 1\nx + 2\nSecond\\ Block:\n5"
                        |> Expect.equal
                            (Ok
                                [ Block "First\\ Block"
                                    [ SingleArity (Assignment (ScalarIdentifier "x")) (Number 1)
                                    , DoubleArity Addition (Variable (ScalarIdentifier "x")) (Number 2)
                                    ]
                                , Block "Second\\ Block"
                                    [ Number 5
                                    ]
                                ]
                            )
            ]
        ]


isErr result =
    case result of
        Ok _ ->
            False

        Err _ ->
            True


isEq result =
    Expect.equal (Ok [ result ])
