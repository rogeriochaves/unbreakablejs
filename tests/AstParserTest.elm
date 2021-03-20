module AstParserTest exposing (suite)

import AstParser exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)
import Types exposing (..)


tracked ( line, column ) =
    Tracked { line = line, column = column, filename = "test.us" }


parse =
    AstParser.parse "test.us"


suite : Test
suite =
    describe "AstParser suite"
        [ test "read int numbers" <|
            \_ ->
                parse "1 + 1"
                    |> isEq
                        (tracked ( 1, 3 )
                            (ReservedApplication Addition
                                [ Untracked (Value (Number 1))
                                , Untracked (Value (Number 1))
                                ]
                            )
                        )
        , test "read float numbers" <|
            \_ ->
                parse "1.5 + 1.3"
                    |> isEq
                        (tracked ( 1, 5 )
                            (ReservedApplication Addition [ Untracked (Value (Number 1.5)), Untracked (Value (Number 1.3)) ])
                        )
        , test "read nested operations" <|
            \_ ->
                parse "1 - (3 - 2)"
                    |> isEq
                        (tracked ( 1, 3 )
                            (ReservedApplication Subtraction
                                [ Untracked (Value (Number 1))
                                , tracked ( 1, 8 )
                                    (ReservedApplication Subtraction [ Untracked (Value (Number 3)), Untracked (Value (Number 2)) ])
                                ]
                            )
                        )

        -- , test "read single-arity symbolic function" <|
        --     \_ ->
        --         parse "\\sqrt{5}"
        --             |> isEq (SingleArity Sqrt (Number 5))
        -- , test "read simple summation function" <|
        --     \_ ->
        --         parse "\\sum{\\mathbf{x}}"
        --             |> isEq (SingleArity Summation (Variable (VectorIdentifier "x")))
        -- , test "read double-arity symbolic function" <|
        --     \_ ->
        --         parse "\\frac{2}{3}"
        --             |> isEq (DoubleArity Frac (Number 2) (Number 3))
        -- -- https://www.overleaf.com/learn/latex/Integrals,_sums_and_limits#Sums_and_products
        -- , test "read iterator functions" <|
        --     \_ ->
        --         parse "\\sum_{x = 1}^{3} 5"
        --             |> isEq (TripleArity (Sum_ "x") (Number 1) (Number 3) (Number 5))
        -- , test "read symbol function aplication with other expression" <|
        --     \_ ->
        --         parse "\\sqrt{9} + 2"
        --             |> isEq
        --                 (DoubleArity Addition
        --                     (SingleArity Sqrt (Number 9))
        --                     (Number 2)
        --                 )
        -- , test "read exponentiation" <|
        --     \_ ->
        --         parse "2 ^ 5"
        --             |> isEq
        --                 (DoubleArity Exponentiation
        --                     (Number 2)
        --                     (Number 5)
        --                 )
        -- , test "read modulo" <|
        --     \_ ->
        --         parse "2 \\mod 5"
        --             |> isEq
        --                 (DoubleArity Modulo
        --                     (Number 2)
        --                     (Number 5)
        --                 )
        -- , test "read integer div" <|
        --     \_ ->
        --         parse "5 \\div 2"
        --             |> isEq
        --                 (DoubleArity EuclideanDivision
        --                     (Number 5)
        --                     (Number 2)
        --                 )
        -- , test "read grouped exponentiation" <|
        --     \_ ->
        --         parse "2 ^ {5 + 1}"
        --             |> isEq
        --                 (DoubleArity Exponentiation
        --                     (Number 2)
        --                     (DoubleArity Addition
        --                         (Number 5)
        --                         (Number 1)
        --                     )
        --                 )
        -- , test "parses factorial" <|
        --     \_ ->
        --         parse "5!"
        --             |> isEq (SingleArity Factorial (Number 5))
        -- , test "parses negation" <|
        --     \_ ->
        --         parse "-5"
        --             |> isEq (SingleArity Negation (Number 5))
        -- , test "parses cardinality" <|
        --     \_ ->
        --         parse "|\\vec{x}|"
        --             |> isEq (SingleArity Cardinality (Variable (VectorIdentifier "x")))
        , describe "multiple lines"
            [ test "parses multiple expressions" <|
                \_ ->
                    parse "1 + 1\n2 + 2"
                        |> Expect.equal
                            (Ok
                                [ tracked ( 1, 3 )
                                    (ReservedApplication Addition
                                        [ Untracked (Value (Number 1))
                                        , Untracked (Value (Number 1))
                                        ]
                                    )
                                , tracked ( 2, 3 )
                                    (ReservedApplication Addition
                                        [ Untracked (Value (Number 2))
                                        , Untracked (Value (Number 2))
                                        ]
                                    )
                                ]
                            )
            , test "breaks for weird things after the end" <|
                \_ ->
                    parse "1 + 1\n2 + 2[1$51"
                        |> isErr
                        |> Expect.true "it should break if anything is not parseable"
            , test "only one expression per line" <|
                \_ ->
                    parse "1 + 1 2 + 2"
                        |> isErr
                        |> Expect.true "it should break there is no line break between expressions"
            , test "allow multiple line breaks" <|
                \_ ->
                    parse "1 + 1\n\n2 + 2\n"
                        |> Expect.equal
                            (Ok
                                [ tracked ( 1, 3 )
                                    (ReservedApplication Addition
                                        [ Untracked (Value (Number 1))
                                        , Untracked (Value (Number 1))
                                        ]
                                    )
                                , tracked ( 3, 3 )
                                    (ReservedApplication Addition
                                        [ Untracked (Value (Number 2))
                                        , Untracked (Value (Number 2))
                                        ]
                                    )
                                ]
                            )
            , test "allow empty lines and trailing spaces" <|
                \_ ->
                    parse "1 + 1 \n \n2 + 2\n"
                        |> Expect.equal
                            (Ok
                                [ tracked ( 1, 3 )
                                    (ReservedApplication Addition
                                        [ Untracked (Value (Number 1))
                                        , Untracked (Value (Number 1))
                                        ]
                                    )
                                , tracked ( 3, 3 )
                                    (ReservedApplication Addition
                                        [ Untracked (Value (Number 2))
                                        , Untracked (Value (Number 2))
                                        ]
                                    )
                                ]
                            )
            ]
        , describe "assignments"
            [ test "parses simple assignment" <|
                \_ ->
                    parse "x = 1 + 1"
                        |> isEq
                            (tracked ( 1, 3 )
                                (ReservedApplication (Assignment "x")
                                    [ tracked ( 1, 7 )
                                        (ReservedApplication Addition [ Untracked <| Value (Number 1), Untracked <| Value (Number 1) ])
                                    ]
                                )
                            )
            , test "does not allow nested assignments" <|
                \_ ->
                    parse "x = 1 + (x = 2)"
                        |> isErr
                        |> Expect.true "nested assignments"
            , test "parses expression with variables" <|
                \_ ->
                    parse "x + 1"
                        |> isEq
                            (tracked ( 1, 3 )
                                (ReservedApplication Addition
                                    [ tracked ( 1, 1 ) <| Variable "x"
                                    , Untracked <| Value (Number 1)
                                    ]
                                )
                            )
            , test "parses assignment with variables" <|
                \_ ->
                    parse "x = y + 1"
                        |> isEq
                            (tracked ( 1, 3 )
                                (ReservedApplication (Assignment "x")
                                    [ tracked ( 1, 7 ) <|
                                        ReservedApplication Addition
                                            [ tracked ( 1, 5 ) <| Variable "y"
                                            , Untracked <| Value (Number 1)
                                            ]
                                    ]
                                )
                            )
            ]
        , describe "functions"
            [ test "parses function declaration" <|
                \_ ->
                    parse "f = (x) => x + 1"
                        |> isEq
                            (tracked ( 1, 3 )
                                (ReservedApplication
                                    (Assignment "f")
                                    [ Untracked
                                        (Value
                                            (Abstraction [ "x" ]
                                                (tracked ( 1, 14 )
                                                    (ReservedApplication Addition
                                                        [ tracked ( 1, 12 ) (Variable "x")
                                                        , Untracked (Value (Number 1))
                                                        ]
                                                    )
                                                )
                                            )
                                        )
                                    ]
                                )
                            )
            , test "parses function declaration with multiple arguments" <|
                \_ ->
                    parse "f = (x, y) => x + 1"
                        |> isEq
                            (tracked ( 1, 3 )
                                (ReservedApplication
                                    (Assignment "f")
                                    [ Untracked
                                        (Value
                                            (Abstraction [ "x", "y" ]
                                                (tracked ( 1, 17 )
                                                    (ReservedApplication Addition
                                                        [ tracked ( 1, 15 ) (Variable "x")
                                                        , Untracked (Value (Number 1))
                                                        ]
                                                    )
                                                )
                                            )
                                        )
                                    ]
                                )
                            )
            , test "does not allow nested function declarations" <|
                \_ ->
                    parse "fn(x) = fn(y) = 1"
                        |> isErr
                        |> Expect.true "nested functions"
            , test "parses function call" <|
                \_ ->
                    parse "f(5)"
                        |> isEq
                            (tracked ( 1, 1 )
                                (Application (tracked ( 1, 1 ) <| Variable "f") [ Untracked <| Value (Number 5) ])
                            )
            , test "parses function call with multiple arguments" <|
                \_ ->
                    parse "f(3, 2)"
                        |> isEq
                            (tracked ( 1, 1 )
                                (Application (tracked ( 1, 1 ) <| Variable "f")
                                    [ Untracked <| Value (Number 3)
                                    , Untracked <| Value (Number 2)
                                    ]
                                )
                            )
            ]

        -- , describe "vectors"
        --     [ test "parses simple vector" <|
        --         \_ ->
        --             parse "(1, 2, 3)"
        --                 |> isEq
        --                     (Vector [ Number 1, Number 2, Number 3 ])
        --     , test "parses vector with expressions inside" <|
        --         \_ ->
        --             parse "(x, x + 1, x + 2)"
        --                 |> isEq
        --                     (Vector
        --                         [ Variable (ScalarIdentifier "x")
        --                         , DoubleArity Addition (Variable (ScalarIdentifier "x")) (Number 1)
        --                         , DoubleArity Addition (Variable (ScalarIdentifier "x")) (Number 2)
        --                         ]
        --                     )
        --     , test "parses vector assignment" <|
        --         \_ ->
        --             parse "\\vec{x} = (1, 2, 3)"
        --                 |> isEq (SingleArity (Assignment (VectorIdentifier "x")) (Vector [ Number 1, Number 2, Number 3 ]))
        --     , test "vector variables can also use mathbf" <|
        --         \_ ->
        --             parse "\\mathbf{x} = (1, 2, 3)"
        --                 |> isEq (SingleArity (Assignment (VectorIdentifier "x")) (Vector [ Number 1, Number 2, Number 3 ]))
        --     , test "parses expression with vector variable" <|
        --         \_ ->
        --             parse "\\vec{x} + 1"
        --                 |> isEq (DoubleArity Addition (Variable (VectorIdentifier "x")) (Number 1))
        --     , test "parses function with vector as param" <|
        --         \_ ->
        --             parse "f(\\vec{x}) = \\vec{x} + 1"
        --                 |> isEq
        --                     (SingleArity
        --                         (Assignment (ScalarIdentifier "f"))
        --                         (Abstraction (VectorIdentifier "x")
        --                             (DoubleArity Addition (Variable (VectorIdentifier "x")) (Number 1))
        --                         )
        --                     )
        --     , test "parses vector index position" <|
        --         \_ ->
        --             parse "x_{3}"
        --                 |> isEq
        --                     (DoubleArity Index (Variable (ScalarIdentifier "x")) (Number 3))
        --     , test "parses vector index position and other operation" <|
        --         \_ ->
        --             parse "x_{3} + 1"
        --                 |> isEq
        --                     (DoubleArity Addition (DoubleArity Index (Variable (ScalarIdentifier "x")) (Number 3)) (Number 1))
        --     , test "parses vector index position inside function assignment" <|
        --         \_ ->
        --             parse "f(\\vec{x}) = x_{3}"
        --                 |> isEq
        --                     (SingleArity
        --                         (Assignment (ScalarIdentifier "f"))
        --                         (Abstraction (VectorIdentifier "x")
        --                             (DoubleArity Index (Variable (ScalarIdentifier "x")) (Number 3))
        --                         )
        --                     )
        --     , test "parses nested index" <|
        --         \_ ->
        --             parse "x_{y_{3}}"
        --                 |> isEq
        --                     (DoubleArity Index (Variable (ScalarIdentifier "x")) (DoubleArity Index (Variable (ScalarIdentifier "y")) (Number 3)))
        --     ]
        -- , describe "mapping function"
        --     [ test "parses mapping function declaration" <|
        --         \_ ->
        --             parse "f(\\vec{x})_{i} = x_{i} + 1"
        --                 |> isEq
        --                     (SingleArity
        --                         (Assignment (ScalarIdentifier "f"))
        --                         (MapAbstraction "x"
        --                             "i"
        --                             (DoubleArity Addition
        --                                 (DoubleArity Index (Variable (ScalarIdentifier "x")) (Variable (ScalarIdentifier "i")))
        --                                 (Number 1)
        --                             )
        --                         )
        --                     )
        --     ]
        -- , describe "blocks"
        --     [ test "parses blocks" <|
        --         \_ ->
        --             parse "Test:\nx = 1\nx + 2"
        --                 |> isEq
        --                     (Block "Test"
        --                         [ SingleArity (Assignment (ScalarIdentifier "x")) (Number 1)
        --                         , DoubleArity Addition (Variable (ScalarIdentifier "x")) (Number 2)
        --                         ]
        --                     )
        --     , test "parses multiple blocks" <|
        --         \_ ->
        --             parse "First\\ Block:\nx = 1\nx + 2\nSecond\\ Block:\n5"
        --                 |> Expect.equal
        --                     (Ok
        --                         [ Block "First\\ Block"
        --                             [ SingleArity (Assignment (ScalarIdentifier "x")) (Number 1)
        --                             , DoubleArity Addition (Variable (ScalarIdentifier "x")) (Number 2)
        --                             ]
        --                         , Block "Second\\ Block"
        --                             [ Number 5
        --                             ]
        --                         ]
        --                     )
        --     ]
        ]


isErr result =
    case result of
        Ok _ ->
            False

        Err _ ->
            True


isEq result =
    Expect.equal (Ok [ result ])