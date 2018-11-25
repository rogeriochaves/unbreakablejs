module MathParserTest exposing (suite)

import Expect exposing (Expectation)
import MathParser exposing (..)
import Test exposing (..)
import Types exposing (..)


suite : Test
suite =
    describe "MathParser suite"
        [ test "read integer number" <|
            \_ ->
                MathParser.parse "1 + 1"
                    |> Expect.equal (Ok (Addition (Integer 1) (Integer 1)))
        , test "read float numbers" <|
            \_ ->
                MathParser.parse "1.5 + 1.3"
                    |> Expect.equal (Ok (Addition (Floating 1.5) (Floating 1.3)))
        , test "read nested operations" <|
            \_ ->
                MathParser.parse "1 - (3 - 2)"
                    |> Expect.equal (Ok (Subtraction (Integer 1) (Subtraction (Integer 3) (Integer 2))))
        , test "read single-arity symbolic function" <|
            \_ ->
                MathParser.parse "\\sqrt{5}"
                    |> Expect.equal (Ok (SymbolicFunction (SingleArity Sqrt (Integer 5))))
        , test "read double-arity symbolic function" <|
            \_ ->
                MathParser.parse "\\frac{2}{3}"
                    |> Expect.equal (Ok (SymbolicFunction (DoubleArity Frac (Integer 2) (Integer 3))))

        -- https://www.overleaf.com/learn/latex/Integrals,_sums_and_limits#Sums_and_products
        , test "read triple arity symbolic function with upper and lower limit and expresion as a body" <|
            \_ ->
                MathParser.parse "\\sum_{1}^{3} 5"
                    |> Expect.equal (Ok (SymbolicFunction (Iterator Sum_ (Integer 1) (Integer 3) (Integer 5))))
        , test "read symbol function aplication with other expression" <|
            \_ ->
                MathParser.parse "\\sqrt{9} + 2"
                    |> Expect.equal
                        (Ok
                            (Addition
                                (SymbolicFunction (SingleArity Sqrt (Integer 9)))
                                (Integer 2)
                            )
                        )
        , test "read exponentiation" <|
            \_ ->
                MathParser.parse "2 ^ 5"
                    |> Expect.equal (Ok (Exponentiation (Integer 2) (Integer 5)))
        ]
