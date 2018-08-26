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
                    |> Expect.equal (Ok (EAdd (EInt 1) (EInt 1)))
        , test "read float numbers" <|
            \_ ->
                MathParser.parse "1.5 + 1.3"
                    |> Expect.equal (Ok (EAdd (EFloat 1.5) (EFloat 1.3)))
        , test "read nested operations" <|
            \_ ->
                MathParser.parse "1 - (3 - 2)"
                    |> Expect.equal (Ok (ESub (EInt 1) (ESub (EInt 3) (EInt 2))))
        , test "read single-arity symbolic function" <|
            \_ ->
                MathParser.parse "\\sqrt{5}"
                    |> Expect.equal (Ok (ESymbolicFunction (SingleArity Sqrt (EInt 5))))
        , test "read double-arity symbolic function" <|
            \_ ->
                MathParser.parse "\\frac{2}{3}"
                    |> Expect.equal (Ok (ESymbolicFunction (DoubleArity Frac (EInt 2) (EInt 3))))
        , test "symbol function aplication with other expression" <|
            \_ ->
                MathParser.parse "\\sqrt{9} + 2"
                    |> Expect.equal (Ok (EAdd (ESymbolicFunction (SingleArity Sqrt (EInt 9))) (EInt 2)))
        , test "read exponentiation" <|
            \_ ->
                MathParser.parse "2 ^ 5"
                    |> Expect.equal (Ok (EExponentiation (EInt 2) (EInt 5)))
        ]
