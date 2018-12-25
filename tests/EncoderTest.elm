module EncoderTest exposing (suite)

import Encoder exposing (..)
import Expect exposing (Expectation)
import MathParser exposing (..)
import Return exposing (Value(..))
import Test exposing (..)
import Types exposing (..)


suite : Test
suite =
    describe "Encoder suite"
        [ test "encode a simple number" <|
            \_ ->
                encode (Number 2)
                    |> Expect.equal "2"
        , test "encodes a operation, asignment and function call" <|
            \_ ->
                encode
                    (SingleArity
                        (Assignment (ScalarIdentifier "f"))
                        (Abstraction (ScalarIdentifier "x")
                            (DoubleArity Addition (Variable (ScalarIdentifier "x")) (Number 1))
                        )
                    )
                    |> Expect.equal "f(x) = x + 1"
        , test "encodes a vector with multiple operations" <|
            \_ ->
                let
                    original =
                        "\\vec{x} = (\\frac{2}{3}, \\sum_{x = 1}^{3} 5, g(x), x_{5})"
                in
                MathParser.parse original
                    |> Result.map (List.map encode)
                    |> Expect.equal (Ok [ original ])
        , test "encodes more cases" <|
            \_ ->
                let
                    original =
                        "f(\\vec{x})_{z} = (1 ^ {x + 1}, 1 - 1, 2 * 2, 3 / 3, \\sqrt{4})"
                in
                MathParser.parse original
                    |> Result.map (List.map encode)
                    |> Expect.equal (Ok [ original ])
        ]
