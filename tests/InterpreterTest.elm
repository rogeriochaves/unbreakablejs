module InterpreterTest exposing (suite)

import Expect exposing (Expectation)
import Interpreter exposing (..)
import MathParser exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Interpreter suite"
        [ describe "parsing and executing"
            [ test "sum integer numbers" <|
                \_ ->
                    MathParser.parse "1 + 1"
                        |> Result.map Interpreter.run
                        |> Expect.equal (Ok [ 2 ])
            , test "sum float numbers" <|
                \_ ->
                    MathParser.parse "1.5 + 1.3"
                        |> Result.map Interpreter.run
                        |> Expect.equal (Ok [ 2.8 ])
            , test "execute nested expressions" <|
                \_ ->
                    MathParser.parse "1 - (3 - 2)"
                        |> Result.map Interpreter.run
                        |> Expect.equal (Ok [ 0 ])
            , test "respects math priority" <|
                \_ ->
                    MathParser.parse "2 + 3 * 2"
                        |> Result.map Interpreter.run
                        |> Expect.equal (Ok [ 8 ])
            , test "respects math priority #2" <|
                \_ ->
                    MathParser.parse "2 * 3 + 2"
                        |> Result.map Interpreter.run
                        |> Expect.equal (Ok [ 8 ])
            , test "symbol function aplication with other expression" <|
                \_ ->
                    MathParser.parse "\\sqrt{9} + 2"
                        |> Result.map Interpreter.run
                        |> Expect.equal (Ok [ 5 ])
            , test "symbol function aplication on a expression" <|
                \_ ->
                    MathParser.parse "\\sqrt{7 + 2}"
                        |> Result.map Interpreter.run
                        |> Expect.equal (Ok [ 3 ])
            , test "exponentiation" <|
                \_ ->
                    MathParser.parse "2 ^ 5"
                        |> Result.map Interpreter.run
                        |> Expect.equal (Ok [ 32 ])
            , test "respects math priority #3" <|
                \_ ->
                    MathParser.parse "2 * 3 ^ 5"
                        |> Result.map Interpreter.run
                        |> Expect.equal (Ok [ 486 ])
            ]
        , describe "symbols"
            [ test "sqrt" <|
                \_ ->
                    MathParser.parse "\\sqrt{9}"
                        |> Result.map Interpreter.run
                        |> Expect.equal (Ok [ 3 ])
            , test "frac" <|
                \_ ->
                    MathParser.parse "\\frac{3}{2}"
                        |> Result.map Interpreter.run
                        |> Expect.equal (Ok [ 1.5 ])
            , test "summation" <|
                \_ ->
                    MathParser.parse "\\sum_{x=1}^{3} 5"
                        |> Result.map Interpreter.run
                        |> Expect.equal (Ok [ 15 ])
            , test "summation using the variable" <|
                \_ ->
                    MathParser.parse "\\sum_{x=1}^{3} x + 1"
                        |> Result.map Interpreter.run
                        |> Expect.equal (Ok [ 9 ])
            ]
        , test "multiple expressions" <|
            \_ ->
                MathParser.parse "1 + 1\n2 + 2"
                    |> Result.map Interpreter.run
                    |> Expect.equal (Ok [ 2, 4 ])
        , describe "equations" <|
            [ test "parses a simple equation and return the result of the variabl" <|
                \_ ->
                    MathParser.parse "x = 2 + 2"
                        |> Result.map Interpreter.run
                        |> Expect.equal (Ok [ 4 ])
            , test "saves the value to the variable" <|
                \_ ->
                    MathParser.parse "x = 2 + 2\nx + 1"
                        |> Result.map Interpreter.run
                        |> Expect.equal (Ok [ 4, 5 ])
            ]
        ]
