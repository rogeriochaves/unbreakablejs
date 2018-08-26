module InterpreterTest exposing (..)

import Expect exposing (Expectation)
import Interpreter exposing (..)
import Parser exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Interpreter suite"
        [ describe "parsing and executing"
            [ test "sum integer numbers" <|
                \_ ->
                    Parser.parse "1 + 1"
                        |> Result.map Interpreter.run
                        |> Expect.equal (Ok 2)
            , test "sum float numbers" <|
                \_ ->
                    Parser.parse "1.5 + 1.3"
                        |> Result.map Interpreter.run
                        |> Expect.equal (Ok 2.8)
            , test "execute nested expressions" <|
                \_ ->
                    Parser.parse "1 - (3 - 2)"
                        |> Result.map Interpreter.run
                        |> Expect.equal (Ok 0)
            , test "respects math priority" <|
                \_ ->
                    Parser.parse "2 + 3 * 2"
                        |> Result.map Interpreter.run
                        |> Expect.equal (Ok 8)
            , test "respects math priority #2" <|
                \_ ->
                    Parser.parse "2 * 3 + 2"
                        |> Result.map Interpreter.run
                        |> Expect.equal (Ok 8)
            , test "symbol function aplication with other expression" <|
                \_ ->
                    Parser.parse "\\sqrt{9} + 2"
                        |> Result.map Interpreter.run
                        |> Expect.equal (Ok 5)
            , test "symbol function aplication on a expression" <|
                \_ ->
                    Parser.parse "\\sqrt{7 + 2}"
                        |> Result.map Interpreter.run
                        |> Expect.equal (Ok 3)
            ]
        , describe "symbols"
            [ test "sqrt" <|
                \_ ->
                    Parser.parse "\\sqrt{9}"
                        |> Result.map Interpreter.run
                        |> Expect.equal (Ok 3)
            , test "frac" <|
                \_ ->
                    Parser.parse "\\frac{3}{2}"
                        |> Result.map Interpreter.run
                        |> Expect.equal (Ok 1.5)
            ]
        ]
