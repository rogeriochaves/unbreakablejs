module ParserTest exposing (..)

import Expect exposing (Expectation)
import Parser exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Parser suite"
        [ test "read integer number" <|
            \_ ->
                Parser.parse "1 + 1"
                    |> Expect.equal (Ok (EAdd (EInt 1) (EInt 1)))
        , test "read float numbers" <|
            \_ ->
                Parser.parse "1.5 + 1.3"
                    |> Expect.equal (Ok (EAdd (EFloat 1.5) (EFloat 1.3)))
        , test "read nested operations" <|
            \_ ->
                Parser.parse "1 - (3 - 2)"
                    |> Expect.equal (Ok (ESub (EInt 1) (ESub (EInt 3) (EInt 2))))
        , test "read function execution" <|
            \_ ->
                Parser.parse "\\sqrt{5}"
                    |> Expect.equal (Ok (EFn (Identifier "sqrt") (EInt 5)))
        ]
