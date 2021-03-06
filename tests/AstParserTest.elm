module AstParserTest exposing (suite)

import AstParser exposing (..)
import Dict
import Expect
import Test exposing (..)
import Types exposing (..)


tracked : ( Int, Int ) -> UntrackedExp -> Expression
tracked ( line, column ) =
    Tracked { line = line, column = column, filename = "test.us" }


undefinedTrack : ( Int, Int ) -> UndefinedReason -> UndefinedTrackInfo
undefinedTrack ( line, column ) reason =
    { line = line, column = column, filename = "test.us", reason = reason }


parse : String -> Result Error Types.Program
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
                            (Operation2 Addition
                                (Untracked (Value (Number 1)))
                                (Untracked (Value (Number 1)))
                            )
                        )
        , test "read float numbers" <|
            \_ ->
                parse "1.5 + 1.3"
                    |> isEq
                        (tracked ( 1, 5 )
                            (Operation2 Addition (Untracked (Value (Number 1.5))) (Untracked (Value (Number 1.3))))
                        )
        , test "read nested operations" <|
            \_ ->
                parse "1 - (3 - 2)"
                    |> isEq
                        (tracked ( 1, 3 )
                            (Operation2 Subtraction
                                (Untracked (Value (Number 1)))
                                (tracked ( 1, 8 )
                                    (Operation2 Subtraction (Untracked (Value (Number 3))) (Untracked (Value (Number 2))))
                                )
                            )
                        )
        , test "parses variables" <|
            \_ ->
                parse "x"
                    |> isEq (tracked ( 1, 1 ) (Variable "x"))
        , test "parses variables with multiple letters" <|
            \_ ->
                parse "age + 1"
                    |> isEq
                        (tracked ( 1, 5 )
                            (Operation2 Addition
                                (tracked ( 1, 1 ) (Variable "age"))
                                (Untracked (Value (Number 1)))
                            )
                        )
        , test "parses boolean true" <|
            \_ ->
                parse "true"
                    |> isEq (Untracked (Value (Boolean True)))
        , test "parses boolean false" <|
            \_ ->
                parse "false"
                    |> isEq (Untracked (Value (Boolean False)))
        , test "parses undefined" <|
            \_ ->
                parse "undefined"
                    |> isEq (Untracked (Value (Undefined [ undefinedTrack ( 1, 1 ) ExplicitUndefined ])))
        , test "read increment" <|
            \_ ->
                parse "x++"
                    |> isEq
                        (tracked ( 1, 2 )
                            (Operation (Increment "x") (tracked ( 1, 1 ) (Variable "x")))
                        )
        , test "read decrement" <|
            \_ ->
                parse "x--"
                    |> isEq
                        (tracked ( 1, 2 )
                            (Operation (Decrement "x") (tracked ( 1, 1 ) (Variable "x")))
                        )
        , test "reads negation" <|
            \_ ->
                parse "!true"
                    |> isEq
                        (tracked ( 1, 1 )
                            (Operation Not (Untracked (Value (Boolean True))))
                        )
        , test "reads double negation" <|
            \_ ->
                parse "!!true"
                    |> isEq
                        (tracked ( 1, 1 )
                            (Operation Not
                                (tracked ( 1, 2 )
                                    (Operation Not (Untracked (Value (Boolean True))))
                                )
                            )
                        )
        , test "reads &&" <|
            \_ ->
                parse "true && false"
                    |> isEq
                        (tracked ( 1, 6 )
                            (Operation2 And
                                (Untracked (Value (Boolean True)))
                                (Untracked (Value (Boolean False)))
                            )
                        )
        , test "reads ||" <|
            \_ ->
                parse "true || false"
                    |> isEq
                        (tracked ( 1, 6 )
                            (Operation2 Or
                                (Untracked (Value (Boolean True)))
                                (Untracked (Value (Boolean False)))
                            )
                        )
        , test "reads negative" <|
            \_ ->
                parse "-1"
                    |> isEq
                        (tracked ( 1, 1 )
                            (Operation Negative (Untracked (Value (Number 1))))
                        )
        , test "reads multiplication" <|
            \_ ->
                parse "2 * 2"
                    |> isEq
                        (tracked ( 1, 3 )
                            (Operation2 Multiplication
                                (Untracked (Value (Number 2)))
                                (Untracked (Value (Number 2)))
                            )
                        )
        , test "reads division" <|
            \_ ->
                parse "2 / 2"
                    |> isEq
                        (tracked ( 1, 3 )
                            (Operation2 Division
                                (Untracked (Value (Number 2)))
                                (Untracked (Value (Number 2)))
                            )
                        )
        , test "reads exponentiation" <|
            \_ ->
                parse "2 ** 2"
                    |> isEq
                        (tracked ( 1, 3 )
                            (Operation2 Exponentiation
                                (Untracked (Value (Number 2)))
                                (Untracked (Value (Number 2)))
                            )
                        )
        , test "reads remainder" <|
            \_ ->
                parse "2 % 2"
                    |> isEq
                        (tracked ( 1, 3 )
                            (Operation2 Remainder
                                (Untracked (Value (Number 2)))
                                (Untracked (Value (Number 2)))
                            )
                        )
        , test "allow spaces at the beginning" <|
            \_ ->
                parse "    \n1 + 1"
                    |> isEq
                        (tracked ( 2, 3 )
                            (Operation2 Addition
                                (Untracked (Value (Number 1)))
                                (Untracked (Value (Number 1)))
                            )
                        )
        , test "allow multiple statement breaks" <|
            \_ ->
                parse "    \n1 + 1;;;;;"
                    |> isEq
                        (tracked ( 2, 3 )
                            (Operation2 Addition
                                (Untracked (Value (Number 1)))
                                (Untracked (Value (Number 1)))
                            )
                        )
        , test "ignore code comments" <|
            \_ ->
                parse "1 + 1 // this is making a sum of two numbers"
                    |> isEq
                        (tracked ( 1, 3 )
                            (Operation2 Addition
                                (Untracked (Value (Number 1)))
                                (Untracked (Value (Number 1)))
                            )
                        )
        , test "ignore code comments at the beginning" <|
            \_ ->
                parse "// this is making a sum of two numbers\n1 + 1"
                    |> isEq
                        (tracked ( 3, 3 )
                            (Operation2 Addition
                                (Untracked (Value (Number 1)))
                                (Untracked (Value (Number 1)))
                            )
                        )
        , describe "multiple lines"
            [ test "parses multiple expressions" <|
                \_ ->
                    parse "1 + 1\n2 + 2"
                        |> Expect.equal
                            (Ok
                                [ tracked ( 1, 3 )
                                    (Operation2 Addition
                                        (Untracked (Value (Number 1)))
                                        (Untracked (Value (Number 1)))
                                    )
                                , tracked ( 2, 3 )
                                    (Operation2 Addition
                                        (Untracked (Value (Number 2)))
                                        (Untracked (Value (Number 2)))
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
            , test "unless there is a semicolon to split them" <|
                \_ ->
                    parse "1 + 1; 2 + 2"
                        |> Expect.equal
                            (Ok
                                [ tracked ( 1, 3 )
                                    (Operation2 Addition
                                        (Untracked (Value (Number 1)))
                                        (Untracked (Value (Number 1)))
                                    )
                                , tracked ( 1, 10 )
                                    (Operation2 Addition
                                        (Untracked (Value (Number 2)))
                                        (Untracked (Value (Number 2)))
                                    )
                                ]
                            )
            , test "allow multiple line breaks" <|
                \_ ->
                    parse "1 + 1\n\n2 + 2\n"
                        |> Expect.equal
                            (Ok
                                [ tracked ( 1, 3 )
                                    (Operation2 Addition
                                        (Untracked (Value (Number 1)))
                                        (Untracked (Value (Number 1)))
                                    )
                                , tracked ( 3, 3 )
                                    (Operation2 Addition
                                        (Untracked (Value (Number 2)))
                                        (Untracked (Value (Number 2)))
                                    )
                                ]
                            )
            , test "allow empty lines and trailing spaces" <|
                \_ ->
                    parse "1 + 1 \n \n2 + 2\n"
                        |> Expect.equal
                            (Ok
                                [ tracked ( 1, 3 )
                                    (Operation2 Addition
                                        (Untracked (Value (Number 1)))
                                        (Untracked (Value (Number 1)))
                                    )
                                , tracked ( 3, 3 )
                                    (Operation2 Addition
                                        (Untracked (Value (Number 2)))
                                        (Untracked (Value (Number 2)))
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
                                (Operation (Assignment "x")
                                    (tracked ( 1, 7 )
                                        (Operation2 Addition (Untracked <| Value (Number 1)) (Untracked <| Value (Number 1)))
                                    )
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
                                (Operation2 Addition
                                    (tracked ( 1, 1 ) <| Variable "x")
                                    (Untracked <| Value (Number 1))
                                )
                            )
            , test "parses assignment with variables" <|
                \_ ->
                    parse "x = y + 1"
                        |> isEq
                            (tracked ( 1, 3 )
                                (Operation (Assignment "x")
                                    (tracked ( 1, 7 ) <|
                                        Operation2 Addition
                                            (tracked ( 1, 5 ) <| Variable "y")
                                            (Untracked <| Value (Number 1))
                                    )
                                )
                            )
            , test "parses assignment with let" <|
                \_ ->
                    parse "let x = 1"
                        |> isEq
                            (tracked ( 1, 7 )
                                (Operation (LetAssignment "x")
                                    (Untracked <| Value (Number 1))
                                )
                            )
            , test "parses += assignment" <|
                \_ ->
                    parse "x += 1"
                        |> isEq
                            (tracked ( 1, 4 )
                                (Operation (Assignment "x")
                                    (tracked ( 1, 3 ) <|
                                        Operation2 Addition
                                            (tracked ( 1, 1 ) <| Variable "x")
                                            (Untracked <| Value (Number 1))
                                    )
                                )
                            )
            , test "parses -= assignment" <|
                \_ ->
                    parse "x -= 1"
                        |> isEq
                            (tracked ( 1, 4 )
                                (Operation (Assignment "x")
                                    (tracked ( 1, 3 ) <|
                                        Operation2 Subtraction
                                            (tracked ( 1, 1 ) <| Variable "x")
                                            (Untracked <| Value (Number 1))
                                    )
                                )
                            )
            , test "parses *= assignment" <|
                \_ ->
                    parse "x *= 1"
                        |> isEq
                            (tracked ( 1, 4 )
                                (Operation (Assignment "x")
                                    (tracked ( 1, 3 ) <|
                                        Operation2 Multiplication
                                            (tracked ( 1, 1 ) <| Variable "x")
                                            (Untracked <| Value (Number 1))
                                    )
                                )
                            )
            , test "parses /= assignment" <|
                \_ ->
                    parse "x /= 1"
                        |> isEq
                            (tracked ( 1, 4 )
                                (Operation (Assignment "x")
                                    (tracked ( 1, 3 ) <|
                                        Operation2 Division
                                            (tracked ( 1, 1 ) <| Variable "x")
                                            (Untracked <| Value (Number 1))
                                    )
                                )
                            )
            , test "parses %= assignment" <|
                \_ ->
                    parse "x %= 1"
                        |> isEq
                            (tracked ( 1, 4 )
                                (Operation (Assignment "x")
                                    (tracked ( 1, 3 ) <|
                                        Operation2 Remainder
                                            (tracked ( 1, 1 ) <| Variable "x")
                                            (Untracked <| Value (Number 1))
                                    )
                                )
                            )
            , test "parses **= assignment" <|
                \_ ->
                    parse "x **= 1"
                        |> isEq
                            (tracked ( 1, 5 )
                                (Operation (Assignment "x")
                                    (tracked ( 1, 3 ) <|
                                        Operation2 Exponentiation
                                            (tracked ( 1, 1 ) <| Variable "x")
                                            (Untracked <| Value (Number 1))
                                    )
                                )
                            )
            ]
        , describe "functions"
            [ test "parses function declaration" <|
                \_ ->
                    parse "f = (x) => x + 1"
                        |> isEq
                            (tracked ( 1, 3 )
                                (Operation
                                    (Assignment "f")
                                    (tracked ( 1, 5 )
                                        (Value
                                            (Abstraction [ "x" ]
                                                (tracked ( 1, 14 )
                                                    (Operation2 Addition
                                                        (tracked ( 1, 12 ) (Variable "x"))
                                                        (Untracked (Value (Number 1)))
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            )
            , test "parses function declaration with let" <|
                \_ ->
                    parse "let f = (x) => x + 1"
                        |> isEq
                            (tracked ( 1, 7 )
                                (Operation
                                    (LetAssignment "f")
                                    (tracked ( 1, 9 )
                                        (Value
                                            (Abstraction [ "x" ]
                                                (tracked ( 1, 18 )
                                                    (Operation2 Addition
                                                        (tracked ( 1, 16 ) (Variable "x"))
                                                        (Untracked (Value (Number 1)))
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            )
            , test "parses curried functions" <|
                \_ ->
                    parse "(x) => (y) => x + y"
                        |> isEq
                            (tracked ( 1, 1 )
                                (Value
                                    (Abstraction [ "x" ]
                                        (tracked ( 1, 8 )
                                            (Value
                                                (Abstraction [ "y" ]
                                                    (tracked ( 1, 17 )
                                                        (Operation2 Addition
                                                            (tracked ( 1, 15 ) (Variable "x"))
                                                            (tracked ( 1, 19 ) (Variable "y"))
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            )
            , test "parses function declaration with multiple arguments" <|
                \_ ->
                    parse "f = (x, y) => x + 1"
                        |> isEq
                            (tracked ( 1, 3 )
                                (Operation
                                    (Assignment "f")
                                    (tracked ( 1, 5 )
                                        (Value
                                            (Abstraction [ "x", "y" ]
                                                (tracked ( 1, 17 )
                                                    (Operation2 Addition
                                                        (tracked ( 1, 15 ) (Variable "x"))
                                                        (Untracked (Value (Number 1)))
                                                    )
                                                )
                                            )
                                        )
                                    )
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
                            (tracked ( 1, 2 )
                                (Application (tracked ( 1, 1 ) <| Variable "f") [ Untracked <| Value (Number 5) ])
                            )
            , test "parses nested function call" <|
                \_ ->
                    parse "f(5)(4)"
                        |> isEq
                            (tracked ( 1, 5 )
                                (Application
                                    (tracked ( 1, 2 )
                                        (Application (tracked ( 1, 1 ) <| Variable "f")
                                            [ Untracked (Value (Number 5))
                                            ]
                                        )
                                    )
                                    [ Untracked (Value (Number 4)) ]
                                )
                            )
            , test "parses expression function call" <|
                \_ ->
                    parse "(1 + 1)(5)"
                        |> isEq
                            (tracked ( 1, 8 )
                                (Application
                                    (tracked ( 1, 4 )
                                        (Operation2 Addition
                                            (Untracked (Value (Number 1)))
                                            (Untracked (Value (Number 1)))
                                        )
                                    )
                                    [ Untracked (Value (Number 5)) ]
                                )
                            )
            , test "parses function call with multiple arguments" <|
                \_ ->
                    parse "f(3, 2)"
                        |> isEq
                            (tracked ( 1, 2 )
                                (Application (tracked ( 1, 1 ) <| Variable "f")
                                    [ Untracked <| Value (Number 3)
                                    , Untracked <| Value (Number 2)
                                    ]
                                )
                            )
            , test "parses named function declaration as let assignments" <|
                \_ ->
                    parse "function plusOne(x) {\nx + 1;\n}"
                        |> isEq
                            (tracked ( 1, 1 )
                                (Operation
                                    (LetAssignment "plusOne")
                                    (tracked ( 1, 1 )
                                        (Value
                                            (Abstraction [ "x" ]
                                                (tracked ( 3, 2 )
                                                    (Block
                                                        [ tracked ( 2, 3 )
                                                            (Operation2 Addition
                                                                (tracked ( 2, 1 ) (Variable "x"))
                                                                (Untracked (Value (Number 1)))
                                                            )
                                                        ]
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            )
            , test "parses anonymous function" <|
                \_ ->
                    parse "function (x) {\nx + 1;\n}"
                        |> isEq
                            (tracked ( 1, 1 )
                                (Value
                                    (Abstraction [ "x" ]
                                        (tracked ( 3, 2 )
                                            (Block
                                                [ tracked ( 2, 3 )
                                                    (Operation2 Addition
                                                        (tracked ( 2, 1 ) (Variable "x"))
                                                        (Untracked (Value (Number 1)))
                                                    )
                                                ]
                                            )
                                        )
                                    )
                                )
                            )
            ]
        , describe "blocks"
            [ test "parses blocks breaking lines" <|
                \_ ->
                    parse "{\n1 + 1\n2 + 2\n}"
                        |> isEq
                            (tracked ( 4, 2 )
                                (Block
                                    [ tracked ( 2, 3 )
                                        (Operation2 Addition
                                            (Untracked (Value (Number 1)))
                                            (Untracked (Value (Number 1)))
                                        )
                                    , tracked ( 3, 3 )
                                        (Operation2 Addition
                                            (Untracked (Value (Number 2)))
                                            (Untracked (Value (Number 2)))
                                        )
                                    ]
                                )
                            )
            , test "parses blocks in the same line" <|
                \_ ->
                    parse "{1 + 1\n2 + 2}"
                        |> isEq
                            (tracked ( 2, 7 )
                                (Block
                                    [ tracked ( 1, 4 )
                                        (Operation2 Addition
                                            (Untracked (Value (Number 1)))
                                            (Untracked (Value (Number 1)))
                                        )
                                    , tracked ( 2, 3 )
                                        (Operation2 Addition
                                            (Untracked (Value (Number 2)))
                                            (Untracked (Value (Number 2)))
                                        )
                                    ]
                                )
                            )
            , test "parses blocks in function body" <|
                \_ ->
                    parse "f = (x) => { 1 + 1\n2 + 2 }"
                        |> isEq
                            (tracked ( 1, 3 )
                                (Operation
                                    (Assignment "f")
                                    (tracked ( 1, 5 )
                                        (Value
                                            (Abstraction [ "x" ]
                                                (tracked ( 2, 8 )
                                                    (Block
                                                        [ tracked ( 1, 16 )
                                                            (Operation2 Addition
                                                                (Untracked (Value (Number 1)))
                                                                (Untracked (Value (Number 1)))
                                                            )
                                                        , tracked ( 2, 3 )
                                                            (Operation2 Addition
                                                                (Untracked (Value (Number 2)))
                                                                (Untracked (Value (Number 2)))
                                                            )
                                                        ]
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            )
            , test "parses assignments inside the block" <|
                \_ ->
                    parse "{x = 1\nx + 1}"
                        |> isEq
                            (tracked ( 2, 7 )
                                (Block
                                    [ tracked ( 1, 4 )
                                        (Operation (Assignment "x")
                                            (Untracked (Value (Number 1)))
                                        )
                                    , tracked ( 2, 3 )
                                        (Operation2 Addition
                                            (tracked ( 2, 1 ) <| Variable "x")
                                            (Untracked (Value (Number 1)))
                                        )
                                    ]
                                )
                            )
            , test "parses return in function body" <|
                \_ ->
                    parse "f = (x) => { return 1 + 1 }"
                        |> isEq
                            (tracked ( 1, 3 )
                                (Operation
                                    (Assignment "f")
                                    (tracked ( 1, 5 )
                                        (Value
                                            (Abstraction [ "x" ]
                                                (tracked ( 1, 28 )
                                                    (Block
                                                        [ tracked ( 1, 14 )
                                                            (Return
                                                                (tracked ( 1, 23 )
                                                                    (Operation2 Addition
                                                                        (Untracked (Value (Number 1)))
                                                                        (Untracked (Value (Number 1)))
                                                                    )
                                                                )
                                                            )
                                                        ]
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            )
            , test "breaks for return outside function body" <|
                \_ ->
                    parse "{ return 1 + 1 }"
                        |> isErr
                        |> Expect.true "it should break for returns in blocks outside function body"
            ]
        , describe "if conditions" <|
            [ test "parses comparison" <|
                \_ ->
                    parse "1 + 1 == 2"
                        |> isEq
                            (tracked ( 1, 7 )
                                (Operation2 SoftEquality
                                    (tracked ( 1, 3 )
                                        (Operation2 Addition
                                            (Untracked (Value (Number 1)))
                                            (Untracked (Value (Number 1)))
                                        )
                                    )
                                    (Untracked (Value (Number 2)))
                                )
                            )
            , test "parses boolean comparison" <|
                \_ ->
                    parse "true == false"
                        |> isEq
                            (tracked ( 1, 6 )
                                (Operation2 SoftEquality
                                    (Untracked (Value (Boolean True)))
                                    (Untracked (Value (Boolean False)))
                                )
                            )
            , test "parses hard equality" <|
                \_ ->
                    parse "1 + 1 === 2"
                        |> isEq
                            (tracked ( 1, 7 )
                                (Operation2 HardEquality
                                    (tracked ( 1, 3 )
                                        (Operation2 Addition
                                            (Untracked (Value (Number 1)))
                                            (Untracked (Value (Number 1)))
                                        )
                                    )
                                    (Untracked (Value (Number 2)))
                                )
                            )
            , test "parses soft not equal" <|
                \_ ->
                    parse "true != false"
                        |> isEq
                            (tracked ( 1, 6 )
                                (Operation2 SoftNotEquality
                                    (Untracked (Value (Boolean True)))
                                    (Untracked (Value (Boolean False)))
                                )
                            )
            , test "parses hard not equal" <|
                \_ ->
                    parse "true !== false"
                        |> isEq
                            (tracked ( 1, 6 )
                                (Operation2 HardNotEquality
                                    (Untracked (Value (Boolean True)))
                                    (Untracked (Value (Boolean False)))
                                )
                            )
            , test "parses greater than comparison" <|
                \_ ->
                    parse "1 > 2"
                        |> isEq
                            (tracked ( 1, 3 )
                                (Operation2
                                    GreaterThan
                                    (Untracked (Value (Number 1)))
                                    (Untracked (Value (Number 2)))
                                )
                            )
            , test "parses smaller than comparison" <|
                \_ ->
                    parse "1 < 2"
                        |> isEq
                            (tracked ( 1, 3 )
                                (Operation2
                                    SmallerThan
                                    (Untracked (Value (Number 1)))
                                    (Untracked (Value (Number 2)))
                                )
                            )
            , test "parses smaller or equal than comparison" <|
                \_ ->
                    parse "1 <= 2"
                        |> isEq
                            (tracked ( 1, 3 )
                                (Operation2
                                    SmallerOrEqualThan
                                    (Untracked (Value (Number 1)))
                                    (Untracked (Value (Number 2)))
                                )
                            )
            , test "parses greater or equal than comparison" <|
                \_ ->
                    parse "1 >= 2"
                        |> isEq
                            (tracked ( 1, 3 )
                                (Operation2
                                    GreaterOrEqualThan
                                    (Untracked (Value (Number 1)))
                                    (Untracked (Value (Number 2)))
                                )
                            )
            , test "parses if condition" <|
                \_ ->
                    parse "if (true) {x = 1}"
                        |> isEq
                            (tracked ( 1, 1 )
                                (IfCondition (Untracked (Value (Boolean True)))
                                    (tracked ( 1, 18 )
                                        (Block
                                            [ tracked ( 1, 14 )
                                                (Operation (Assignment "x")
                                                    (Untracked (Value (Number 1)))
                                                )
                                            ]
                                        )
                                    )
                                )
                            )
            , test "parses if-else condition" <|
                \_ ->
                    parse "if (true) { x = 1 } else { x = 2 }"
                        |> isEq
                            (tracked ( 1, 1 )
                                (IfElseCondition (Untracked (Value (Boolean True)))
                                    (tracked ( 1, 20 )
                                        (Block
                                            [ tracked ( 1, 15 )
                                                (Operation (Assignment "x")
                                                    (Untracked (Value (Number 1)))
                                                )
                                            ]
                                        )
                                    )
                                    (tracked ( 1, 35 )
                                        (Block
                                            [ tracked ( 1, 30 )
                                                (Operation (Assignment "x")
                                                    (Untracked (Value (Number 2)))
                                                )
                                            ]
                                        )
                                    )
                                )
                            )
            ]
        , describe "loops" <|
            [ test "parses while loop" <|
                \_ ->
                    parse "while (true) { 1 + 1 }"
                        |> isEq
                            (tracked ( 1, 1 )
                                (While
                                    (Untracked (Value (Boolean True)))
                                    (tracked ( 1, 23 )
                                        (Block
                                            [ tracked ( 1, 18 )
                                                (Operation2 Addition
                                                    (Untracked (Value (Number 1)))
                                                    (Untracked (Value (Number 1)))
                                                )
                                            ]
                                        )
                                    )
                                )
                            )
            , test "parses for-loop" <|
                \_ ->
                    parse "for (let i = 0; i < 5; i++) { 1 + 1 }"
                        |> isEq
                            (tracked ( 1, 1 )
                                (ForLoop
                                    (tracked ( 1, 12 )
                                        (Operation (LetAssignment "i")
                                            (Untracked (Value (Number 0)))
                                        )
                                    )
                                    (tracked ( 1, 19 )
                                        (Operation2
                                            SmallerThan
                                            (tracked ( 1, 17 ) (Variable "i"))
                                            (Untracked (Value (Number 5)))
                                        )
                                    )
                                    (tracked ( 1, 25 )
                                        (Operation (Increment "i") (tracked ( 1, 24 ) (Variable "i")))
                                    )
                                    (tracked ( 1, 38 )
                                        (Block
                                            [ tracked ( 1, 33 )
                                                (Operation2 Addition
                                                    (Untracked (Value (Number 1)))
                                                    (Untracked (Value (Number 1)))
                                                )
                                            ]
                                        )
                                    )
                                )
                            )
            ]
        , describe "strings" <|
            [ test "parses string" <|
                \_ ->
                    parse "\"foo\""
                        |> isEq
                            (Untracked (Value (String "foo")))
            , test "breaks on multiline" <|
                \_ ->
                    parse "\"foo\nbar\""
                        |> isErr
                        |> Expect.true "it should break for unclosed multiline strings"
            , test "parses single-quote strings" <|
                \_ ->
                    parse "'foo'"
                        |> isEq
                            (Untracked (Value (String "foo")))
            ]
        , describe "arrays"
            [ test "parses simple array" <|
                \_ ->
                    parse "[1, 2, 3]"
                        |> isEq
                            (tracked ( 1, 1 )
                                (ArrayExpression
                                    [ Untracked (Value (Number 1))
                                    , Untracked (Value (Number 2))
                                    , Untracked (Value (Number 3))
                                    ]
                                )
                            )
            , test "gets array position" <|
                \_ ->
                    parse "a[1]"
                        |> isEq
                            (tracked ( 1, 2 )
                                (Operation2 Member
                                    (tracked ( 1, 1 ) (Variable "a"))
                                    (Untracked (Value (Number 1)))
                                )
                            )
            , test "gets array of arrays position" <|
                \_ ->
                    parse "a[1][0]"
                        |> isEq
                            (tracked ( 1, 5 )
                                (Operation2 Member
                                    (tracked ( 1, 2 )
                                        (Operation2 Member
                                            (tracked ( 1, 1 ) (Variable "a"))
                                            (Untracked (Value (Number 1)))
                                        )
                                    )
                                    (Untracked (Value (Number 0)))
                                )
                            )
            , test "gets array position of expression" <|
                \_ ->
                    parse "(1 + 1)[1]"
                        |> isEq
                            (tracked ( 1, 8 )
                                (Operation2 Member
                                    (tracked ( 1, 4 )
                                        (Operation2 Addition
                                            (Untracked (Value (Number 1)))
                                            (Untracked (Value (Number 1)))
                                        )
                                    )
                                    (Untracked (Value (Number 1)))
                                )
                            )
            ]
        , describe "objects"
            [ test "parses object with multiple types of keys" <|
                \_ ->
                    parse "{ foo: 'bar', 'baz qux': 5, \"pudim\": false, 5: 7, 5.7: 8 }"
                        |> isEq
                            (tracked ( 1, 1 )
                                (ObjectExpression
                                    (Dict.fromList
                                        [ ( "foo", Untracked (Value (String "bar")) )
                                        , ( "baz qux", Untracked (Value (Number 5)) )
                                        , ( "pudim", Untracked (Value (Boolean False)) )
                                        , ( "5", Untracked (Value (Number 7)) )
                                        , ( "5.7", Untracked (Value (Number 8)) )
                                        ]
                                    )
                                )
                            )
            , test "parses empty object" <|
                \_ ->
                    parse "x = {}"
                        |> isEq
                            (tracked ( 1, 3 )
                                (Operation (Assignment "x")
                                    (tracked ( 1, 5 ) (ObjectExpression Dict.empty))
                                )
                            )
            , test "gets property from object using dot notation" <|
                \_ ->
                    parse "a.foo"
                        |> isEq
                            (tracked ( 1, 2 )
                                (Operation2 Member
                                    (tracked ( 1, 1 ) (Variable "a"))
                                    (Untracked (Value (String "foo")))
                                )
                            )
            ]
        ]


isErr : Result error value -> Bool
isErr result =
    case result of
        Ok _ ->
            False

        Err _ ->
            True


isEq : a -> Result error (List a) -> Expect.Expectation
isEq result =
    Expect.equal (Ok [ result ])
