module Interpreter exposing (..)

import Parser exposing (..)


run : Expression -> Float
run expr =
    case expr of
        EInt val ->
            toFloat val

        EFloat val ->
            val

        EAdd e1 e2 ->
            run e1 + run e2

        ESub e1 e2 ->
            run e1 - run e2

        EMul e1 e2 ->
            run e1 * run e2

        EDiv e1 e2 ->
            run e1 / run e2

        EFn id expr ->
            case id of
                Identifier "sqrt" ->
                    sqrt (run expr)

                _ ->
                    Debug.crash "unknown function"
