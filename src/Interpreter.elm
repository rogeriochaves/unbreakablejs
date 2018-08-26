module Interpreter exposing (run, runSymbol)

import Types exposing (..)


run : Expression -> Float
run expr =
    case expr of
        Integer val ->
            toFloat val

        Floating val ->
            val

        Addition e1 e2 ->
            run e1 + run e2

        Subtraction e1 e2 ->
            run e1 - run e2

        Multiplication e1 e2 ->
            run e1 * run e2

        Division e1 e2 ->
            run e1 / run e2

        Exponentiation e1 e2 ->
            run e1 ^ run e2

        SymbolicFunction symbol ->
            runSymbol symbol


runSymbol : Symbol -> Float
runSymbol symbol =
    case symbol of
        SingleArity sym expr1 ->
            case sym of
                Sqrt ->
                    sqrt (run expr1)

        DoubleArity sym expr1 expr2 ->
            case sym of
                Frac ->
                    run expr1 / run expr2
