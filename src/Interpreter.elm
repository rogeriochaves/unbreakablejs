module Interpreter exposing (..)

import Types exposing (..)


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

        ESymbolicFunction symbol ->
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
