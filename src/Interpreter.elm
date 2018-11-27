module Interpreter exposing (run, runSymbol)

import Types exposing (..)


run : Types.Program -> List Float
run =
    List.map runExpression


runExpression : Expression -> Float
runExpression expr =
    case expr of
        Integer val ->
            toFloat val

        Floating val ->
            val

        Addition e1 e2 ->
            runExpression e1 + runExpression e2

        Subtraction e1 e2 ->
            runExpression e1 - runExpression e2

        Multiplication e1 e2 ->
            runExpression e1 * runExpression e2

        Division e1 e2 ->
            runExpression e1 / runExpression e2

        Exponentiation e1 e2 ->
            runExpression e1 ^ runExpression e2

        SymbolicFunction symbol ->
            runSymbol symbol


runSymbol : Symbol -> Float
runSymbol symbol =
    case symbol of
        SingleArity sym expr1 ->
            case sym of
                Sqrt ->
                    sqrt (runExpression expr1)

        DoubleArity sym expr1 expr2 ->
            case sym of
                Frac ->
                    runExpression expr1 / runExpression expr2

        Iterator sym expr1 expr2 expr3 ->
            case sym of
                Sum_ ->
                    let
                        lowerLimit =
                            runExpression expr1

                        upperLimit =
                            runExpression expr2

                        range =
                            -- TODO: remove round, make sure expression is int
                            List.range (round lowerLimit) (round upperLimit)
                    in
                    List.foldl (\curr total -> total + runExpression expr3) 0 range
