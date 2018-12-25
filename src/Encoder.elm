module Encoder exposing (encode)

import Return exposing (Value(..), throwError)
import Types exposing (..)


encode : Expression -> String
encode expr =
    let
        encodeIdentifier id =
            case id of
                ScalarIdentifier name ->
                    name

                VectorIdentifier name ->
                    "\\vec{" ++ name ++ "}"

        encodeSingleOp op e1 =
            case op of
                Assignment id ->
                    case e1 of
                        Abstraction _ _ ->
                            encodeIdentifier id ++ encode e1

                        MapAbstraction _ _ _ ->
                            encodeIdentifier id ++ encode e1

                        _ ->
                            encodeIdentifier id ++ " = " ++ encode e1

                Application e0 ->
                    encode e0 ++ "(" ++ encode e1 ++ ")"

                Sqrt ->
                    "\\sqrt{" ++ encode e1 ++ "}"

        encodeDoubleOp op e1 e2 =
            case op of
                Addition ->
                    encode e1 ++ " + " ++ encode e2

                Subtraction ->
                    encode e1 ++ " - " ++ encode e2

                Multiplication ->
                    encode e1 ++ " * " ++ encode e2

                Division ->
                    encode e1 ++ " / " ++ encode e2

                Exponentiation ->
                    let
                        simple =
                            encode e1 ++ " ^ " ++ encode e2

                        complex =
                            encode e1 ++ " ^ {" ++ encode e2 ++ "}"
                    in
                    case e2 of
                        Number _ ->
                            simple

                        Vector _ ->
                            simple

                        Variable _ ->
                            simple

                        _ ->
                            complex

                Frac ->
                    "\\frac{" ++ encode e1 ++ "}{" ++ encode e2 ++ "}"

                Index ->
                    encode e1 ++ "_{" ++ encode e2 ++ "}"
    in
    case expr of
        Number num ->
            String.fromFloat num

        Vector items ->
            "("
                ++ (List.map encode items
                        |> String.join ", "
                   )
                ++ ")"

        Variable id ->
            encodeIdentifier id

        SingleArity op e ->
            encodeSingleOp op e

        DoubleArity op e1 e2 ->
            encodeDoubleOp op e1 e2

        TripleArity op e1 e2 e3 ->
            case op of
                Sum_ var ->
                    "\\sum_{" ++ var ++ " = " ++ encode e1 ++ "}^{" ++ encode e2 ++ "} " ++ encode e3

        Abstraction id e ->
            "(" ++ encodeIdentifier id ++ ") = " ++ encode e

        MapAbstraction var index e ->
            "(\\vec{" ++ var ++ "})_{" ++ index ++ "} = " ++ encode e
