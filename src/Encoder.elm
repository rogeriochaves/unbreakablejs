module Encoder exposing (encode)

import Dict
import Types exposing (..)


encode : Value -> String
encode value =
    case value of
        Number num ->
            String.fromFloat num

        Array items ->
            "["
                ++ (items
                        |> List.map encode
                        |> String.join ", "
                   )
                ++ "]"

        Object dict ->
            "Object {"
                ++ (Dict.toList dict
                        |> List.map
                            (\( key, val ) ->
                                if String.contains " " key then
                                    "\"" ++ key ++ "\": " ++ encode val

                                else
                                    key ++ ": " ++ encode val
                            )
                        |> String.join ", "
                   )
                ++ "}"

        Abstraction _ _ ->
            "[Function]"

        Undefined _ ->
            "undefined"

        Boolean bool ->
            if bool then
                "true"

            else
                "false"

        String string ->
            "\"" ++ string ++ "\""

        ReturnValue val ->
            encode val
