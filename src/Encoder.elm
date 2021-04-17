module Encoder exposing (encode)

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

        Object _ ->
            "[object Object]"

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
