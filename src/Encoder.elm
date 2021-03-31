module Encoder exposing (encode)

import Types exposing (..)


removeTracking : Types.Expression -> Types.UntrackedExp
removeTracking expr =
    case expr of
        Types.Tracked _ e ->
            e

        Types.Untracked e ->
            e


getValue : UntrackedExp -> Value
getValue val =
    case val of
        Value v ->
            v

        _ ->
            Debug.todo "not implemented yet"


encode : Value -> String
encode value =
    case value of
        Number num ->
            String.fromFloat num

        Vector items ->
            "["
                ++ (items
                        |> List.map (removeTracking >> getValue >> encode)
                        |> String.join ", "
                   )
                ++ "]"

        Abstraction _ _ ->
            "[Function]"

        Undefined _ ->
            "undefined"

        Boolean bool ->
            if bool then
                "true"

            else
                "false"
