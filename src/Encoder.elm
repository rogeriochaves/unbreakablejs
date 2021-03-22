module Encoder exposing (encode)

import Types exposing (..)


removeTracking : Types.Expression -> Types.UntrackedExp
removeTracking expr =
    case expr of
        Types.Tracked _ e ->
            e

        Types.Untracked e ->
            e


encode : UntrackedExp -> String
encode expr =
    case expr of
        Value val ->
            case val of
                Number num ->
                    String.fromFloat num

                Vector items ->
                    "["
                        ++ (items
                                |> List.map (removeTracking >> encode)
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

        _ ->
            "not implemented yet"
