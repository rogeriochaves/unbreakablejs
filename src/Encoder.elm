module Encoder exposing (encode)

import Return exposing (Value(..), throwError)
import Types exposing (..)


encode : Expression -> String
encode expr =
    "not implemented yet"



-- case expr of
--     Number num ->
--         String.fromFloat num
--     Vector items ->
--         "("
--             ++ (List.map encode items
--                     |> String.join ", "
--                )
--             ++ ")"
--     Variable id ->
--         id
--     Reserved reserved ->
--         case reserved of
--             Addition ->
--                 "+"
--     Application
