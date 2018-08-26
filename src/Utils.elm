module Utils exposing (..)

import Combine exposing (..)


parserFromMaybe : String -> Maybe a -> Parser s a
parserFromMaybe err maybe =
    case maybe of
        Just value ->
            succeed value

        Nothing ->
            fail err
