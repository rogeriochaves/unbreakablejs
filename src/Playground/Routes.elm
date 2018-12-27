module Playground.Routes exposing (Page(..), routes, toPath)

import Browser.Navigation
import Url.Parser exposing ((</>), Parser, map, oneOf, parse, s, top)


type Page
    = Playground
    | About


routes : Parser (Page -> a) a
routes =
    oneOf
        [ map Playground top
        , map About (s "about")
        ]


toPath : Page -> String
toPath page =
    case page of
        Playground ->
            "/"

        About ->
            "/about"
