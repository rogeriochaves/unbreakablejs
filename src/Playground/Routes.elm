module Playground.Routes exposing (Page(..), routes, toPath)

import Browser.Navigation
import Url.Parser exposing ((</>), Parser, fragment, map, oneOf, parse, string, top)


type Page
    = Playground


routes : Parser (Page -> a) a
routes =
    oneOf
        [ map Playground top
        ]


toPath : Page -> String
toPath page =
    case page of
        Playground ->
            "/"
