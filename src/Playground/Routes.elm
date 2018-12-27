module Playground.Routes exposing (Page(..), routes, toPath)

import Browser.Navigation
import Url.Parser exposing ((</>), Parser, fragment, map, oneOf, parse, top)


type Page
    = Playground
    | About


routes : Parser (Page -> a) a
routes =
    oneOf
        [ mapFragment About "about"
        , map Playground top
        ]


mapFragment : Page -> String -> Parser (Page -> c) c
mapFragment page path =
    map
        (\frag ->
            if Maybe.withDefault "" frag == path then
                page

            else
                Playground
        )
        (fragment identity)


toPath : Page -> String
toPath page =
    case page of
        Playground ->
            "/"

        About ->
            "#about"
