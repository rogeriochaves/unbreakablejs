module Playground.Components exposing (column, container, header, row)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Playground.Style as Style
import Playground.Types exposing (..)


header : Html Msg
header =
    let
        menuLink attrs =
            a (Style.menuLink ++ [ style "padding" "10px 20px", class "menuLink" ] ++ attrs)

        submenuItem attrs =
            a (Style.submenuItem ++ [ style "padding" "10px 20px", style "display" "block", class "submenuItem" ] ++ attrs)
    in
    column [ style "padding-top" "20px" ]
        [ row []
            [ h1 (Style.title ++ [ style "margin" "0 0 5px -1px" ]) [ text "Rubber" ]
            , h2 (Style.smallSubtitle ++ [ style "margin-top" "0", style "padding-bottom" "10px" ]) [ text "Evaluate LaTeX math code (beta)" ]
            ]
        , column [ style "justify-content" "center", style "flex-grow" "1" ]
            [ menuLink [ href "#", onClick ClearPlayground ] [ text "Playground" ]
            , menuLink [ href "#", class "submenuLink" ]
                [ text "Examples "
                , span Style.utf8Icon [ text "▼" ]
                , row (Style.submenu ++ [ style "position" "absolute", style "min-width" "150px", style "margin" "5px 0 0 -10px", class "submenu" ])
                    [ submenuItem [ href "#", onClick (SetExample Basics) ] [ text "Basic Samples" ]
                    , submenuItem [ href "#", onClick (SetExample Softmax) ] [ text "Softmax" ]
                    , submenuItem [ href "#", onClick (SetExample Bitcoin) ] [ text "Bitcoin Paper Attack Chance" ]
                    ]
                ]
            , menuLink [ href "#about" ] [ text "About" ]
            , menuLink [ href "https://rogeriochaves.gitbooks.io/rubber/?", target "_blank" ] [ text "Docs" ]
            ]
        ]


row : List (Html.Attribute a) -> List (Html a) -> Html a
row =
    div


column : List (Html.Attribute a) -> List (Html a) -> Html a
column attrs =
    div ([ style "display" "flex" ] ++ attrs)


container : List (Html.Attribute a) -> List (Html a) -> Html a
container attrs children =
    column ([ style "justify-content" "center" ] ++ attrs)
        [ row [ style "flex-grow" "1", style "max-width" "1140px", style "padding" "0 10px" ]
            children
        ]
