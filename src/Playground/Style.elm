module Playground.Style exposing (card, cell, cellLabelInput, cellLabelOutput, errorMessage, footer, general, header, menuLink, monospace, notebook, selectedCell, smallSubtitle, submenu, submenuItem, title, toolbarButton, utf8Icon, warnMessage)

import Html exposing (..)
import Html.Attributes exposing (..)


general =
    [ style "font-family" "sans-serif"
    ]


card =
    [ style "background" "#FFF"
    , style "box-shadow" "0px 0px 12px 1px rgba(87, 87, 87, 0.2)"
    ]


header =
    card


title =
    [ style "font-weight" "normal"
    ]


smallSubtitle =
    [ style "font-weight" "normal"
    , style "color" "#AAA"
    , style "font-size" "15px"
    ]


menuLink =
    [ style "color" "rgb(40, 126, 213)"
    , style "font-size" "20px"
    , style "cursor" "pointer"
    , style "text-decoration" "none"
    ]


submenuItem =
    menuLink
        ++ [ style "font-size" "18px"
           , style "color" "#666"
           ]


utf8Icon =
    [ style "font-size" "0.8em"
    ]


submenu =
    [ style "box-shadow" "0px 0px 12px 1px rgba(87, 87, 87, 0.2)"
    , style "background" "#FFF"
    ]


notebook =
    card


cell =
    [ style "border" "1px solid #FFF"
    , style "border-left" "5px solid #FFF"
    ]


selectedCell =
    [ style "border" "1px solid #999"
    , style "border-left" "5px solid #42A5F5"
    ]


cellLabelInput =
    [ style "color" "#303F9F"
    , style "font-size" "14px"
    , monospace
    , style "text-align" "right"
    ]


cellLabelOutput =
    cellLabelInput ++ [ style "color" "#298539" ]


errorMessage =
    [ style "color" "#E75C58"
    , monospace
    ]


warnMessage =
    [ style "color" "#bd8d04"
    , monospace
    ]


toolbarButton =
    [ style "border" "1px solid #999"
    , style "background" "#FFF"
    , style "font-size" "13px"
    , style "border-radius" "5px"
    , style "cursor" "pointer"
    ]


footer =
    general
        ++ [ style "color" "#999"
           , style "font-size" "13px"
           ]


monospace =
    style "font-family" "monospace, sans-serif"
