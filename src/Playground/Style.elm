module Playground.Style exposing (cell, cellLabelInput, cellLabelOutput, errorMessage, general, header, monospace, notebook, selectedCell, title, toolbarButton)

import Html exposing (..)
import Html.Attributes exposing (..)


general =
    [ style "font-family" "sans-serif"
    ]


header =
    [ style "background" "#FFF"
    , style "box-shadow" "0px 0px 12px 1px rgba(87, 87, 87, 0.2)"
    ]


title =
    [ style "font-weight" "normal"
    ]


notebook =
    [ style "background" "#FFF"
    , style "box-shadow" "0px 0px 12px 1px rgba(87, 87, 87, 0.2)"
    ]


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
    cellLabelInput ++ [ style "color" "#D84315" ]


errorMessage =
    [ style "color" "#E75C58"
    , monospace
    ]


toolbarButton =
    [ style "border" "1px solid #999"
    , style "background" "#FFF"
    , style "font-size" "13px"
    , style "border-radius" "5px"
    , style "cursor" "pointer"
    ]


monospace =
    style "font-family" "monospace, sans-serif"
