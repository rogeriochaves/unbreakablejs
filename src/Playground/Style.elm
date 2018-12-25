module Playground.Style exposing (body, cell, cellLabelInput, cellLabelOutput, notebook, selectedCell)

import Html exposing (..)
import Html.Attributes exposing (..)


body =
    [ style "font-family" "sans-serif"
    , style "background" "#EEE"
    ]


notebook =
    [ style "background" "#FFF"
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
    , style "font-family" "monospace, sans-serif"
    , style "text-align" "right"
    ]


cellLabelOutput =
    cellLabelInput ++ [ style "color" "#D84315" ]
