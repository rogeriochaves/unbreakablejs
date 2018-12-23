module Playground exposing (main)

import AutoExpand as AutoExpand
import Browser
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed
import Interpreter
import List.Extra
import MathParser
import Playground.Style as Style


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    { cells : List Cell
    }


type alias Cell =
    { input : String
    , autoexpand : AutoExpand.State
    , result : String
    }


type Msg
    = UpdateInput Int { textValue : String, state : AutoExpand.State }
    | AddCell


init : () -> ( Model, Cmd Msg )
init flags =
    ( { cells =
            [ newCell 0
            ]
      }
    , Cmd.none
    )


newCell index =
    { input = ""
    , autoexpand = AutoExpand.initState (autoExpandConfig index)
    , result = ""
    }


emptyLatexState =
    { counters = Dict.fromList [ ( "s1", 0 ), ( "s2", 0 ), ( "s3", 0 ), ( "tno", 0 ), ( "eqno", 0 ) ]
    , crossReferences = Dict.empty
    , dictionary = Dict.empty
    , tableOfContents = []
    , macroDictionary = Dict.empty
    }


view : Model -> Html Msg
view model =
    div (Style.body ++ [ style "padding" "20px", style "margin" "-8px" ])
        [ h1 [] [ text "Explain Math" ]
        , toolbar
        , div (Style.notebook ++ [ style "padding" "20px" ])
            (List.indexedMap cell model.cells)
        ]


toolbar : Html Msg
toolbar =
    div [ style "padding-bottom" "20px" ]
        [ button [ onClick AddCell ] [ text "Add" ]
        , button [] [ text "Run" ]
        ]


cell : Int -> Cell -> Html Msg
cell index item =
    div []
        [ AutoExpand.view (autoExpandConfig index) item.autoexpand item.input
        , h2 [] [ text "latex output" ]
        , renderLatex item
        , h2 [] [ text "result" ]
        , text item.result
        ]


renderLatex : Cell -> Html Msg
renderLatex item =
    let
        convertedText =
            "$$\n" ++ String.replace "\n" "\\\\" item.input ++ "\n$$"
    in
    div []
        [ Html.Keyed.node "div"
            []
            [ ( item.input, div [ class "raw-math" ] [ text <| convertedText ] )
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateInput index { state, textValue } ->
            let
                updateCell cell_ =
                    { cell_
                        | autoexpand = state
                        , input = textValue
                        , result =
                            MathParser.parse textValue
                                |> Result.andThen Interpreter.run
                                |> Debug.toString
                    }
            in
            ( { model | cells = List.Extra.updateIfIndex ((==) index) updateCell model.cells }
            , Cmd.none
            )

        AddCell ->
            ( { model | cells = model.cells ++ [ newCell (List.length model.cells) ] }
            , Cmd.none
            )


autoExpandConfig : Int -> AutoExpand.Config Msg
autoExpandConfig index =
    AutoExpand.config
        { onInput = UpdateInput index
        , padding = 12
        , lineHeight = 21
        , minRows = 1
        , maxRows = 4
        }
        |> AutoExpand.withAttribute (Html.Attributes.style "resize" "none")
        |> AutoExpand.withAttribute (Html.Attributes.style "width" "100%")
