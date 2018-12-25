module Playground exposing (main)

import AutoExpand as AutoExpand
import Browser
import Dict
import Encoder exposing (encode)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed
import Interpreter
import List.Extra
import MathParser
import Parser exposing (Problem(..))
import Playground.Style as Style
import Return exposing (Value(..))
import Types exposing (Error)


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
    , selectedCell : Int
    }


type alias Cell =
    { input : String
    , autoexpand : AutoExpand.State
    , result : Return.Value
    }


type Msg
    = UpdateInput Int { textValue : String, state : AutoExpand.State }
    | AddCell
    | SelectCell Int
    | RunCell


init : () -> ( Model, Cmd Msg )
init flags =
    ( { cells =
            [ newCell 0
            ]
      , selectedCell = -1
      }
    , Cmd.none
    )


newCell : Int -> Cell
newCell index =
    { input = ""
    , autoexpand = AutoExpand.initState (autoExpandConfig index)
    , result = Void
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
        , toolbarView
        , div (Style.notebook ++ [ style "padding" "15px" ])
            (List.indexedMap (cellView model) model.cells)
        ]


toolbarView : Html Msg
toolbarView =
    div [ style "padding-bottom" "20px" ]
        [ button [ onClick AddCell ] [ text "Add" ]
        , button [ onClick RunCell ] [ text "Run" ]
        ]


cellView : Model -> Int -> Cell -> Html Msg
cellView model index item =
    let
        cellStyle =
            if model.selectedCell == index then
                Style.selectedCell

            else
                Style.cell
    in
    div (cellStyle ++ [ onClick (SelectCell index), style "padding" "5px", style "margin-bottom" "20px" ])
        [ div [ style "display" "flex" ]
            [ cellLabelView Style.cellLabelInput "Input:"
            , if String.isEmpty item.input || index == model.selectedCell then
                AutoExpand.view (autoExpandConfig index) item.autoexpand item.input

              else
                renderLatex item.input
            ]
        , renderResult item
        ]


renderResult : Cell -> Html Msg
renderResult item =
    case item.result of
        Expression expr ->
            div [ style "display" "flex" ]
                [ cellLabelView Style.cellLabelOutput "Output:"
                , renderLatex (encode expr)
                ]

        Void ->
            div [] []

        Error err ->
            let
                row =
                    err.row - 1

                msg =
                    if row <= 0 then
                        Debug.toString err.problem

                    else
                        "Error on line "
                            ++ String.fromInt row
                            ++ ", column "
                            ++ String.fromInt err.col
                            ++ ": "
                            ++ Debug.toString err.problem
            in
            div (Style.errorMessage ++ [ style "display" "flex" ])
                [ cellLabelView Style.cellLabelOutput ""
                , text msg
                ]


cellLabelView : List (Attribute Msg) -> String -> Html Msg
cellLabelView attrs str =
    div (attrs ++ [ style "width" "90px", style "padding-right" "5px", style "padding-top" "8px" ]) [ text str ]


renderLatex : String -> Html Msg
renderLatex str =
    let
        convertedText =
            "$$\n" ++ String.replace "\n" "\\\\" str ++ "\n$$"
    in
    div [ style "width" "80%", style "margin-top" "-8px", style "padding-left" "5px" ]
        [ Html.Keyed.node "div"
            []
            [ ( str, div [ class "raw-math" ] [ text <| convertedText ] )
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateInput index { state, textValue } ->
            let
                updateCell cell_ =
                    { cell_ | autoexpand = state, input = textValue }
            in
            ( { model | cells = List.Extra.updateIfIndex ((==) index) updateCell model.cells }
            , Cmd.none
            )

        AddCell ->
            ( { model | cells = model.cells ++ [ newCell (List.length model.cells) ] }
            , Cmd.none
            )

        SelectCell index ->
            ( { model | selectedCell = index }, Cmd.none )

        RunCell ->
            let
                getLastResult res =
                    case res of
                        Ok values ->
                            List.Extra.last values
                                |> Maybe.withDefault Void

                        Err errs ->
                            List.Extra.last errs
                                |> Maybe.map Error
                                |> Maybe.withDefault Void

                updateCell cell_ =
                    { cell_
                        | result =
                            MathParser.parse cell_.input
                                |> Result.andThen Interpreter.run
                                |> getLastResult
                    }

                updatedModel =
                    { model
                        | cells = List.Extra.updateIfIndex ((==) model.selectedCell) updateCell model.cells
                        , selectedCell = model.selectedCell + 1
                    }
            in
            if updatedModel.selectedCell == List.length model.cells then
                updatedModel
                    |> update AddCell

            else
                ( updatedModel, Cmd.none )


autoExpandConfig : Int -> AutoExpand.Config Msg
autoExpandConfig index =
    AutoExpand.config
        { onInput = UpdateInput index
        , padding = 5
        , lineHeight = 18
        , minRows = 1
        , maxRows = 4
        }
        |> AutoExpand.withAttribute (style "resize" "none")
        |> AutoExpand.withAttribute (style "flex-grow" "1")
        |> AutoExpand.withAttribute (style "background" "#f7f7f7")
        |> AutoExpand.withAttribute (style "border" "1px solid #cfcfcf")
        |> AutoExpand.withAttribute (style "font-size" "14px")
        |> AutoExpand.withAttribute (style "font-family" "monospace, sans-serif")
        |> AutoExpand.withAttribute (onFocus (SelectCell index))
