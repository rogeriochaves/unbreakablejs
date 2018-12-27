module Playground exposing (main)

import AutoExpand as AutoExpand
import Browser exposing (UrlRequest(..))
import Browser.Dom exposing (focus)
import Browser.Navigation exposing (Key, load, pushUrl)
import Dict
import Encoder exposing (encode)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed
import Interpreter
import Json.Decode as Json
import List.Extra
import MathParser
import Parser exposing (Problem(..))
import Playground.Routes exposing (..)
import Playground.Style as Style
import Return exposing (Value(..))
import Task
import Types exposing (Error)
import Url exposing (Url)
import Url.Parser exposing (parse)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        , onUrlChange = OnUrlChange
        , onUrlRequest = OnUrlRequest
        }


type alias Model =
    { cells : List Cell
    , state : Interpreter.State
    , selectedCell : Int
    , page : Page
    , key : Key
    }


type alias Cell =
    { input : String
    , autoexpand : AutoExpand.State
    , result : Maybe Return.Value
    }


type Msg
    = NoOp
    | UpdateInput Int { textValue : String, state : AutoExpand.State }
    | AddCell
    | SelectCell Int
    | RunCell
    | SetExample Example
    | KeyDown (Maybe Int)
    | OnUrlChange Url
    | OnUrlRequest UrlRequest
    | Go Page


type Example
    = Basics
    | Softmax
    | Bitcoin


init : () -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    { cells =
        [ newCell 0 ""
        ]
    , state = Interpreter.newState
    , selectedCell = -1
    , page = Playground
    , key = key
    }
        |> update (OnUrlChange url)


newCell : Int -> String -> Cell
newCell index input =
    { input = input
    , autoexpand = AutoExpand.initState (autoExpandConfig (List.length <| String.split "\n" input) index)
    , result = Nothing
    }


emptyLatexState =
    { counters = Dict.fromList [ ( "s1", 0 ), ( "s2", 0 ), ( "s3", 0 ), ( "tno", 0 ), ( "eqno", 0 ) ]
    , crossReferences = Dict.empty
    , dictionary = Dict.empty
    , tableOfContents = []
    , macroDictionary = Dict.empty
    }


view : Model -> Browser.Document Msg
view model =
    { title = "Rubber - Evaluate LaTeX math code"
    , body =
        [ case model.page of
            Playground ->
                playground model

            About ->
                text "about the project"
        ]
    }


playground : Model -> Html Msg
playground model =
    row (Style.general ++ [ id "main", style "margin" "-8px" ])
        [ column (Style.header ++ [ style "justify-content" "center" ])
            [ row [ style "flex-grow" "1", style "max-width" "1140px", style "padding" "0 10px" ]
                (header model)
            ]
        , column [ style "justify-content" "center", style "padding-top" "20px" ]
            [ row (Style.notebook ++ [ style "padding" "15px", style "flex-grow" "1", style "max-width" "1140px" ])
                (List.indexedMap (cellView model) model.cells)
            ]
        ]


header : Model -> List (Html Msg)
header model =
    let
        menuLink attrs =
            a (Style.menuLink ++ [ style "padding" "10px 20px", class "menuLink" ] ++ attrs)

        submenuItem attrs =
            a (Style.submenuItem ++ [ style "padding" "10px 20px", style "display" "block", class "submenuItem" ] ++ attrs)
    in
    [ column [ style "padding-top" "20px" ]
        [ row []
            [ h1 (Style.title ++ [ style "margin" "0 0 5px -1px" ]) [ text "Rubber" ]
            , h2 (Style.smallSubtitle ++ [ style "margin-top" "0", style "padding-bottom" "10px" ]) [ text "Evaluate LaTeX math code (beta)" ]
            ]
        , column [ style "justify-content" "center", style "flex-grow" "1" ]
            [ menuLink [ href "#" ] [ text "Playground" ]
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
            , menuLink [ href "#docs" ] [ text "Docs" ]
            ]
        ]
    , toolbarView
    ]


row : List (Html.Attribute a) -> List (Html a) -> Html a
row =
    div


column : List (Html.Attribute a) -> List (Html a) -> Html a
column attrs =
    div ([ style "display" "flex" ] ++ attrs)


toolbarView : Html Msg
toolbarView =
    let
        toolbarButton attrs =
            button
                (Style.toolbarButton
                    ++ [ class "toolbarButton"
                       , style "margin-right" "5px"
                       , style "padding" "5px 10px"
                       ]
                    ++ attrs
                )
    in
    column [ style "padding-bottom" "20px" ]
        [ toolbarButton [ onClick AddCell ] [ text "Add Cell" ]
        , toolbarButton [ onClick RunCell ] [ text "Run Cell" ]
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
    row (cellStyle ++ [ onClick (SelectCell index), style "padding" "5px" ])
        [ column []
            [ cellLabelView Style.cellLabelInput "Input:"
            , if item.result == Nothing || index == model.selectedCell then
                AutoExpand.view (autoExpandConfig 1 index) item.autoexpand item.input

              else
                renderLatex item.input
            ]
        , renderResult item
        ]


renderResult : Cell -> Html Msg
renderResult item =
    case item.result of
        Just (Expression expr) ->
            column []
                [ cellLabelView Style.cellLabelOutput "Output:"
                , renderLatex (encode expr)
                ]

        Just (Error err) ->
            let
                row_ =
                    err.row - 1

                msg =
                    if row_ <= 0 then
                        Debug.toString err.problem

                    else
                        "Error on line "
                            ++ String.fromInt row_
                            ++ ", column "
                            ++ String.fromInt err.col
                            ++ ": "
                            ++ Debug.toString err.problem
            in
            column (Style.errorMessage ++ [ style "padding-bottom" "20px" ])
                [ cellLabelView Style.cellLabelOutput ""
                , text msg
                ]

        Just Void ->
            div [] []

        Nothing ->
            div [] []


cellLabelView : List (Attribute Msg) -> String -> Html Msg
cellLabelView attrs str =
    row (attrs ++ [ style "width" "90px", style "padding-right" "5px", style "padding-top" "8px" ]) [ text str ]


renderLatex : String -> Html Msg
renderLatex str =
    let
        convertedText =
            "$$\n" ++ String.replace "\n" "\\\\" str ++ "\n$$"
    in
    row [ style "width" "80%", style "margin-top" "-8px", style "padding-left" "5px" ]
        [ Html.Keyed.node "div"
            []
            [ ( str, div [ class "raw-math" ] [ text <| convertedText ] )
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateInput index { state, textValue } ->
            let
                updateCell cell_ =
                    { cell_ | autoexpand = state, input = textValue }
            in
            if index == model.selectedCell then
                ( { model | cells = List.Extra.updateIfIndex ((==) index) updateCell model.cells }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        AddCell ->
            let
                index =
                    List.length model.cells
            in
            { model | cells = model.cells ++ [ newCell index "" ] }
                |> update (SelectCell index)

        SelectCell index ->
            ( { model | selectedCell = index }
            , Task.attempt (\_ -> NoOp) (focus ("cellinput-" ++ String.fromInt index))
            )

        RunCell ->
            let
                getLastResult res =
                    case res of
                        Ok values ->
                            List.Extra.last values

                        Err errs ->
                            List.Extra.last errs
                                |> Maybe.map (\e -> ( model.state, Error e ))

                runCell : Cell -> Maybe Interpreter.LineResult
                runCell cell_ =
                    if String.isEmpty (String.trim cell_.input) then
                        Nothing

                    else
                        MathParser.parse cell_.input
                            |> Result.andThen (Interpreter.run model.state)
                            |> getLastResult

                result =
                    List.Extra.getAt model.selectedCell model.cells
                        |> Maybe.andThen runCell
                        |> Maybe.withDefault ( model.state, Void )

                updateCell cell_ =
                    { cell_ | result = Just <| Tuple.second result }

                updated =
                    { model
                        | state = Tuple.first result
                        , cells = List.Extra.updateIfIndex ((==) model.selectedCell) updateCell model.cells
                    }
                        |> update (SelectCell (model.selectedCell + 1))

                updatedModel =
                    Tuple.first updated

                updatedCmd =
                    Tuple.second updated
            in
            if updatedModel.selectedCell == List.length model.cells then
                updatedModel
                    |> update AddCell
                    |> Tuple.mapSecond (\cmd -> Cmd.batch [ cmd, updatedCmd ])

            else
                updated

        SetExample example ->
            case example of
                Basics ->
                    { model
                        | state = Interpreter.newState
                        , cells =
                            [ newCell 0 "1 + 1"
                            , newCell 1 "\\frac{25}{2}"
                            , newCell 2 "12!"
                            , newCell 3 "x = 5\n\\vec{y} = (1, 2, 3)"
                            , newCell 4 "\\sqrt{x}"
                            , newCell 5 "\\sum_{i = 1}^{100} (2 * i + 1)"
                            , newCell 6 "f(x) = x + 1\nf(5)"
                            , newCell 7 "f(\\vec{v})_{j} = v_{j} + 1\nf(\\vec{y})"
                            ]
                    }
                        |> update (SelectCell 0)

                Softmax ->
                    { model
                        | state = Interpreter.newState
                        , cells =
                            [ newCell 0 "\\sigma(\\vec{z})_{j}=\\frac{e^{z_{j}}}{\\sum_{k=1}^{n} e^{z_{k}}}"
                            , newCell 1 "\\vec{v} = (1.0, 2.0, 3.0, 4.0, 1.0, 2.0, 3.0)\nn = 7\n\\sigma(\\vec{v})"
                            , newCell 2 "\\sum_{i = 1}^{n} \\sigma(\\vec{v})_{i}"
                            ]
                    }
                        |> update (SelectCell 0)

                Bitcoin ->
                    { model
                        | state = Interpreter.newState
                        , cells =
                            [ newCell 0 "q = 0.1\nz = 2\np = 1 - q\n\\lambda = z * \\frac{q}{p}"
                            , newCell 1 "1 - \\sum_{k = 0}^{z} \\frac{(\\lambda ^ k) * e ^ {-\\lambda}}{k!} * (1 - (q / p) ^ {(z - k)})"
                            ]
                        , selectedCell = 0
                    }
                        |> update (SelectCell 0)

        KeyDown key ->
            case key of
                Just 13 ->
                    update RunCell model

                _ ->
                    ( model, Cmd.none )

        OnUrlChange url ->
            ( { model | page = Maybe.withDefault Playground <| parse routes url }, Cmd.none )

        OnUrlRequest urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model, pushUrl model.key <| Url.toString url )

                External url ->
                    ( model, load url )

        Go page ->
            ( model, pushUrl model.key <| toPath page )


autoExpandConfig : Int -> Int -> AutoExpand.Config Msg
autoExpandConfig minRows index =
    AutoExpand.config
        { onInput = UpdateInput index
        , padding = 5
        , lineHeight = 18
        , minRows = minRows
        , maxRows = 50
        }
        |> AutoExpand.withAttribute (style "resize" "none")
        |> AutoExpand.withAttribute (style "flex-grow" "1")
        |> AutoExpand.withAttribute (style "background" "#f7f7f7")
        |> AutoExpand.withAttribute (style "border" "1px solid #cfcfcf")
        |> AutoExpand.withAttribute (style "font-size" "14px")
        |> AutoExpand.withAttribute (style "font-family" "monospace, sans-serif")
        |> AutoExpand.withAttribute (onFocus (SelectCell index))
        |> AutoExpand.withAttribute (on "keydown" (Json.map KeyDown keyCodeWithShift))
        |> AutoExpand.withAttribute (id ("cellinput-" ++ String.fromInt index))


keyCodeWithShift =
    Json.map2
        (\shift keyCode ->
            if shift then
                Just keyCode

            else
                Nothing
        )
        (Json.field "shiftKey" Json.bool)
        (Json.field "keyCode" Json.int)
