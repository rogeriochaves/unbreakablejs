module Playground exposing (main)

import AstParser
import AutoExpand as AutoExpand
import Browser exposing (UrlRequest(..))
import Browser.Dom exposing (focus)
import Browser.Navigation exposing (Key, load, pushUrl)
import Debug
import Encoder exposing (encode)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed
import Interpreter
import Json.Decode as Json
import List.Extra
import Markdown
import Parser exposing (DeadEnd, Problem(..))
import Playground.Components exposing (..)
import Playground.Routes exposing (..)
import Playground.Style as Style
import Playground.Types exposing (..)
import Task
import Types exposing (..)
import Url exposing (Url)
import Url.Parser exposing (parse)


main : Platform.Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        , onUrlChange = OnUrlChange
        , onUrlRequest = OnUrlRequest
        }


init : () -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    { cells =
        [ newCell 0 ""
        ]
    , state = emptyState
    , selectedCell = -1
    , page = Playground
    , key = key
    }
        |> update (OnUrlChange url)


newCell : Int -> String -> Cell
newCell index input =
    { input = input
    , autoexpand = AutoExpand.initState (autoExpandConfig (List.length <| String.split "\n" input) index)
    , result = Ok ( emptyState, Nothing )
    , submittedInput = ""
    }


view : Model -> Browser.Document Msg
view model =
    { title = "unbreakable.js"
    , body =
        [ row (Style.general ++ [ id "main", style "margin" "-8px", style "min-height" "90vh" ])
            (case model.page of
                Playground ->
                    playground model
            )
        , container (Style.footer ++ [ style "padding-top" "25px" ])
            [ text "Did you like this project? Drop me a message on "
            , a [ href "https://twitter.com/_rchaves_" ] [ text "twitter" ]
            ]
        , a [ href "https://github.com/rogeriochaves/unbreakablejs/" ]
            [ img
                [ style "position" "fixed"
                , style "top" "0"
                , style "right" "0"
                , style "border" "0"
                , src "https://s3.amazonaws.com/github/ribbons/forkme_right_darkblue_121621.png"
                , alt "Fork me on GitHub"
                ]
                []
            ]
        ]
    }


playground : Model -> List (Html Msg)
playground model =
    [ container Style.header
        [ Playground.Components.header
        , toolbarView
        ]
    , container [ style "padding-top" "20px" ]
        [ row (Style.notebook ++ [ style "padding" "20px" ])
            (List.indexedMap (cellView model) model.cells)
        ]
    ]


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
            , AutoExpand.view (autoExpandConfig 1 index) item.autoexpand item.input
            ]
        , renderResult model index item
        ]


removeTracking : Expression -> UntrackedExp
removeTracking expr =
    case expr of
        Tracked _ e ->
            e

        Untracked e ->
            e


cellName : Int -> String
cellName index =
    "Cell " ++ String.fromInt index


renderResult : Model -> Int -> Cell -> Html Msg
renderResult model index item =
    let
        getLine filename line =
            let
                cellIndex =
                    filename
                        |> String.replace "Cell " ""
                        |> String.toInt
                        |> Maybe.withDefault 0
            in
            List.Extra.getAt cellIndex model.cells
                |> Maybe.andThen (List.Extra.getAt line << String.split "\n" << .submittedInput)
                |> Maybe.withDefault ("<line " ++ String.fromInt line ++ " not found>")

        expressionResult =
            item.result
                |> Result.map Tuple.second
                |> Result.map (Maybe.map .result)
    in
    case expressionResult of
        Err error ->
            let
                firstError =
                    List.head error
                        |> Maybe.withDefault { row = 0, col = 0, problem = Problem "There is an error on the playground, please report" }

                msg =
                    "Syntax error. I could not parse the code. The problem happened here:\n\n"
                        ++ String.fromInt firstError.row
                        ++ "| "
                        ++ getLine (cellName index) (firstError.row - 1)
                        ++ "\n"
                        ++ String.repeat (firstError.col + String.length (String.fromInt firstError.row)) "-"
                        ++ "^\n\n"
                        ++ Debug.toString firstError.problem

                -- ++ Debug.toString error
            in
            column (Style.errorMessage ++ [ style "padding-top" "7px", style "padding-bottom" "10px" ])
                [ cellLabelView Style.cellLabelOutput ""
                , pre [ style "font-size" "14px", style "margin" "0" ] [ text msg ]
                ]

        Ok (Just (Undefined stack)) ->
            let
                stackMsgs =
                    stack
                        |> List.indexedMap
                            (\i error ->
                                let
                                    msgGotFrom =
                                        if i == 0 then
                                            if List.length stack > 1 then
                                                "How come? First I got undefined from "

                                            else
                                                "I got undefined from "

                                        else
                                            "Then from "

                                    filename =
                                        if error.filename == cellName index then
                                            "here"

                                        else
                                            error.filename

                                    repetitionN =
                                        error.column + String.length (String.fromInt error.line) + 1

                                    reason =
                                        case error.reason of
                                            VariableNotDefined identifier ->
                                                identifier ++ " is not defined"

                                            OperationWithUndefined operationName ->
                                                operationName ++ " with undefined"

                                            MissingPositionalArgument index_ paramName ->
                                                let
                                                    posName =
                                                        case index_ + 1 of
                                                            1 ->
                                                                "1st"

                                                            2 ->
                                                                "2nd"

                                                            3 ->
                                                                "3rd"

                                                            _ ->
                                                                String.fromInt index_ ++ "th"
                                                in
                                                "missing argument " ++ paramName ++ " (" ++ posName ++ " argument)"

                                            VoidReturn ->
                                                "function returned void"

                                            IfWithoutElse ->
                                                "if condition evaluated to false and there is no else case"

                                            ExplicitUndefined ->
                                                "explicitly given undefined value"

                                            LoopNeverTrue ->
                                                "loop condition never evaluated to true so the loop was never executed"

                                            KeyNotInObject obj key ->
                                                "key " ++ encode key ++ " not found on " ++ encode obj

                                            AssignmentToUndefined name ->
                                                name ++ " got assigned to undefined"

                                            NotAFunction val ->
                                                encode val ++ " is not a function"
                                in
                                msgGotFrom
                                    ++ filename
                                    ++ ":\n\n"
                                    ++ String.fromInt error.line
                                    ++ "| "
                                    -- TODO: cannot get line from function defined on previous cell
                                    ++ getLine error.filename (error.line - 1)
                                    ++ "\n"
                                    ++ String.repeat repetitionN "-"
                                    ++ "^\n"
                                    -- ++ String.repeat repetitionN " "
                                    ++ reason
                                    ++ "\n\n"
                            )
            in
            column []
                [ cellLabelView (Style.cellLabelOutput ++ Style.warnMessage) "Output:"
                , div [ style "padding-top" "7px", Style.monospace ]
                    [ div [] [ text "undefined" ]
                    , pre (Style.warnMessage ++ [ style "font-size" "14px", style "margin" "0", style "padding-top" "20px" ])
                        [ text <| String.join "\n" stackMsgs
                        ]
                    ]
                ]

        Ok (Just expr) ->
            column []
                [ cellLabelView Style.cellLabelOutput "Output:"
                , div [ style "padding-top" "7px", Style.monospace ] [ text <| encode expr ]
                ]

        Ok Nothing ->
            div [] []


cellLabelView : List (Attribute Msg) -> String -> Html Msg
cellLabelView attrs str =
    row (attrs ++ [ style "width" "90px", style "padding-right" "5px", style "padding-top" "8px" ]) [ text str ]


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
                -- List.Extra.last errs
                --     -- TODO: map syntax errors
                --     |> Maybe.map (\e -> ( model.state, Untracked <| Value <| Undefined [] ))
                runCell : Cell -> Result Error ( State, Maybe ExpressionResult )
                runCell cell_ =
                    if String.isEmpty (String.trim cell_.input) then
                        Ok ( model.state, Nothing )

                    else
                        AstParser.parse ("Cell " ++ String.fromInt model.selectedCell) cell_.input
                            |> Result.map (Interpreter.run model.state)
                            |> Result.map (\( state, results ) -> ( state, List.Extra.last results ))

                result =
                    List.Extra.getAt model.selectedCell model.cells
                        |> Maybe.map runCell
                        |> Maybe.withDefault (Ok ( model.state, Nothing ))

                -- TODO: map syntax errors
                updateCell cell_ =
                    { cell_ | result = result, submittedInput = cell_.input }

                updated =
                    let
                        state =
                            case result of
                                Ok ( state_, _ ) ->
                                    state_

                                _ ->
                                    model.state
                    in
                    { model
                        | state = state
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

        ClearPlayground ->
            ( { model
                | cells =
                    [ newCell 0 ""
                    ]
                , state = emptyState
                , selectedCell = -1
              }
            , Cmd.none
            )

        SetExample example ->
            case example of
                Basics ->
                    { model
                        | state = emptyState
                        , cells =
                            [ newCell 0 "1 + 1"
                            , newCell 1 "\\frac{25}{2}"
                            , newCell 2 "12!"
                            , newCell 3 "x = 5\n\\mathbf{y} = (1, 2, 3)"
                            , newCell 4 "\\sqrt{x}"
                            , newCell 5 "\\sum_{i = 1}^{100} (2 * i + 1)"
                            , newCell 6 "f(x) = x + 1\nf(5)"
                            , newCell 7 "f(\\mathbf{v})_{j} = v_{j} + 1\nf(\\mathbf{y})"
                            ]
                    }
                        |> update (SelectCell 0)

                Softmax ->
                    { model
                        | state = emptyState
                        , cells =
                            [ newCell 0 "\\sigma(\\mathbf{z})_{j}=\\frac{e^{z_{j}}}{\\sum_{k=1}^{n} e^{z_{k}}}"
                            , newCell 1 "\\mathbf{v} = (1.0, 2.0, 3.0, 4.0, 1.0, 2.0, 3.0)\nn = 7\n\\sigma(\\mathbf{v})"
                            , newCell 2 "\\sum_{i = 1}^{n} \\sigma(\\mathbf{v})_{i}"
                            ]
                    }
                        |> update (SelectCell 0)

                Bitcoin ->
                    { model
                        | state = emptyState
                        , cells =
                            [ newCell 0 "q = 0.1\nz = 2\np = 1 - q\n\\lambda = z * \\frac{q}{p}"
                            , newCell 1 "1 - \\sum_{k = 0}^{z} \\frac{(\\lambda ^ k) * e ^ {-\\lambda}}{k!} * (1 - (q / p) ^ {(z - k)})"
                            ]
                        , selectedCell = 0
                    }
                        |> update (SelectCell 0)

                Statistics ->
                    { model
                        | state = emptyState
                        , cells =
                            [ newCell 0 "\\mathbf{x} = (1, 3, 3, 6, 7, 8, 9)\nn = |\\mathbf{x}|"
                            , newCell 1 "Mean:\n\\bar{x} = \\frac{\\sum{\\mathbf{x}}}{n}"
                            , newCell 2 "Median:\n\\tilde{x} = x_{(n \\div 2 + 1)}"
                            , newCell 3 "Quartiles\\ and\\ IQR:\n\\operatorname{Q1} = x_{(n \\div 4 + 1)}\n\\operatorname{Q3} = x_{(n - n \\div 4)}\n\\operatorname{IQR} = \\operatorname{Q3} - \\operatorname{Q1}"
                            , newCell 4 "Outliers:\n\\operatorname{lower} = \\operatorname{Q1} - 1.5 * \\operatorname{IQR}\n\\operatorname{upper} = \\operatorname{Q3} + 1.5 * \\operatorname{IQR}\n(\\operatorname{lower}, \\operatorname{upper})"
                            , newCell 5 "Variance:\nv = \\frac{\\sum_{i = 1}^{n} (x_{i} - \\bar{x}) ^ 2}{n - 1}"
                            , newCell 6 "Standard\\ Deviation:\ns = \\sqrt{v}"
                            , newCell 7 "Z-Score:\nz(\\mathbf{x})_{i} = \\frac{x_{i} - \\bar{x}}{s}\nz(\\mathbf{x})"
                            , newCell 8 "Pearson's\\ R:\n\\mathbf{x} = (50, 100, 200, 300)\n\\bar{x} = 162.5\ns = 110.9\n\\mathbf{a} = z(\\mathbf{x})\n\n\\mathbf{y} = (50, 70, 70, 95)\n\\bar{x} = 71.3\ns = 18.4\n\\mathbf{b} = z(\\mathbf{y})\n\nn = |\\mathbf{x}|\nr = \\frac{\\sum_{i = 1}^{n} a_{i}*b_{i}}{n - 1}\n"
                            , newCell 9 "Regression\\ Coefficient:\n\\operatorname{Sx} = 110.9\n\\operatorname{Sy} = 18.4\nb = r * (\\frac{\\operatorname{Sy}}{\\operatorname{Sx}})"
                            , newCell 10 "Intercept:\n\\bar{x} = 162.5\n\\bar{y} = 71.3\na = \\bar{y} - b * \\bar{x}"
                            , newCell 11 "Regression\\ Line:\ny(\\mathbf{x})_{i} = a + b * x_{i}\ny(\\mathbf{x})"
                            , newCell 12 "Coefficient\\ of\\ Determination:\nr ^ 2"
                            , newCell 13 "Probability\\ Mass\\ Function:\n\\mathbf{x} = (1, 2, 3, 4, 5, 6)\n\\mathbf{f} = (10, 20, 40, 80, 40, 20)\np(\\mathbf{x})_{i} = \\frac{f_{i}}{\\sum{\\mathbf{f}}}\np(\\mathbf{x})"
                            , newCell 14 "Expected\\ Value:\nn = |\\mathbf{x}|\n\\operatorname{E}(\\mathbf{x}) = \\sum_{i = 1}^{n} x_{i} * p(\\mathbf{x})_{i}\n\\mu = \\operatorname{E}(\\mathbf{x})"
                            , newCell 15 "Variance\\ of\\ a\\ Random\\ Value:\ng(\\mathbf{x})_{i} = (x_{i} - \\mu) ^ 2\nv = \\operatorname{E}(g(\\mathbf{x}))\n\\sigma = \\sqrt{v}"
                            , newCell 16 "Normal\\ Distribution:\nf(x) = \\frac{1}{\\sigma * \\sqrt{2 * \\pi}} * e ^ {-0.5 * (\\frac{x - \\mu}{\\sigma}) ^ 2}\nf(4)"
                            , newCell 17 "Standard\\ Normal\\ Distribution:\nx = \\mu + z * \\sigma"
                            , newCell 18 "Binomial\\ Distribution:\nn = 4\np = 0.48\n\\operatorname{P}(x) = \\frac{n!}{x! * (n-x)!} * p ^ x * (1 - p) ^ {n - x}\n\\operatorname{P}(3)"
                            , newCell 19 "Cumulative\\ Binomial:\n\\operatorname{F}(x) = \\sum_{k = 0}^{x} \\frac{n!}{k! * (n-k)!} * p ^ k * (1 - p) ^ {n - k}\n\\operatorname{F}(3)"
                            , newCell 20 "Binomial\\ Mean:\n\\mu = n * p"
                            , newCell 21 "Binomial\\ Standard\\ Deviation:\n\\sigma = \\sqrt{n * p * (1 - p)}"
                            , newCell 22 "Sampling\\ Distribution\\ of\\ the\\ Sample\\ Mean:\n\\mu = 3.85\n\\sigma = 1.25\nn = 6\n\\bar{\\mu} = \\mu\n\\bar{\\sigma} = \\frac{\\sigma}{\\sqrt{n}}"
                            , newCell 23 "Sampling\\ Proportion\\ Standard\\ Deviation:\n\\pi = 0.2\nn = 400\n\\bar{\\sigma} = \\sqrt{\\frac{\\pi * (1 - \\pi)}{n}}"
                            , newCell 24 "Confidence\\ Interval\\ with\\ Population\\ Standard\\ Deviation:\n\\sigma = 0.8\n\\bar{x} = 3.8\nn = 150\nz = 1.96\n\\operatorname{SE} = \\frac{\\sigma}{\\sqrt{n}}\n\\operatorname{CI} = z * \\operatorname{SE}\n(\\bar{x} - \\operatorname{CI}, \\bar{x} + \\operatorname{CI})"
                            , newCell 25 "Confidence\\ Interval\\ without\\ Population\\ Standard\\ Deviation:\ns = 0.7\nt = 1.96\n\\operatorname{SE} = \\frac{s}{\\sqrt{n}}\n\\operatorname{CI} = t * \\operatorname{SE}\n(\\bar{x} - \\operatorname{CI}, \\bar{x} + \\operatorname{CI})\n                            "
                            , newCell 26 "Confidence\\ Interval\\ for\\ Proportion\\ without\\ Population\\ Standard\\ Deviation:\nn = 55\np = 0.77\nz = 2.58\n\\operatorname{SE} = \\sqrt{\\frac{p * (1 - p)}{n}}\n\\operatorname{CI} = z * \\operatorname{SE}\n(p - \\operatorname{CI}, p + \\operatorname{CI})"
                            , newCell 27 "Selecting\\ a\\ Sample\\ Size:\n\\sigma = 1.25\nz = 1.96\nm = 0.3\nn = \\frac{\\sigma ^ 2 * z ^ 2}{m ^ 2}"
                            , newCell 28 "Selecting\\ a\\ Sample\\ Size\\ for\\ Proportion:\np = 0.5\nz = 1.645\nm = 0.1\nn = \\frac{p * (1 - p) * z ^ 2}{m ^ 2}"
                            , newCell 29 "Hypothesis\\ Testing:\n\\operatorname{null} = 68\nn = 40\n\\bar{x} = 64\ns = 3\n\\alpha = 0.05\n\\operatorname{SE} = \\frac{s}{\\sqrt{n}}\nt = \\frac{\\bar{x} - \\operatorname{null}}{\\operatorname{SE}}\n\\operatorname{df} = n - 1\n\\operatorname{t95} = 2.042\n(t, -\\operatorname{t95}, \\operatorname{t95})"
                            , newCell 30 "Hypothesis\\ Testing\\ for\\ Proportion:\n\\operatorname{null} = 0.86\nn = 900\np = 0.84\n\\alpha = 0.05\n\\operatorname{SE} = \\sqrt{\\frac{\\operatorname{null} * (1 - \\operatorname{null})}{n}}\nz = \\frac{p - \\operatorname{null}}{\\operatorname{SE}}\n\\operatorname{z95} = 1.96\n(-\\operatorname{z95}, z, \\operatorname{z95})"
                            ]
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


keyCodeWithShift : Json.Decoder (Maybe Int)
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
