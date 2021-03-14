module Playground exposing (main)

import AstParser
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
import Markdown
import Parser exposing (Problem(..))
import Playground.Components exposing (..)
import Playground.Routes exposing (..)
import Playground.Style as Style
import Playground.Types exposing (..)
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


view : Model -> Browser.Document Msg
view model =
    { title = "Rubber - Evaluate LaTeX math code"
    , body =
        [ row (Style.general ++ [ id "main", style "margin" "-8px", style "min-height" "90vh" ])
            (case model.page of
                Playground ->
                    playground model

                About ->
                    about
            )
        , container (Style.footer ++ [ style "padding-top" "25px" ])
            [ text "Did you like this project? Drop me a message on "
            , a [ href "https://twitter.com/_rchaves_" ] [ text "twitter" ]
            ]
        , a [ href "https://github.com/rogeriochaves/rubber/" ]
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


about : List (Html Msg)
about =
    let
        softmax =
            "$$\n\\sigma(\\mathbf{z})_{j}=\\frac{e^{z_{j}}}{\\sum_{k=1}^{n} e^{z_{k}}}\n$$"
    in
    [ container Style.header
        [ Playground.Components.header
        ]
    , container [ style "padding-top" "20px" ]
        [ row (Style.card ++ [ style "padding" "20px" ])
            [ Markdown.toHtml [ style "line-height" "1.5em" ]
                """
# Why Rubber?

Rubber is a tool for interpreting a subset of LaTeX math formulas, it was born out of my own frustrations with math formulas.

More often then not, academic papers uses a lot of math to describe its achieving, even in areas that relate a lot to Computer Science, such as Machine Learning, people don't show code, they show math, which is sometimes much harder to read, at least for me.

For example, take the softmax formula, see how it looks mathematically:

            """
            , Html.Keyed.node "div"
                []
                [ ( softmax, div [ class "raw-math", style "padding-bottom" "10px" ] [ text <| softmax ] )
                ]
            , Markdown.toHtml [ style "line-height" "1.5em" ]
                """

Now the same thing, with python code:

```python
def softmax(vector):
  vector_exp = [math.exp(i) for i in vector]
  return [i / sum(vector_exp) for i in vector_exp]
```

Which one was easier for you to read? Personally, and I'm probably biased because I'm a dev, but code is much easier.

If someone in my team was writing code like people write math, using all this weird symbols, one-letter meaningless variables, trying to be more abstract then clear all the time, I'd tell them to immediatly stop coding and go read the [Clean Code](https://www.amazon.com/Clean-Code-Handbook-Software-Craftsmanship/dp/0132350882) book because this is unmaintanable.

More importantly, with the python code I can just copy and paste it and run on my machine, and debug for myself, and try to figure out the logic behind it. While with math I have to translate it, either with pen and paper, or to another programming language, but the problem is: I'm not a good interpreter as the computer is, it is too easy for me to make mistakes when computing or translating math formulas by myself and achieve the wrong conclusions about some genius formulas.

So **Rubber** comes trying to change this, I want to be able to run and explore math formulas directly, and LaTeX is the ubiquitous language for that.

There are a lot of popular math-focused languages out there, like [MATLAB](https://www.mathworks.com/products/matlab.html), [R](https://www.r-project.org/), [Julia](https://julialang.org/), but all of them have their own syntax, they don't use LaTeX so you are never sure if something is lost in translation.

Other languages or tools do have a support for true math formulas, sometimes even some support to LaTeX code, but they are closed-source and very expensive, such as [Wolfram Mathematica](http://www.wolfram.com/mathematica) or [Maple](https://www.maplesoft.com).

The goal of **Rubber** is to allow you to copy the same LaTeX code that was put in some paper, and run it. Ideally, paper authors would publish the LaTeX code as an attachment to every formula used, and they would had run it already on **Rubber** knowing it works, allowing people to copy and paste and explore on their own. To achieve that, this project has to be as open and free as possible.

If you like this idea, and would like me to keep developing it, drop me a message on [twitter](https://twitter.com/_rchaves_), or at least [some stars on github](https://github.com/rogeriochaves/rubber/) because I have no trackings on this website so I have no way of knowing it unless you tell me.

Thank you very much!
                """
            ]
        ]
    ]


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
            , if item.result == Nothing || index == model.selectedCell then
                AutoExpand.view (autoExpandConfig 1 index) item.autoexpand item.input

              else
                renderLatex item.input
            ]
        , renderResult item
        ]


removeTracking : Types.Expression -> Types.UntrackedExp
removeTracking expr =
    case expr of
        Types.Tracked _ e ->
            e

        Types.Untracked e ->
            e


renderResult : Cell -> Html Msg
renderResult item =
    case Maybe.map removeTracking item.result of
        Just (Types.Value (Types.Undefined stack)) ->
            let
                msg =
                    "Undefined. Stacktrace: "
                        ++ Debug.toString stack
            in
            column (Style.errorMessage ++ [ style "padding-bottom" "20px" ])
                [ cellLabelView Style.cellLabelOutput ""
                , text msg
                ]

        Just expr ->
            column []
                [ cellLabelView Style.cellLabelOutput "Output:"
                , renderLatex (encode expr)
                ]

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
                                -- TODO: map syntax errors
                                |> Maybe.map (\e -> ( model.state, Types.Untracked <| Types.Value <| Types.Undefined [] ))

                runCell : Cell -> Maybe Interpreter.LineResult
                runCell cell_ =
                    if String.isEmpty (String.trim cell_.input) then
                        Nothing

                    else
                        AstParser.parse cell_.input
                            |> Result.map (Interpreter.run model.state)
                            |> getLastResult

                result =
                    List.Extra.getAt model.selectedCell model.cells
                        |> Maybe.andThen runCell
                        -- TODO: map syntax errors
                        |> Maybe.withDefault ( model.state, Types.Untracked <| Types.Value <| Types.Undefined [] )

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

        ClearPlayground ->
            ( { model
                | cells =
                    [ newCell 0 ""
                    ]
                , state = Interpreter.newState
                , selectedCell = -1
              }
            , Cmd.none
            )

        SetExample example ->
            case example of
                Basics ->
                    { model
                        | state = Interpreter.newState
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
                        | state = Interpreter.newState
                        , cells =
                            [ newCell 0 "\\sigma(\\mathbf{z})_{j}=\\frac{e^{z_{j}}}{\\sum_{k=1}^{n} e^{z_{k}}}"
                            , newCell 1 "\\mathbf{v} = (1.0, 2.0, 3.0, 4.0, 1.0, 2.0, 3.0)\nn = 7\n\\sigma(\\mathbf{v})"
                            , newCell 2 "\\sum_{i = 1}^{n} \\sigma(\\mathbf{v})_{i}"
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

                Statistics ->
                    { model
                        | state = Interpreter.newState
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
