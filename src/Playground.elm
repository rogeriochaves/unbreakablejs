module Playground exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Interpreter
import MathParser


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view =
            \model ->
                { title = "Explain Math Playground"
                , body =
                    [ view model ]
                }
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    { input : String
    , result : String
    }


type Msg
    = UpdateInput String


init : () -> ( Model, Cmd Msg )
init flags =
    ( { input = "", result = "" }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "latex code input" ]
        , textarea [ onInput UpdateInput ] [ text model.input ]
        , h1 [] [ text "result" ]
        , text model.result
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateInput input ->
            ( { model
                | input = input
                , result =
                    MathParser.parse input
                        |> Result.map Interpreter.run
                        |> Debug.toString
              }
            , Cmd.none
            )
