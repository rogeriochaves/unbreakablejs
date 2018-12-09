module Playground exposing (main)

import Browser
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed
import Interpreter
import MathParser


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
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


emptyLatexState =
    { counters = Dict.fromList [ ( "s1", 0 ), ( "s2", 0 ), ( "s3", 0 ), ( "tno", 0 ), ( "eqno", 0 ) ]
    , crossReferences = Dict.empty
    , dictionary = Dict.empty
    , tableOfContents = []
    , macroDictionary = Dict.empty
    }


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Explain Math" ]
        , h2 [] [ text "latex code input" ]
        , textarea [ onInput UpdateInput ] [ text model.input ]
        , h2 [] [ text "latex output" ]
        , renderLatex model
        , h2 [] [ text "result" ]
        , text model.result
        ]


renderLatex model =
    let
        convertedText =
            "$$\n" ++ String.replace "\n" "\\\\" model.input ++ "\n$$"
    in
    div []
        [ Html.Keyed.node "div"
            []
            [ ( model.input, div [ class "raw-math" ] [ text <| convertedText ] )
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateInput input ->
            ( { model
                | input = input
                , result =
                    MathParser.parse input
                        |> Result.andThen Interpreter.run
                        |> Debug.toString
              }
            , Cmd.none
            )
