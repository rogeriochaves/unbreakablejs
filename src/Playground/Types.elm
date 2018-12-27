module Playground.Types exposing (Cell, Example(..), Model, Msg(..))

import AutoExpand as AutoExpand
import Browser exposing (UrlRequest(..))
import Browser.Navigation exposing (Key, load, pushUrl)
import Interpreter
import Playground.Routes exposing (..)
import Return exposing (Value(..))
import Url exposing (Url)


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
