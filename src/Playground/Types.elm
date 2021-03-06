module Playground.Types exposing (Cell, Example(..), Model, Msg(..))

import AutoExpand as AutoExpand
import Browser exposing (UrlRequest(..))
import Browser.Navigation exposing (Key)
import Playground.Routes exposing (..)
import Types exposing (..)
import Url exposing (Url)


type alias Model =
    { cells : List Cell
    , state : State
    , selectedCell : Int
    , page : Page
    , key : Key
    }


type alias Cell =
    { input : String
    , autoexpand : AutoExpand.State
    , result : Result Types.Error ( State, Maybe ExpressionResult )
    , submittedInput : String
    }


type Msg
    = NoOp
    | UpdateInput Int { textValue : String, state : AutoExpand.State }
    | AddCell
    | SelectCell Int
    | RunCell
    | ClearPlayground
    | SetExample Example
    | KeyDown (Maybe Int)
    | OnUrlChange Url
    | OnUrlRequest UrlRequest
    | Go Page


type Example
    = Basics
    | Softmax
    | Bitcoin
    | Statistics
