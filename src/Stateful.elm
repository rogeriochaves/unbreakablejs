module Stateful exposing (..)

import Dict
import Types exposing (..)


mergeStates : State -> State -> State
mergeStates a b =
    { variables = Dict.union a.variables b.variables }


session : State -> ExpressionResult
session state =
    { outScope = state
    , inScope = emptyState
    , result = Undefined []
    }


map : (a -> b) -> Stateful a -> Stateful b
map fn session_ =
    { outScope = session_.outScope
    , inScope = session_.inScope
    , result = fn session_.result
    }


andThen : (a -> Stateful a -> Stateful b) -> Stateful a -> Stateful b
andThen fn session_ =
    fn session_.result session_


run : (State -> Stateful b) -> Stateful a -> Stateful b
run fn { outScope, inScope } =
    let
        state =
            mergeStates inScope outScope
    in
    moveStateOutsideScope (fn state) ( outScope, inScope )


moveStateOutsideScope : Stateful a -> ( State, State ) -> Stateful a
moveStateOutsideScope expressionResult ( prevOutScope, prevInScope ) =
    let
        outScopeFiltered =
            mergeStates
                { variables =
                    Dict.filter
                        (\identifier _ ->
                            not (Dict.member identifier prevInScope.variables)
                        )
                        expressionResult.outScope.variables
                }
                prevOutScope

        inScopeUpdated =
            mergeStates
                { variables =
                    Dict.filter
                        (\identifier _ ->
                            Dict.member identifier prevInScope.variables
                        )
                        expressionResult.outScope.variables
                }
                (mergeStates expressionResult.inScope prevInScope)
    in
    { outScope = outScopeFiltered, inScope = inScopeUpdated, result = expressionResult.result }
