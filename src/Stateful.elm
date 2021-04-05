module Stateful exposing (..)

import Dict
import Types exposing (..)


mergeStates : State -> State -> State
mergeStates (State a) (State b) =
    State (Dict.union a b)


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
    fn (mergeStates inScope outScope)
        |> moveStateOutsideScope ( outScope, inScope )


runInScope : (State -> Stateful b) -> Stateful a -> Stateful b
runInScope fn { outScope, inScope } =
    fn inScope
        |> moveStateOutsideScope ( outScope, inScope )


getVariables : State -> Dict.Dict String ( State, Value )
getVariables (State state) =
    state


moveStateOutsideScope : ( State, State ) -> Stateful a -> Stateful a
moveStateOutsideScope ( prevOutScope, prevInScope ) expressionResult =
    let
        outScopeFiltered =
            mergeStates
                (State
                    (Dict.filter
                        (\identifier _ ->
                            not (Dict.member identifier (getVariables prevInScope))
                        )
                        (getVariables expressionResult.outScope)
                    )
                )
                prevOutScope

        inScopeUpdated =
            mergeStates
                (State
                    (Dict.filter
                        (\identifier _ ->
                            Dict.member identifier (getVariables prevInScope)
                        )
                        (getVariables expressionResult.outScope)
                    )
                )
                (mergeStates expressionResult.inScope prevInScope)
    in
    { outScope = outScopeFiltered, inScope = inScopeUpdated, result = expressionResult.result }
