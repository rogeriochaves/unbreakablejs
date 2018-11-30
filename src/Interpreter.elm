module Interpreter exposing (run, runSymbol)

import Dict exposing (Dict)
import Parser exposing (DeadEnd, Problem(..))
import Types exposing (..)


type alias State =
    { variables : Dict String Float
    , functions : Dict String FunctionSchema
    , error : Maybe DeadEnd
    }


newState : State
newState =
    { variables = Dict.empty
    , functions = Dict.empty
    , error = Nothing
    }


type alias LineResult =
    ( State, Float )


run : Types.Program -> Result Error (List Float)
run expressions =
    let
        iterate : Expression -> Result Error (List LineResult) -> Result Error (List LineResult)
        iterate expr accummulated =
            Result.andThen (iterateWithoutError expr) accummulated

        iterateWithoutError : Expression -> List LineResult -> Result Error (List LineResult)
        iterateWithoutError expr acc =
            let
                lastLineResult =
                    List.head acc
                        |> Maybe.withDefault ( newState, 0 )

                lineResult =
                    runExpression (Tuple.first lastLineResult) expr
            in
            case (Tuple.first lineResult).error of
                Nothing ->
                    Ok (lineResult :: acc)

                Just error ->
                    Err [ error ]
    in
    expressions
        |> List.foldl iterate (Ok [])
        |> Result.map (List.reverse >> List.map Tuple.second)


runExpression : State -> Expression -> LineResult
runExpression state expr =
    case expr of
        Integer val ->
            ( state, toFloat val )

        Floating val ->
            ( state, val )

        Identifier name ->
            -- TODO: break if variable is not available
            ( state, Dict.get name state.variables |> Maybe.withDefault 0 )

        Addition e1 e2 ->
            ( state, getExpressionValue state e1 + getExpressionValue state e2 )

        Subtraction e1 e2 ->
            ( state, getExpressionValue state e1 - getExpressionValue state e2 )

        Multiplication e1 e2 ->
            ( state, getExpressionValue state e1 * getExpressionValue state e2 )

        Division e1 e2 ->
            ( state, getExpressionValue state e1 / getExpressionValue state e2 )

        Exponentiation e1 e2 ->
            ( state, getExpressionValue state e1 ^ getExpressionValue state e2 )

        SymbolicFunction symbol ->
            runSymbol state symbol

        Equation variableName e ->
            let
                lineResult =
                    getExpressionValue state e
            in
            ( setVariable variableName lineResult state, lineResult )

        FunctionDeclaration name schema ->
            ( setFunction name schema state, 0 )

        FunctionCall name param ->
            let
                callFunction (FunctionSchema paramName body) =
                    let
                        param_ =
                            getExpressionValue state param

                        state_ =
                            setVariable paramName param_ state
                    in
                    getExpressionValue state_ body
            in
            -- TODO: break if function is not available
            ( state
            , Dict.get name state.functions
                |> Maybe.map callFunction
                |> Maybe.withDefault 0
            )


runSymbol : State -> Symbol -> LineResult
runSymbol state symbol =
    case symbol of
        SingleArity sym expr1 ->
            case sym of
                Sqrt ->
                    ( state, sqrt (getExpressionValue state expr1) )

        DoubleArity sym expr1 expr2 ->
            case sym of
                Frac ->
                    ( state, getExpressionValue state expr1 / getExpressionValue state expr2 )

        Iterator sym identifier expr1 expr2 expr3 ->
            case sym of
                Sum_ ->
                    let
                        lowerLimit =
                            getExpressionValue state expr1

                        upperLimit =
                            getExpressionValue state expr2

                        range =
                            List.range (round lowerLimit) (round upperLimit)

                        iterate curr total =
                            let
                                state_ =
                                    setVariable identifier (toFloat curr) state
                            in
                            total + getExpressionValue state_ expr3
                    in
                    if lowerLimit /= toFloat (round lowerLimit) then
                        throwError ("Error on sum_: cannot use " ++ String.fromFloat lowerLimit ++ " as a lower limit, it has to be an integer") state

                    else if upperLimit /= toFloat (round upperLimit) || upperLimit < lowerLimit then
                        throwError ("Error on sum_: cannot use " ++ String.fromFloat upperLimit ++ " as an upper limit, it has to be an integer higher than lower limit") state

                    else
                        ( state, List.foldl iterate 0 range )


getExpressionValue : State -> Expression -> Float
getExpressionValue state =
    runExpression state >> Tuple.second


setVariable : String -> Float -> State -> State
setVariable name value state =
    { state | variables = Dict.insert name value state.variables }


setFunction : String -> FunctionSchema -> State -> State
setFunction name functionSchema state =
    { state | functions = Dict.insert name functionSchema state.functions }


throwError : String -> State -> LineResult
throwError error state =
    ( { state | error = Just { row = 0, col = 0, problem = Problem error } }, 0 )
