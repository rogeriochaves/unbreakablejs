module Interpreter exposing (ReturnValue(..), run, runSymbol)

import Dict exposing (Dict)
import Parser exposing (DeadEnd, Problem(..))
import Types exposing (..)


type alias State =
    { variables : Dict String Float
    , functions : Dict String FunctionSchema
    }


newState : State
newState =
    { variables = Dict.empty
    , functions = Dict.empty
    }


type ReturnValue
    = NumVal Float
    | VoidVal
    | ErrorVal DeadEnd


type alias LineResult =
    ( State, ReturnValue )


run : Types.Program -> Result Error (List ReturnValue)
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
                        |> Maybe.withDefault ( newState, VoidVal )

                lineResult =
                    runExpression (Tuple.first lastLineResult) expr
            in
            case Tuple.second lineResult of
                ErrorVal error ->
                    Err [ error ]

                _ ->
                    Ok (lineResult :: acc)
    in
    expressions
        |> List.foldl iterate (Ok [])
        |> Result.map (List.reverse >> List.map Tuple.second)


runExpression : State -> Expression -> LineResult
runExpression state expr =
    case expr of
        Integer val ->
            ( state, NumVal (toFloat val) )

        Floating val ->
            ( state, NumVal val )

        Identifier name ->
            -- TODO: break if variable is not available
            ( state, Dict.get name state.variables |> Maybe.withDefault 0 |> NumVal )

        Addition e1 e2 ->
            ( state, applyExpressions state e1 (+) e2 )

        Subtraction e1 e2 ->
            ( state, applyExpressions state e1 (-) e2 )

        Multiplication e1 e2 ->
            ( state, applyExpressions state e1 (*) e2 )

        Division e1 e2 ->
            ( state, applyExpressions state e1 (/) e2 )

        Exponentiation e1 e2 ->
            ( state, applyExpressions state e1 (^) e2 )

        SymbolicFunction symbol ->
            runSymbol state symbol

        Equation variableName e ->
            case getExpressionValue state e of
                NumVal num ->
                    ( setVariable variableName num state, VoidVal )

                VoidVal ->
                    ( state, throwError ("Cannot set variable" ++ variableName ++ "to void") )

                ErrorVal error ->
                    ( state, ErrorVal error )

        FunctionDeclaration name schema ->
            ( setFunction name schema state, VoidVal )

        FunctionCall name param ->
            let
                callFunction (FunctionSchema paramName body) =
                    getExpressionValue state param
                        |> returnAndThen
                            (\param_ ->
                                getExpressionValue (setVariable paramName param_ state) body
                            )
            in
            -- TODO: break if function is not available
            ( state
            , Dict.get name state.functions
                |> Maybe.map callFunction
                |> Maybe.withDefault (NumVal 0)
            )


runSymbol : State -> Symbol -> LineResult
runSymbol state symbol =
    case symbol of
        SingleArity sym expr1 ->
            case sym of
                Sqrt ->
                    ( state, applyExpression state sqrt expr1 )

        DoubleArity sym expr1 expr2 ->
            case sym of
                Frac ->
                    ( state, applyExpressions state expr1 (/) expr2 )

        Iterator sym identifier expr1 expr2 expr3 ->
            case sym of
                Sum_ ->
                    ( state
                    , returnAndThen2
                        (\lowerLimit upperLimit ->
                            let
                                range =
                                    List.range (round lowerLimit) (round upperLimit)

                                iterate curr total =
                                    let
                                        state_ =
                                            setVariable identifier (toFloat curr) state
                                    in
                                    map2NumReturn (\result total_ -> total_ + result) (getExpressionValue state_ expr3) total
                            in
                            if lowerLimit /= toFloat (round lowerLimit) then
                                throwError ("Error on sum_: cannot use " ++ String.fromFloat lowerLimit ++ " as a lower limit, it has to be an integer")

                            else if upperLimit /= toFloat (round upperLimit) || upperLimit < lowerLimit then
                                throwError ("Error on sum_: cannot use " ++ String.fromFloat upperLimit ++ " as an upper limit, it has to be an integer higher than lower limit")

                            else
                                List.foldl iterate (NumVal 0) range
                        )
                        (getExpressionValue state expr1)
                        (getExpressionValue state expr2)
                    )


getExpressionValue : State -> Expression -> ReturnValue
getExpressionValue state =
    runExpression state >> Tuple.second


setVariable : String -> Float -> State -> State
setVariable name value state =
    { state | variables = Dict.insert name value state.variables }


setFunction : String -> FunctionSchema -> State -> State
setFunction name functionSchema state =
    { state | functions = Dict.insert name functionSchema state.functions }


throwError : String -> ReturnValue
throwError error =
    ErrorVal { row = 0, col = 0, problem = Problem error }


mapNumReturn : (Float -> Float) -> ReturnValue -> ReturnValue
mapNumReturn fn returnVal =
    case returnVal of
        NumVal float ->
            NumVal <| fn float

        VoidVal ->
            throwError "Cannot apply function to void"

        ErrorVal _ ->
            returnVal


map2NumReturn : (Float -> Float -> Float) -> ReturnValue -> ReturnValue -> ReturnValue
map2NumReturn fn returnVal returnVal2 =
    case ( returnVal, returnVal2 ) of
        ( NumVal float1, NumVal float2 ) ->
            NumVal <| fn float1 float2

        ( ErrorVal _, _ ) ->
            returnVal

        ( _, ErrorVal _ ) ->
            returnVal2

        ( VoidVal, _ ) ->
            throwError "Cannot apply function to void"

        ( _, VoidVal ) ->
            throwError "Cannot apply function to void"


applyExpressions : State -> Expression -> (Float -> Float -> Float) -> Expression -> ReturnValue
applyExpressions state e1 fn e2 =
    map2NumReturn fn (getExpressionValue state e1) (getExpressionValue state e2)


applyExpression : State -> (Float -> Float) -> Expression -> ReturnValue
applyExpression state fn e1 =
    mapNumReturn fn (getExpressionValue state e1)


returnAndThen : (Float -> ReturnValue) -> ReturnValue -> ReturnValue
returnAndThen fn returnVal =
    case returnVal of
        NumVal float ->
            fn float

        VoidVal ->
            throwError "Cannot apply function to void"

        ErrorVal _ ->
            returnVal


returnAndThen2 : (Float -> Float -> ReturnValue) -> ReturnValue -> ReturnValue -> ReturnValue
returnAndThen2 fn returnVal returnVal2 =
    case ( returnVal, returnVal2 ) of
        ( NumVal float1, NumVal float2 ) ->
            fn float1 float2

        ( ErrorVal _, _ ) ->
            returnVal

        ( _, ErrorVal _ ) ->
            returnVal2

        ( VoidVal, _ ) ->
            throwError "Cannot apply function to void"

        ( _, VoidVal ) ->
            throwError "Cannot apply function to void"
