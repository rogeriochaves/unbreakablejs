module Interpreter exposing (run, runSymbol)

import Dict exposing (Dict)
import Parser exposing (DeadEnd, Problem(..))
import Return exposing (throwError)
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


type alias LineResult =
    ( State, Return.Value )


run : Types.Program -> Result Error (List Return.Value)
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
                        |> Maybe.withDefault ( newState, Return.Void )

                lineResult =
                    runExpression (Tuple.first lastLineResult) expr
            in
            case Tuple.second lineResult of
                Return.Error error ->
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
        Number val ->
            ( state, Return.Num val )

        Identifier name ->
            ( state
            , Dict.get name state.variables
                |> Maybe.map Return.Num
                |> Maybe.withDefault (throwError (name ++ " is not defined"))
            )

        InfixFunction func e1 e2 ->
            runInfix state func e1 e2

        SymbolicFunction symbol ->
            runSymbol state symbol

        Assignment variableName e ->
            case getExpressionValue state e of
                Return.Num num ->
                    ( setVariable variableName num state, Return.Void )

                Return.Void ->
                    ( state, throwError ("Cannot set variable" ++ variableName ++ "to void") )

                Return.Error error ->
                    ( state, Return.Error error )

        FunctionDeclaration name schema ->
            ( setFunction name schema state, Return.Void )

        FunctionCall name param ->
            let
                callFunction (FunctionSchema paramName body) =
                    getExpressionValue state param
                        |> Return.andThen
                            (\param_ ->
                                getExpressionValue (setVariable paramName param_ state) body
                            )
            in
            ( state
            , Dict.get name state.functions
                |> Maybe.map callFunction
                |> Maybe.withDefault (throwError (name ++ " is not defined"))
            )


runInfix : State -> Infix -> Expression -> Expression -> LineResult
runInfix state func e1 e2 =
    let
        operator =
            case func of
                Addition ->
                    (+)

                Subtraction ->
                    (-)

                Multiplication ->
                    (*)

                Division ->
                    (/)

                Exponentiation ->
                    (^)
    in
    applyExpressions state e1 operator e2


runSymbol : State -> Symbol -> LineResult
runSymbol state symbol =
    case symbol of
        SingleArity sym expr1 ->
            case sym of
                Sqrt ->
                    applyExpression state sqrt expr1

        DoubleArity sym expr1 expr2 ->
            case sym of
                Frac ->
                    applyExpressions state expr1 (/) expr2

        Iterator sym identifier expr1 expr2 expr3 ->
            case sym of
                Sum_ ->
                    ( state
                    , Return.andThen2
                        (\lowerLimit upperLimit ->
                            let
                                range =
                                    List.range (round lowerLimit) (round upperLimit)

                                iterate curr total =
                                    let
                                        state_ =
                                            setVariable identifier (toFloat curr) state
                                    in
                                    total
                                        |> Return.map2 (\result total_ -> total_ + result) (getExpressionValue state_ expr3)
                            in
                            if lowerLimit /= toFloat (round lowerLimit) then
                                throwError ("Error on sum_: cannot use " ++ String.fromFloat lowerLimit ++ " as a lower limit, it has to be an integer")

                            else if upperLimit /= toFloat (round upperLimit) || upperLimit < lowerLimit then
                                throwError ("Error on sum_: cannot use " ++ String.fromFloat upperLimit ++ " as an upper limit, it has to be an integer higher than lower limit")

                            else
                                List.foldl iterate (Return.Num 0) range
                        )
                        (getExpressionValue state expr1)
                        (getExpressionValue state expr2)
                    )


getExpressionValue : State -> Expression -> Return.Value
getExpressionValue state =
    runExpression state >> Tuple.second


setVariable : String -> Float -> State -> State
setVariable name value state =
    { state | variables = Dict.insert name value state.variables }


setFunction : String -> FunctionSchema -> State -> State
setFunction name functionSchema state =
    { state | functions = Dict.insert name functionSchema state.functions }


applyExpressions : State -> Expression -> (Float -> Float -> Float) -> Expression -> LineResult
applyExpressions state e1 fn e2 =
    ( state
    , getExpressionValue state e2
        |> Return.map2 fn (getExpressionValue state e1)
    )


applyExpression : State -> (Float -> Float) -> Expression -> LineResult
applyExpression state fn expr =
    ( state, Return.map fn <| getExpressionValue state expr )
