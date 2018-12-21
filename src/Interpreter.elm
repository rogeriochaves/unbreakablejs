module Interpreter exposing (run)

import Dict exposing (Dict)
import Parser exposing (DeadEnd, Problem(..))
import Return exposing (Value(..), throwError)
import Types exposing (..)


type alias State =
    { scalars : Dict String Float
    , vectors : Dict String (List Expression)
    , functions : Dict String ( Identifier, Expression )
    }


newState : State
newState =
    { scalars = Dict.empty
    , vectors = Dict.empty
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
                        |> Maybe.withDefault ( newState, Void )

                lineResult =
                    runExpression (Tuple.first lastLineResult) expr
            in
            case Tuple.second lineResult of
                Error error ->
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
            ( state, Expression (Number val) )

        Vector items ->
            let
                appendOrLiftError curr acc =
                    case ( acc, curr ) of
                        ( Expression (Vector items_), Expression e ) ->
                            Expression (Vector (items_ ++ [ e ]))

                        ( Expression (Vector _), invalid ) ->
                            invalid

                        _ ->
                            acc
            in
            ( state
            , items
                |> List.map (eval state)
                |> List.foldl appendOrLiftError (Expression <| Vector [])
            )

        Variable identifier ->
            case identifier of
                ScalarIdentifier name ->
                    ( state
                    , Dict.get name state.scalars
                        |> Maybe.map (Expression << Number)
                        |> Maybe.withDefault (Expression (Variable identifier))
                    )

                VectorIdentifier name ->
                    ( state
                    , Dict.get name state.vectors
                        |> Maybe.map (Expression << Vector)
                        |> Maybe.withDefault (Expression (Variable identifier))
                    )

        Abstraction param body ->
            ( state
            , eval state body
                |> Return.map (Abstraction param)
            )

        SingleArity func e ->
            runSingleArity state func e

        DoubleArity func e1 e2 ->
            ( state, runDoubleArity state func e1 e2 )

        TripleArity func e1 e2 e3 ->
            ( state, runTripleArity state func e1 e2 e3 )


runSingleArity : State -> SingleArity -> Expression -> LineResult
runSingleArity state func expr =
    case func of
        Assignment identifier ->
            case identifier of
                ScalarIdentifier name ->
                    case eval state expr of
                        Expression (Number num) ->
                            ( setScalar name num state, Void )

                        Expression (Vector _) ->
                            Debug.todo "not implemented yet"

                        Void ->
                            ( state, throwError ("Cannot set variable" ++ name ++ "to void") )

                        Error error ->
                            ( state, Error error )

                        Expression (Abstraction params body) ->
                            ( setFunction name params body state, Void )

                        Expression e ->
                            ( state, Expression (SingleArity (Assignment identifier) e) )

                VectorIdentifier name ->
                    case eval state expr of
                        Expression (Vector v) ->
                            ( setVector name v state, Void )

                        _ ->
                            Debug.todo "not implemented"

        Application e1 ->
            case eval state e1 of
                Expression (Variable (ScalarIdentifier name)) ->
                    ( state
                    , Dict.get name state.functions
                        |> Maybe.map (callFunction state expr)
                        |> Maybe.withDefault
                            (eval state expr
                                |> Return.orElse (SingleArity func)
                                |> Return.andThenNum (Expression << SingleArity func << Number)
                            )
                    )

                _ ->
                    Debug.todo "not implemented"

        Sqrt ->
            ( state
            , eval state expr
                |> Return.mapNum sqrt
                |> Return.orElse (SingleArity func)
            )


callFunction : State -> Expression -> ( Identifier, Expression ) -> Return.Value
callFunction state args ( functionParam, functionBody ) =
    case functionParam of
        ScalarIdentifier paramName ->
            eval state args
                |> Return.andThenNum
                    (\param_ ->
                        eval (setScalar paramName param_ state) functionBody
                    )

        VectorIdentifier paramName ->
            eval state args
                |> Return.andThenVector
                    (\param_ ->
                        eval (setVector paramName param_ state) functionBody
                    )


runDoubleArity : State -> DoubleArity -> Expression -> Expression -> Return.Value
runDoubleArity state func e1 e2 =
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

                Frac ->
                    (/)
    in
    eval state e2
        |> Return.mapNum2 (DoubleArity func) operator (eval state e1)


runTripleArity : State -> TripleArity -> Expression -> Expression -> Expression -> Return.Value
runTripleArity state func expr1 expr2 expr3 =
    case func of
        Sum_ identifier ->
            let
                forLoop lowerLimit upperLimit =
                    if lowerLimit /= toFloat (round lowerLimit) then
                        throwError ("Error on sum_: cannot use " ++ String.fromFloat lowerLimit ++ " as a lower limit, it has to be an integer")

                    else if upperLimit /= toFloat (round upperLimit) || upperLimit < lowerLimit then
                        throwError ("Error on sum_: cannot use " ++ String.fromFloat upperLimit ++ " as an upper limit, it has to be an integer higher than lower limit")

                    else
                        List.range (round lowerLimit) (round upperLimit)
                            |> List.foldl iterate (Expression (Number 0))

                iterate curr total =
                    let
                        state_ =
                            setScalar identifier (toFloat curr) state
                    in
                    eval state_ expr3
                        |> Return.mapNum2 (\_ e -> e) (\result total_ -> total_ + result) total
                        |> Return.orElse (TripleArity func expr1 expr2)
            in
            Return.andThenNum2 (\e1 e2 -> TripleArity func e1 e2 expr3)
                forLoop
                (eval state expr1)
                (eval state expr2)


eval : State -> Expression -> Return.Value
eval state =
    runExpression state >> Tuple.second


setScalar : String -> Float -> State -> State
setScalar name value state =
    { state | scalars = Dict.insert name value state.scalars }


setVector : String -> List Expression -> State -> State
setVector name value state =
    { state | vectors = Dict.insert name value state.vectors }


setFunction : String -> Identifier -> Expression -> State -> State
setFunction name param body state =
    { state | functions = Dict.insert name ( param, body ) state.functions }
