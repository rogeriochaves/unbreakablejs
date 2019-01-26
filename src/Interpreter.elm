module Interpreter exposing (LineResult, State, newState, run)

import Dict exposing (Dict)
import List.Extra
import Parser exposing (DeadEnd, Problem(..))
import Return exposing (Value(..), throwError)
import Types exposing (..)


type alias State =
    { scalars : Dict String Float
    , vectors : Dict String (List Expression)
    , functions : Dict String ( Identifier, Expression )
    , mapFunctions : Dict String ( String, String, Expression )
    }


newState : State
newState =
    { scalars = Dict.empty
    , vectors = Dict.empty
    , functions = Dict.empty
    , mapFunctions = Dict.empty
    }
        |> setScalar "e" e
        |> setScalar "\\pi" pi


type alias LineResult =
    ( State, Return.Value )


run : State -> Types.Program -> Result Error (List LineResult)
run state expressions =
    let
        iterate : Expression -> Result Error (List LineResult) -> Result Error (List LineResult)
        iterate expr accummulated =
            Result.andThen (iterateWithoutError expr) accummulated

        iterateWithoutError : Expression -> List LineResult -> Result Error (List LineResult)
        iterateWithoutError expr acc =
            let
                lastLineResult =
                    List.head acc
                        |> Maybe.withDefault ( state, Void )

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
        |> Result.map List.reverse


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
            , Expression (Abstraction param body)
            )

        MapAbstraction param index body ->
            ( state
            , Expression (MapAbstraction param index body)
            )

        SingleArity func e ->
            runSingleArity state func e

        DoubleArity func e1 e2 ->
            ( state, runDoubleArity state func e1 e2 )

        TripleArity func e1 e2 e3 ->
            ( state, runTripleArity state func e1 e2 e3 )

        Block name blockExpressions ->
            case run state blockExpressions of
                Err errors ->
                    ( state
                    , List.reverse errors
                        |> List.head
                        |> Maybe.map Error
                        |> Maybe.withDefault (throwError "error in block with no error")
                    )

                Ok results ->
                    List.reverse results
                        |> List.head
                        |> Maybe.withDefault ( state, Void )


runSingleArity : State -> SingleArity -> Expression -> LineResult
runSingleArity state func expr =
    case func of
        Assignment identifier ->
            case identifier of
                ScalarIdentifier name ->
                    case eval state expr of
                        Expression (Number num) ->
                            ( setScalar name num state, Expression (Number num) )

                        Expression (Vector _) ->
                            ( state, throwError ("Cannot assign vector to scalar variables, use \\vec{" ++ name ++ "} instead") )

                        Void ->
                            ( state, throwError ("Cannot set variable " ++ name ++ " to void") )

                        Error error ->
                            ( state, Error error )

                        Expression (Abstraction params body) ->
                            ( setFunction name params body state, Void )

                        Expression (MapAbstraction params index body) ->
                            ( setMapFunction name params index body state, Void )

                        Expression e ->
                            ( state, Expression (SingleArity (Assignment identifier) e) )

                VectorIdentifier name ->
                    case eval state expr of
                        Expression (Number _) ->
                            ( state, throwError "Cannot assign scalar to vector variables" )

                        Expression (Vector v) ->
                            ( setVector name v state, Expression (Vector v) )

                        Void ->
                            ( state, throwError ("Cannot set variable " ++ name ++ " to void") )

                        Error error ->
                            ( state, Error error )

                        Expression e ->
                            ( state, Expression (SingleArity (Assignment identifier) e) )

        Application e1 ->
            case eval state e1 of
                Expression (Variable (ScalarIdentifier name)) ->
                    ( state
                    , case ( Dict.get name state.mapFunctions, Dict.get name state.functions ) of
                        ( Just mapFn, _ ) ->
                            callMapFunction func state expr mapFn

                        ( Nothing, Just fn ) ->
                            callFunction func state expr fn

                        ( Nothing, Nothing ) ->
                            eval state expr
                                |> Return.andThenNum (SingleArity func) (Expression << SingleArity func << Number)
                    )

                _ ->
                    Debug.todo "not implemented"

        Sqrt ->
            ( state
            , eval state expr
                |> Return.mapNum (SingleArity func) sqrt
            )

        Factorial ->
            let
                factorial : Float -> Value
                factorial num =
                    if not (isInteger num) || num < 0 then
                        throwError ("Cannot calculate factorial for " ++ String.fromFloat num ++ ", only for positive integers")

                    else
                        List.range 1 (round num)
                            |> List.foldl (*) 1
                            |> (Expression << Number << toFloat)
            in
            ( state
            , eval state expr
                |> Return.andThenNum (SingleArity func) factorial
            )

        Negation ->
            ( state
            , eval state expr
                |> Return.mapNum (SingleArity func) negate
            )

        Summation ->
            ( state
            , eval state expr
                |> Return.andThenVector (SingleArity func)
                    (List.foldl
                        (\curr acc ->
                            case acc of
                                Expression (Number n) ->
                                    if n == 0 then
                                        Expression curr

                                    else
                                        Expression (DoubleArity Addition (Number n) curr)

                                Expression e ->
                                    Expression (DoubleArity Addition e curr)

                                acc_ ->
                                    acc_
                        )
                        (Expression (Number 0))
                        >> Return.andThen (eval state)
                    )
            )

        Cardinality ->
            ( state
            , eval state expr
                |> Return.andThenVector (SingleArity func)
                    (\items -> List.length items |> toFloat |> Number |> Expression)
            )


callFunction : SingleArity -> State -> Expression -> ( Identifier, Expression ) -> Return.Value
callFunction func state args ( functionParam, functionBody ) =
    case functionParam of
        ScalarIdentifier paramName ->
            eval state args
                |> Return.andThenNum (SingleArity func)
                    (\param_ ->
                        eval (setScalar paramName param_ state) functionBody
                    )

        VectorIdentifier paramName ->
            eval state args
                |> Return.andThenVector (SingleArity func)
                    (\param_ ->
                        eval (setVector paramName param_ state) functionBody
                    )


callMapFunction : SingleArity -> State -> Expression -> ( String, String, Expression ) -> Return.Value
callMapFunction func state args ( functionParam, functionIndex, functionBody ) =
    eval state args
        |> Return.andThenVector (SingleArity func)
            (\items ->
                List.Extra.indexedFoldl
                    (\i _ acc ->
                        let
                            state_ =
                                state
                                    |> setVector functionParam items
                                    |> setScalar functionIndex (toFloat <| i + 1)
                        in
                        case ( acc, eval state_ functionBody ) of
                            ( Expression (Vector items_), Expression e ) ->
                                Expression (Vector (items_ ++ [ e ]))

                            ( Expression (Vector items_), error ) ->
                                error

                            ( acc_, _ ) ->
                                acc_
                    )
                    (Expression (Vector []))
                    items
            )


runDoubleArity : State -> DoubleArity -> Expression -> Expression -> Return.Value
runDoubleArity state func e1 e2 =
    let
        numOp operator =
            eval state e2
                |> Return.mapNum2 (DoubleArity func) operator (eval state e1)
    in
    case func of
        Addition ->
            numOp (+)

        Subtraction ->
            numOp (-)

        Multiplication ->
            numOp (*)

        Division ->
            numOp (/)

        Exponentiation ->
            numOp (^)

        Frac ->
            numOp (/)

        Index ->
            let
                vector =
                    case e1 of
                        Variable (ScalarIdentifier name) ->
                            Variable (VectorIdentifier name)

                        _ ->
                            e1
            in
            eval state vector
                |> Return.andThenVector (\v -> DoubleArity Index v e2)
                    (\items ->
                        eval state e2
                            |> Return.andThenNum
                                (DoubleArity Index (Vector items))
                                (\index ->
                                    if not (isInteger index) || index < 1 then
                                        throwError ("Cannot use " ++ String.fromFloat index ++ " as an index, it has to be a positive integer")

                                    else
                                        case List.head <| List.drop (round index - 1) items of
                                            Just item ->
                                                Expression item

                                            Nothing ->
                                                throwError ("Index " ++ String.fromFloat index ++ " out of bounds")
                                )
                    )

        Modulo ->
            eval state e2
                |> Return.andThenNum2 (DoubleArity func)
                    (\a b ->
                        if isInteger a && isInteger b then
                            Expression (Number (toFloat (round a |> modBy (round b))))

                        else
                            throwError ("Modulo operation can only be performed on integers, you are trying to calculate " ++ String.fromFloat a ++ " \\mod " ++ String.fromFloat b)
                    )
                    (eval state e1)

        EuclideanDivision ->
            eval state e2
                |> Return.andThenNum2 (DoubleArity func)
                    (\a b ->
                        if isInteger a && isInteger b then
                            Expression (Number (toFloat <| floor <| a / b))

                        else
                            throwError ("Euclidean division can only be performed on integers, you are trying to calculate " ++ String.fromFloat a ++ " \\div " ++ String.fromFloat b)
                    )
                    (eval state e1)


isInteger : Float -> Bool
isInteger n =
    n == toFloat (round n)


runTripleArity : State -> TripleArity -> Expression -> Expression -> Expression -> Return.Value
runTripleArity state func expr1 expr2 expr3 =
    case func of
        Sum_ identifier ->
            let
                forLoop lowerLimit upperLimit =
                    if not (isInteger lowerLimit) then
                        throwError ("Error on sum_: cannot use " ++ String.fromFloat lowerLimit ++ " as a lower limit, it has to be an integer")

                    else if not (isInteger upperLimit) || upperLimit < lowerLimit then
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
                        |> Return.mapNum2 (\_ -> TripleArity func expr1 expr2) (\result total_ -> total_ + result) total
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


setMapFunction : String -> String -> String -> Expression -> State -> State
setMapFunction name param index body state =
    { state | mapFunctions = Dict.insert name ( param, index, body ) state.mapFunctions }
