module Interpreter exposing (LineResult, State, newState, run)

import Dict exposing (Dict)
import List.Extra
import Parser exposing (Problem(..))
import Return exposing (throwError)
import Types exposing (..)


type alias State =
    { variables : Dict String Value
    }


newState : State
newState =
    { variables = Dict.empty
    }


type alias LineResult =
    ( State, Expression )


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
                        -- TODO: track here
                        |> Maybe.withDefault ( state, Untracked (Value (Undefined [])) )

                lineResult =
                    runExpression (Tuple.first lastLineResult) expr
            in
            case Tuple.second lineResult of
                Untracked (Error error) ->
                    Err [ error ]

                Tracked _ (Error error) ->
                    Err [ error ]

                _ ->
                    Ok (lineResult :: acc)
    in
    expressions
        |> List.foldl iterate (Ok [])
        |> Result.map List.reverse


runExpression : State -> Expression -> LineResult
runExpression state expr =
    let
        trackStack =
            case expr of
                Tracked info _ ->
                    [ info ]

                _ ->
                    []
    in
    case expr |> removeTracking of
        Value (Vector items) ->
            let
                appendOrLiftError curr acc =
                    case ( acc, curr ) of
                        ( Vector items_, e ) ->
                            Vector (items_ ++ [ e ])

                        _ ->
                            acc
            in
            ( state
            , items
                |> List.map (eval state)
                |> List.foldl appendOrLiftError (Vector [])
                |> Value
                |> Untracked
            )

        Value val ->
            ( state, Untracked (Value val) )

        Variable identifier ->
            ( state
            , Dict.get identifier state.variables
                |> Maybe.map Value
                |> Maybe.withDefault (Value (Undefined []))
                |> Untracked
            )

        ReservedApplication symbol args ->
            let
                evaluatedArgs =
                    List.map (eval state) args
            in
            applyReserved state symbol evaluatedArgs trackStack

        Application fn args ->
            let
                evaluatedArgs =
                    List.map (eval state) args
            in
            case eval state fn |> removeTracking of
                Value (Abstraction paramNames functionBody) ->
                    ( state, callFunction state ( paramNames, functionBody ) evaluatedArgs )

                Value (Undefined stacktrace) ->
                    ( state, Untracked (Value (Undefined (stacktrace ++ trackStack))) )

                _ ->
                    Debug.todo "not implemented"

        -- MapAbstraction param index body ->
        --     ( state
        --     , Expression (MapAbstraction param index body)
        --     )
        -- SingleArity func e ->
        --     runSingleArity state func e
        -- DoubleArity func e1 e2 ->
        --     ( state, runDoubleArity state func e1 e2 )
        -- TripleArity func e1 e2 e3 ->
        --     ( state, runTripleArity state func e1 e2 e3 )
        Block name blockExpressions ->
            case run state blockExpressions of
                Err errors ->
                    ( state
                    , List.reverse errors
                        |> List.head
                        |> Maybe.map (Untracked << Error)
                        |> Maybe.withDefault (throwError "error in block with no error")
                    )

                Ok results ->
                    List.reverse results
                        |> List.head
                        |> Maybe.withDefault ( state, Untracked (Value (Undefined [])) )

        Error e ->
            ( state, Untracked (Error e) )


applyReserved : State -> Reserved -> List Expression -> List TrackInfo -> ( State, Expression )
applyReserved state reserved evaluatedArgs trackStack =
    case reserved of
        Addition ->
            ( state, Return.mapNumArgs2 trackStack (+) evaluatedArgs )

        Subtraction ->
            ( state, Return.mapNumArgs2 trackStack (-) evaluatedArgs )

        Assignment name ->
            case Return.argOrDefault 0 evaluatedArgs |> removeTracking of
                Value val ->
                    ( setVariable name val state, Untracked (Value (Undefined trackStack)) )

                _ ->
                    Debug.todo "not implemented"



-- runSingleArity : State -> SingleArity -> Expression -> LineResult
-- runSingleArity state func expr =
--     case func of
--         Assignment identifier ->
--             case identifier of
--                 ScalarIdentifier name ->
--                     case eval state expr of
--                         Expression (Number num) ->
--                             ( setVariable name num state, Expression (Number num) )
--                         Expression (Vector _) ->
--                             ( state, throwError ("Cannot assign vector to scalar variables, use \\vec{" ++ name ++ "} instead") )
--                         Undefined ->
--                             ( state, throwError ("Cannot set variable " ++ name ++ " to Undefined") )
--                         Error error ->
--                             ( state, Error error )
--                         Expression (Abstraction params body) ->
--                             ( setFunction name params body state, Undefined )
--                         Expression (MapAbstraction params index body) ->
--                             ( setMapFunction name params index body state, Undefined )
--                         Expression e ->
--                             ( state, Expression (SingleArity (Assignment identifier) e) )
--                 VectorIdentifier name ->
--                     case eval state expr of
--                         Expression (Number _) ->
--                             ( state, throwError "Cannot assign scalar to vector variables" )
--                         Expression (Vector v) ->
--                             ( setVector name v state, Expression (Vector v) )
--                         Undefined ->
--                             ( state, throwError ("Cannot set variable " ++ name ++ " to Undefined") )
--                         Error error ->
--                             ( state, Error error )
--                         Expression e ->
--                             ( state, Expression (SingleArity (Assignment identifier) e) )
--         Application e1 ->
--             case eval state e1 of
--                 Expression (Variable (ScalarIdentifier name)) ->
--                     ( state
--                     , case ( Dict.get name state.mapFunctions, Dict.get name state.functions ) of
--                         ( Just mapFn, _ ) ->
--                             callMapFunction func state expr mapFn
--                         ( Nothing, Just fn ) ->
--                             callFunction func state expr fn
--                         ( Nothing, Nothing ) ->
--                             eval state expr
--                                 |> Return.andThenNum (SingleArity func) (Expression << SingleArity func << Number)
--                     )
--                 _ ->
--                     Debug.todo "not implemented"
--         Sqrt ->
--             ( state
--             , eval state expr
--                 |> Return.mapNum (SingleArity func) sqrt
--             )
--         Factorial ->
--             let
--                 factorial : Float -> Value
--                 factorial num =
--                     if not (isInteger num) || num < 0 then
--                         throwError ("Cannot calculate factorial for " ++ String.fromFloat num ++ ", only for positive integers")
--                     else
--                         List.range 1 (round num)
--                             |> List.foldl (*) 1
--                             |> (Expression << Number << toFloat)
--             in
--             ( state
--             , eval state expr
--                 |> Return.andThenNum (SingleArity func) factorial
--             )
--         Negation ->
--             ( state
--             , eval state expr
--                 |> Return.mapNum (SingleArity func) negate
--             )
--         Summation ->
--             ( state
--             , eval state expr
--                 |> Return.andThenVector (SingleArity func)
--                     (List.foldl
--                         (\curr acc ->
--                             case acc of
--                                 Expression (Number n) ->
--                                     if n == 0 then
--                                         Expression curr
--                                     else
--                                         Expression (DoubleArity Addition (Number n) curr)
--                                 Expression e ->
--                                     Expression (DoubleArity Addition e curr)
--                                 acc_ ->
--                                     acc_
--                         )
--                         (Expression (Number 0))
--                         >> Return.andThen (eval state)
--                     )
--             )
--         Cardinality ->
--             ( state
--             , eval state expr
--                 |> Return.andThenVector (SingleArity func)
--                     (\items -> List.length items |> toFloat |> Number |> Expression)
--             )


callFunction : State -> ( List String, Expression ) -> List Expression -> Expression
callFunction state ( paramNames, functionBody ) args =
    let
        closure =
            List.Extra.indexedFoldl
                (\index paramName state_ ->
                    case Return.argOrDefault index args |> removeTracking of
                        Value val ->
                            setVariable paramName val state_

                        _ ->
                            Debug.todo "not implemented"
                )
                state
                paramNames
    in
    eval closure functionBody



-- mapTracking : (UntrackedExp -> UntrackedExp) -> Expression -> Expression
-- mapTracking fn expr =
--     case expr of
--         Tracked info e ->
--             Tracked info (fn e)
--         Untracked e ->
--             Untracked e


removeTracking : Expression -> UntrackedExp
removeTracking expr =
    case expr of
        Tracked _ e ->
            e

        Untracked e ->
            e


mapTracking : (UntrackedExp -> UntrackedExp) -> Expression -> Expression
mapTracking fn expr =
    case expr of
        Tracked info e ->
            Tracked info (fn e)

        Untracked e ->
            Untracked (fn e)



-- Debug.todo "call function"
-- eval state args
--     |> Return.andThenNum (SingleArity func)
--         (\param_ ->
--             eval (setVariable paramName param_ state) functionBody
--         )
-- callMapFunction : SingleArity -> State -> Expression -> ( String, String, Expression ) -> Return.Value
-- callMapFunction func state args ( functionParam, functionIndex, functionBody ) =
--     eval state args
--         |> Return.andThenVector (SingleArity func)
--             (\items ->
--                 List.Extra.indexedFoldl
--                     (\i _ acc ->
--                         let
--                             state_ =
--                                 state
--                                     |> setVector functionParam items
--                                     |> setVariable functionIndex (toFloat <| i + 1)
--                         in
--                         case ( acc, eval state_ functionBody ) of
--                             ( Expression (Vector items_), Expression e ) ->
--                                 Expression (Vector (items_ ++ [ e ]))
--                             ( Expression (Vector items_), error ) ->
--                                 error
--                             ( acc_, _ ) ->
--                                 acc_
--                     )
--                     (Expression (Vector []))
--                     items
--             )
-- runDoubleArity : State -> DoubleArity -> Expression -> Expression -> Return.Value
-- runDoubleArity state func e1 e2 =
--     let
--         numOp operator =
--             eval state e2
--                 |> Return.mapNum2 (DoubleArity func) operator (eval state e1)
--     in
--     case func of
--         Addition ->
--             numOp (+)
--         Subtraction ->
--             numOp (-)
--         Multiplication ->
--             numOp (*)
--         Division ->
--             numOp (/)
--         Exponentiation ->
--             numOp (^)
--         Frac ->
--             numOp (/)
--         Index ->
--             let
--                 vector =
--                     case e1 of
--                         Variable (ScalarIdentifier name) ->
--                             Variable (VectorIdentifier name)
--                         _ ->
--                             e1
--             in
--             eval state vector
--                 |> Return.andThenVector (\v -> DoubleArity Index v e2)
--                     (\items ->
--                         eval state e2
--                             |> Return.andThenNum
--                                 (DoubleArity Index (Vector items))
--                                 (\index ->
--                                     if not (isInteger index) || index < 1 then
--                                         throwError ("Cannot use " ++ String.fromFloat index ++ " as an index, it has to be a positive integer")
--                                     else
--                                         case List.head <| List.drop (round index - 1) items of
--                                             Just item ->
--                                                 Expression item
--                                             Nothing ->
--                                                 throwError ("Index " ++ String.fromFloat index ++ " out of bounds")
--                                 )
--                     )
--         Modulo ->
--             eval state e2
--                 |> Return.andThenNum2 (DoubleArity func)
--                     (\a b ->
--                         if isInteger a && isInteger b then
--                             Expression (Number (toFloat (round a |> modBy (round b))))
--                         else
--                             throwError ("Modulo operation can only be performed on integers, you are trying to calculate " ++ String.fromFloat a ++ " \\mod " ++ String.fromFloat b)
--                     )
--                     (eval state e1)
--         EuclideanDivision ->
--             eval state e2
--                 |> Return.andThenNum2 (DoubleArity func)
--                     (\a b ->
--                         if isInteger a && isInteger b then
--                             Expression (Number (toFloat <| floor <| a / b))
--                         else
--                             throwError ("Euclidean division can only be performed on integers, you are trying to calculate " ++ String.fromFloat a ++ " \\div " ++ String.fromFloat b)
--                     )
--                     (eval state e1)
-- isInteger : Float -> Bool
-- isInteger n =
--     n == toFloat (round n)
-- runTripleArity : State -> TripleArity -> Expression -> Expression -> Expression -> Return.Value
-- runTripleArity state func expr1 expr2 expr3 =
--     case func of
--         Sum_ identifier ->
--             let
--                 forLoop lowerLimit upperLimit =
--                     if not (isInteger lowerLimit) then
--                         throwError ("Error on sum_: cannot use " ++ String.fromFloat lowerLimit ++ " as a lower limit, it has to be an integer")
--                     else if not (isInteger upperLimit) || upperLimit < lowerLimit then
--                         throwError ("Error on sum_: cannot use " ++ String.fromFloat upperLimit ++ " as an upper limit, it has to be an integer higher than lower limit")
--                     else
--                         List.range (round lowerLimit) (round upperLimit)
--                             |> List.foldl iterate (Expression (Number 0))
--                 iterate curr total =
--                     let
--                         state_ =
--                             setVariable identifier (toFloat curr) state
--                     in
--                     eval state_ expr3
--                         |> Return.mapNum2 (\_ -> TripleArity func expr1 expr2) (\result total_ -> total_ + result) total
--             in
--             Return.andThenNum2 (\e1 e2 -> TripleArity func e1 e2 expr3)
--                 forLoop
--                 (eval state expr1)
--                 (eval state expr2)


eval : State -> Expression -> Expression
eval state =
    runExpression state >> Tuple.second


setVariable : String -> Value -> State -> State
setVariable name value state =
    { state | variables = Dict.insert name value state.variables }
