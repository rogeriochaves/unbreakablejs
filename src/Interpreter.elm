module Interpreter exposing (run)

import Dict
import Fuzz exposing (result)
import List.Extra
import Parser exposing (Problem(..))
import Stateful
import Test.Runner.Failure exposing (Reason(..))
import Types exposing (..)


run : State -> Types.Program -> ( State, List ExpressionResult )
run state expressions =
    let
        iterate_ : Expression -> Stateful (List ExpressionResult) -> Stateful (List ExpressionResult)
        iterate_ expr acc =
            let
                statefulResult =
                    Stateful.run (eval expr) acc
            in
            statefulResult
                |> Stateful.map (\_ -> statefulResult :: acc.result)
    in
    expressions
        |> List.foldl iterate_ (Stateful state emptyState [])
        |> (\{ outScope, inScope, result } ->
                ( Stateful.mergeStates inScope outScope
                , List.reverse result
                )
           )


runBlock : State -> List Expression -> ( State, Maybe Value )
runBlock state blockExpressions =
    let
        iterate_ : Expression -> Stateful (Maybe Value) -> Stateful (Maybe Value)
        iterate_ expr acc =
            if acc.result == Nothing then
                acc
                    |> Stateful.run (eval expr)
                    |> Stateful.map
                        (\result ->
                            case expr |> removeTracking of
                                Return _ ->
                                    Just result

                                _ ->
                                    Nothing
                        )

            else
                acc
    in
    -- TODO: return last if outside function? (right now returns void)
    blockExpressions
        |> List.foldl iterate_ (Stateful state emptyState Nothing)
        |> (\{ outScope, result } -> ( outScope, result ))


eval : Expression -> State -> ExpressionResult
eval expr state =
    let
        trackStack : UndefinedReason -> List UndefinedTrackInfo
        trackStack reason =
            case expr of
                Tracked info _ ->
                    [ { column = info.column, line = info.line, filename = info.filename, reason = reason } ]

                _ ->
                    []

        return result =
            { outScope = emptyState, inScope = emptyState, result = result }
    in
    case expr |> removeTracking of
        ArrayExpression items ->
            evalList items state
                |> Stateful.map Array

        Value val ->
            return val

        Variable identifier ->
            return
                (Dict.get identifier state.variables
                    |> Maybe.withDefault (Undefined (trackStack <| VariableNotDefined identifier))
                )

        Operation symbol expr0 ->
            Stateful.session state
                |> Stateful.run (eval expr0)
                |> Stateful.andThen
                    (\arg0 ->
                        Stateful.run (\_ -> applyOperation symbol arg0)
                    )

        Operation2 symbol expr0 expr1 ->
            Stateful.session state
                |> Stateful.run (eval expr0)
                |> Stateful.andThen
                    (\arg0 ->
                        Stateful.run (eval expr1)
                            >> Stateful.andThen
                                (\arg1 ->
                                    Stateful.run (\_ -> applyOperation2 symbol arg0 arg1 trackStack)
                                )
                    )

        Application fn args ->
            evalList args state
                |> Stateful.andThen
                    (\evaluatedArgs ->
                        Stateful.run (eval fn)
                            >> Stateful.andThen
                                (\abstraction ->
                                    case abstraction of
                                        Abstraction paramNames functionBody ->
                                            Stateful.run (callFunction trackStack ( paramNames, functionBody ) evaluatedArgs)

                                        Undefined stacktrace ->
                                            Stateful.run (\_ -> return (Undefined stacktrace))

                                        _ ->
                                            Debug.todo "not implemented"
                                )
                    )

        Block blockExpressions ->
            let
                ( outScope, result ) =
                    runBlock state blockExpressions
            in
            Stateful outScope
                emptyState
                (result
                    |> Maybe.withDefault (Undefined (trackStack VoidReturn))
                )

        Return returnExpr ->
            eval returnExpr state

        IfCondition condition exprIfTrue ->
            Stateful.session state
                |> Stateful.run (eval condition)
                |> Stateful.andThen
                    (\conditionResult ->
                        if valueToBool conditionResult then
                            Stateful.run (eval exprIfTrue)

                        else
                            Stateful.run
                                (eval
                                    (Untracked (Value (Undefined (trackStack IfWithoutElse))))
                                )
                    )

        While condition exprWhile ->
            let
                whileLoop : Value -> ExpressionResult -> ExpressionResult
                whileLoop prevResult session =
                    session
                        |> Stateful.run (eval condition)
                        |> Stateful.andThen
                            (\conditionResult ->
                                if valueToBool conditionResult then
                                    Stateful.run (eval exprWhile)
                                        >> Stateful.andThen whileLoop

                                else
                                    Stateful.map (\_ -> prevResult)
                            )
            in
            Stateful.session state
                |> whileLoop (Undefined (trackStack LoopNeverTrue))


evalList : List Expression -> State -> Stateful (List Value)
evalList expressions state =
    let
        iterate_ : Expression -> Stateful (List Value) -> Stateful (List Value)
        iterate_ expr acc =
            acc
                |> Stateful.run (eval expr)
                |> Stateful.map (\result -> result :: acc.result)
    in
    expressions
        |> List.foldl iterate_ (Stateful state emptyState [])
        |> Stateful.map List.reverse


applyOperation : Operation -> Value -> ExpressionResult
applyOperation operation arg0 =
    case operation of
        Assignment name ->
            Stateful
                { variables = Dict.fromList [ ( name, arg0 ) ] }
                emptyState
                arg0

        LetAssignment name ->
            Stateful
                emptyState
                { variables = Dict.fromList [ ( name, arg0 ) ] }
                arg0


applyOperation2 : Operation2 -> Value -> Value -> (UndefinedReason -> List UndefinedTrackInfo) -> ExpressionResult
applyOperation2 reserved arg0 arg1 trackStack =
    let
        return result =
            { outScope = emptyState, inScope = emptyState, result = result }

        undefinedStack =
            case ( arg0, arg1 ) of
                ( Undefined stack, _ ) ->
                    stack

                ( _, Undefined stack ) ->
                    stack

                _ ->
                    []
    in
    case reserved of
        Addition ->
            let
                numberSum =
                    case ( valueToNumber arg0, valueToNumber arg1 ) of
                        ( Just a, Just b ) ->
                            Number (a + b)

                        _ ->
                            Undefined (undefinedStack ++ trackStack (OperationWithUndefined "addition"))

                stringConcat =
                    String (valueToString arg0 ++ valueToString arg1)
            in
            return
                (case ( arg0, arg1 ) of
                    ( String _, _ ) ->
                        stringConcat

                    ( _, String _ ) ->
                        stringConcat

                    ( Array _, _ ) ->
                        stringConcat

                    ( _, Array _ ) ->
                        stringConcat

                    ( Boolean _, _ ) ->
                        numberSum

                    ( _, Boolean _ ) ->
                        numberSum

                    ( Number _, _ ) ->
                        numberSum

                    ( _, Number _ ) ->
                        numberSum

                    _ ->
                        stringConcat
                )

        Subtraction ->
            return
                (case ( valueToNumber arg0, valueToNumber arg1 ) of
                    ( Just a, Just b ) ->
                        Number (a - b)

                    _ ->
                        Undefined (undefinedStack ++ trackStack (OperationWithUndefined "subtraction"))
                )

        SoftEquality ->
            let
                numberComparison =
                    case ( valueToNumber arg0, valueToNumber arg1 ) of
                        ( Just a, Just b ) ->
                            Boolean (a == b)

                        _ ->
                            Boolean False
            in
            return
                (case ( arg0, arg1 ) of
                    ( Boolean _, _ ) ->
                        numberComparison

                    ( _, Boolean _ ) ->
                        numberComparison

                    ( String a, b ) ->
                        Boolean (a == valueToString b)

                    ( a, String b ) ->
                        Boolean (valueToString a == b)

                    _ ->
                        numberComparison
                )

        GreaterThan ->
            return
                (case ( valueToNumber arg0, valueToNumber arg1 ) of
                    ( Just a, Just b ) ->
                        Boolean (a > b)

                    _ ->
                        Boolean False
                )

        SmallerThan ->
            return
                (case ( valueToNumber arg0, valueToNumber arg1 ) of
                    ( Just a, Just b ) ->
                        Boolean (a < b)

                    _ ->
                        Boolean False
                )


valueToBool : Value -> Bool
valueToBool value =
    case value of
        Boolean a ->
            a

        Number a ->
            if a == 0 then
                False

            else
                True

        Abstraction _ _ ->
            True

        Array _ ->
            True

        Undefined _ ->
            False

        String "" ->
            False

        String _ ->
            True


valueToNumber : Value -> Maybe Float
valueToNumber value =
    case value of
        Number a ->
            Just a

        Boolean a ->
            Just
                (if a then
                    1

                 else
                    0
                )

        Array _ ->
            valueToNumber (String (valueToString value))

        String "" ->
            Just 0

        String str ->
            String.toFloat str

        _ ->
            Nothing


valueToString : Value -> String
valueToString value =
    case value of
        Number a ->
            String.fromFloat a

        Boolean a ->
            if a then
                "true"

            else
                "false"

        Array item ->
            List.map valueToString item
                |> String.join ","

        String str ->
            str

        Undefined _ ->
            ""

        Abstraction _ _ ->
            -- TODO: modern browsers can do better than that
            "[Function]"



-- runSingleArity : State -> SingleArity -> Expression -> ExpressionResult
-- runSingleArity state func expr =
--     case func of
--         Assignment identifier ->
--             case identifier of
--                 ScalarIdentifier name ->
--                     case eval state expr of
--                         Expression (Number num) ->
--                             ( setVariable name num state, Expression (Number num) )
--                         Expression (Array _) ->
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
--                         Expression (Array v) ->
--                             ( setVector name v state, Expression (Array v) )
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


callFunction : (UndefinedReason -> List UndefinedTrackInfo) -> ( List String, Expression ) -> List Value -> State -> ExpressionResult
callFunction trackStack ( paramNames, functionBody ) args state =
    let
        argOrDefault : Int -> String -> Value
        argOrDefault index paramName =
            List.drop index args
                |> List.head
                -- TODO: track here
                |> Maybe.withDefault (Undefined (trackStack (MissingPositionalArgument index paramName)))

        closure =
            List.Extra.indexedFoldl
                (\index paramName ->
                    setVariable paramName (argOrDefault index paramName)
                )
                state
                paramNames
    in
    -- TODO: closure should be only inScope
    eval functionBody closure


removeTracking : Expression -> UntrackedExp
removeTracking expr =
    case expr of
        Tracked _ e ->
            e

        Untracked e ->
            e



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
--                             ( Expression (Array items_), Expression e ) ->
--                                 Expression (Array (items_ ++ [ e ]))
--                             ( Expression (Array items_), error ) ->
--                                 error
--                             ( acc_, _ ) ->
--                                 acc_
--                     )
--                     (Expression (Array []))
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
--                                 (DoubleArity Index (Array items))
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


setVariable : String -> Value -> State -> State
setVariable name value state =
    { state | variables = Dict.insert name value state.variables }
