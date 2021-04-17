module Interpreter exposing (run)

import Dict
import Fuzz exposing (result)
import List.Extra
import Parser exposing (Problem(..))
import Stateful
import Test.Runner.Failure exposing (Reason(..))
import Types exposing (..)


type alias TrackStack =
    UndefinedReason -> List UndefinedTrackInfo


run : State -> Types.Program -> ( State, List ExpressionResult )
run state expressions =
    let
        iterate_ : Expression -> Stateful (List ExpressionResult) -> Stateful (List ExpressionResult)
        iterate_ expr acc =
            let
                statefulResult =
                    Stateful.run (eval expr) acc

                outScope =
                    case expr |> removeTracking of
                        Operation (LetAssignment _) _ ->
                            Stateful.mergeStates statefulResult.inScope statefulResult.outScope

                        _ ->
                            statefulResult.outScope
            in
            { statefulResult | outScope = outScope, inScope = emptyState }
                |> Stateful.map (\_ -> statefulResult :: acc.result)
    in
    expressions
        |> List.foldl iterate_ (Stateful state emptyState [])
        |> (\{ outScope, inScope, result } ->
                ( Stateful.mergeStates inScope outScope
                , List.reverse result
                )
           )


runBlock : State -> List Expression -> TrackStack -> Stateful Value
runBlock state blockExpressions trackStack =
    let
        iterate_ : Expression -> Stateful Value -> Stateful Value
        iterate_ expr acc =
            case acc.result of
                ReturnValue _ ->
                    acc

                _ ->
                    acc
                        |> Stateful.run (eval expr)
    in
    blockExpressions
        |> List.foldl iterate_ (Stateful state emptyState (Undefined (trackStack VoidReturn)))


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
        Value val ->
            return val

        ArrayExpression items ->
            evalList items state
                |> Stateful.map Array

        ObjectExpression dict ->
            evalList (Dict.values dict) state
                |> Stateful.map
                    (\result ->
                        Object (List.map2 Tuple.pair (Dict.keys dict) result |> Dict.fromList)
                    )

        Variable identifier ->
            getVariable identifier state
                |> Maybe.map
                    (\( state_, result ) ->
                        { outScope = emptyState, inScope = state_, result = result }
                    )
                |> Maybe.withDefault
                    (return (Undefined (trackStack <| VariableNotDefined identifier)))

        Operation symbol expr0 ->
            Stateful.session state
                |> Stateful.run (eval expr0)
                |> Stateful.andThen
                    (\arg0 ->
                        Stateful.runInScope (\inScope -> applyOperation inScope symbol arg0 trackStack)
                    )

        Operation2 symbol expr0 expr1 ->
            Stateful.session state
                |> Stateful.run (eval expr0)
                |> Stateful.andThen
                    (\arg0 ->
                        Stateful.run (eval expr1)
                            >> Stateful.andThen
                                (\arg1 ->
                                    Stateful.run (\_ -> return (applyOperation2 symbol arg0 arg1 trackStack))
                                )
                    )

        Application fn args ->
            let
                call evaluatedArgs abstraction =
                    case abstraction of
                        Abstraction paramNames functionBody ->
                            Stateful.run (callFunction trackStack ( paramNames, functionBody ) evaluatedArgs)

                        _ ->
                            Stateful.run
                                (\_ ->
                                    return (buildUndefined abstraction trackStack (NotAFunction abstraction))
                                )
            in
            evalList args state
                |> Stateful.andThen
                    (\evaluatedArgs ->
                        Stateful.run (eval fn)
                            >> Stateful.andThen (call evaluatedArgs)
                    )

        Block blockExpressions ->
            runBlock state blockExpressions trackStack

        Return returnExpr ->
            eval returnExpr state
                |> Stateful.map ReturnValue

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

        IfElseCondition condition exprIfTrue exprIfFalse ->
            Stateful.session state
                |> Stateful.run (eval condition)
                |> Stateful.andThen
                    (\conditionResult ->
                        if valueToBool conditionResult then
                            Stateful.run (eval exprIfTrue)

                        else
                            Stateful.run (eval exprIfFalse)
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

        ForLoop assignment condition increment exprFor ->
            let
                forLoop : Value -> ExpressionResult -> ExpressionResult
                forLoop prevResult session =
                    session
                        |> Stateful.run (eval condition)
                        |> Stateful.andThen
                            (\conditionResult ->
                                if valueToBool conditionResult then
                                    Stateful.run (eval exprFor)
                                        >> Stateful.run (eval increment)
                                        >> Stateful.andThen forLoop

                                else
                                    Stateful.map (\_ -> prevResult)
                            )
            in
            Stateful.session state
                |> Stateful.run (eval assignment)
                |> forLoop (Undefined (trackStack LoopNeverTrue))


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


applyOperation : State -> Operation -> Value -> TrackStack -> ExpressionResult
applyOperation inScope operation arg0 trackStack =
    let
        returnValue =
            Stateful emptyState emptyState

        buildUndefined_ =
            buildUndefined arg0 trackStack
    in
    case operation of
        Assignment name ->
            let
                assignValue =
                    case arg0 of
                        Undefined _ ->
                            buildUndefined_ (AssignmentToUndefined name)

                        _ ->
                            arg0
            in
            Stateful
                (emptyState |> setVariable name ( inScope, assignValue ))
                emptyState
                assignValue

        LetAssignment name ->
            let
                assignValue =
                    case arg0 of
                        Undefined _ ->
                            buildUndefined_ (AssignmentToUndefined name)

                        _ ->
                            arg0
            in
            Stateful
                emptyState
                (emptyState |> setVariable name ( inScope, assignValue ))
                assignValue

        Increment name ->
            let
                newValue =
                    applyOperation2 Addition arg0 (Number 1) trackStack
            in
            Stateful
                (emptyState |> setVariable name ( inScope, newValue ))
                emptyState
                arg0

        Decrement name ->
            let
                newValue =
                    applyOperation2 Subtraction arg0 (Number 1) trackStack
            in
            Stateful
                (emptyState |> setVariable name ( inScope, newValue ))
                emptyState
                arg0

        Not ->
            returnValue (Boolean (not (valueToBool arg0)))

        Negative ->
            returnValue
                (valueToNumber arg0
                    |> Maybe.map ((*) -1)
                    |> Maybe.map Number
                    |> Maybe.withDefault
                        (buildUndefined_ (OperationWithUndefined "negative"))
                )


applyOperation2 : Operation2 -> Value -> Value -> TrackStack -> Value
applyOperation2 reserved arg0 arg1 trackStack =
    let
        buildUndefined_ =
            buildUndefined2 arg0 arg1 trackStack

        numberComparison comparator =
            case ( valueToNumber arg0, valueToNumber arg1 ) of
                ( Just a, Just b ) ->
                    comparator a b

                _ ->
                    False

        softEquality =
            case ( arg0, arg1 ) of
                ( Undefined _, Undefined _ ) ->
                    True

                ( Boolean _, _ ) ->
                    numberComparison (==)

                ( _, Boolean _ ) ->
                    numberComparison (==)

                ( String a, b ) ->
                    a == valueToString b

                ( a, String b ) ->
                    valueToString a == b

                _ ->
                    numberComparison (==)
    in
    case reserved of
        Addition ->
            let
                numberSum =
                    case ( valueToNumber arg0, valueToNumber arg1 ) of
                        ( Just a, Just b ) ->
                            Number (a + b)

                        _ ->
                            buildUndefined_ (OperationWithUndefined "addition")

                stringConcat =
                    String (valueToString arg0 ++ valueToString arg1)
            in
            case ( arg0, arg1 ) of
                ( String _, _ ) ->
                    stringConcat

                ( _, String _ ) ->
                    stringConcat

                ( Array _, _ ) ->
                    stringConcat

                ( _, Array _ ) ->
                    stringConcat

                ( Object _, _ ) ->
                    stringConcat

                ( _, Object _ ) ->
                    stringConcat

                ( Boolean _, _ ) ->
                    numberSum

                ( _, Boolean _ ) ->
                    numberSum

                ( Number _, _ ) ->
                    numberSum

                ( _, Number _ ) ->
                    numberSum

                ( Undefined _, Undefined _ ) ->
                    numberSum

                _ ->
                    stringConcat

        Subtraction ->
            case ( valueToNumber arg0, valueToNumber arg1 ) of
                ( Just a, Just b ) ->
                    Number (a - b)

                _ ->
                    buildUndefined_ (OperationWithUndefined "subtraction")

        Multiplication ->
            case ( valueToNumber arg0, valueToNumber arg1 ) of
                ( Just a, Just b ) ->
                    Number (a * b)

                _ ->
                    buildUndefined_ (OperationWithUndefined "multiplication")

        Division ->
            case ( valueToNumber arg0, valueToNumber arg1 ) of
                ( Just a, Just b ) ->
                    Number (a / b)

                _ ->
                    buildUndefined_ (OperationWithUndefined "division")

        Exponentiation ->
            case ( valueToNumber arg0, valueToNumber arg1 ) of
                ( Just a, Just b ) ->
                    Number (a ^ b)

                _ ->
                    buildUndefined_ (OperationWithUndefined "exponentiation")

        Remainder ->
            case ( valueToNumber arg0, valueToNumber arg1 ) of
                ( Just a, Just b ) ->
                    round (a * 10000)
                        |> remainderBy (round (b * 10000))
                        |> toFloat
                        |> (\val -> val / 10000)
                        |> Number

                _ ->
                    buildUndefined_ (OperationWithUndefined "remainder")

        SoftEquality ->
            Boolean softEquality

        HardEquality ->
            case ( arg0, arg1 ) of
                ( Undefined _, Undefined _ ) ->
                    Boolean True

                _ ->
                    Boolean (arg0 == arg1)

        SoftNotEquality ->
            Boolean (not softEquality)

        HardNotEquality ->
            case ( arg0, arg1 ) of
                ( Undefined _, Undefined _ ) ->
                    Boolean False

                _ ->
                    Boolean (arg0 /= arg1)

        GreaterThan ->
            Boolean (numberComparison (>))

        SmallerThan ->
            Boolean (numberComparison (<))

        GreaterOrEqualThan ->
            Boolean (numberComparison (>) || softEquality)

        SmallerOrEqualThan ->
            Boolean (numberComparison (<) || softEquality)

        Member ->
            let
                returnUndefined =
                    buildUndefined_ (KeyNotInObject arg0 arg1)
            in
            case ( arg0, arg1 ) of
                ( Array arr, Number index ) ->
                    if toFloat (truncate index) == index then
                        List.drop (truncate index) arr
                            |> List.head
                            |> Maybe.withDefault returnUndefined

                    else
                        returnUndefined

                ( Object dict, key ) ->
                    Dict.get (valueToString key) dict
                        |> Maybe.withDefault returnUndefined

                ( String str, Number index ) ->
                    if toFloat (truncate index) == index then
                        let
                            char =
                                String.dropLeft (truncate index) str
                                    |> String.left 1
                        in
                        if char /= "" then
                            String char

                        else
                            returnUndefined

                    else
                        returnUndefined

                _ ->
                    returnUndefined

        And ->
            Boolean (valueToBool arg0 && valueToBool arg1)

        Or ->
            Boolean (valueToBool arg0 || valueToBool arg1)


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

        Object _ ->
            True

        Undefined _ ->
            False

        String "" ->
            False

        String _ ->
            True

        ReturnValue val ->
            valueToBool val


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

        Object _ ->
            "[object Object]"

        String str ->
            str

        Undefined _ ->
            "undefined"

        Abstraction _ _ ->
            -- TODO: modern browsers can do better than that
            "[Function]"

        ReturnValue val ->
            valueToString val


buildUndefined : Value -> TrackStack -> UndefinedReason -> Value
buildUndefined value trackStack reason =
    case value of
        Undefined undefinedStack ->
            Undefined (undefinedStack ++ trackStack reason)

        _ ->
            Undefined (trackStack reason)


buildUndefined2 : Value -> Value -> TrackStack -> UndefinedReason -> Value
buildUndefined2 value value2 trackStack reason =
    case ( value, value2 ) of
        ( Undefined undefinedStack, _ ) ->
            Undefined (undefinedStack ++ trackStack reason)

        ( _, Undefined undefinedStack ) ->
            Undefined (undefinedStack ++ trackStack reason)

        _ ->
            Undefined (trackStack reason)


callFunction : TrackStack -> ( List String, Expression ) -> List Value -> State -> ExpressionResult
callFunction trackStack ( paramNames, functionBody ) args state =
    let
        argOrDefault : Int -> String -> Value
        argOrDefault index paramName =
            List.drop index args
                |> List.head
                -- TODO: track here
                |> Maybe.withDefault (Undefined (trackStack (MissingPositionalArgument index paramName)))

        inState =
            List.Extra.indexedFoldl
                (\index paramName ->
                    setVariable paramName ( state, argOrDefault index paramName )
                )
                emptyState
                paramNames

        closure =
            Stateful.mergeStates inState state

        expressionResult =
            eval functionBody closure
                |> Stateful.moveStateOutsideScope ( state, inState )
                |> Stateful.map
                    (\result ->
                        case ( result, removeTracking functionBody ) of
                            ( ReturnValue val, _ ) ->
                                val

                            ( _, Block _ ) ->
                                Undefined (trackStack VoidReturn)

                            _ ->
                                result
                    )
    in
    expressionResult


removeTracking : Expression -> UntrackedExp
removeTracking expr =
    case expr of
        Tracked _ e ->
            e

        Untracked e ->
            e


setVariable : String -> ( State, Value ) -> State -> State
setVariable name value (State state) =
    State (Dict.insert name value state)


getVariable : String -> State -> Maybe ( State, Value )
getVariable name (State state) =
    Dict.get name state
