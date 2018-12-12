module Interpreter exposing (run)

import Dict exposing (Dict)
import Parser exposing (DeadEnd, Problem(..))
import Return exposing (throwError)
import Types exposing (..)


type alias State =
    { variables : Dict String Float
    , functions : Dict String ( String, Expression )
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

        Vector items ->
            ( state, Return.Vector <| List.map (eval state) items )

        Variable name ->
            ( state
            , Dict.get name state.variables
                |> Maybe.map Return.Num
                |> Maybe.withDefault (Return.Expression (Variable name))
            )

        Abstraction param body ->
            ( state
            , Return.Expression (Abstraction param (Return.reencode <| eval state body))
            )

        SingleArityApplication func e ->
            runSingleArity state func e

        DoubleArityApplication func e1 e2 ->
            ( state, runDoubleArity state func e1 e2 )

        TripleArityApplication func e1 e2 e3 ->
            ( state, runTripleArity state func e1 e2 e3 )


runSingleArity : State -> SingleArity -> Expression -> LineResult
runSingleArity state func expr =
    case func of
        Assignment name ->
            case eval state expr of
                Return.Num num ->
                    ( setVariable name num state, Return.Void )

                Return.Vector _ ->
                    Debug.todo "not implemented yet"

                Return.Void ->
                    ( state, throwError ("Cannot set variable" ++ name ++ "to void") )

                Return.Error error ->
                    ( state, Return.Error error )

                Return.Expression (Abstraction params body) ->
                    ( setFunction name params body state, Return.Void )

                Return.Expression e ->
                    ( state, Return.Expression (SingleArityApplication (Assignment name) e) )

        NamedFunction name ->
            let
                substituteParams ( param, body ) =
                    substitute param expr body
            in
            ( state
            , Dict.get name state.functions
                |> Maybe.map substituteParams
                |> Maybe.map (eval state)
                |> Maybe.withDefault
                    (eval state expr
                        |> Return.andThenNum (Return.Expression << Number)
                        |> Return.orElse (SingleArityApplication func)
                    )
            )

        Sqrt ->
            ( state
            , eval state expr
                |> Return.mapNum sqrt
                |> Return.orElse (SingleArityApplication func)
            )


substitute : String -> Expression -> Expression -> Expression
substitute param value expr =
    case expr of
        Number val ->
            Number val

        Vector _ ->
            Debug.todo "not implemented"

        Variable name ->
            if name == param then
                value

            else
                expr

        Abstraction param_ body ->
            Abstraction param_ (substitute param value body)

        SingleArityApplication func e ->
            SingleArityApplication func (substitute param value e)

        DoubleArityApplication func e1 e2 ->
            DoubleArityApplication func (substitute param value e1) (substitute param value e2)

        TripleArityApplication func e1 e2 e3 ->
            TripleArityApplication func (substitute param value e1) (substitute param value e2) (substitute param value e3)


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
        |> Return.mapNum2 (DoubleArityApplication func) operator (eval state e1)


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
                            |> List.foldl iterate (Return.Num 0)

                iterate curr total =
                    substitute identifier (Number <| toFloat curr) expr3
                        |> eval state
                        |> Return.mapNum2 (\_ e -> e) (\result total_ -> total_ + result) total
                        |> Return.orElse (TripleArityApplication func expr1 expr2)
            in
            Return.andThenNum2 (\e1 e2 -> TripleArityApplication func e1 e2 expr3)
                forLoop
                (eval state expr1)
                (eval state expr2)


eval : State -> Expression -> Return.Value
eval state =
    runExpression state >> Tuple.second


setVariable : String -> Float -> State -> State
setVariable name value state =
    { state | variables = Dict.insert name value state.variables }


setFunction : String -> String -> Expression -> State -> State
setFunction name param body state =
    { state | functions = Dict.insert name ( param, body ) state.functions }
