module Return exposing (Value(..), andThen, andThenNum, andThenNum2, map, mapNum, mapNum2, orElse, throwError)

import Parser exposing (DeadEnd, Problem(..))
import Types exposing (..)


type Value
    = Expression Types.Expression
    | Error DeadEnd
    | Void


throwError : String -> Value
throwError error =
    Error { row = 0, col = 0, problem = Problem error }


map : (Types.Expression -> Types.Expression) -> Value -> Value
map fn =
    andThen (fn >> Expression)


andThen : (Types.Expression -> Value) -> Value -> Value
andThen fn val =
    case val of
        Expression e ->
            fn e

        Error e ->
            Error e

        Void ->
            throwError "Void return inside another expression"


mapNum : (Float -> Float) -> Value -> Value
mapNum fn =
    andThenNum (fn >> Types.Number >> Expression)


andThenNum : (Float -> Value) -> Value -> Value
andThenNum fn =
    andThen
        (\expr ->
            case expr of
                Types.Number float ->
                    fn float

                Types.Vector _ ->
                    throwError "Cannot apply function to vector"

                other ->
                    Expression other
        )


mapNum2 : (Types.Expression -> Types.Expression -> Types.Expression) -> (Float -> Float -> Float) -> Value -> Value -> Value
mapNum2 builder fn =
    andThenNum2 builder (\a -> fn a >> Types.Number >> Expression)


andThenNum2 : (Types.Expression -> Types.Expression -> Types.Expression) -> (Float -> Float -> Value) -> Value -> Value -> Value
andThenNum2 builder fn val val2 =
    case ( val, val2 ) of
        ( Expression (Types.Number float1), Expression (Types.Number float2) ) ->
            fn float1 float2

        ( Expression (Types.Number float1), Expression e ) ->
            Expression (builder (Types.Number float1) e)

        ( Expression e, Expression (Types.Number float1) ) ->
            Expression (builder e (Types.Number float1))

        ( Expression (Types.Vector _), _ ) ->
            throwError "Cannot apply function to vector"

        ( _, Expression (Types.Vector _) ) ->
            throwError "Cannot apply function to vector"

        ( Error _, _ ) ->
            val

        ( _, Error _ ) ->
            val2

        ( Void, _ ) ->
            throwError "Cannot apply function to void"

        ( _, Void ) ->
            throwError "Cannot apply function to void"

        ( Expression e, Expression e2 ) ->
            Expression (builder e e2)


orElse : (Types.Expression -> Types.Expression) -> Value -> Value
orElse builder val =
    case val of
        Expression (Types.Number float) ->
            Expression (Types.Number float)

        -- TODO vector
        Expression e ->
            Expression (builder e)

        Error e ->
            Error e

        Void ->
            Void
