module Return exposing (Value(..), andThenNum, andThenNum2, mapNum, mapNum2, orElse, reencode, throwError)

import Parser exposing (DeadEnd, Problem(..))
import Types exposing (..)


type Value
    = Num Float
    | Vector (List Value)
    | Void
    | Expression Types.Expression
    | Error DeadEnd


throwError : String -> Value
throwError error =
    Error { row = 0, col = 0, problem = Problem error }


mapNum2 : (Types.Expression -> Types.Expression -> Types.Expression) -> (Float -> Float -> Float) -> Value -> Value -> Value
mapNum2 builder fn =
    andThenNum2 builder (\a -> fn a >> Num)


mapNum : (Float -> Float) -> Value -> Value
mapNum fn =
    andThenNum (fn >> Num)


andThenNum : (Float -> Value) -> Value -> Value
andThenNum fn val =
    case val of
        Num float ->
            fn float

        Vector _ ->
            throwError "Cannot apply function to vector"

        Void ->
            throwError "Cannot apply function to void"

        Error _ ->
            val

        Expression e ->
            Expression e


orElse : (Types.Expression -> Types.Expression) -> Value -> Value
orElse builder val =
    case val of
        Expression e ->
            Expression (builder e)

        _ ->
            val


andThenNum2 : (Types.Expression -> Types.Expression -> Types.Expression) -> (Float -> Float -> Value) -> Value -> Value -> Value
andThenNum2 builder fn val val2 =
    case ( val, val2 ) of
        ( Num float1, Num float2 ) ->
            fn float1 float2

        ( Vector _, _ ) ->
            throwError "Cannot apply function to vector"

        ( _, Vector _ ) ->
            throwError "Cannot apply function to vector"

        ( Error _, _ ) ->
            val

        ( _, Error _ ) ->
            val2

        ( Void, _ ) ->
            throwError "Cannot apply function to void"

        ( _, Void ) ->
            throwError "Cannot apply function to void"

        ( Expression e, result ) ->
            Expression (builder e (reencode result))

        ( result, Expression e ) ->
            Expression (builder (reencode result) e)


reencode : Value -> Types.Expression
reencode val =
    case val of
        Num float ->
            Number float

        Vector items ->
            Types.Vector (List.map reencode items)

        -- TODO
        Void ->
            Number 0

        -- TODO
        Error _ ->
            Number 0

        Expression e ->
            e
