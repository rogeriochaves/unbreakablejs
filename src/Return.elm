module Return exposing (Value(..), andThen, andThen2, map, map2, throwError)

import Parser exposing (DeadEnd, Problem(..))
import Types exposing (..)


type Value
    = Num Float
    | Void
    | Expression Types.Expression
    | Error DeadEnd


throwError : String -> Value
throwError error =
    Error { row = 0, col = 0, problem = Problem error }


map : (Types.Expression -> Types.Expression) -> (Float -> Float) -> Value -> Value
map builder fn =
    andThen builder (fn >> Num)


map2 : (Types.Expression -> Types.Expression -> Types.Expression) -> (Float -> Float -> Float) -> Value -> Value -> Value
map2 builder fn =
    andThen2 builder (\a -> fn a >> Num)


andThen : (Types.Expression -> Types.Expression) -> (Float -> Value) -> Value -> Value
andThen builder fn val =
    case val of
        Num float ->
            fn float

        Void ->
            throwError "Cannot apply function to void"

        Error _ ->
            val

        Expression e ->
            Expression (builder e)


andThen2 : (Types.Expression -> Types.Expression -> Types.Expression) -> (Float -> Float -> Value) -> Value -> Value -> Value
andThen2 builder fn val val2 =
    case ( val, val2 ) of
        ( Num float1, Num float2 ) ->
            fn float1 float2

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

        -- TODO
        Void ->
            Number 0

        -- TODO
        Error _ ->
            Number 0

        Expression e ->
            e
