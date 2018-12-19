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


andThen2 : (Types.Expression -> Types.Expression -> Value) -> Value -> Value -> Value
andThen2 fn val val2 =
    case ( val, val2 ) of
        ( Expression e, Expression e2 ) ->
            fn e e2

        ( Error _, _ ) ->
            val

        ( _, Error _ ) ->
            val2

        ( Void, _ ) ->
            throwError "Cannot apply function to void"

        ( _, Void ) ->
            throwError "Cannot apply void to function"


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
andThenNum2 builder fn =
    andThen2
        (\expr1 expr2 ->
            case ( expr1, expr2 ) of
                ( Types.Number float1, Types.Number float2 ) ->
                    fn float1 float2

                ( Types.Number float1, e ) ->
                    Expression (builder (Types.Number float1) e)

                ( e, Types.Number float2 ) ->
                    Expression (builder e (Types.Number float2))

                ( e1, e2 ) ->
                    Expression (builder e1 e2)
        )


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
