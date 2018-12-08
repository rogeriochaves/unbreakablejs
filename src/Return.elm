module Return exposing (Value(..), andThen, andThen2, map, map2, throwError)

import Parser exposing (DeadEnd, Problem(..))


type Value
    = Num Float
    | Void
    | Error DeadEnd


throwError : String -> Value
throwError error =
    Error { row = 0, col = 0, problem = Problem error }


map : (Float -> Float) -> Value -> Value
map fn =
    andThen (fn >> Num)


map2 : (Float -> Float -> Float) -> Value -> Value -> Value
map2 fn =
    andThen2 (\a -> fn a >> Num)


andThen : (Float -> Value) -> Value -> Value
andThen fn val =
    case val of
        Num float ->
            fn float

        Void ->
            throwError "Cannot apply function to void"

        Error _ ->
            val


andThen2 : (Float -> Float -> Value) -> Value -> Value -> Value
andThen2 fn val val2 =
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
