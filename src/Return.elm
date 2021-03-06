module Return exposing (Value(..), andThen, andThenArgs2, andThenVector, map, mapNumArgs2, throwError)

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



-- mapNum : (Types.Expression -> Types.Expression) -> (Float -> Float) -> Value -> Value
-- mapNum builder fn =
--     andThenNum builder (fn >> Types.Number >> Expression)
-- andThenNum : (Types.Expression -> Types.Expression) -> (Float -> Value) -> Value -> Value
-- andThenNum builder fn =
--     andThen
--         (\expr ->
--             case expr of
--                 Types.Number float ->
--                     fn float
--                 Types.Vector _ ->
--                     throwError "Cannot apply function to vector"
--                 other ->
--                     Expression (builder other)
--         )


andThenVector : (Types.Expression -> Types.Expression) -> (List Expression -> Value) -> Value -> Value
andThenVector builder fn =
    andThen
        (\expr ->
            case expr of
                Types.Number float ->
                    throwError "Vector expected"

                Types.Vector items ->
                    fn items

                other ->
                    Expression (builder other)
        )



-- mapNum2 : (Types.Expression -> Types.Expression -> Types.Expression) -> (Float -> Float -> Float) -> Value -> Value -> Value
-- mapNum2 builder fn =
--     andThenNum2 builder (\a -> fn a >> Types.Number >> Expression)
-- andThenNum2 : (Types.Expression -> Types.Expression -> Types.Expression) -> (Float -> Float -> Value) -> Value -> Value -> Value
-- andThenNum2 builder fn =
--     andThen2
--         (\expr1 expr2 ->
--             case ( expr1, expr2 ) of
--                 ( Types.Number float1, Types.Number float2 ) ->
--                     fn float1 float2
--                 ( Types.Number float1, e ) ->
--                     Expression (builder (Types.Number float1) e)
--                 ( e, Types.Number float2 ) ->
--                     Expression (builder e (Types.Number float2))
--                 ( e1, e2 ) ->
--                     Expression (builder e1 e2)
--         )


mapNumArgs2 : (Float -> Float -> Float) -> List Value -> Value
mapNumArgs2 fn =
    andThenNumArgs2 (\a -> fn a >> Types.Number >> Expression)


andThenNumArgs2 : (Float -> Float -> Value) -> List Value -> Value
andThenNumArgs2 fn =
    andThenArgs2
        (\arg1 arg2 ->
            case ( arg1, arg2 ) of
                ( Expression (Number float1), Expression (Number float2) ) ->
                    fn float1 float2

                ( Expression (Number _), e ) ->
                    e

                ( e, _ ) ->
                    e
        )


andThenArgs2 : (Value -> Value -> Value) -> List Value -> Value
andThenArgs2 fn args =
    fn (argOrDefault 0 args) (argOrDefault 1 args)


argOrDefault : Int -> List Value -> Value
argOrDefault index args =
    List.drop (index - 1) args
        |> List.head
        |> Maybe.withDefault Void
