module Return exposing (andThen, andThenArgs2, argOrDefault, map, mapNumArgs2, throwError)

import Parser exposing (DeadEnd, Problem(..))
import Types exposing (..)


throwError : String -> Expression
throwError error =
    Error { row = 0, col = 0, problem = Problem error }


map : (Types.Expression -> Types.Expression) -> Expression -> Expression
map fn =
    andThen fn


andThen : (Types.Expression -> Expression) -> Expression -> Expression
andThen fn val =
    fn val


andThen2 : (Types.Expression -> Types.Expression -> Expression) -> Expression -> Expression -> Expression
andThen2 fn val val2 =
    fn val val2



-- mapNum : (Types.Expression -> Types.Expression) -> (Float -> Float) -> Expression -> Expression
-- mapNum builder fn =
--     andThenNum builder (fn >> Types.Number >> Expression)
-- andThenNum : (Types.Expression -> Types.Expression) -> (Float -> Expression) -> Expression -> Expression
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
-- andThenVector : (Types.Expression -> Types.Expression) -> (List Expression -> Expression) -> Expression -> Expression
-- andThenVector builder fn =
--     andThen
--         (\expr ->
--             case expr of
--                 Types.Number float ->
--                     throwError "Vector expected"
--                 Types.Vector items ->
--                     fn items
--                 other ->
--                     Expression (builder other)
--         )
-- mapNum2 : (Types.Expression -> Types.Expression -> Types.Expression) -> (Float -> Float -> Float) -> Expression -> Expression -> Expression
-- mapNum2 builder fn =
--     andThenNum2 builder (\a -> fn a >> Types.Number >> Expression)
-- andThenNum2 : (Types.Expression -> Types.Expression -> Types.Expression) -> (Float -> Float -> Expression) -> Expression -> Expression -> Expression
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


mapNumArgs2 : (Float -> Float -> Float) -> List Expression -> Expression
mapNumArgs2 fn =
    andThenNumArgs2 (\a -> fn a >> Types.Number)


andThenNumArgs2 : (Float -> Float -> Expression) -> List Expression -> Expression
andThenNumArgs2 fn =
    andThenArgs2
        (\arg0 arg1 ->
            case ( arg0, arg1 ) of
                ( Number float1, Number float2 ) ->
                    fn float1 float2

                ( Number _, e ) ->
                    e

                ( e, _ ) ->
                    e
        )


andThenArgs2 : (Expression -> Expression -> Expression) -> List Expression -> Expression
andThenArgs2 fn args =
    fn (argOrDefault 0 args) (argOrDefault 1 args)


argOrDefault : Int -> List Expression -> Expression
argOrDefault index args =
    List.drop index args
        |> List.head
        |> Maybe.withDefault Void
