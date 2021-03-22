module Return exposing (andThen, andThenArgs2, argOrDefault, map, mapNumArgs2)

import Html exposing (track)
import Parser exposing (DeadEnd, Problem(..))
import Types exposing (..)


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


mapNumArgs2 : List UndefinedTrackInfo -> (Float -> Float -> a) -> (a -> Value) -> List Expression -> Expression
mapNumArgs2 trackStack fn wrapper =
    -- TODO: keep tracking of original function
    andThenNumArgs2 trackStack (\a -> fn a >> wrapper >> Value >> Untracked)


andThenNumArgs2 : List UndefinedTrackInfo -> (Float -> Float -> Expression) -> List Expression -> Expression
andThenNumArgs2 trackStack fn =
    andThenArgs2 trackStack
        (\arg0 arg1 ->
            case ( removeTracking arg0, removeTracking arg1 ) of
                ( Value (Number float1), Value (Number float2) ) ->
                    fn float1 float2

                ( Value (Undefined stack), _ ) ->
                    Untracked (Value (Undefined (stack ++ trackStack)))

                ( _, Value (Undefined stack) ) ->
                    Untracked (Value (Undefined (stack ++ trackStack)))

                _ ->
                    -- TODO: what about sum with boolean? Or with object? They should show a better error message
                    Untracked (Value (Undefined trackStack))
        )


removeTracking : Expression -> UntrackedExp
removeTracking expr =
    case expr of
        Tracked _ e ->
            e

        Untracked e ->
            e


andThenArgs2 : List UndefinedTrackInfo -> (Expression -> Expression -> Expression) -> List Expression -> Expression
andThenArgs2 trackStack fn args =
    fn (argOrDefault trackStack 0 args) (argOrDefault trackStack 1 args)


argOrDefault : List UndefinedTrackInfo -> Int -> List Expression -> Expression
argOrDefault trackStack index args =
    List.drop index args
        |> List.head
        -- TODO: track here
        |> Maybe.withDefault (Untracked <| Value (Undefined trackStack))
