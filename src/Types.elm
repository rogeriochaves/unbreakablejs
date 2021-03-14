module Types exposing (..)

import Parser exposing (..)


type alias Program =
    List Expression


type alias Error =
    List DeadEnd


type Expression
    = Tracked { line : Int, column : Int } UntrackedExp
    | Untracked UntrackedExp


{-| λ-calculus
-}
type UntrackedExp
    = Value Value
    | Variable String
    | Application Expression (List Expression)
    | ReservedApplication Reserved (List Expression)
    | Block String (List Expression)
    | Error DeadEnd


type Value
    = Number Float
    | Vector (List Expression)
    | Abstraction (List String) Expression
    | Undefined


type Reserved
    = Addition
    | Subtraction
    | Assignment String



-- | Abstraction Identifier Expression
-- | MapAbstraction String String Expression
-- | SingleArity SingleArity Expression
-- | DoubleArity DoubleArity Expression Expression
-- | TripleArity TripleArity Expression Expression Expression
-- | Block String (List Expression)
-- type SingleArity
--     = Application Expression
--     | Assignment Identifier
--     | Sqrt
--     | Factorial
--     | Negation
--     | Summation
--     | Cardinality
-- type DoubleArity
--     = Addition
--     | Subtraction
--     | Multiplication
--     | Division
--     | Exponentiation
--     | Frac
--     | Index
--     | Modulo
--     | EuclideanDivision
-- type TripleArity
--     = Sum_ String
-- type Identifier
--     = ScalarIdentifier String
--     | VectorIdentifier String
-- singleAritySymbolsMap : Dict.Dict String SingleArity
-- singleAritySymbolsMap =
--     Dict.fromList
--         [ ( "sqrt", Sqrt )
--         , ( "sum", Summation )
--         ]
-- doubleAritySymbolsMap : Dict.Dict String DoubleArity
-- doubleAritySymbolsMap =
--     Dict.fromList [ ( "frac", Frac ) ]
-- tripleAritySymbolsMap : Dict.Dict String String
-- tripleAritySymbolsMap =
--     Dict.fromList [ ( "sum_", "sum_" ) ]
