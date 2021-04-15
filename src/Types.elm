module Types exposing (..)

import Dict exposing (Dict)
import Parser exposing (..)


type alias Program =
    List Expression


type alias Error =
    List DeadEnd


type alias TrackInfo =
    { line : Int, column : Int, filename : String }


type alias UndefinedTrackInfo =
    { line : Int, column : Int, filename : String, reason : UndefinedReason }


type Expression
    = Tracked TrackInfo UntrackedExp
    | Untracked UntrackedExp


{-| λ-calculus
-}
type UntrackedExp
    = Value Value
    | Variable String
    | Application Expression (List Expression)
    | Operation Operation Expression
    | Operation2 Operation2 Expression Expression
    | Block (List Expression)
    | Return Expression
    | IfCondition Expression Expression
    | IfElseCondition Expression Expression Expression
    | While Expression Expression
    | ForLoop Expression Expression Expression Expression
    | ArrayExpression (List Expression)


type Value
    = Number Float
    | Array (List Value)
    | Abstraction (List String) Expression
    | Undefined (List UndefinedTrackInfo)
    | Boolean Bool
    | String String
    | ReturnValue Value


type Operation
    = Assignment String
    | LetAssignment String
    | Increment String
    | Decrement String


type Operation2
    = Addition
    | Subtraction
    | SoftEquality
    | GreaterThan
    | SmallerThan
    | Member


type UndefinedReason
    = VariableNotDefined String
    | OperationWithUndefined String
    | MissingPositionalArgument Int String
    | VoidReturn
    | IfWithoutElse
    | ExplicitUndefined
    | LoopNeverTrue
    | KeyNotInObject Value Value


type State
    = State (Dict String ( State, Value ))


emptyState : State
emptyState =
    State Dict.empty


type alias ExpressionResult =
    Stateful Value


type alias Stateful a =
    { outScope : State, inScope : State, result : a }



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
