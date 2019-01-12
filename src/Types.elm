module Types exposing (DoubleArity(..), Error, Expression(..), Identifier(..), Program, SingleArity(..), TripleArity(..), doubleAritySymbolsMap, singleAritySymbolsMap, tripleAritySymbolsMap)

import Dict
import Parser exposing (..)


type alias Program =
    List Expression


type alias Error =
    List DeadEnd


{-| λ-calculus
-}
type Expression
    = Number Float
    | Vector (List Expression)
    | Variable Identifier
    | Abstraction Identifier Expression
    | MapAbstraction String String Expression
    | SingleArity SingleArity Expression
    | DoubleArity DoubleArity Expression Expression
    | TripleArity TripleArity Expression Expression Expression


type SingleArity
    = Application Expression
    | Assignment Identifier
    | Sqrt
    | Factorial
    | Negation
    | Summation


type DoubleArity
    = Addition
    | Subtraction
    | Multiplication
    | Division
    | Exponentiation
    | Frac
    | Index


type TripleArity
    = Sum_ String


type Identifier
    = ScalarIdentifier String
    | VectorIdentifier String


singleAritySymbolsMap : Dict.Dict String SingleArity
singleAritySymbolsMap =
    Dict.fromList
        [ ( "sqrt", Sqrt )
        , ( "sum", Summation )
        ]


doubleAritySymbolsMap : Dict.Dict String DoubleArity
doubleAritySymbolsMap =
    Dict.fromList [ ( "frac", Frac ) ]


tripleAritySymbolsMap : Dict.Dict String String
tripleAritySymbolsMap =
    Dict.fromList [ ( "sum_", "sum_" ) ]
