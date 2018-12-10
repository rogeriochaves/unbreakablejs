module Types exposing (DoubleArity(..), Error, Expression(..), Program, SingleArity(..), TripleArity(..), doubleAritySymbolsMap, singleAritySymbolsMap, tripleAritySymbolsMap)

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
    | Variable String
    | Abstraction String Expression
    | SingleArityApplication SingleArity Expression
    | DoubleArityApplication DoubleArity Expression Expression
    | TripleArityApplication TripleArity Expression Expression Expression


type SingleArity
    = NamedFunction String
    | Assignment String
    | Sqrt


type DoubleArity
    = Addition
    | Subtraction
    | Multiplication
    | Division
    | Exponentiation
    | Frac


type TripleArity
    = Sum_ String


singleAritySymbolsMap : Dict.Dict String SingleArity
singleAritySymbolsMap =
    Dict.fromList [ ( "sqrt", Sqrt ) ]


doubleAritySymbolsMap : Dict.Dict String DoubleArity
doubleAritySymbolsMap =
    Dict.fromList [ ( "frac", Frac ) ]


tripleAritySymbolsMap : Dict.Dict String String
tripleAritySymbolsMap =
    Dict.fromList [ ( "sum_", "sum_" ) ]
