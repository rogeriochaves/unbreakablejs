module Types exposing (DoubleAritySymbol(..), Expression(..), IteratorSymbol(..), SingleAritySymbol(..), Symbol(..), doubleAritySymbolsMap, iteratorSymbolsMap, singleAritySymbolsMap)

import Dict
import Parser exposing (..)


type Expression
    = Integer Int
    | Floating Float
    | Addition Expression Expression
    | Subtraction Expression Expression
    | Multiplication Expression Expression
    | Division Expression Expression
    | Exponentiation Expression Expression
    | SymbolicFunction Symbol


type Symbol
    = SingleArity SingleAritySymbol Expression
    | DoubleArity DoubleAritySymbol Expression Expression
    | Iterator IteratorSymbol Expression Expression Expression


type SingleAritySymbol
    = Sqrt


type DoubleAritySymbol
    = Frac


type IteratorSymbol
    = Sum_


singleAritySymbolsMap : Dict.Dict String SingleAritySymbol
singleAritySymbolsMap =
    Dict.fromList [ ( "sqrt", Sqrt ) ]


doubleAritySymbolsMap : Dict.Dict String DoubleAritySymbol
doubleAritySymbolsMap =
    Dict.fromList [ ( "frac", Frac ) ]


iteratorSymbolsMap : Dict.Dict String IteratorSymbol
iteratorSymbolsMap =
    Dict.fromList [ ( "sum_", Sum_ ) ]