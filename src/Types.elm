module Types exposing (DoubleAritySymbol(..), Expression(..), SingleAritySymbol(..), Symbol(..), doubleAritySymbolsMap, singleAritySymbolsMap)

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


type SingleAritySymbol
    = Sqrt


type DoubleAritySymbol
    = Frac


singleAritySymbolsMap : Dict.Dict String SingleAritySymbol
singleAritySymbolsMap =
    Dict.fromList [ ( "sqrt", Sqrt ) ]


doubleAritySymbolsMap : Dict.Dict String DoubleAritySymbol
doubleAritySymbolsMap =
    Dict.fromList [ ( "frac", Frac ) ]
