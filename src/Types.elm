module Types exposing (..)

import Combine exposing (..)
import Dict
import Utils exposing (..)


type Expression
    = EInt Int
    | EFloat Float
    | EAdd Expression Expression
    | ESub Expression Expression
    | EMul Expression Expression
    | EDiv Expression Expression
    | EExponentiation Expression Expression
    | ESymbolicFunction Symbol


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


findSymbol : Dict.Dict String a -> String -> Parser s a
findSymbol map name =
    Dict.get name map
        |> parserFromMaybe ("could not find symbol " ++ name)
