module Types exposing (..)

import Dict
import Parser exposing (..)


type alias Program =
    List Expression


type alias Error =
    List DeadEnd


type Expression
    = Number Float
    | SymbolicFunction Symbol
    | Identifier String
    | InfixFunction Infix Expression Expression
    | Equation String Expression
    | FunctionDeclaration String FunctionSchema
    | FunctionCall String Expression


type Infix
    = Addition
    | Subtraction
    | Multiplication
    | Division
    | Exponentiation


type FunctionSchema
    = FunctionSchema String Expression


type Symbol
    = SingleArity SingleAritySymbol Expression
    | DoubleArity DoubleAritySymbol Expression Expression
    | Iterator IteratorSymbol String Expression Expression Expression


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
