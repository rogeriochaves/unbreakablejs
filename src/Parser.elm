module Parser exposing (..)

import Combine exposing (..)
import Combine.Num exposing (int)
import Types exposing (..)


int : Parser s Expression
int =
    EInt <$> Combine.Num.int <?> "integer"


float : Parser s Expression
float =
    EFloat <$> Combine.Num.float <?> "float"


identifier : Parser s String
identifier =
    regex "[_a-zA-Z][_a-zA-Z0-9]*" <?> "identifier"


addop : Parser s (Expression -> Expression -> Expression)
addop =
    choice
        [ EAdd <$ string "+"
        , ESub <$ string "-"
        ]


mulop : Parser s (Expression -> Expression -> Expression)
mulop =
    choice
        [ EMul <$ string "*"
        , EDiv <$ string "/"
        ]


expoop : Parser s (Expression -> Expression -> Expression)
expoop =
    EExponentiation <$ string "^"


expr : Parser s Expression
expr =
    let
        priorities =
            { first = functionParensAndAtoms
            , second = expoop
            , third = mulop
            , fourth = addop
            }
    in
    chainl priorities.fourth
        (chainl priorities.third
            (chainl priorities.second
                priorities.first
            )
        )


functionParensAndAtoms : Parser s Expression
functionParensAndAtoms =
    lazy <|
        \() -> whitespace *> (parens expr <|> symbolicFunction <|> atom) <* whitespace


atom : Parser s Expression
atom =
    choice [ float, int ]


symbolicFunction : Parser s Expression
symbolicFunction =
    let
        singleArity =
            lazy <|
                \() ->
                    (SingleArity <$> (identifier >>= findSymbol singleAritySymbolsMap))
                        <*> braces expr

        doubleArity =
            lazy <|
                \() ->
                    (DoubleArity <$> (identifier >>= findSymbol doubleAritySymbolsMap))
                        <*> braces expr
                        <*> braces expr
    in
    lazy <|
        \() ->
            (ESymbolicFunction <$ string "\\")
                <*> (singleArity <|> doubleArity)


parse : String -> Result String Expression
parse s =
    case Combine.parse (expr <* end) s of
        Ok ( _, _, n ) ->
            Ok n

        Err ( _, stream, ms ) ->
            Err ("parse error: " ++ toString ms ++ ", " ++ toString stream)
