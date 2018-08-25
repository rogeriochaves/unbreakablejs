module Parser exposing (..)

import Combine exposing (..)
import Combine.Num exposing (int)


type Expression
    = EInt Int
    | EFloat Float
    | EAdd Expression Expression
    | ESub Expression Expression
    | EMul Expression Expression
    | EDiv Expression Expression
    | EFn Identifier Expression


type Identifier
    = Identifier String


int : Parser s Expression
int =
    EInt <$> Combine.Num.int <?> "integer"


float : Parser s Expression
float =
    EFloat <$> Combine.Num.float <?> "float"


identifier : Parser s Identifier
identifier =
    Identifier <$> regex "[_a-zA-Z][_a-zA-Z0-9]*" <?> "identifier"


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


expr : Parser s Expression
expr =
    lazy (\() -> chainl addop term)


term : Parser s Expression
term =
    lazy (\() -> chainl mulop factor)


factor : Parser s Expression
factor =
    lazy <|
        \() -> whitespace *> (parens expr <|> function <|> atom) <* whitespace


atom : Parser s Expression
atom =
    choice [ float, int ]


function : Parser s Expression
function =
    lazy <|
        \() ->
            (EFn <$ string "\\")
                <*> identifier
                <*> braces expr


parse : String -> Result String Expression
parse s =
    case Combine.parse (expr <* end) s of
        Ok ( _, _, n ) ->
            Ok n

        Err ( _, stream, ms ) ->
            Err ("parse error: " ++ toString ms ++ ", " ++ toString stream)
