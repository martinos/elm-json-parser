module Main exposing (..)

import Html exposing (..)
import Parser exposing (Parser, (|.), (|=), succeed, symbol, float, ignore, zeroOrMore, run, keyword, keep, delayedCommit, int, andThen, oneOf, float, lazy)


main =
    div [] [ run (jsonValue) objectToParse |> toString |> text ]


objectToParse =
    """{"name" : "Martin", "age" : [23, {"name": "Joe"}]}
"""


arrayToParse =
    """[3,3]
"""


string : (String -> a) -> Parser a
string parsed =
    succeed parsed
        |. symbol "\""
        |= keep zeroOrMore (\c -> c /= '"')
        |. symbol "\""


field : Parser ( String, JsonValue )
field =
    succeed (,)
        |. spaces
        |= string identity
        |. spaces
        |. symbol ":"
        |. spaces
        |= lazy (\_ -> jsonValue)


jsonValue : Parser JsonValue
jsonValue =
    oneOf
        [ string StringValue
        , numeric
        , object
        , lazy (\n -> list)
        ]


numeric =
    succeed NumericValue |= float


spaces : Parser ()
spaces =
    ignore zeroOrMore (\c -> c == ' ' || c == '\n')


object : Parser JsonValue
object =
    succeed ObjectValue
        |. symbol "{"
        |. spaces
        |= andThen (\n -> fieldListHelp [ n ]) (lazy (\_ -> field))
        |. spaces
        |. symbol "}"


fieldListHelp : List ( String, JsonValue ) -> Parser (List ( String, JsonValue ))
fieldListHelp revInts =
    oneOf
        [ nextField
            |> andThen (\n -> fieldListHelp (n :: revInts))
        , succeed (List.reverse revInts)
        ]


nextField : Parser ( String, JsonValue )
nextField =
    delayedCommit spaces <|
        succeed identity
            |. symbol ","
            |. spaces
            |= field


list : Parser JsonValue
list =
    succeed ArrayValue
        |. symbol "["
        |. spaces
        |= andThen (\n -> listHelp [ n ]) jsonValue
        |. spaces
        |. symbol "]"


listHelp : List JsonValue -> Parser (List JsonValue)
listHelp revInts =
    oneOf
        [ nextValue
            |> andThen (\n -> listHelp (n :: revInts))
        , succeed (List.reverse revInts)
        ]


nextValue : Parser JsonValue
nextValue =
    delayedCommit spaces <|
        succeed identity
            |. symbol ","
            |. spaces
            |= jsonValue


type JsonValue
    = ObjectValue (List ( String, JsonValue ))
    | ArrayValue (List JsonValue)
    | BoolValue Bool
    | NullValue
    | NumericValue Float
    | StringValue String
