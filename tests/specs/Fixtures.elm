module Fixtures exposing (..)

import Dict
import Json.Encode as Encode
import JsonSchema exposing (..)
import JsonSchema.Model


objectSchema : Schema
objectSchema =
    object
        [ title "object schema title"
        , description "object schema description"
        , properties
            [ optional "firstName" <| string []
            , required "lastName" <| string []
            ]
        , minProperties 3
        , maxProperties 6
        ]


arraySchema : Schema
arraySchema =
    array
        [ title "array schema title"
        , description "array schema description"
        , items <| string []
        , minItems 3
        , maxItems 6
        ]


stringSchema : Schema
stringSchema =
    string
        [ title "string schema title"
        , description "string schema description"
        , minLength 2
        , maxLength 8
        , pattern "^foo$"
        , format dateTime
        ]


stringEnumSchema : Schema
stringEnumSchema =
    string
        [ title "string schema title"
        , enum [ "a", "b" ]
        ]


integerEnumSchema : Schema
integerEnumSchema =
    integer
        [ title "integer schema title"
        , enum [ 1, 2 ]
        ]


integerSchema : Schema
integerSchema =
    integer
        [ title "integer schema title"
        , description "integer schema description"
        , minimum 2
        , maximum 8
        ]


numberSchema : Schema
numberSchema =
    number
        [ title "number schema title"
        , description "number schema description"
        , minimum 2.5
        , maximum 8.3
        ]


numberEnumSchema : Schema
numberEnumSchema =
    number
        [ title "number schema title"
        , enum [ 1.2, 3.4 ]
        ]


booleanSchema : Schema
booleanSchema =
    boolean
        [ title "boolean schema title"
        , description "boolean schema description"
        ]


booleanEnumSchema : Schema
booleanEnumSchema =
    boolean
        [ title "Boolean that can only be True"
        , enum [ True ]
        ]


nullSchema : Schema
nullSchema =
    null
        [ title "null schema title"
        , description "null schema description"
        ]


refSchema : Schema
refSchema =
    JsonSchema.Model.Ref
        { title = Just "ref schema title"
        , description = Just "ref schema description"
        , ref = "refurl"
        , examples = []
        , definitions = Dict.empty
        }


recursiveSchema : Schema
recursiveSchema =
    recurse "recursive"
        (\ref ->
            array
                [ items ref
                ]
        )


oneOfSchema : Schema
oneOfSchema =
    oneOf
        [ title "oneOf schema title"
        , description "oneOf schema description"
        ]
        [ integer [], string [] ]


anyOfSchema : Schema
anyOfSchema =
    anyOf
        [ title "anyOf schema title"
        , description "anyOf schema description"
        ]
        [ integer [], string [] ]


allOfSchema : Schema
allOfSchema =
    allOf
        [ title "allOf schema title"
        , description "allOf schema description"
        ]
        [ integer [], string [] ]


fallbackSchema : Schema
fallbackSchema =
    JsonSchema.Model.Fallback
        (Encode.object
            [ ( "foo", Encode.string "bar" ) ]
        )
