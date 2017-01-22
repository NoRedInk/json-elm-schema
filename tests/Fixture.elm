module Fixture exposing (..)

import JsonSchema exposing (..)
import Model exposing (Schema)


objectSchema : Schema
objectSchema =
    object
        [ title "object schema title"
        , description "object schema description"
        , properties
            [ optional "firstName" <| string []
            , required "lastName" <| string []
            ]
        ]


arraySchema : Schema
arraySchema =
    array
        [ title "array schema title"
        , description "array schema description"
        , items <| string []
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


booleanSchema : Schema
booleanSchema =
    boolean
        [ title "boolean schema title"
        , description "boolean schema description"
        ]


nullSchema : Schema
nullSchema =
    null
        [ title "null schema title"
        , description "null schema description"
        ]
