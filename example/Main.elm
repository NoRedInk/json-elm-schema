port module Main exposing (..)

import JsonSchema exposing (..)
import JsonSchema.Encoder exposing (EncoderProgram, encodeSchemaProgram)


personSchema : Schema
personSchema =
    object
        [ title "person"
        , properties
            [ required "firstName" <| string []
            , required "lastName" <| string []
            , optional "age" <|
                integer
                    [ description "Age in years"
                    , minimum 0
                    ]
            , optional "height" <|
                number
                    [ description "Height in meters"
                    , minimum 0
                    ]
            ]
        ]


main : EncoderProgram
main =
    encodeSchemaProgram personSchema emit


port emit : String -> Cmd a
