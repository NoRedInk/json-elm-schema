port module Main exposing (..)

import Json.Encode as Encode
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
                    , examples Encode.int [ 5, 50 ]
                    ]
            , optional "height" <|
                number
                    [ description "Height in meters"
                    , minimum 0
                    ]
            ]
        , examples Encode.object
            [ [ ( "firstName", Encode.string "foof" )
              , ( "lastName", Encode.string "fool" )
              , ( "age", Encode.int 60 )
              , ( "height", Encode.float 1.8 )
              ]
            , [ ( "firstName", Encode.string "barf" )
              , ( "lastName", Encode.string "barl" )
              , ( "age", Encode.int 30 )
              , ( "height", Encode.float 1.6 )
              ]
            ]
        ]


main : EncoderProgram
main =
    encodeSchemaProgram personSchema emit


port emit : String -> Cmd a
