module Fixture exposing (..)

import JsonSchema exposing (..)
import Model exposing (Schema)


testSchema : Schema
testSchema =
    object
        [ title "Example Schema"
        , description "This is a description"
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
