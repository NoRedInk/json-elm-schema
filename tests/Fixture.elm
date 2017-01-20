module Fixture exposing (..)

import JsonSchema exposing (..)


testSchema : Schema
testSchema =
    object
        [ title "Example Schema"
        , exactProperties
            [ required "firstName" <| string []
            , required "lastName" <| string []
            , notRequired "age" <|
                integer
                    [ description "Age in years"
                    , minimum 0
                    ]
            ]
        ]
