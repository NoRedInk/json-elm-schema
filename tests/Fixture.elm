module Fixture exposing (..)

import JsonSchema exposing (..)


testSchema : Schema
testSchema =
    object
        [ title "Example Schema"
        , description "This is a description"
        , exactProperties
            [ required "firstName" <| string []
            ]
        ]



-- object
--     [ title "Example Schema"
--     , exactProperties
--         [ required "firstName" <| string []
--         , required "lastName" <| string []
--         , notRequired "age" <|
--             integer
--                 [ description "Age in years"
--                 , minimum 0
--                 ]
--         ]
--     ]
