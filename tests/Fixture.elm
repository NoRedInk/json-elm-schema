module Fixture exposing (..)

import JsonSchema exposing (..)


testSchema : Schema
testSchema =
    Object
        { title = Just "Example Schema"
        , description = Nothing
        , properties =
            [ Required "firstName"
                (String
                    { title = Nothing
                    , description = Nothing
                    }
                )
            , Required "lastName"
                (String
                    { title = Nothing
                    , description = Nothing
                    }
                )
            , NotRequired "age"
                (Integer
                    { title = Nothing
                    , description = Just "Age in years"
                    , minimum = Just 0
                    , maximum = Nothing
                    }
                )
            ]
        }
