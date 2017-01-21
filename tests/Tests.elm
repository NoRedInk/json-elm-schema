module Tests exposing (..)

import Test exposing (..)
import Encoder exposing (encoder)
import Fixture
import Helpers exposing (expectAt, lengthAt)


all : Test
all =
    describe "JsonSchema"
        [ describe "ObjectSchema"
            [ test "has the right type" <|
                \() ->
                    encoder Fixture.objectSchema
                        |> expectAt
                            [ "type" ]
                            "object"
            , test "adds the right properties to 'required'" <|
                \() ->
                    encoder Fixture.objectSchema
                        |> expectAt
                            [ "required", "0" ]
                            "lastName"
            , test "array 'required' has correct length" <|
                \() ->
                    encoder Fixture.objectSchema
                        |> lengthAt [ "required" ] 1
            , test "first object property exists as nested schema" <|
                \() ->
                    encoder Fixture.objectSchema
                        |> expectAt
                            [ "properties", "firstName", "type" ]
                            "string"
            , test "second object property exists as nested schema" <|
                \() ->
                    encoder Fixture.objectSchema
                        |> expectAt
                            [ "properties", "lastName", "type" ]
                            "string"
            ]
        ]
