module Tests exposing (..)

import Test exposing (..)
import Encoder exposing (encoder)
import Json.Decode as Decode
import Fixture
import Helpers exposing (expectAt, lengthAt)


all : Test
all =
    describe "JsonSchema"
        [ objectSchema
        , arraySchema
        , stringSchema
        , integerSchema
        , numberSchema
        , booleanSchema
        , nullSchema
        ]


objectSchema : Test
objectSchema =
    describe "object schema"
        [ test "title property is set" <|
            \() ->
                encoder Fixture.objectSchema
                    |> expectAt
                        [ "title" ]
                        ( Decode.string, "object schema title" )
        , test "description property is set" <|
            \() ->
                encoder Fixture.objectSchema
                    |> expectAt
                        [ "description" ]
                        ( Decode.string, "object schema description" )
        , test "has the right type" <|
            \() ->
                encoder Fixture.objectSchema
                    |> expectAt
                        [ "type" ]
                        ( Decode.string, "object" )
        , test "adds the right properties to 'required'" <|
            \() ->
                encoder Fixture.objectSchema
                    |> expectAt
                        [ "required", "0" ]
                        ( Decode.string, "lastName" )
        , test "array 'required' has correct length" <|
            \() ->
                encoder Fixture.objectSchema
                    |> lengthAt [ "required" ] 1
        , test "first object property exists as nested schema" <|
            \() ->
                encoder Fixture.objectSchema
                    |> expectAt
                        [ "properties", "firstName", "type" ]
                        ( Decode.string, "string" )
        , test "second object property exists as nested schema" <|
            \() ->
                encoder Fixture.objectSchema
                    |> expectAt
                        [ "properties", "lastName", "type" ]
                        ( Decode.string, "string" )
        ]


arraySchema : Test
arraySchema =
    describe "array schema"
        [ test "title property is set" <|
            \() ->
                encoder Fixture.arraySchema
                    |> expectAt
                        [ "title" ]
                        ( Decode.string, "array schema title" )
        , test "description property is set" <|
            \() ->
                encoder Fixture.arraySchema
                    |> expectAt
                        [ "description" ]
                        ( Decode.string, "array schema description" )
        , test "has the right type" <|
            \() ->
                encoder Fixture.arraySchema
                    |> expectAt
                        [ "type" ]
                        ( Decode.string, "array" )
        , test "items property contains nested schema" <|
            \() ->
                encoder Fixture.arraySchema
                    |> expectAt
                        [ "items", "type" ]
                        ( Decode.string, "string" )
        ]


stringSchema : Test
stringSchema =
    describe "string schema"
        [ test "title property is set" <|
            \() ->
                encoder Fixture.stringSchema
                    |> expectAt
                        [ "title" ]
                        ( Decode.string, "string schema title" )
        , test "description property is set" <|
            \() ->
                encoder Fixture.stringSchema
                    |> expectAt
                        [ "description" ]
                        ( Decode.string, "string schema description" )
        , test "has the right type" <|
            \() ->
                encoder Fixture.stringSchema
                    |> expectAt
                        [ "type" ]
                        ( Decode.string, "string" )
        ]


integerSchema : Test
integerSchema =
    describe "integer schema"
        [ test "title property is set" <|
            \() ->
                encoder Fixture.integerSchema
                    |> expectAt
                        [ "title" ]
                        ( Decode.string, "integer schema title" )
        , test "description property is set" <|
            \() ->
                encoder Fixture.integerSchema
                    |> expectAt
                        [ "description" ]
                        ( Decode.string, "integer schema description" )
        , test "has the right type" <|
            \() ->
                encoder Fixture.integerSchema
                    |> expectAt
                        [ "type" ]
                        ( Decode.string, "integer" )
        , test "minimum property is set" <|
            \() ->
                encoder Fixture.integerSchema
                    |> expectAt
                        [ "minimum" ]
                        ( Decode.int, 2 )
        , test "maximum property is set" <|
            \() ->
                encoder Fixture.integerSchema
                    |> expectAt
                        [ "maximum" ]
                        ( Decode.int, 8 )
        ]


numberSchema : Test
numberSchema =
    describe "number schema"
        [ test "title property is set" <|
            \() ->
                encoder Fixture.numberSchema
                    |> expectAt
                        [ "title" ]
                        ( Decode.string, "number schema title" )
        , test "description property is set" <|
            \() ->
                encoder Fixture.numberSchema
                    |> expectAt
                        [ "description" ]
                        ( Decode.string, "number schema description" )
        , test "has the right type" <|
            \() ->
                encoder Fixture.numberSchema
                    |> expectAt
                        [ "type" ]
                        ( Decode.string, "number" )
        , test "minimum property is set" <|
            \() ->
                encoder Fixture.numberSchema
                    |> expectAt
                        [ "minimum" ]
                        ( Decode.float, 2.5 )
        , test "maximum property is set" <|
            \() ->
                encoder Fixture.numberSchema
                    |> expectAt
                        [ "maximum" ]
                        ( Decode.float, 8.3 )
        ]


booleanSchema : Test
booleanSchema =
    describe "boolean schema"
        [ test "title property is set" <|
            \() ->
                encoder Fixture.booleanSchema
                    |> expectAt
                        [ "title" ]
                        ( Decode.string, "boolean schema title" )
        , test "description property is set" <|
            \() ->
                encoder Fixture.booleanSchema
                    |> expectAt
                        [ "description" ]
                        ( Decode.string, "boolean schema description" )
        , test "has the right type" <|
            \() ->
                encoder Fixture.booleanSchema
                    |> expectAt
                        [ "type" ]
                        ( Decode.string, "boolean" )
        ]


nullSchema : Test
nullSchema =
    describe "null schema"
        [ test "title property is set" <|
            \() ->
                encoder Fixture.nullSchema
                    |> expectAt
                        [ "title" ]
                        ( Decode.string, "null schema title" )
        , test "description property is set" <|
            \() ->
                encoder Fixture.nullSchema
                    |> expectAt
                        [ "description" ]
                        ( Decode.string, "null schema description" )
        , test "has the right type" <|
            \() ->
                encoder Fixture.nullSchema
                    |> expectAt
                        [ "type" ]
                        ( Decode.string, "null" )
        ]
