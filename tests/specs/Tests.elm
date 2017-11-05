module Tests exposing (..)

import Expect
import Fixtures exposing (..)
import Helpers exposing (expectAt, expectEqualResult, lengthAt)
import Json.Decode as Decode
import JsonSchema exposing (..)
import JsonSchema.Encoder exposing (encode, encodeValue)
import JsonSchema.Util exposing (hash)
import Test exposing (..)


objectSchemaSpec : Test
objectSchemaSpec =
    describe "object schema"
        [ test "title property is set" <|
            \() ->
                encode objectSchema
                    |> expectAt
                        [ "title" ]
                        ( Decode.string, "object schema title" )
        , test "description property is set" <|
            \() ->
                encode objectSchema
                    |> expectAt
                        [ "description" ]
                        ( Decode.string, "object schema description" )
        , test "minProperties property is set" <|
            \() ->
                encode objectSchema
                    |> expectAt
                        [ "minProperties" ]
                        ( Decode.int, 3 )
        , test "maxProperties property is set" <|
            \() ->
                encode objectSchema
                    |> expectAt
                        [ "maxProperties" ]
                        ( Decode.int, 6 )
        , test "has the right type" <|
            \() ->
                encode objectSchema
                    |> expectAt
                        [ "type" ]
                        ( Decode.string, "object" )
        , test "adds the right properties to 'required'" <|
            \() ->
                encode objectSchema
                    |> expectAt
                        [ "required", "0" ]
                        ( Decode.string, "lastName" )
        , test "array 'required' has correct length" <|
            \() ->
                encode objectSchema
                    |> lengthAt [ "required" ] 1
        , test "first object property exists as nested schema" <|
            \() ->
                encode objectSchema
                    |> expectAt
                        [ "properties", "firstName", "type" ]
                        ( Decode.string, "string" )
        , test "second object property exists as nested schema" <|
            \() ->
                encode objectSchema
                    |> expectAt
                        [ "properties", "lastName", "type" ]
                        ( Decode.string, "string" )
        ]


arraySchemaSpec : Test
arraySchemaSpec =
    describe "array schema"
        [ test "title property is set" <|
            \() ->
                encode arraySchema
                    |> expectAt
                        [ "title" ]
                        ( Decode.string, "array schema title" )
        , test "description property is set" <|
            \() ->
                encode arraySchema
                    |> expectAt
                        [ "description" ]
                        ( Decode.string, "array schema description" )
        , test "has the right type" <|
            \() ->
                encode arraySchema
                    |> expectAt
                        [ "type" ]
                        ( Decode.string, "array" )
        , test "items property contains nested schema" <|
            \() ->
                encode arraySchema
                    |> expectAt
                        [ "items", "type" ]
                        ( Decode.string, "string" )
        , test "minItems property contains nested schema" <|
            \() ->
                encode arraySchema
                    |> expectAt
                        [ "minItems" ]
                        ( Decode.int, 3 )
        , test "maxItems property contains nested schema" <|
            \() ->
                encode arraySchema
                    |> expectAt
                        [ "maxItems" ]
                        ( Decode.int, 6 )
        ]


stringSchemaSpec : Test
stringSchemaSpec =
    describe "string schema"
        [ test "title property is set" <|
            \() ->
                encode stringSchema
                    |> expectAt
                        [ "title" ]
                        ( Decode.string, "string schema title" )
        , test "description property is set" <|
            \() ->
                encode stringSchema
                    |> expectAt
                        [ "description" ]
                        ( Decode.string, "string schema description" )
        , test "has the right type" <|
            \() ->
                encode stringSchema
                    |> expectAt
                        [ "type" ]
                        ( Decode.string, "string" )
        , test "minLength property is set" <|
            \() ->
                encode stringSchema
                    |> expectAt
                        [ "minLength" ]
                        ( Decode.int, 2 )
        , test "maxLength property is set" <|
            \() ->
                encode stringSchema
                    |> expectAt
                        [ "maxLength" ]
                        ( Decode.int, 8 )
        , test "pattern property is set" <|
            \() ->
                encode stringSchema
                    |> expectAt
                        [ "pattern" ]
                        ( Decode.string, "^foo$" )
        , test "format property is set" <|
            \() ->
                encode stringSchema
                    |> expectAt
                        [ "format" ]
                        ( Decode.string, "date-time" )
        ]


stringEnumSchemaSpec : Test
stringEnumSchemaSpec =
    describe "string enum schema"
        [ test "title property is set" <|
            \() ->
                encode stringEnumSchema
                    |> expectAt
                        [ "title" ]
                        ( Decode.string, "string schema title" )
        , test "enum property is set" <|
            \() ->
                encode stringEnumSchema
                    |> expectAt
                        [ "enum" ]
                        ( Decode.list Decode.string, [ "a", "b" ] )
        ]


integerEnumSchemaSpec : Test
integerEnumSchemaSpec =
    describe "integer enum schema"
        [ test "title property is set" <|
            \() ->
                encode integerEnumSchema
                    |> expectAt
                        [ "title" ]
                        ( Decode.string, "integer schema title" )
        , test "enum property is set" <|
            \() ->
                encode integerEnumSchema
                    |> expectAt
                        [ "enum" ]
                        ( Decode.list Decode.int, [ 1, 2 ] )
        ]


integerSchemaSpec : Test
integerSchemaSpec =
    describe "integer schema"
        [ test "title property is set" <|
            \() ->
                encode integerSchema
                    |> expectAt
                        [ "title" ]
                        ( Decode.string, "integer schema title" )
        , test "description property is set" <|
            \() ->
                encode integerSchema
                    |> expectAt
                        [ "description" ]
                        ( Decode.string, "integer schema description" )
        , test "has the right type" <|
            \() ->
                encode integerSchema
                    |> expectAt
                        [ "type" ]
                        ( Decode.string, "integer" )
        , test "minimum property is set" <|
            \() ->
                encode integerSchema
                    |> expectAt
                        [ "minimum" ]
                        ( Decode.int, 2 )
        , test "maximum property is set" <|
            \() ->
                encode integerSchema
                    |> expectAt
                        [ "maximum" ]
                        ( Decode.int, 8 )
        ]


numberSchemaSpec : Test
numberSchemaSpec =
    describe "number schema"
        [ test "title property is set" <|
            \() ->
                encode numberSchema
                    |> expectAt
                        [ "title" ]
                        ( Decode.string, "number schema title" )
        , test "description property is set" <|
            \() ->
                encode numberSchema
                    |> expectAt
                        [ "description" ]
                        ( Decode.string, "number schema description" )
        , test "has the right type" <|
            \() ->
                encode numberSchema
                    |> expectAt
                        [ "type" ]
                        ( Decode.string, "number" )
        , test "minimum property is set" <|
            \() ->
                encode numberSchema
                    |> expectAt
                        [ "minimum" ]
                        ( Decode.float, 2.5 )
        , test "maximum property is set" <|
            \() ->
                encode numberSchema
                    |> expectAt
                        [ "maximum" ]
                        ( Decode.float, 8.3 )
        ]


numberEnumSchemaSpec : Test
numberEnumSchemaSpec =
    describe "number enum schema"
        [ test "title property is set" <|
            \() ->
                encode numberEnumSchema
                    |> expectAt
                        [ "title" ]
                        ( Decode.string, "number schema title" )
        , test "enum property is set" <|
            \() ->
                encode numberEnumSchema
                    |> expectAt
                        [ "enum" ]
                        ( Decode.list Decode.float, [ 1.2, 3.4 ] )
        ]


booleanSchemaSpec : Test
booleanSchemaSpec =
    describe "boolean schema"
        [ test "title property is set" <|
            \() ->
                encode booleanSchema
                    |> expectAt
                        [ "title" ]
                        ( Decode.string, "boolean schema title" )
        , test "description property is set" <|
            \() ->
                encode booleanSchema
                    |> expectAt
                        [ "description" ]
                        ( Decode.string, "boolean schema description" )
        , test "has the right type" <|
            \() ->
                encode booleanSchema
                    |> expectAt
                        [ "type" ]
                        ( Decode.string, "boolean" )
        ]


booleanEnumSchemaSpec : Test
booleanEnumSchemaSpec =
    describe "boolean enum schema"
        [ test "title property is set" <|
            \() ->
                encode booleanEnumSchema
                    |> expectAt
                        [ "title" ]
                        ( Decode.string, "Boolean that can only be True" )
        , test "enum property is set" <|
            \() ->
                encode booleanEnumSchema
                    |> expectAt
                        [ "enum" ]
                        ( Decode.list Decode.bool, [ True ] )
        ]


nullSchemaSpec : Test
nullSchemaSpec =
    describe "null schema"
        [ test "title property is set" <|
            \() ->
                encode nullSchema
                    |> expectAt
                        [ "title" ]
                        ( Decode.string, "null schema title" )
        , test "description property is set" <|
            \() ->
                encode nullSchema
                    |> expectAt
                        [ "description" ]
                        ( Decode.string, "null schema description" )
        , test "has the right type" <|
            \() ->
                encode nullSchema
                    |> expectAt
                        [ "type" ]
                        ( Decode.string, "null" )
        ]


refSchemaSpec : Test
refSchemaSpec =
    describe "ref schema"
        [ test "title property is set" <|
            \() ->
                encode refSchema
                    |> expectAt
                        [ "title" ]
                        ( Decode.string, "ref schema title" )
        , test "description property is set" <|
            \() ->
                encode refSchema
                    |> expectAt
                        [ "description" ]
                        ( Decode.string, "ref schema description" )
        , test "has a ref" <|
            \() ->
                encode refSchema
                    |> expectAt
                        [ "$ref" ]
                        ( Decode.string, "refurl" )
        ]


oneOfSpec : Test
oneOfSpec =
    describe "oneOf schema"
        [ test "title property is set" <|
            \() ->
                encode oneOfSchema
                    |> expectAt
                        [ "title" ]
                        ( Decode.string, "oneOf schema title" )
        , test "description property is set" <|
            \() ->
                encode oneOfSchema
                    |> expectAt
                        [ "description" ]
                        ( Decode.string, "oneOf schema description" )
        , test "subSchemas are set" <|
            \() ->
                encode oneOfSchema
                    |> Expect.all
                        [ expectAt
                            [ "oneOf", "0", "type" ]
                            ( Decode.string, "integer" )
                        , expectAt
                            [ "oneOf", "1", "type" ]
                            ( Decode.string, "string" )
                        ]
        ]


anyOfSpec : Test
anyOfSpec =
    describe "anyOf schema"
        [ test "title property is set" <|
            \() ->
                encode anyOfSchema
                    |> expectAt
                        [ "title" ]
                        ( Decode.string, "anyOf schema title" )
        , test "description property is set" <|
            \() ->
                encode anyOfSchema
                    |> expectAt
                        [ "description" ]
                        ( Decode.string, "anyOf schema description" )
        , test "subSchemas are set" <|
            \() ->
                encode anyOfSchema
                    |> Expect.all
                        [ expectAt
                            [ "anyOf", "0", "type" ]
                            ( Decode.string, "integer" )
                        , expectAt
                            [ "anyOf", "1", "type" ]
                            ( Decode.string, "string" )
                        ]
        ]


allOfSpec : Test
allOfSpec =
    describe "allOf schema"
        [ test "title property is set" <|
            \() ->
                encode allOfSchema
                    |> expectAt
                        [ "title" ]
                        ( Decode.string, "allOf schema title" )
        , test "description property is set" <|
            \() ->
                encode allOfSchema
                    |> expectAt
                        [ "description" ]
                        ( Decode.string, "allOf schema description" )
        , test "subSchemas are set" <|
            \() ->
                encode allOfSchema
                    |> Expect.all
                        [ expectAt
                            [ "allOf", "0", "type" ]
                            ( Decode.string, "integer" )
                        , expectAt
                            [ "allOf", "1", "type" ]
                            ( Decode.string, "string" )
                        ]
        ]


recursiveSchemaSpec : Test
recursiveSchemaSpec =
    describe "internal ref"
        [ test "is turned into a ref" <|
            \() ->
                encode recursiveSchema
                    |> expectAt
                        [ "$ref" ]
                        ( Decode.string, "#/definitions/recursive" )
        , test "can be found in the definitions group" <|
            \() ->
                encode recursiveSchema
                    |> expectAt
                        [ "definitions", "recursive", "type" ]
                        ( Decode.string, "array" )
        ]


formatDateTime : Test
formatDateTime =
    let
        schema : Schema
        schema =
            string [ format dateTime ]
    in
    describe "format dateTime"
        [ test "format property is set" <|
            \() ->
                encode schema
                    |> expectAt
                        [ "format" ]
                        ( Decode.string, "date-time" )
        ]


formatEmail : Test
formatEmail =
    let
        schema : Schema
        schema =
            string [ format email ]
    in
    describe "format email"
        [ test "format property is set" <|
            \() ->
                encode schema
                    |> expectAt
                        [ "format" ]
                        ( Decode.string, "email" )
        ]


formatHostname : Test
formatHostname =
    let
        schema : Schema
        schema =
            string [ format hostname ]
    in
    describe "format hostname"
        [ test "format property is set" <|
            \() ->
                encode schema
                    |> expectAt
                        [ "format" ]
                        ( Decode.string, "hostname" )
        ]


formatIpv4 : Test
formatIpv4 =
    let
        schema : Schema
        schema =
            string [ format ipv4 ]
    in
    describe "format ipv4"
        [ test "format property is set" <|
            \() ->
                encode schema
                    |> expectAt
                        [ "format" ]
                        ( Decode.string, "ipv4" )
        ]


formatIpv6 : Test
formatIpv6 =
    let
        schema : Schema
        schema =
            string [ format ipv6 ]
    in
    describe "format ipv6"
        [ test "format property is set" <|
            \() ->
                encode schema
                    |> expectAt
                        [ "format" ]
                        ( Decode.string, "ipv6" )
        ]


formatUri : Test
formatUri =
    let
        schema : Schema
        schema =
            string [ format uri ]
    in
    describe "format uri"
        [ test "format property is set" <|
            \() ->
                encode schema
                    |> expectAt
                        [ "format" ]
                        ( Decode.string, "uri" )
        ]


formatCustom : Test
formatCustom =
    let
        schema : Schema
        schema =
            string [ format (customFormat "foo") ]
    in
    describe "format customFormat"
        [ test "format property is set" <|
            \() ->
                encode schema
                    |> expectAt
                        [ "format" ]
                        ( Decode.string, "foo" )
        ]


fallbackSchemaSpec : Test
fallbackSchemaSpec =
    describe "fallback schema"
        [ test "fallback value is re-encoded" <|
            \() ->
                encode fallbackSchema
                    |> expectAt
                        [ "foo" ]
                        ( Decode.string, "bar" )
        ]
