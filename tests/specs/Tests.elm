module Tests exposing (spec)

import Expect
import Helpers exposing (expectAt, lengthAt, expectEqualResult)
import Json.Decode as Decode
import Json.Encode as Encode
import JsonSchema exposing (..)
import JsonSchema.Encoder exposing (encode, encodeValue)
import JsonSchema.Decoder exposing (decoder)
import JsonSchema.Validator as Validator
import Test exposing (..)


spec : Test
spec =
    describe "JsonSchema"
        [ objectSchemaSpec
        , arraySchemaSpec
        , stringSchemaSpec
        , stringEnumSchemaSpec
        , integerSchemaSpec
        , integerEnumSchemaSpec
        , numberSchemaSpec
        , numberEnumSchemaSpec
        , booleanSchemaSpec
        , nullSchemaSpec
        , oneOfSpec
        , anyOfSpec
        , allOfSpec
        , formatDateTime
        , formatEmail
        , formatHostname
        , formatIpv4
        , formatIpv6
        , formatUri
        , formatCustom
        , deepValidation
        ]


objectSchemaSpec : Test
objectSchemaSpec =
    let
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
    in
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
            , test "decoder" <|
                \() ->
                    encode objectSchema
                        |> Decode.decodeString decoder
                        |> expectEqualResult objectSchema
            , test "validate valid" <|
                \() ->
                    Encode.object [ ( "firstName", Encode.string "foo" ), ( "lastName", Encode.string "bar" ) ]
                        |> Validator.validate objectSchema
                        |> Expect.equal []
            , test "validate valid optional field missing" <|
                \() ->
                    Encode.object [ ( "lastName", Encode.string "bar" ) ]
                        |> Validator.validate objectSchema
                        |> Expect.equal []
            , test "validate valid additionalItems" <|
                \() ->
                    Encode.object [ ( "unknown", Encode.int 1 ), ( "firstName", Encode.string "foo" ), ( "lastName", Encode.string "bar" ) ]
                        |> Validator.validate objectSchema
                        |> Expect.equal []
            , test "validate optional field wrong type" <|
                \() ->
                    Encode.object [ ( "firstName", Encode.int 1 ), ( "lastName", Encode.string "bar" ) ]
                        |> Validator.validate objectSchema
                        |> Expect.equal [ ( [ "firstName" ], Validator.DecodeError "Expecting a String but instead got: 1" ) ]
            , test "validate required field missing" <|
                \() ->
                    Encode.object [ ( "firstName", Encode.string "bar" ) ]
                        |> Validator.validate objectSchema
                        |> Expect.equal [ ( [], Validator.RequiredPropertyMissing "lastName" ) ]
            , test "validate multiple errors" <|
                \() ->
                    Encode.object [ ( "firstName", Encode.int 1 ) ]
                        |> Validator.validate objectSchema
                        |> Expect.equal
                            [ ( [ "firstName" ], Validator.DecodeError "Expecting a String but instead got: 1" )
                            , ( [], Validator.RequiredPropertyMissing "lastName" )
                            ]
            ]


arraySchemaSpec : Test
arraySchemaSpec =
    let
        arraySchema : Schema
        arraySchema =
            array
                [ title "array schema title"
                , description "array schema description"
                , items <| string []
                , minItems 3
                , maxItems 6
                ]
    in
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
            , test "decoder" <|
                \() ->
                    encode arraySchema
                        |> Decode.decodeString decoder
                        |> expectEqualResult arraySchema
            , test "validate valid" <|
                \() ->
                    Encode.list [ Encode.string "foo", Encode.string "bar", Encode.string "baz" ]
                        |> Validator.validate arraySchema
                        |> Expect.equal []
            , test "validate too short" <|
                \() ->
                    Encode.list [ Encode.string "foo", Encode.string "bar" ]
                        |> Validator.validate arraySchema
                        |> Expect.equal [ ( [], Validator.HasFewerItemsThan 3 ) ]
            , test "validate too long" <|
                \() ->
                    Encode.list
                        [ Encode.string "foo"
                        , Encode.string "bar"
                        , Encode.string "baz"
                        , Encode.string "foo"
                        , Encode.string "bar"
                        , Encode.string "baz"
                        , Encode.string "foo"
                        ]
                        |> Validator.validate arraySchema
                        |> Expect.equal [ ( [], Validator.HasMoreItemsThan 6 ) ]
            , test "validate wrong item type" <|
                \() ->
                    Encode.list
                        [ Encode.string "foo"
                        , Encode.string "bar"
                        , Encode.string "baz"
                        , Encode.int 1
                        , Encode.string "bar"
                        , Encode.string "baz"
                        ]
                        |> Validator.validate arraySchema
                        |> Expect.equal
                            [ ( [ "3" ]
                              , Validator.DecodeError "Expecting a String but instead got: 1"
                              )
                            ]
            , test "validate multiple errors" <|
                \() ->
                    Encode.list
                        [ Encode.string "foo"
                        , Encode.int 1
                        ]
                        |> Validator.validate arraySchema
                        |> Expect.equal
                            [ ( [ "1" ]
                              , Validator.DecodeError "Expecting a String but instead got: 1"
                              )
                            , ( [], Validator.HasFewerItemsThan 3 )
                            ]
            ]


stringSchemaSpec : Test
stringSchemaSpec =
    let
        stringSchema : Schema
        stringSchema =
            string
                [ title "string schema title"
                , description "string schema description"
                , minLength 2
                , maxLength 8
                , pattern "f"
                , format dateTime
                ]
    in
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
                            ( Decode.string, "f" )
            , test "format property is set" <|
                \() ->
                    encode stringSchema
                        |> expectAt
                            [ "format" ]
                            ( Decode.string, "date-time" )
            , test "decoder" <|
                \() ->
                    encode stringSchema
                        |> Decode.decodeString decoder
                        |> expectEqualResult stringSchema
            , test "validate valid" <|
                \() ->
                    Encode.string "foo"
                        |> Validator.validate stringSchema
                        |> Expect.equal []
            , test "validate wrong pattern" <|
                \() ->
                    Encode.string "goo"
                        |> Validator.validate stringSchema
                        |> Expect.equal [ ( [], Validator.DoesNotMatchPattern "f" ) ]
            , test "validate too short" <|
                \() ->
                    Encode.string "f"
                        |> Validator.validate stringSchema
                        |> Expect.equal [ ( [], Validator.IsShorterThan 2 ) ]
            , test "validate too long" <|
                \() ->
                    Encode.string "foooooooo"
                        |> Validator.validate stringSchema
                        |> Expect.equal [ ( [], Validator.IsLongerThan 8 ) ]
            , test "validate multiple errors" <|
                \() ->
                    Encode.string "goooooooo"
                        |> Validator.validate stringSchema
                        |> Expect.equal [ ( [], Validator.IsLongerThan 8 ), ( [], Validator.DoesNotMatchPattern "f" ) ]
            ]


stringEnumSchemaSpec : Test
stringEnumSchemaSpec =
    let
        stringEnumSchema : Schema
        stringEnumSchema =
            string
                [ title "string schema title"
                , enum [ "a", "b" ]
                ]
    in
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
            , test "decoder" <|
                \() ->
                    encode stringEnumSchema
                        |> Decode.decodeString decoder
                        |> expectEqualResult stringEnumSchema
            , test "validate valid 1" <|
                \() ->
                    Encode.string "a"
                        |> Validator.validate stringEnumSchema
                        |> Expect.equal []
            , test "validate valid 2" <|
                \() ->
                    Encode.string "b"
                        |> Validator.validate stringEnumSchema
                        |> Expect.equal []
            , test "validate invalid" <|
                \() ->
                    Encode.string "c"
                        |> Validator.validate stringEnumSchema
                        |> Expect.equal [ ( [], Validator.NotInEnumeration ) ]
            ]


integerEnumSchemaSpec : Test
integerEnumSchemaSpec =
    let
        integerEnumSchema : Schema
        integerEnumSchema =
            integer
                [ title "integer schema title"
                , enum [ 1, 2 ]
                ]
    in
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
            , test "decoder" <|
                \() ->
                    encode integerEnumSchema
                        |> Decode.decodeString decoder
                        |> expectEqualResult integerEnumSchema
            , test "validate valid 1" <|
                \() ->
                    Encode.int 1
                        |> Validator.validate integerEnumSchema
                        |> Expect.equal []
            , test "validate valid 2" <|
                \() ->
                    Encode.int 2
                        |> Validator.validate integerEnumSchema
                        |> Expect.equal []
            , test "validate invalid" <|
                \() ->
                    Encode.int 3
                        |> Validator.validate integerEnumSchema
                        |> Expect.equal [ ( [], Validator.NotInEnumeration ) ]
            ]


integerSchemaSpec : Test
integerSchemaSpec =
    let
        integerSchema : Schema
        integerSchema =
            integer
                [ title "integer schema title"
                , description "integer schema description"
                , minimum 2
                , maximum 8
                ]
    in
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
            , test "decoder" <|
                \() ->
                    encode integerSchema
                        |> Decode.decodeString decoder
                        |> expectEqualResult integerSchema
            , test "validate valid" <|
                \() ->
                    Encode.int 4
                        |> Validator.validate integerSchema
                        |> Expect.equal []
            , test "validate too small" <|
                \() ->
                    Encode.int 1
                        |> Validator.validate integerSchema
                        |> Expect.equal [ ( [], Validator.IsLessThan 2 ) ]
            , test "validate too large" <|
                \() ->
                    Encode.int 9
                        |> Validator.validate integerSchema
                        |> Expect.equal [ ( [], Validator.IsMoreThan 8 ) ]
            ]


numberSchemaSpec : Test
numberSchemaSpec =
    let
        numberSchema : Schema
        numberSchema =
            number
                [ title "number schema title"
                , description "number schema description"
                , minimum 2.5
                , maximum 8.3
                ]
    in
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
            , test "decoder" <|
                \() ->
                    encode numberSchema
                        |> Decode.decodeString decoder
                        |> expectEqualResult numberSchema
            , test "validate valid" <|
                \() ->
                    Encode.float 4
                        |> Validator.validate numberSchema
                        |> Expect.equal []
            , test "validate too small" <|
                \() ->
                    Encode.float 2.4
                        |> Validator.validate numberSchema
                        |> Expect.equal [ ( [], Validator.IsLessThan 2.5 ) ]
            , test "validate too large" <|
                \() ->
                    Encode.float 8.4
                        |> Validator.validate numberSchema
                        |> Expect.equal [ ( [], Validator.IsMoreThan 8.3 ) ]
            ]


numberEnumSchemaSpec : Test
numberEnumSchemaSpec =
    let
        numberEnumSchema : Schema
        numberEnumSchema =
            number
                [ title "number schema title"
                , enum [ 1.2, 3.4 ]
                ]
    in
        describe "number schema"
            [ test "title property is set" <|
                \() ->
                    encode numberEnumSchema
                        |> expectAt
                            [ "title" ]
                            ( Decode.string, "number schema title" )
            , test "description property is set" <|
                \() ->
                    encode numberEnumSchema
                        |> expectAt
                            [ "enum" ]
                            ( Decode.list Decode.float, [ 1.2, 3.4 ] )
            , test "decoder" <|
                \() ->
                    encode numberEnumSchema
                        |> Decode.decodeString decoder
                        |> expectEqualResult numberEnumSchema
            , test "validate valid 1" <|
                \() ->
                    Encode.float 1.2
                        |> Validator.validate numberEnumSchema
                        |> Expect.equal []
            , test "validate valid 2" <|
                \() ->
                    Encode.float 3.4
                        |> Validator.validate numberEnumSchema
                        |> Expect.equal []
            , test "validate invalid" <|
                \() ->
                    Encode.float 2.3
                        |> Validator.validate numberEnumSchema
                        |> Expect.equal [ ( [], Validator.NotInEnumeration ) ]
            ]


booleanSchemaSpec : Test
booleanSchemaSpec =
    let
        booleanSchema : Schema
        booleanSchema =
            boolean
                [ title "boolean schema title"
                , description "boolean schema description"
                ]
    in
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
            , test "decoder" <|
                \() ->
                    encode booleanSchema
                        |> Decode.decodeString decoder
                        |> expectEqualResult booleanSchema
            , test "validate valid" <|
                \() ->
                    Encode.bool True
                        |> Validator.validate booleanSchema
                        |> Expect.equal []
            , test "validate invalid type" <|
                \() ->
                    Encode.int 1
                        |> Validator.validate booleanSchema
                        |> Expect.equal [ ( [], Validator.DecodeError "Expecting a Bool but instead got: 1" ) ]
            ]


nullSchemaSpec : Test
nullSchemaSpec =
    let
        nullSchema : Schema
        nullSchema =
            null
                [ title "null schema title"
                , description "null schema description"
                ]
    in
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
            , test "decoder" <|
                \() ->
                    encode nullSchema
                        |> Decode.decodeString decoder
                        |> expectEqualResult nullSchema
            , test "validate valid" <|
                \() ->
                    Encode.null
                        |> Validator.validate nullSchema
                        |> Expect.equal []
            , test "validate invalid type" <|
                \() ->
                    Encode.int 1
                        |> Validator.validate nullSchema
                        |> Expect.equal [ ( [], Validator.DecodeError "Expecting null but instead got: 1" ) ]
            ]


oneOfSpec : Test
oneOfSpec =
    let
        oneOfSchema =
            oneOf
                [ title "oneOf schema title"
                , description "oneOf schema description"
                ]
                [ string [ pattern "a" ], string [ pattern "b" ] ]
    in
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
                                ( Decode.string, "string" )
                            , expectAt
                                [ "oneOf", "1", "type" ]
                                ( Decode.string, "string" )
                            ]
            , test "decoder" <|
                \() ->
                    encode oneOfSchema
                        |> Decode.decodeString decoder
                        |> expectEqualResult oneOfSchema
            , test "validate valid 1" <|
                \() ->
                    Encode.string "a"
                        |> Validator.validate oneOfSchema
                        |> Expect.equal []
            , test "validate valid 2" <|
                \() ->
                    Encode.string "b"
                        |> Validator.validate oneOfSchema
                        |> Expect.equal []
            , test "validate no match" <|
                \() ->
                    Encode.string "c"
                        |> Validator.validate oneOfSchema
                        |> Expect.equal [ ( [], Validator.TooFewMatches ) ]
            , test "validate too many" <|
                \() ->
                    Encode.string "ab"
                        |> Validator.validate oneOfSchema
                        |> Expect.equal [ ( [], Validator.TooManyMatches ) ]
            ]


anyOfSpec : Test
anyOfSpec =
    let
        anyOfSchema =
            anyOf
                [ title "anyOf schema title"
                , description "anyOf schema description"
                ]
                [ string [ pattern "a" ], string [ pattern "b" ] ]
    in
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
                                ( Decode.string, "string" )
                            , expectAt
                                [ "anyOf", "1", "type" ]
                                ( Decode.string, "string" )
                            ]
            , test "decoder" <|
                \() ->
                    encode anyOfSchema
                        |> Decode.decodeString decoder
                        |> expectEqualResult anyOfSchema
            , test "validate valid 1" <|
                \() ->
                    Encode.string "a"
                        |> Validator.validate anyOfSchema
                        |> Expect.equal []
            , test "validate valid 2" <|
                \() ->
                    Encode.string "b"
                        |> Validator.validate anyOfSchema
                        |> Expect.equal []
            , test "validate valid 3" <|
                \() ->
                    Encode.string "ab"
                        |> Validator.validate anyOfSchema
                        |> Expect.equal []
            , test "validate no match" <|
                \() ->
                    Encode.string "c"
                        |> Validator.validate anyOfSchema
                        |> Expect.equal [ ( [], Validator.TooFewMatches ) ]
            ]


allOfSpec : Test
allOfSpec =
    let
        allOfSchema =
            allOf
                [ title "allOf schema title"
                , description "allOf schema description"
                ]
                [ string [ pattern "a" ], string [ pattern "b" ] ]
    in
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
                                ( Decode.string, "string" )
                            , expectAt
                                [ "allOf", "1", "type" ]
                                ( Decode.string, "string" )
                            ]
            , test "decoder" <|
                \() ->
                    encode allOfSchema
                        |> Decode.decodeString decoder
                        |> expectEqualResult allOfSchema
            , test "validate valid" <|
                \() ->
                    Encode.string "ab"
                        |> Validator.validate allOfSchema
                        |> Expect.equal []
            , test "validate invalid 1" <|
                \() ->
                    Encode.string "a"
                        |> Validator.validate allOfSchema
                        |> Expect.equal [ ( [], Validator.TooFewMatches ) ]
            , test "validate invalid 2" <|
                \() ->
                    Encode.string "b"
                        |> Validator.validate allOfSchema
                        |> Expect.equal [ ( [], Validator.TooFewMatches ) ]
            , test "validate invalid 3" <|
                \() ->
                    Encode.string "c"
                        |> Validator.validate allOfSchema
                        |> Expect.equal [ ( [], Validator.TooFewMatches ) ]
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
            , test "decoder" <|
                \() ->
                    encode schema
                        |> Decode.decodeString decoder
                        |> expectEqualResult schema
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
            , test "decoder" <|
                \() ->
                    encode schema
                        |> Decode.decodeString decoder
                        |> expectEqualResult schema
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
            , test "decoder" <|
                \() ->
                    encode schema
                        |> Decode.decodeString decoder
                        |> expectEqualResult schema
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
            , test "decoder" <|
                \() ->
                    encode schema
                        |> Decode.decodeString decoder
                        |> expectEqualResult schema
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
            , test "decoder" <|
                \() ->
                    encode schema
                        |> Decode.decodeString decoder
                        |> expectEqualResult schema
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
            , test "decoder" <|
                \() ->
                    encode schema
                        |> Decode.decodeString decoder
                        |> expectEqualResult schema
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
            , test "decoder" <|
                \() ->
                    encode schema
                        |> Decode.decodeString decoder
                        |> expectEqualResult schema
            ]


deepValidation : Test
deepValidation =
    let
        deepSchema =
            object
                [ properties
                    [ required "field"
                        (array
                            [ items
                                (string
                                    [ pattern "a" ]
                                )
                            ]
                        )
                    ]
                ]
    in
        describe "deep validation"
            [ test "validate valid" <|
                \() ->
                    Encode.object
                        [ ( "field"
                          , Encode.list
                                [ Encode.string "a" ]
                          )
                        ]
                        |> Validator.validate deepSchema
                        |> Expect.equal []
            , test "validate invalid 1" <|
                \() ->
                    Encode.object
                        [ ( "field"
                          , Encode.list
                                [ Encode.string "b" ]
                          )
                        ]
                        |> Validator.validate deepSchema
                        |> Expect.equal
                            [ ( [ "field"
                                , "0"
                                ]
                              , Validator.DoesNotMatchPattern "a"
                              )
                            ]
            ]
