module DecoderSpec exposing (..)

import Expect exposing (pass)
import Fixtures
import Json.Decode as Decode
import JsonSchema exposing (..)
import JsonSchema.Decoder exposing (decoder)
import JsonSchema.Encoder exposing (encode)
import Test exposing (..)


spec : Test
spec =
    describe "Decoder" <|
        List.map testSchemaDecoder
            [ Fixtures.objectSchema
            , Fixtures.arraySchema
            , Fixtures.stringSchema
            , Fixtures.stringEnumSchema
            , Fixtures.integerEnumSchema
            , Fixtures.integerSchema
            , Fixtures.numberSchema
            , Fixtures.numberEnumSchema
            , Fixtures.booleanSchema
            , Fixtures.nullSchema
            , Fixtures.refSchema
            , Fixtures.recursiveSchema
            , Fixtures.oneOfSchema
            , Fixtures.anyOfSchema
            , Fixtures.allOfSchema
            , Fixtures.fallbackSchema
            , string [ format dateTime ]
            , string [ format email ]
            , string [ format hostname ]
            , string [ format ipv4 ]
            , string [ format ipv6 ]
            , string [ format uri ]
            , string [ format (customFormat "foo") ]
            ]


{-| Test a decoder by taking a json schema string, decoding and then encoding it and expecting the same result as before.
To make it easier to write this test, we generate the json schema string to test from an elm schema.
This means that in practice, we test 1. encode -> 2. decode -> 3. encode, comparing the output from 1 and 3.
-}
testSchemaDecoder : Schema -> Test
testSchemaDecoder schema =
    let
        jsonSchema : String
        jsonSchema =
            encode schema
    in
    test ("decoding work with schema: " ++ jsonSchema) <|
        \_ ->
            jsonSchema
                |> Decode.decodeString decoder
                |> Result.map encode
                |> Expect.equal (Ok jsonSchema)
