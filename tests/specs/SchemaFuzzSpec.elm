module SchemaFuzzSpec exposing (spec)

import Expect exposing (pass)
import Json.Decode as Decode
import JsonSchema
import Model exposing (Schema)
import SchemaFuzz exposing (schemaString, schemaValue)
import Test exposing (..)


spec : Test
spec =
    describe "SchemaFuzz"
        [ numberFuzz
        ]


simpleNumberSchema : Schema
simpleNumberSchema =
    JsonSchema.number []


numberFuzz : Test
numberFuzz =
    describe "Fuzzing a number"
        [ fuzz (schemaValue simpleNumberSchema) "it generates numbers" <|
            \value ->
                case Decode.decodeValue Decode.float value of
                    Ok _ ->
                        Expect.pass

                    Err _ ->
                        Expect.fail <| "Expected a number but got: " ++ (toString value)
        ]
