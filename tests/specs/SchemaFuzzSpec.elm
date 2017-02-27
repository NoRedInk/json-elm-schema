module SchemaFuzzSpec exposing (spec)

import Expect exposing (pass)
import Json.Encode as Encode
import JsonSchema exposing (..)
import JsonSchema.Encoder as Encoder
import JsonSchema.Fuzz exposing (schemaValue)
import Native.JsonSchema
import Test exposing (..)


spec : Test
spec =
    describe "SchemaFuzz" <|
        List.map testSchemaFuzzer
            [ number []
            , number [ minimum -2.5 ]
            , number [ maximum 3.14 ]
            , number [ minimum -2.5, maximum 3.14 ]
            , number [ enum [ 1.2, 3.4 ] ]
            , integer []
            , integer [ minimum -33 ]
            , integer [ maximum 3 ]
            , integer [ minimum -33, maximum 5 ]
            , integer [ enum [ 1, 2 ] ]
            , string []
            , string [ minLength 5 ]
            , string [ maxLength 10 ]
            , string [ minLength 5, maxLength 10 ]
            , string [ enum [ "a", "b" ] ]
            , anyOf [] [ string [], integer [] ]
            ]


testSchemaFuzzer : Schema -> Test
testSchemaFuzzer schema =
    fuzz (schemaValue schema) ("fuzzer works with schema: " ++ (toString schema)) <|
        \value ->
            let
                jsonValue =
                    Encode.encode 2 value

                jsonSchema =
                    Encoder.encoder schema
            in
                Native.JsonSchema.validate jsonSchema jsonValue
                    |> expectOk


expectOk : Result String a -> Expect.Expectation
expectOk result =
    case result of
        Ok _ ->
            Expect.pass

        Err message ->
            Expect.fail message
