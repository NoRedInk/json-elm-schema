module SchemaFuzzSpec exposing (spec)

import Expect exposing (pass)
import JsonSchema exposing (..)
import JsonSchema.Fuzz exposing (schemaValue)
import JsonSchema.Validator
import Test exposing (..)


spec : Test
spec =
    describe "SchemaFuzz" <|
        List.map testSchemaFuzzer
            ( [ number []
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
              , array [ items (string []) ]
              , array [ items (string []), minItems 5 ]
              , array [ items (string []), maxItems 10 ]
              , array [ items (string []), minItems 5, maxItems 10 ]
              ] ++ tupleExamples
            )

tupleExamples : List Schema
tupleExamples =
    [ tuple [ ]
    , tuple [ tupleItems [ string [], integer [], number [] ] ]
    , tuple [ additionalItems (string []) ]
    , tuple [ tupleItems [ string [], number [] ], additionalItems (integer []) ]
    , tuple [ maxItems 0 ]
    , tuple [ maxItems 1 ]
    , tuple [ maxItems 2 ]
    , tuple [ tupleItems [ integer [], string [], number [] ], maxItems 2 ]
    , tuple [ tupleItems [ integer [], string [], number [] ], maxItems 3 ]
    , tuple [ tupleItems [ integer [], string [], number [] ], maxItems 4 ]
    , tuple [ additionalItems (integer []), maxItems 1 ]
    , tuple [ additionalItems (integer []), maxItems 2 ]
    , tuple [ tupleItems [ number [], string [] ], additionalItems (integer []), maxItems 1 ]
    , tuple [ tupleItems [ number [], string [] ], additionalItems (integer []), maxItems 2 ]
    , tuple [ tupleItems [ number [], string [] ], additionalItems (integer []), maxItems 3 ]
    , tuple [ tupleItems [ number [], string [] ], additionalItems (integer []), maxItems 4 ]
    , tuple [ minItems 0 ]
    , tuple [ minItems 1 ]
    , tuple [ minItems 2 ]
    , tuple [ tupleItems [ integer [], number [], string [] ], minItems 2 ]
    , tuple [ tupleItems [ integer [], number [], string [] ], minItems 3 ]
    , tuple [ tupleItems [ integer [], number [], string [] ], minItems 4 ]
    , tuple [ additionalItems (number []), minItems 1 ]
    , tuple [ additionalItems (number []), minItems 2 ]
    , tuple [ tupleItems [ integer [], number [] ], additionalItems (string []), minItems 1 ]
    , tuple [ tupleItems [ integer [], number [] ], additionalItems (string []), minItems 2 ]
    , tuple [ tupleItems [ integer [], number [] ], additionalItems (string []), minItems 3 ]
    , tuple [ tupleItems [ integer [], number [] ], additionalItems (string []), minItems 4 ]
    , tuple [ minItems 0, maxItems 0 ]
    , tuple [ minItems 0, maxItems 1 ]
    , tuple [ minItems 1, maxItems 2 ]
    , tuple [ minItems 2, maxItems 2 ]
    , tuple [ minItems 3, maxItems 10 ]
    , tuple [ tupleItems [ integer [], number [], string [] ], minItems 0, maxItems 0 ]
    , tuple [ tupleItems [ integer [], number [], string [] ], minItems 0, maxItems 1 ]
    , tuple [ tupleItems [ integer [], number [], string [] ], minItems 1, maxItems 2 ]
    , tuple [ tupleItems [ integer [], number [], string [] ], minItems 2, maxItems 2 ]
    , tuple [ tupleItems [ integer [], number [], string [] ], minItems 3, maxItems 10 ]
    , tuple [ additionalItems (number []), minItems 0, maxItems 0 ]
    , tuple [ additionalItems (number []), minItems 0, maxItems 1 ]
    , tuple [ additionalItems (number []), minItems 1, maxItems 2 ]
    , tuple [ additionalItems (number []), minItems 2, maxItems 2 ]
    , tuple [ additionalItems (number []), minItems 3, maxItems 10 ]
    , tuple [ tupleItems [ integer [], number [] ], additionalItems (string []), minItems 0, maxItems 0 ]
    , tuple [ tupleItems [ integer [], number [] ], additionalItems (string []), minItems 0, maxItems 1 ]
    , tuple [ tupleItems [ integer [], number [] ], additionalItems (string []), minItems 1, maxItems 2 ]
    , tuple [ tupleItems [ integer [], number [] ], additionalItems (string []), minItems 2, maxItems 2 ]
    , tuple [ tupleItems [ integer [], number [] ], additionalItems (string []), minItems 3, maxItems 10 ]
    ]


{-| Test a schema fuzzer by checking all the values it produces conform to
the schema it is based on.
-}
testSchemaFuzzer : Schema -> Test
testSchemaFuzzer schema =
    fuzz (schemaValue schema)
        ("fuzzer works with schema: " ++ toString schema)
    <|
        \value ->
            JsonSchema.Validator.validate schema value
                |> Expect.equal []
