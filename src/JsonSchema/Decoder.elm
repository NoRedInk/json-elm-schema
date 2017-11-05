module JsonSchema.Decoder exposing (decoder)

{-| Decoding a JSON Schema to a `JsonSchema.Schema`

@docs decoder

-}

import Dict exposing (Dict)
import Json.Decode exposing (Decoder, andThen, bool, fail, field, float, int, keyValuePairs, lazy, list, map, map2, maybe, nullable, oneOf, string, succeed, value)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, required)
import JsonSchema.Model exposing (..)


type alias ObjectProperties =
    List (ObjectProperty NoDefinitions)


{-| Decoder for a JSON Schema
-}
decoder : Decoder Schema
decoder =
    map2 fromSubSchema definitionsDecoder schemaDecoder


definitionsDecoder : Decoder Definitions
definitionsDecoder =
    field "definitions"
        (keyValuePairs schemaDecoder
            |> map (List.map (Tuple.mapFirst ((++) "#/definitions/")) >> Dict.fromList)
        )
        |> maybe
        |> map (Maybe.withDefault Dict.empty)


schemaDecoder : Decoder SubSchema
schemaDecoder =
    lazy
        (\_ ->
            oneOf
                [ decode ObjectSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> optional "examples" (list value) []
                    |> custom objectPropertiesDecoder
                    |> maybeOptional "minProperties" int
                    |> maybeOptional "maxProperties" int
                    |> withType "object"
                    |> hardcoded NoDefinitions
                    |> map Object
                , decode ArraySchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> optional "examples" (list value) []
                    |> maybeOptional "items" schemaDecoder
                    |> maybeOptional "minItems" int
                    |> maybeOptional "maxItems" int
                    |> withType "array"
                    |> hardcoded NoDefinitions
                    |> map Array
                , decode StringSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> optional "examples" (list value) []
                    |> maybeOptional "enum" (list string)
                    |> maybeOptional "minLength" int
                    |> maybeOptional "maxLength" int
                    |> maybeOptional "pattern" string
                    |> maybeOptional "format" (map stringFormat string)
                    |> withType "string"
                    |> map String
                , decode IntegerSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> optional "examples" (list value) []
                    |> maybeOptional "enum" (list int)
                    |> maybeOptional "minimum" int
                    |> maybeOptional "maximum" int
                    |> withType "integer"
                    |> map Integer
                , decode NumberSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> optional "examples" (list value) []
                    |> maybeOptional "enum" (list float)
                    |> maybeOptional "minimum" float
                    |> maybeOptional "maximum" float
                    |> withType "number"
                    |> map Number
                , decode BooleanSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> optional "examples" (list value) []
                    |> maybeOptional "enum" (list bool)
                    |> withType "boolean"
                    |> map Boolean
                , decode NullSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> optional "examples" (list value) []
                    |> withType "null"
                    |> map Null
                , decode RefSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> optional "examples" (list value) []
                    |> required "$ref" string
                    |> hardcoded NoDefinitions
                    |> map Ref
                , decode BaseCombinatorSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> optional "examples" (list value) []
                    |> required "oneOf" (list schemaDecoder)
                    |> hardcoded NoDefinitions
                    |> map OneOf
                , decode BaseCombinatorSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> optional "examples" (list value) []
                    |> required "anyOf" (list schemaDecoder)
                    |> hardcoded NoDefinitions
                    |> map AnyOf
                , decode BaseCombinatorSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> optional "examples" (list value) []
                    |> required "allOf" (list schemaDecoder)
                    |> hardcoded NoDefinitions
                    |> map AllOf
                , map Fallback value
                ]
        )


objectPropertiesDecoder : Decoder ObjectProperties
objectPropertiesDecoder =
    decode toObjectProperties
        |> optional "properties" (keyValuePairs schemaDecoder) []
        |> optional "required" (list string) []


toObjectProperties : List ( String, SubSchema ) -> List String -> ObjectProperties
toObjectProperties propertyList required =
    let
        toProperty : ( String, SubSchema ) -> ObjectProperty NoDefinitions
        toProperty ( name, propertySchema ) =
            if List.member name required then
                Required name propertySchema
            else
                Optional name propertySchema
    in
    -- keyValuePairs seems to return JSON (key, value) pairs in reverse order.
    -- This is still the same JSON sementically (keys are unordered), but we'd
    -- Prefer a decode -> encode cycle not to change the JSON in any way.
    List.map toProperty (List.reverse propertyList)


{-| Ensure a decoder has a specific "type" value.
-}
withType : String -> Decoder a -> Decoder a
withType typeString decoder =
    field "type" (constant typeString string)
        |> andThen (always decoder)


{-| Decode into a specific expected value or fail.
-}
constant : a -> Decoder a -> Decoder a
constant expectedValue decoder =
    decoder
        |> andThen
            (\actualValue ->
                if actualValue == expectedValue then
                    succeed actualValue
                else
                    fail <| "Expected value: " ++ toString expectedValue ++ " but got value: " ++ toString actualValue
            )


maybeOptional : String -> Decoder a -> Decoder (Maybe a -> b) -> Decoder b
maybeOptional key decoder =
    optional key (nullable decoder) Nothing


stringFormat : String -> StringFormat
stringFormat format =
    case format of
        "date-time" ->
            DateTime

        "email" ->
            Email

        "hostname" ->
            Hostname

        "ipv4" ->
            Ipv4

        "ipv6" ->
            Ipv6

        "uri" ->
            Uri

        _ ->
            Custom format
