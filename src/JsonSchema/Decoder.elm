module JsonSchema.Decoder exposing (decoder)

{-| Decoding a JSON Schema to a `JsonSchema.Schema`

@docs decoder

-}

import Dict exposing (Dict)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import JsonSchema.Model as Model


type alias ObjectProperties =
    List (Model.ObjectProperty Model.NoDefinitions)


{-| Decoder for a JSON Schema
-}
decoder : Decoder Model.Schema
decoder =
    map2 Model.fromSubSchema definitionsDecoder schemaDecoder


definitionsDecoder : Decoder Model.Definitions
definitionsDecoder =
    field "definitions"
        (keyValuePairs schemaDecoder
            |> map (List.map (Tuple.mapFirst ((++) "#/definitions/")) >> Dict.fromList)
        )
        |> maybe
        |> map (Maybe.withDefault Dict.empty)


schemaDecoder : Decoder Model.SubSchema
schemaDecoder =
    lazy
        (\_ ->
            oneOf
                [ decode Model.ObjectSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> optional "examples" (list value) []
                    |> custom objectPropertiesDecoder
                    |> maybeOptional "minProperties" int
                    |> maybeOptional "maxProperties" int
                    |> withType "object"
                    |> hardcoded Model.NoDefinitions
                    |> map Model.Object
                , decode Model.ArraySchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> optional "examples" (list value) []
                    |> maybeOptional "items" schemaDecoder
                    |> maybeOptional "minItems" int
                    |> maybeOptional "maxItems" int
                    |> withType "array"
                    |> hardcoded Model.NoDefinitions
                    |> map Model.Array
                , decode Model.StringSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> optional "examples" (list value) []
                    |> maybeOptional "enum" (list string)
                    |> maybeOptional "minLength" int
                    |> maybeOptional "maxLength" int
                    |> maybeOptional "pattern" string
                    |> maybeOptional "format" (map stringFormat string)
                    |> withType "string"
                    |> map Model.String
                , decode Model.IntegerSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> optional "examples" (list value) []
                    |> maybeOptional "enum" (list int)
                    |> maybeOptional "minimum" int
                    |> maybeOptional "maximum" int
                    |> withType "integer"
                    |> map Model.Integer
                , decode Model.NumberSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> optional "examples" (list value) []
                    |> maybeOptional "enum" (list float)
                    |> maybeOptional "minimum" float
                    |> maybeOptional "maximum" float
                    |> withType "number"
                    |> map Model.Number
                , decode Model.BooleanSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> optional "examples" (list value) []
                    |> maybeOptional "enum" (list bool)
                    |> withType "boolean"
                    |> map Model.Boolean
                , decode Model.NullSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> optional "examples" (list value) []
                    |> withType "null"
                    |> map Model.Null
                , decode Model.RefSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> optional "examples" (list value) []
                    |> required "$ref" string
                    |> hardcoded Model.NoDefinitions
                    |> map Model.Ref
                , decode Model.BaseCombinatorSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> optional "examples" (list value) []
                    |> required "oneOf" (list schemaDecoder)
                    |> hardcoded Model.NoDefinitions
                    |> map Model.OneOf
                , decode Model.BaseCombinatorSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> optional "examples" (list value) []
                    |> required "anyOf" (list schemaDecoder)
                    |> hardcoded Model.NoDefinitions
                    |> map Model.AnyOf
                , decode Model.BaseCombinatorSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> optional "examples" (list value) []
                    |> required "allOf" (list schemaDecoder)
                    |> hardcoded Model.NoDefinitions
                    |> map Model.AllOf
                , map Model.Fallback value
                ]
        )


objectPropertiesDecoder : Decoder ObjectProperties
objectPropertiesDecoder =
    decode toObjectProperties
        |> optional "properties" (keyValuePairs schemaDecoder) []
        |> optional "required" (list string) []


toObjectProperties : List ( String, Model.SubSchema ) -> List String -> ObjectProperties
toObjectProperties propertyList required =
    Debug.crash "TODO"


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


stringFormat : String -> Model.StringFormat
stringFormat format =
    case format of
        "date-time" ->
            Model.DateTime

        "email" ->
            Model.Email

        "hostname" ->
            Model.Hostname

        "ipv4" ->
            Model.Ipv4

        "ipv6" ->
            Model.Ipv6

        "uri" ->
            Model.Uri

        _ ->
            Model.Custom format
