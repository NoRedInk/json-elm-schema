module JsonSchema.Decoder exposing (decoder)

{-| Decoding a JSON Schema to an `JsonSchema.Schema`

@docs decoder

-}

import Dict exposing (Dict)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import JsonSchema.Model as Model exposing (Schema)
import Set


type PreSchema
    = Object PreObjectSchema
    | Array PreArraySchema
    | Tuple PreTupleSchema
    | String PreStringSchema
    | Integer PreIntegerSchema
    | Number PreNumberSchema
    | Boolean PreBooleanSchema
    | Null PreBaseSchema
    | Ref PreRefSchema
    | OneOf PreBaseCombinatorSchema
    | AnyOf PreBaseCombinatorSchema
    | AllOf PreBaseCombinatorSchema
    | Fallback Value


type alias PreObjectSchema =
    { title : Maybe String
    , description : Maybe String
    , properties : Dict String PreSchema
    , required : List String
    , minProperties : Maybe Int
    , maxProperties : Maybe Int
    , examples : List Encode.Value
    }


type alias PreArraySchema =
    { title : Maybe String
    , description : Maybe String
    , items : Maybe PreSchema
    , minItems : Maybe Int
    , maxItems : Maybe Int
    , examples : List Encode.Value
    }

type alias PreTupleSchema =
    { title : Maybe String
    , description : Maybe String
    , items : Maybe (List PreSchema)
    , minItems : Maybe Int
    , maxItems : Maybe Int
    , examples : List Encode.Value
    }

type alias PreStringSchema =
    { title : Maybe String
    , description : Maybe String
    , minLength : Maybe Int
    , maxLength : Maybe Int
    , pattern : Maybe String
    , format : Maybe String
    , enum : Maybe (List String)
    , examples : List Encode.Value
    }


type alias PreIntegerSchema =
    { title : Maybe String
    , description : Maybe String
    , minimum : Maybe Int
    , maximum : Maybe Int
    , enum : Maybe (List Int)
    , examples : List Encode.Value
    }


type alias PreNumberSchema =
    { title : Maybe String
    , description : Maybe String
    , minimum : Maybe Float
    , maximum : Maybe Float
    , enum : Maybe (List Float)
    , examples : List Encode.Value
    }


type alias PreBooleanSchema =
    { title : Maybe String
    , description : Maybe String
    , enum : Maybe (List Bool)
    , examples : List Encode.Value
    }


type alias PreBaseSchema =
    { title : Maybe String
    , description : Maybe String
    , examples : List Encode.Value
    }


type alias PreRefSchema =
    { title : Maybe String
    , description : Maybe String
    , ref : String
    , examples : List Encode.Value
    }


type alias PreBaseCombinatorSchema =
    { title : Maybe String
    , description : Maybe String
    , subSchemas : List PreSchema
    , examples : List Encode.Value
    }


type alias Definitions =
    Dict String PreSchema


{-| Decoder for a JSON Schema
-}
decoder : Decoder Schema
decoder =
    map2 toSchema definitionsDecoder preSchemaDecoder


definitionsDecoder : Decoder Definitions
definitionsDecoder =
    field "definitions"
        (keyValuePairs preSchemaDecoder
            |> map (List.map (Tuple.mapFirst ((++) "#/definitions/")) >> Dict.fromList)
        )
        |> maybe
        |> map (Maybe.withDefault Dict.empty)


preSchemaDecoder : Decoder PreSchema
preSchemaDecoder =
    lazy
        (\_ ->
            oneOf
                [ decode PreObjectSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> optional "properties" (dict preSchemaDecoder) Dict.empty
                    |> optional "required" (list string) []
                    |> maybeOptional "minProperties" int
                    |> maybeOptional "maxProperties" int
                    |> optional "examples" (list value) []
                    |> withType "object"
                    |> map Object
                , decode PreArraySchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> maybeOptional "items" preSchemaDecoder
                    |> maybeOptional "minItems" int
                    |> maybeOptional "maxItems" int
                    |> optional "examples" (list value) []
                    |> withType "array"
                    |> map Array
                , decode PreTupleSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> maybeOptional "items" (list preSchemaDecoder)
                    |> maybeOptional "minItems" int
                    |> maybeOptional "maxItems" int
                    |> optional "examples" (list value) []
                    |> withType "array"
                    |> map Tuple
                , decode PreStringSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> maybeOptional "minLength" int
                    |> maybeOptional "maxLength" int
                    |> maybeOptional "pattern" string
                    |> maybeOptional "format" string
                    |> maybeOptional "enum" (list string)
                    |> optional "examples" (list value) []
                    |> withType "string"
                    |> map String
                , decode PreIntegerSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> maybeOptional "minimum" int
                    |> maybeOptional "maximum" int
                    |> maybeOptional "enum" (list int)
                    |> optional "examples" (list value) []
                    |> withType "integer"
                    |> map Integer
                , decode PreNumberSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> maybeOptional "minimum" float
                    |> maybeOptional "maximum" float
                    |> maybeOptional "enum" (list float)
                    |> optional "examples" (list value) []
                    |> withType "number"
                    |> map Number
                , decode PreBooleanSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> maybeOptional "enum" (list bool)
                    |> optional "examples" (list value) []
                    |> withType "boolean"
                    |> map Boolean
                , decode PreBaseSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> optional "examples" (list value) []
                    |> withType "null"
                    |> map Null
                , decode PreRefSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> required "$ref" string
                    |> optional "examples" (list value) []
                    |> map Ref
                , decode PreBaseCombinatorSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> required "oneOf" (list preSchemaDecoder)
                    |> optional "examples" (list value) []
                    |> map OneOf
                , decode PreBaseCombinatorSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> required "anyOf" (list preSchemaDecoder)
                    |> optional "examples" (list value) []
                    |> map AnyOf
                , decode PreBaseCombinatorSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> required "allOf" (list preSchemaDecoder)
                    |> optional "examples" (list value) []
                    |> map AllOf
                , map Fallback value
                ]
        )


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


toSchema : Definitions -> PreSchema -> Schema
toSchema definitions preSchema =
    case preSchema of
        Object { title, description, required, properties, examples, minProperties, maxProperties } ->
            let
                requiredSet =
                    Set.fromList required

                objectProperty ( key, preSchema ) =
                    toSchema definitions preSchema
                        |> (if Set.member key requiredSet then
                                Model.Required key
                            else
                                Model.Optional key
                           )

                schemaProperties =
                    Dict.toList properties
                        |> List.map objectProperty
            in
            Model.Object
                { properties = schemaProperties
                , title = title
                , description = description
                , minProperties = minProperties
                , maxProperties = maxProperties
                , examples = examples
                }

        Array content ->
            Model.Array
                { content | items = Maybe.map (toSchema definitions) content.items }

        Tuple content ->
            Model.Tuple
                { content | items = Maybe.map (List.map (toSchema definitions)) content.items }

        String content ->
            Model.String
                { content
                    | format = Maybe.map stringFormat content.format
                }

        Integer content ->
            Model.Integer content

        Number content ->
            Model.Number content

        Boolean content ->
            Model.Boolean content

        Null content ->
            Model.Null content

        OneOf content ->
            Model.OneOf { content | subSchemas = List.map (toSchema definitions) content.subSchemas }

        AnyOf content ->
            Model.AnyOf { content | subSchemas = List.map (toSchema definitions) content.subSchemas }

        AllOf content ->
            Model.AllOf { content | subSchemas = List.map (toSchema definitions) content.subSchemas }

        Ref content ->
            Dict.get content.ref definitions
                |> Maybe.map (\preSchema -> Model.Lazy (\_ -> toSchema definitions preSchema))
                |> Maybe.withDefault (Model.Ref content)

        Fallback value ->
            Model.Fallback value


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
