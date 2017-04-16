module JsonSchema.Decoder exposing (decoder)

{-| Decoding a JSON Schema to an `JsonSchema.Schema`

@docs decoder
-}

import JsonSchema.Model as Model exposing (Schema)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Dict exposing (Dict)
import Set


type PreSchema
    = Object PreObjectSchema
    | Array PreArraySchema
    | String PreStringSchema
    | Integer PreIntegerSchema
    | Number PreNumberSchema
    | Boolean PreBaseSchema
    | Null PreBaseSchema
    | OneOf PreBaseCombinatorSchema
    | AnyOf PreBaseCombinatorSchema
    | AllOf PreBaseCombinatorSchema
    | Fallback Value


type alias PreObjectSchema =
    { title : Maybe String
    , description : Maybe String
    , properties : Dict String PreSchema
    , required : List String
    }


type alias PreArraySchema =
    { title : Maybe String
    , description : Maybe String
    , items : Maybe PreSchema
    , minItems : Maybe Int
    , maxItems : Maybe Int
    }


type alias PreStringSchema =
    { title : Maybe String
    , description : Maybe String
    , minLength : Maybe Int
    , maxLength : Maybe Int
    , pattern : Maybe String
    , format : Maybe String
    , enum : Maybe (List String)
    }


type alias PreIntegerSchema =
    { title : Maybe String
    , description : Maybe String
    , minimum : Maybe Int
    , maximum : Maybe Int
    , enum : Maybe (List Int)
    }


type alias PreNumberSchema =
    { title : Maybe String
    , description : Maybe String
    , minimum : Maybe Float
    , maximum : Maybe Float
    , enum : Maybe (List Float)
    }


type alias PreBaseSchema =
    { title : Maybe String
    , description : Maybe String
    }


type alias PreBaseCombinatorSchema =
    { title : Maybe String
    , description : Maybe String
    , subSchemas : List PreSchema
    }


type alias Definitions =
    Dict String PreSchema


{-| Decoder for a JSON Schema
-}
decoder : Decoder Schema
decoder =
    map toSchema preSchemaDecoder


preSchemaDecoder : Decoder PreSchema
preSchemaDecoder =
    lazy
        (\_ ->
            oneOf
                [ decode PreObjectSchema
                    |> maybe "title" string
                    |> maybe "description" string
                    |> optional "properties" (dict preSchemaDecoder) Dict.empty
                    |> optional "required" (list string) []
                    |> withType "object"
                    |> map Object
                , decode PreArraySchema
                    |> maybe "title" string
                    |> maybe "description" string
                    |> maybe "items" preSchemaDecoder
                    |> maybe "minItems" int
                    |> maybe "maxItems" int
                    |> withType "array"
                    |> map Array
                , decode PreStringSchema
                    |> maybe "title" string
                    |> maybe "description" string
                    |> maybe "minLength" int
                    |> maybe "maxLength" int
                    |> maybe "pattern" string
                    |> maybe "format" string
                    |> maybe "enum" (list string)
                    |> withType "string"
                    |> map String
                , decode PreIntegerSchema
                    |> maybe "title" string
                    |> maybe "description" string
                    |> maybe "minimum" int
                    |> maybe "maximum" int
                    |> maybe "enum" (list int)
                    |> withType "integer"
                    |> map Integer
                , decode PreNumberSchema
                    |> maybe "title" string
                    |> maybe "description" string
                    |> maybe "minimum" float
                    |> maybe "maximum" float
                    |> maybe "enum" (list float)
                    |> withType "number"
                    |> map Number
                , decode PreBaseSchema
                    |> maybe "title" string
                    |> maybe "description" string
                    |> withType "boolean"
                    |> map Boolean
                , decode PreBaseSchema
                    |> maybe "title" string
                    |> maybe "description" string
                    |> withType "null"
                    |> map Null
                , decode PreBaseCombinatorSchema
                    |> maybe "title" string
                    |> maybe "description" string
                    |> required "oneOf" (list preSchemaDecoder)
                    |> map OneOf
                , decode PreBaseCombinatorSchema
                    |> maybe "title" string
                    |> maybe "description" string
                    |> required "anyOf" (list preSchemaDecoder)
                    |> map AnyOf
                , decode PreBaseCombinatorSchema
                    |> maybe "title" string
                    |> maybe "description" string
                    |> required "allOf" (list preSchemaDecoder)
                    |> map AllOf
                , map Fallback value
                ]
        )



-- HELPERS --


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
                    fail <| "Expected value: " ++ (toString expectedValue) ++ " but got value: " ++ (toString actualValue)
            )


maybe : String -> Decoder a -> Decoder (Maybe a -> b) -> Decoder b
maybe key decoder =
    optional key (nullable decoder) Nothing



-- CONSTRUCTORS --


toSchema : PreSchema -> Schema
toSchema preSchema =
    case preSchema of
        Object { title, description, required, properties } ->
            let
                requiredSet =
                    Set.fromList required

                objectProperty ( key, preSchema ) =
                    toSchema preSchema
                        |> if Set.member key requiredSet then
                            Model.Required key
                           else
                            Model.Optional key

                schemaProperties =
                    Dict.toList properties
                        |> List.map objectProperty
            in
                Model.Object
                    { properties = schemaProperties
                    , title = title
                    , description = description
                    }

        Array content ->
            Model.Array
                { content | items = Maybe.map toSchema content.items }

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
            Model.OneOf { content | subSchemas = List.map toSchema content.subSchemas }

        AnyOf content ->
            Model.AnyOf { content | subSchemas = List.map toSchema content.subSchemas }

        AllOf content ->
            Model.AllOf { content | subSchemas = List.map toSchema content.subSchemas }

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
