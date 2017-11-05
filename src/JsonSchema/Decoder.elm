module JsonSchema.Decoder exposing (decoder)

{-| Decoding a JSON Schema to a `JsonSchema.Schema`

@docs decoder

-}

import Dict exposing (Dict)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import JsonSchema.Model as Model


type alias ObjectSchema =
    { title : Maybe String
    , description : Maybe String
    , properties : ObjectProperties
    , minProperties : Maybe Int
    , maxProperties : Maybe Int
    , examples : List Encode.Value
    , definitions : Model.NoDefinitions
    }


type alias ArraySchema =
    { title : Maybe String
    , description : Maybe String
    , items : Maybe Model.SubSchema
    , minItems : Maybe Int
    , maxItems : Maybe Int
    , examples : List Encode.Value
    , definitions : Model.NoDefinitions
    }


type alias StringSchema =
    { title : Maybe String
    , description : Maybe String
    , minLength : Maybe Int
    , maxLength : Maybe Int
    , pattern : Maybe String
    , format : Maybe Model.StringFormat
    , enum : Maybe (List String)
    , examples : List Encode.Value
    }


type alias IntegerSchema =
    { title : Maybe String
    , description : Maybe String
    , minimum : Maybe Int
    , maximum : Maybe Int
    , enum : Maybe (List Int)
    , examples : List Encode.Value
    }


type alias NumberSchema =
    { title : Maybe String
    , description : Maybe String
    , minimum : Maybe Float
    , maximum : Maybe Float
    , enum : Maybe (List Float)
    , examples : List Encode.Value
    }


type alias BooleanSchema =
    { title : Maybe String
    , description : Maybe String
    , enum : Maybe (List Bool)
    , examples : List Encode.Value
    }


type alias BaseSchema =
    { title : Maybe String
    , description : Maybe String
    , examples : List Encode.Value
    }


type alias RefSchema =
    { title : Maybe String
    , description : Maybe String
    , ref : String
    , examples : List Encode.Value
    , definitions : Model.NoDefinitions
    }


type alias BaseCombinatorSchema =
    { title : Maybe String
    , description : Maybe String
    , subSchemas : List Model.SubSchema
    , examples : List Encode.Value
    , definitions : Model.NoDefinitions
    }


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
                [ decode ObjectSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> custom objectPropertiesDecoder
                    |> maybeOptional "minProperties" int
                    |> maybeOptional "maxProperties" int
                    |> optional "examples" (list value) []
                    |> withType "object"
                    |> hardcoded Model.NoDefinitions
                    |> map Model.Object
                , decode ArraySchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> maybeOptional "items" schemaDecoder
                    |> maybeOptional "minItems" int
                    |> maybeOptional "maxItems" int
                    |> optional "examples" (list value) []
                    |> withType "array"
                    |> hardcoded Model.NoDefinitions
                    |> map Model.Array
                , decode StringSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> maybeOptional "minLength" int
                    |> maybeOptional "maxLength" int
                    |> maybeOptional "pattern" string
                    |> maybeOptional "format" (map stringFormat string)
                    |> maybeOptional "enum" (list string)
                    |> optional "examples" (list value) []
                    |> withType "string"
                    |> map Model.String
                , decode IntegerSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> maybeOptional "minimum" int
                    |> maybeOptional "maximum" int
                    |> maybeOptional "enum" (list int)
                    |> optional "examples" (list value) []
                    |> withType "integer"
                    |> map Model.Integer
                , decode NumberSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> maybeOptional "minimum" float
                    |> maybeOptional "maximum" float
                    |> maybeOptional "enum" (list float)
                    |> optional "examples" (list value) []
                    |> withType "number"
                    |> map Model.Number
                , decode BooleanSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> maybeOptional "enum" (list bool)
                    |> optional "examples" (list value) []
                    |> withType "boolean"
                    |> map Model.Boolean
                , decode BaseSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> optional "examples" (list value) []
                    |> withType "null"
                    |> map Model.Null
                , decode RefSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> required "$ref" string
                    |> optional "examples" (list value) []
                    |> hardcoded Model.NoDefinitions
                    |> map Model.Ref
                , decode BaseCombinatorSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> required "oneOf" (list schemaDecoder)
                    |> optional "examples" (list value) []
                    |> hardcoded Model.NoDefinitions
                    |> map Model.OneOf
                , decode BaseCombinatorSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> required "anyOf" (list schemaDecoder)
                    |> optional "examples" (list value) []
                    |> hardcoded Model.NoDefinitions
                    |> map Model.AnyOf
                , decode BaseCombinatorSchema
                    |> maybeOptional "title" string
                    |> maybeOptional "description" string
                    |> required "allOf" (list schemaDecoder)
                    |> optional "examples" (list value) []
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
