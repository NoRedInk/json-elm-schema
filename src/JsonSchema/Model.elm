module JsonSchema.Model exposing (..)

import Dict exposing (Dict)
import Json.Decode
import Json.Encode as Encode


type alias Schema =
    SchemaTemplate Definitions


type alias SubSchema =
    SchemaTemplate NoDefinitions


type SchemaTemplate definitions
    = Object (ObjectSchema definitions)
    | Array (ArraySchema definitions)
    | String StringSchema
    | Integer IntegerSchema
    | Number NumberSchema
    | Boolean BooleanSchema
    | Null NullSchema
    | Ref (RefSchema definitions)
    | OneOf (BaseCombinatorSchema definitions)
    | AnyOf (BaseCombinatorSchema definitions)
    | AllOf (BaseCombinatorSchema definitions)
    | Fallback Json.Decode.Value


type alias Definitions =
    Dict String SubSchema


type NoDefinitions
    = NoDefinitions


type alias BaseSchema extras =
    { extras
        | title : Maybe String
        , description : Maybe String
        , examples : List Encode.Value
    }


type alias WithEnumSchema primitive extras =
    { extras
        | enum : Maybe (List primitive)
    }


type alias ObjectSchema definitions =
    BaseSchema
        { properties : List (ObjectProperty NoDefinitions)
        , minProperties : Maybe Int
        , maxProperties : Maybe Int
        , definitions : definitions
        }


type alias ArraySchema definitions =
    BaseSchema
        { items : Maybe SubSchema
        , minItems : Maybe Int
        , maxItems : Maybe Int
        , definitions : definitions
        }


type alias BaseNumberSchema num =
    WithEnumSchema num
        (BaseSchema
            { minimum : Maybe num
            , maximum : Maybe num
            }
        )


type alias IntegerSchema =
    BaseNumberSchema Int


type alias NumberSchema =
    BaseNumberSchema Float


type ObjectProperty definitions
    = Required String (SchemaTemplate definitions)
    | Optional String (SchemaTemplate definitions)


type alias StringSchema =
    WithEnumSchema String
        (BaseSchema
            { minLength : Maybe Int
            , maxLength : Maybe Int
            , pattern : Maybe String
            , format : Maybe StringFormat
            }
        )


type alias BooleanSchema =
    WithEnumSchema Bool (BaseSchema {})


type alias NullSchema =
    BaseSchema {}


type alias RefSchema definitions =
    BaseSchema
        { ref : String
        , definitions : definitions
        }


type alias BaseCombinatorSchema definitions =
    BaseSchema
        { subSchemas : List SubSchema
        , definitions : definitions
        }


{-| One of the built-in string formats defined by the json schema specification,
or a custom format your schema validator understands.
-}
type StringFormat
    = DateTime
    | Email
    | Hostname
    | Ipv4
    | Ipv6
    | Uri
    | Custom String


{-| Split a schema into its definitions and subschema components.
-}
toSubSchema : Schema -> ( Definitions, SubSchema )
toSubSchema schema =
    case schema of
        Object objectSchema ->
            ( objectSchema.definitions
            , Object { objectSchema | definitions = NoDefinitions }
            )

        Array arraySchema ->
            ( arraySchema.definitions
            , Array { arraySchema | definitions = NoDefinitions }
            )

        String stringSchema ->
            ( Dict.empty, String stringSchema )

        Integer integerSchema ->
            ( Dict.empty, Integer integerSchema )

        Number numberSchema ->
            ( Dict.empty, Number numberSchema )

        Boolean booleanSchema ->
            ( Dict.empty, Boolean booleanSchema )

        Null nullSchema ->
            ( Dict.empty, Null nullSchema )

        Ref refSchema ->
            ( refSchema.definitions
            , Ref { refSchema | definitions = NoDefinitions }
            )

        OneOf oneOfSchema ->
            ( oneOfSchema.definitions
            , OneOf { oneOfSchema | definitions = NoDefinitions }
            )

        AnyOf anyOfSchema ->
            ( anyOfSchema.definitions
            , AnyOf { anyOfSchema | definitions = NoDefinitions }
            )

        AllOf allOfSchema ->
            ( allOfSchema.definitions
            , AllOf { allOfSchema | definitions = NoDefinitions }
            )

        Fallback fallbackSchema ->
            ( Dict.empty, Fallback fallbackSchema )


{-| Join a subschema with definitions to create a full schema.
-}
fromSubSchema : Definitions -> SubSchema -> Schema
fromSubSchema definitions subSchema =
    case subSchema of
        Object objectSchema ->
            Object { objectSchema | definitions = definitions }

        Array arraySchema ->
            Array { arraySchema | definitions = definitions }

        String stringSchema ->
            String stringSchema

        Integer integerSchema ->
            Integer integerSchema

        Number numberSchema ->
            Number numberSchema

        Boolean booleanSchema ->
            Boolean booleanSchema

        Null nullSchema ->
            Null nullSchema

        Ref refSchema ->
            Ref { refSchema | definitions = definitions }

        OneOf oneOfSchema ->
            OneOf { oneOfSchema | definitions = definitions }

        AnyOf anyOfSchema ->
            AnyOf { anyOfSchema | definitions = definitions }

        AllOf allOfSchema ->
            AllOf { allOfSchema | definitions = definitions }

        Fallback fallbackSchema ->
            Fallback fallbackSchema
