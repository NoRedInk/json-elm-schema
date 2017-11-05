module JsonSchema exposing (Schema, allOf, anyOf, array, boolean, customFormat, dateTime, description, email, enum, examples, format, hostname, integer, ipv4, ipv6, items, maxItems, maxLength, maxProperties, maximum, minItems, minLength, minProperties, minimum, null, number, object, oneOf, optional, pattern, properties, recurse, required, string, title, uri)

{-| This library allows you to write your json schema files in elm, preventing inadvertent errors.


# General

@docs Schema


# Schema types

@docs object, array, string, integer, number, boolean, null, oneOf, allOf, anyOf


# Keywords

@docs title, description, enum, examples, minimum, maximum, properties, items, minItems, maxItems, minLength, maxLength, pattern, format, minProperties, maxProperties


# Property constructors

@docs required, optional


# String formats

@docs dateTime, email, hostname, ipv4, ipv6, uri, customFormat


# Helpers

@docs recurse

-}

import Dict exposing (Dict)
import Json.Encode as Encode
import JsonSchema.Model exposing (..)


{-| A type representing a json schema.
-}
type alias Schema =
    JsonSchema.Model.Schema


defaultBaseSchema : BaseSchema {}
defaultBaseSchema =
    { title = Nothing
    , description = Nothing
    , examples = []
    }


defaultObject : ObjectSchema Definitions
defaultObject =
    { title = Nothing
    , description = Nothing
    , properties = []
    , minProperties = Nothing
    , maxProperties = Nothing
    , examples = []
    , definitions = Dict.empty
    }


defaultArray : ArraySchema Definitions
defaultArray =
    { title = Nothing
    , description = Nothing
    , items = Nothing
    , minItems = Nothing
    , maxItems = Nothing
    , examples = []
    , definitions = Dict.empty
    }


defaultString : StringSchema
defaultString =
    { title = Nothing
    , description = Nothing
    , enum = Nothing
    , minLength = Nothing
    , maxLength = Nothing
    , pattern = Nothing
    , format = Nothing
    , examples = []
    }


defaultInteger : IntegerSchema
defaultInteger =
    { title = Nothing
    , description = Nothing
    , enum = Nothing
    , minimum = Nothing
    , maximum = Nothing
    , examples = []
    }


defaultNumber : NumberSchema
defaultNumber =
    { title = Nothing
    , description = Nothing
    , enum = Nothing
    , minimum = Nothing
    , maximum = Nothing
    , examples = []
    }


defaultBoolean : BooleanSchema
defaultBoolean =
    { title = Nothing
    , description = Nothing
    , enum = Nothing
    , examples = []
    }


defaultCombinatorSchema : BaseCombinatorSchema NoDefinitions
defaultCombinatorSchema =
    { title = Nothing
    , description = Nothing
    , subSchemas = []
    , examples = []
    , definitions = NoDefinitions
    }


type alias BaseSchemaProperty extras =
    BaseSchema extras -> BaseSchema extras


type alias WithEnumSchemaProperty primitive extras =
    WithEnumSchema primitive extras -> WithEnumSchema primitive extras


type alias ObjectSchemaProperty definitions =
    ObjectSchema definitions -> ObjectSchema definitions


type alias ArraySchemaProperty definitions =
    ArraySchema definitions -> ArraySchema definitions


type alias StringSchemaProperty =
    StringSchema -> StringSchema


type alias BaseNumberSchemaProperty num =
    BaseNumberSchema num -> BaseNumberSchema num


type alias IntegerSchemaProperty =
    IntegerSchema -> IntegerSchema


type alias NumberSchemaProperty =
    NumberSchema -> NumberSchema


type alias BooleanSchemaProperty =
    BooleanSchema -> BooleanSchema


type alias NullSchemaProperty =
    NullSchema -> NullSchema


type alias BaseCombinatorSchemaProperty definitions =
    BaseCombinatorSchema definitions -> BaseCombinatorSchema definitions


{-| Create a required property.
-}
required : String -> Schema -> ObjectProperty Definitions
required =
    Required


{-| Create an optional property.
-}
optional : String -> Schema -> ObjectProperty Definitions
optional =
    Optional


{-| `title` keyword.
-}
title : String -> BaseSchemaProperty extras
title text schema =
    { schema | title = Just text }


{-| `description` keyword
-}
description : String -> BaseSchemaProperty extras
description text schema =
    { schema | description = Just text }


{-| `enum` keyword
-}
enum : List primitive -> WithEnumSchemaProperty primitive extras
enum allowedValues schema =
    { schema | enum = Just allowedValues }


{-| `minimum` keyword
-}
minimum : num -> BaseNumberSchemaProperty num
minimum number schema =
    { schema | minimum = Just number }


{-| `maximum` keyword
-}
maximum : num -> BaseNumberSchemaProperty num
maximum number schema =
    { schema | maximum = Just number }


{-| `properties` keyword
-}
properties : List (ObjectProperty Definitions) -> ObjectSchemaProperty Definitions
properties definitionProperties schema =
    List.map splitDefinitions definitionProperties
        |> List.unzip
        |> Tuple.mapFirst concatUnion
        |> (\( definitions, properties ) ->
                { schema
                    | properties = schema.properties ++ properties
                    , definitions = Dict.union schema.definitions definitions
                }
           )



-- { schema | properties = schema.properties ++ properties }


{-| `examples` keyword
-}
examples : (a -> Encode.Value) -> List a -> BaseSchemaProperty extras
examples encoder ex schema =
    { schema | examples = List.map encoder ex }


{-| `minProperties` keyword
-}
minProperties : Int -> ObjectSchemaProperty definitions
minProperties min schema =
    { schema | minProperties = Just min }


{-| `maxProperties` keyword
-}
maxProperties : Int -> ObjectSchemaProperty definitions
maxProperties max schema =
    { schema | maxProperties = Just max }


{-| `items` keyword
-}
items : Schema -> ArraySchemaProperty Definitions
items itemSchemaWithDefs schema =
    let
        ( definitions, itemSchema ) =
            toSubSchema itemSchemaWithDefs
    in
    { schema
        | items = Just itemSchema
        , definitions = Dict.union schema.definitions definitions
    }


{-| `minItems` keyword
-}
minItems : Int -> ArraySchemaProperty definitions
minItems min schema =
    { schema | minItems = Just min }


{-| `maxItems` keyword
-}
maxItems : Int -> ArraySchemaProperty definitions
maxItems max schema =
    { schema | maxItems = Just max }


{-| `minLength` keyword
-}
minLength : Int -> StringSchemaProperty
minLength length schema =
    { schema | minLength = Just length }


{-| `maxLength` keyword
-}
maxLength : Int -> StringSchemaProperty
maxLength length schema =
    { schema | maxLength = Just length }


{-| `pattern` keyword
-}
pattern : String -> StringSchemaProperty
pattern regex schema =
    { schema | pattern = Just regex }


{-| `format` keyword
-}
format : StringFormat -> StringSchemaProperty
format formatOption schema =
    { schema | format = Just formatOption }


{-| `date-time` format
-}
dateTime : StringFormat
dateTime =
    DateTime


{-| `email` format
-}
email : StringFormat
email =
    Email


{-| `hostname` format
-}
hostname : StringFormat
hostname =
    Hostname


{-| `ipv4` format
-}
ipv4 : StringFormat
ipv4 =
    Ipv4


{-| `ipv6` format
-}
ipv6 : StringFormat
ipv6 =
    Ipv6


{-| `uri` format
-}
uri : StringFormat
uri =
    Uri


{-| Create a custom value to pass to `format`.
This allows you to use any format supported by your json schema validator.
-}
customFormat : String -> StringFormat
customFormat =
    Custom


{-| Create an object type schema.
-}
object : List (ObjectSchemaProperty Definitions) -> Schema
object props =
    List.foldl (<|) defaultObject props
        |> Object


{-| Create an array type schema.
-}
array : List (ArraySchemaProperty Definitions) -> Schema
array props =
    List.foldl (<|) defaultArray props
        |> Array


{-| Create a string type schema.
-}
string : List StringSchemaProperty -> Schema
string props =
    List.foldl (<|) defaultString props
        |> String


{-| Create an integer type schema.
-}
integer : List IntegerSchemaProperty -> Schema
integer props =
    List.foldl (<|) defaultInteger props
        |> Integer


{-| Create a number type schema.
-}
number : List NumberSchemaProperty -> Schema
number props =
    List.foldl (<|) defaultNumber props
        |> Number


{-| Create a boolean type schema.
-}
boolean : List BooleanSchemaProperty -> Schema
boolean props =
    List.foldl (<|) defaultBoolean props
        |> Boolean


{-| Create a null type schema.
-}
null : List NullSchemaProperty -> Schema
null props =
    List.foldl (<|) defaultBaseSchema props
        |> Null


{-| Create a oneOf type schema.
-}
oneOf : List (BaseCombinatorSchemaProperty NoDefinitions) -> List Schema -> Schema
oneOf props childSchemas =
    let
        base : BaseCombinatorSchema NoDefinitions
        base =
            List.foldl (<|) defaultCombinatorSchema props

        combiner : List SubSchema -> SubSchema
        combiner subSchemas =
            OneOf { base | subSchemas = subSchemas }
    in
    combine combiner childSchemas


{-| Create an allOf type schema.
-}
allOf : List (BaseCombinatorSchemaProperty NoDefinitions) -> List Schema -> Schema
allOf props childSchemas =
    let
        base : BaseCombinatorSchema NoDefinitions
        base =
            List.foldl (<|) defaultCombinatorSchema props

        combiner : List SubSchema -> SubSchema
        combiner subSchemas =
            AllOf { base | subSchemas = subSchemas }
    in
    combine combiner childSchemas


{-| Create an anyOf type schema.
-}
anyOf : List (BaseCombinatorSchemaProperty NoDefinitions) -> List Schema -> Schema
anyOf props childSchemas =
    let
        base : BaseCombinatorSchema NoDefinitions
        base =
            List.foldl (<|) defaultCombinatorSchema props

        combiner : List SubSchema -> SubSchema
        combiner subSchemas =
            AnyOf { base | subSchemas = subSchemas }
    in
    combine combiner childSchemas


{-| Create a recursive schema.

Pass the schema a unique name and a function that returns the recursive schema.
The function is passed a reference to itself, which can be embeded anywhere in the returned schema.

-}
recurse : String -> (Schema -> Schema) -> Schema
recurse name schemaCreator =
    let
        refName : String
        refName =
            "#/definitions/" ++ name

        refSchema : Schema
        refSchema =
            Ref
                { title = Nothing
                , description = Nothing
                , examples = []
                , ref = refName
                , definitions = Dict.empty
                }

        ( definitions, createdSchema ) =
            schemaCreator refSchema
                |> toSubSchema
    in
    Ref
        { title = Nothing
        , description = Nothing
        , examples = []
        , ref = refName
        , definitions = Dict.insert refName createdSchema definitions
        }


splitDefinitions : ObjectProperty Definitions -> ( Definitions, ObjectProperty NoDefinitions )
splitDefinitions objectProperty =
    case objectProperty of
        Required name schema ->
            toSubSchema schema
                |> Tuple.mapSecond (Required name)

        Optional name schema ->
            toSubSchema schema
                |> Tuple.mapSecond (Optional name)


combine : (List SubSchema -> SubSchema) -> List Schema -> Schema
combine combiner schemas =
    List.map toSubSchema schemas
        |> List.unzip
        |> Tuple.mapFirst concatUnion
        |> Tuple.mapSecond combiner
        |> uncurry fromSubSchema


concatUnion : List (Dict comparable b) -> Dict comparable b
concatUnion dicts =
    List.foldl Dict.union Dict.empty dicts
