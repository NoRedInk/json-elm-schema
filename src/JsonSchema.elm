module JsonSchema exposing (Schema, additionalItems, allOf, anyOf, array, tuple, boolean, customFormat, dateTime, description, email, enum, examples, format, hostname, integer, ipv4, ipv6, items, lazy, maxItems, maxLength, maxProperties, maximum, minItems, minLength, minProperties, minimum, null, number, object, oneOf, optional, pattern, properties, required, string, title, tupleItems, uri)

{-| This library allows you to write your json schema files in elm, preventing inadvertent errors.


# General

@docs Schema


# Schema types

@docs object, array, tuple, string, integer, number, boolean, null, oneOf, allOf, anyOf, lazy


# Keywords

@docs title, description, enum, examples, minimum, maximum, properties, items, minItems, maxItems, tupleItems, additionalItems, minLength, maxLength, pattern, format, minProperties, maxProperties


# Property constructors

@docs required, optional


# String formats

@docs dateTime, email, hostname, ipv4, ipv6, uri, customFormat

-}

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


defaultObject : ObjectSchema
defaultObject =
    { title = Nothing
    , description = Nothing
    , properties = []
    , minProperties = Nothing
    , maxProperties = Nothing
    , examples = []
    }


defaultArray : ArraySchema
defaultArray =
    { title = Nothing
    , description = Nothing
    , items = Nothing
    , minItems = Nothing
    , maxItems = Nothing
    , examples = []
    }


defaultTuple : TupleSchema
defaultTuple =
    { title = Nothing
    , description = Nothing
    , items = Nothing
    , minItems = Nothing
    , maxItems = Nothing
    , additionalItems = Nothing
    , examples = []
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


defaultCombinatorSchema : BaseCombinatorSchema
defaultCombinatorSchema =
    { title = Nothing
    , description = Nothing
    , subSchemas = []
    , examples = []
    }


type alias BaseSchemaProperty extras =
    BaseSchema extras -> BaseSchema extras


type alias WithEnumSchemaProperty primitive extras =
    WithEnumSchema primitive extras -> WithEnumSchema primitive extras


type alias ObjectSchemaProperty =
    ObjectSchema -> ObjectSchema


type alias ArraySchemaProperty =
    ArraySchema -> ArraySchema


type alias TupleSchemaProperty =
    TupleSchema -> TupleSchema


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


type alias BaseCombinatorSchemaProperty =
    BaseCombinatorSchema -> BaseCombinatorSchema


{-| Create a required property.
-}
required : String -> Schema -> ObjectProperty
required =
    Required


{-| Create an optional property.
-}
optional : String -> Schema -> ObjectProperty
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
properties : List ObjectProperty -> ObjectSchemaProperty
properties properties schema =
    { schema | properties = schema.properties ++ properties }


{-| `examples` keyword
-}
examples : (a -> Encode.Value) -> List a -> BaseSchemaProperty extras
examples encoder ex schema =
    { schema | examples = List.map encoder ex }


{-| `minProperties` keyword
-}
minProperties : Int -> ObjectSchemaProperty
minProperties min schema =
    { schema | minProperties = Just min }


{-| `maxProperties` keyword
-}
maxProperties : Int -> ObjectSchemaProperty
maxProperties max schema =
    { schema | maxProperties = Just max }


{-| `items` keyword
-}
items : Schema -> ArraySchemaProperty
items items schema =
    { schema | items = Just items }


{-| `minItems` keyword
-}
minItems : Int -> { a | minItems : Maybe Int } -> { a | minItems : Maybe Int }
minItems min schema =
    { schema | minItems = Just min }


{-| `maxItems` keyword
-}
maxItems : Int -> { a | maxItems : Maybe Int } -> { a | maxItems : Maybe Int }
maxItems max schema =
    { schema | maxItems = Just max }


{-| `items` keyword for tuples (heretogeneous javascript arrays)
-}
tupleItems : List Schema -> TupleSchemaProperty
tupleItems items schema =
    { schema | items = Just items }


{-| `additionalItems` keyword for tuples
-}
additionalItems : Schema -> TupleSchemaProperty
additionalItems additionalItems schema =
    { schema | additionalItems = Just additionalItems }


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
object : List ObjectSchemaProperty -> Schema
object props =
    List.foldl (<|) defaultObject props
        |> Object


{-| Create an array type schema.
-}
array : List ArraySchemaProperty -> Schema
array props =
    List.foldl (<|) defaultArray props
        |> Array


{-| Create an array type schema for tuples (heretogeneous javascript arrays).
-}
tuple : List TupleSchemaProperty -> Schema
tuple props =
    List.foldl (<|) defaultTuple props
        |> Tuple


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
oneOf : List BaseCombinatorSchemaProperty -> List Schema -> Schema
oneOf props subSchemas =
    List.foldl (<|) { defaultCombinatorSchema | subSchemas = subSchemas } props
        |> OneOf


{-| Create an allOf type schema.
-}
allOf : List BaseCombinatorSchemaProperty -> List Schema -> Schema
allOf props subSchemas =
    List.foldl (<|) { defaultCombinatorSchema | subSchemas = subSchemas } props
        |> AllOf


{-| Create an anyOf type schema.
-}
anyOf : List BaseCombinatorSchemaProperty -> List Schema -> Schema
anyOf props subSchemas =
    List.foldl (<|) { defaultCombinatorSchema | subSchemas = subSchemas } props
        |> AnyOf


{-| Create a lazy type schema.
-}
lazy : (() -> Schema) -> Schema
lazy thunk =
    Lazy thunk
