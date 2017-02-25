module JsonSchema exposing (Schema, required, optional, title, description, minimum, maximum, properties, items, minLength, maxLength, pattern, format, dateTime, email, hostname, ipv4, ipv6, uri, customFormat, object, array, string, integer, number, boolean, null, oneOf, allOf, anyOf)

{-| This library allows you to write your json schema files in elm, preventing inadvertent errors.

# General
@docs Schema

# Schema types
@docs object, array, string, integer, number, boolean, null, oneOf, allOf, anyOf

# Keywords
@docs title, description, minimum, maximum, properties, items, minLength, maxLength, pattern, format

# Property constructors
@docs required, optional

# String formats
@docs dateTime, email, hostname, ipv4, ipv6, uri, customFormat
-}

import JsonSchema.Model exposing (..)


{-| A type representing a json schema.
-}
type alias Schema =
    JsonSchema.Model.Schema


defaultBaseSchema : BaseSchema {}
defaultBaseSchema =
    { title = Nothing
    , description = Nothing
    }


defaultObject : ObjectSchema
defaultObject =
    { title = Nothing
    , description = Nothing
    , properties = []
    }


defaultArray : ArraySchema
defaultArray =
    { title = Nothing
    , description = Nothing
    , items = Nothing
    }


defaultString : StringSchema
defaultString =
    { title = Nothing
    , description = Nothing
    , minLength = Nothing
    , maxLength = Nothing
    , pattern = Nothing
    , format = Nothing
    }


defaultInteger : IntegerSchema
defaultInteger =
    { title = Nothing
    , description = Nothing
    , minimum = Nothing
    , maximum = Nothing
    }


defaultNumber : NumberSchema
defaultNumber =
    { title = Nothing
    , description = Nothing
    , minimum = Nothing
    , maximum = Nothing
    }


defaultCombinatorSchema : BaseCombinatorSchema
defaultCombinatorSchema =
    { title = Nothing
    , description = Nothing
    , subSchemas = []
    }


type alias BaseSchemaProperty extras =
    BaseSchema extras -> BaseSchema extras


type alias ObjectSchemaProperty =
    ObjectSchema -> ObjectSchema


type alias ArraySchemaProperty =
    ArraySchema -> ArraySchema


type alias StringSchemaProperty =
    StringSchema -> StringSchema


type alias BaseNumberSchemaProperty num =
    BaseNumberSchema num -> BaseNumberSchema num


type alias IntegerSchemaProperty =
    IntegerSchema -> IntegerSchema


type alias NumberSchemaProperty =
    NumberSchema -> NumberSchema


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


{-| `items` keyword
-}
items : Schema -> ArraySchemaProperty
items items schema =
    { schema | items = Just items }


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


{-| Create an oject type schema.
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
boolean : List (BaseSchemaProperty {}) -> Schema
boolean props =
    List.foldl (<|) defaultBaseSchema props
        |> Boolean


{-| Create a null type schema.
-}
null : List (BaseSchemaProperty {}) -> Schema
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
