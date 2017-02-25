module JsonSchema exposing (..)

import JsonSchema.Model exposing (..)


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


required : String -> Schema -> ObjectProperty
required =
    Required


optional : String -> Schema -> ObjectProperty
optional =
    Optional


title : String -> BaseSchemaProperty extras
title text schema =
    { schema | title = Just text }


description : String -> BaseSchemaProperty extras
description text schema =
    { schema | description = Just text }


minimum : num -> BaseNumberSchemaProperty num
minimum number schema =
    { schema | minimum = Just number }


maximum : num -> BaseNumberSchemaProperty num
maximum number schema =
    { schema | maximum = Just number }


properties : List ObjectProperty -> ObjectSchemaProperty
properties properties schema =
    { schema | properties = schema.properties ++ properties }


items : Schema -> ArraySchemaProperty
items items schema =
    { schema | items = Just items }


minLength : Int -> StringSchemaProperty
minLength length schema =
    { schema | minLength = Just length }


maxLength : Int -> StringSchemaProperty
maxLength length schema =
    { schema | maxLength = Just length }


pattern : String -> StringSchemaProperty
pattern regex schema =
    { schema | pattern = Just regex }


format : StringFormat -> StringSchemaProperty
format formatOption schema =
    { schema | format = Just formatOption }


dateTime : StringFormat
dateTime =
    DateTime


email : StringFormat
email =
    Email


hostname : StringFormat
hostname =
    Hostname


ipv4 : StringFormat
ipv4 =
    Ipv4


ipv6 : StringFormat
ipv6 =
    Ipv6


uri : StringFormat
uri =
    Uri


customFormat : String -> StringFormat
customFormat =
    Custom


object : List ObjectSchemaProperty -> Schema
object props =
    List.foldl (<|) defaultObject props
        |> Object


array : List ArraySchemaProperty -> Schema
array props =
    List.foldl (<|) defaultArray props
        |> Array


string : List StringSchemaProperty -> Schema
string props =
    List.foldl (<|) defaultString props
        |> String


integer : List IntegerSchemaProperty -> Schema
integer props =
    List.foldl (<|) defaultInteger props
        |> Integer


number : List NumberSchemaProperty -> Schema
number props =
    List.foldl (<|) defaultNumber props
        |> Number


boolean : List (BaseSchemaProperty {}) -> Schema
boolean props =
    List.foldl (<|) defaultBaseSchema props
        |> Boolean


null : List (BaseSchemaProperty {}) -> Schema
null props =
    List.foldl (<|) defaultBaseSchema props
        |> Null


oneOf : List BaseCombinatorSchemaProperty -> List Schema -> Schema
oneOf props subSchemas =
    List.foldl (<|) { defaultCombinatorSchema | subSchemas = subSchemas } props
        |> OneOf


allOf : List BaseCombinatorSchemaProperty -> List Schema -> Schema
allOf props subSchemas =
    List.foldl (<|) { defaultCombinatorSchema | subSchemas = subSchemas } props
        |> AllOf


anyOf : List BaseCombinatorSchemaProperty -> List Schema -> Schema
anyOf props subSchemas =
    List.foldl (<|) { defaultCombinatorSchema | subSchemas = subSchemas } props
        |> AnyOf
