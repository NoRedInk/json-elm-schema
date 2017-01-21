module JsonSchema exposing (..)

import Model exposing (..)


defaultObject : ObjectSchema
defaultObject =
    { title = Nothing
    , description = Nothing
    , properties = []
    }


defaultString : StringSchema
defaultString =
    { title = Nothing
    , description = Nothing
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


type alias BaseSchemaProperty extras =
    BaseSchema extras -> BaseSchema extras


type alias ObjectSchemaProperty =
    ObjectSchema -> ObjectSchema


type alias StringSchemaProperty =
    StringSchema -> StringSchema


type alias BaseNumberSchemaProperty num =
    BaseNumberSchema num -> BaseNumberSchema num


type alias IntegerSchemaProperty =
    IntegerSchema -> IntegerSchema


type alias NumberSchemaProperty =
    NumberSchema -> NumberSchema


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


object : List ObjectSchemaProperty -> Schema
object props =
    List.foldl (<|) defaultObject props
        |> Object


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
