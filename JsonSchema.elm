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


type alias BaseSchemaProperty a =
    BaseSchema a -> BaseSchema a


type alias ObjectSchemaProperty =
    ObjectSchema -> ObjectSchema


type alias StringSchemaProperty =
    StringSchema -> StringSchema


type alias IntegerSchemaProperty =
    IntegerSchema -> IntegerSchema


required : String -> Schema -> ObjectProperty
required =
    Required


optional : String -> Schema -> ObjectProperty
optional =
    Optional


title : String -> BaseSchemaProperty a
title text schema =
    { schema | title = Just text }


description : String -> BaseSchemaProperty a
description text schema =
    { schema | description = Just text }


minimum : Int -> IntegerSchemaProperty
minimum int schema =
    { schema | minimum = Just int }


maximum : Int -> IntegerSchemaProperty
maximum int schema =
    { schema | maximum = Just int }


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
