module JsonSchema exposing (..)

import Json.Encode as Encode
import Maybe.Extra


-- TODO
-- FIGURE OUT DRY TECHNIQUE, then implement
-- | Array ArraySchema
-- | Number NumberSchema
-- | Integer IntegerSchema


type Schema
    = Object ObjectSchema
    | String StringSchema
    | Null


type alias BaseSchema extras =
    { extras
        | title : Maybe String
        , description : Maybe String
    }


type alias ObjectSchema =
    BaseSchema { properties : List Property }


defaultObject : ObjectSchema
defaultObject =
    { title = Nothing
    , description = Nothing
    , properties = []
    }


type Property
    = Required String Schema
    | NotRequired String Schema


type alias StringSchema =
    BaseSchema {}


defaultString : StringSchema
defaultString =
    { title = Nothing
    , description = Nothing
    }


type Prop
    = Title String
    | Description String
    | Properties (List Property)


title : String -> Prop
title =
    Title


required : String -> Schema -> Property
required =
    Required


description : String -> Prop
description =
    Description


exactProperties : List Property -> Prop
exactProperties =
    -- TODO implement this to be different from
    -- additionalProperties
    Properties


encoder : Schema -> String
encoder schema =
    schema
        |> convert
        |> Encode.encode 4


object : List Prop -> Schema
object props =
    List.foldl objectFolder defaultObject props
        |> Object


string : List Prop -> Schema
string props =
    List.foldl stringFolder defaultString props
        |> String


objectFolder : Prop -> ObjectSchema -> ObjectSchema
objectFolder prop schema =
    case prop of
        Title title ->
            { schema | title = Just title }

        Description description ->
            { schema | description = Just description }

        Properties properties ->
            { schema | properties = properties }


stringFolder : Prop -> StringSchema -> StringSchema
stringFolder prop schema =
    case prop of
        Title title ->
            { schema | title = Just title }

        Description description ->
            { schema | description = Just description }

        Properties properties ->
            schema


convert : Schema -> Encode.Value
convert schema =
    case schema of
        Object objectSchema ->
            [ Just ( "type", Encode.string "object" )
            , Maybe.map ((,) "title" << Encode.string) objectSchema.title
            , Maybe.map ((,) "description" << Encode.string) objectSchema.description
            , Just ( "properties", (convertProperty objectSchema.properties) )
            , Just ( "required", (findRequiredFields objectSchema.properties) )
            ]
                |> Maybe.Extra.values
                |> Encode.object

        String stringSchema ->
            [ Just ( "type", Encode.string "string" )
            , Maybe.map ((,) "title" << Encode.string) stringSchema.title
            , Maybe.map ((,) "description" << Encode.string) stringSchema.description
            ]
                |> Maybe.Extra.values
                |> Encode.object

        Null ->
            Encode.null


convertProperty : List Property -> Encode.Value
convertProperty properties =
    properties
        |> List.map
            (\property ->
                case property of
                    Required name schema ->
                        ( name, convert schema )

                    NotRequired name schema ->
                        ( name, convert schema )
            )
        |> Encode.object


findRequiredFields : List Property -> Encode.Value
findRequiredFields properties =
    properties
        |> List.map
            (\property ->
                case property of
                    Required name _ ->
                        Just name

                    NotRequired _ _ ->
                        Nothing
            )
        |> Maybe.Extra.values
        |> List.map Encode.string
        |> Encode.list
