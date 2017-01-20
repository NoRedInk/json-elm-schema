module Main exposing (..)

import Json.Encode as Encode
import Maybe.Extra


type Schema
    = Object ObjectSchema
    | Array ArraySchema
    | String StringSchema
    | Number NumberSchema
    | Integer IntegerSchema
    | Null


type alias ObjectSchema =
    { properties : List Property
    , title : Maybe String
    , description : Maybe String
    }


type Property
    = Required String Schema
    | NotRequired String Schema


type alias ArraySchema =
    { title : Maybe String
    , description : Maybe String
    }


type alias StringSchema =
    { title : Maybe String
    , description : Maybe String
    }


type alias NumberSchema =
    { title : Maybe String
    , description : Maybe String
    }


type alias IntegerSchema =
    { title : Maybe String
    , description : Maybe String
    , maximum : Maybe Int
    , minimum : Maybe Int
    }


encoder : Schema -> String
encoder schema =
    schema
        |> convert
        |> Encode.encode 4


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

        Array arraySchema ->
            [ Just ( "type", Encode.string "array" )
            , Maybe.map ((,) "title" << Encode.string) arraySchema.title
            , Maybe.map ((,) "description" << Encode.string) arraySchema.description
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

        Number numberSchema ->
            [ Just ( "type", Encode.string "number" )
            , Maybe.map ((,) "title" << Encode.string) numberSchema.title
            , Maybe.map ((,) "description" << Encode.string) numberSchema.description
            ]
                |> Maybe.Extra.values
                |> Encode.object

        Integer integerSchema ->
            [ Just ( "type", Encode.string "integer" )
            , Maybe.map ((,) "title" << Encode.string) integerSchema.title
            , Maybe.map ((,) "description" << Encode.string) integerSchema.description
            , Maybe.map ((,) "maximum" << Encode.int) integerSchema.maximum
            , Maybe.map ((,) "minimum" << Encode.int) integerSchema.minimum
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



-- object [ ("name", object [("something", null)])
--      , ("age", int 42)
--      ]
