module Encoder exposing (..)

import Model exposing (..)
import Json.Encode as Encode
import Maybe.Extra


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

        String stringSchema ->
            [ Just ( "type", Encode.string "string" )
            , Maybe.map ((,) "title" << Encode.string) stringSchema.title
            , Maybe.map ((,) "description" << Encode.string) stringSchema.description
            ]
                |> Maybe.Extra.values
                |> Encode.object

        Integer integerSchema ->
            [ Just ( "type", Encode.string "integer" )
            , Maybe.map ((,) "title" << Encode.string) integerSchema.title
            , Maybe.map ((,) "description" << Encode.string) integerSchema.description
            , Maybe.map ((,) "minimum" << Encode.int) integerSchema.minimum
            , Maybe.map ((,) "maximum" << Encode.int) integerSchema.maximum
            ]
                |> Maybe.Extra.values
                |> Encode.object

        Number numberSchema ->
            [ Just ( "type", Encode.string "integer" )
            , Maybe.map ((,) "title" << Encode.string) numberSchema.title
            , Maybe.map ((,) "description" << Encode.string) numberSchema.description
            , Maybe.map ((,) "minimum" << Encode.float) numberSchema.minimum
            , Maybe.map ((,) "maximum" << Encode.float) numberSchema.maximum
            ]
                |> Maybe.Extra.values
                |> Encode.object

        Null ->
            Encode.null


convertProperty : List ObjectProperty -> Encode.Value
convertProperty properties =
    properties
        |> List.map
            (\property ->
                case property of
                    Required name schema ->
                        ( name, convert schema )

                    Optional name schema ->
                        ( name, convert schema )
            )
        |> Encode.object


findRequiredFields : List ObjectProperty -> Encode.Value
findRequiredFields properties =
    properties
        |> List.map
            (\property ->
                case property of
                    Required name _ ->
                        Just name

                    Optional _ _ ->
                        Nothing
            )
        |> Maybe.Extra.values
        |> List.map Encode.string
        |> Encode.list
