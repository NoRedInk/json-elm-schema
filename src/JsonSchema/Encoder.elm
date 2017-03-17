module JsonSchema.Encoder exposing (encode, encodeValue)

{-| Encoding elm json schemas to real json.

@docs encode, encodeValue
-}

import JsonSchema.Model exposing (..)
import Json.Encode as Encode
import Maybe.Extra


{-| Encode an elm json schema into a json string.
-}
encode : Schema -> String
encode schema =
    schema
        |> encodeValue
        |> Encode.encode 2


{-| Encode an elm json schema into a json value.
-}
encodeValue : Schema -> Encode.Value
encodeValue schema =
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
            , Maybe.map ((,) "items" << encodeValue) arraySchema.items
            , Maybe.map ((,) "minItems" << Encode.int) arraySchema.minItems
            , Maybe.map ((,) "maxItems" << Encode.int) arraySchema.maxItems
            ]
                |> Maybe.Extra.values
                |> Encode.object

        String stringSchema ->
            [ Just ( "type", Encode.string "string" )
            , Maybe.map ((,) "title" << Encode.string) stringSchema.title
            , Maybe.map ((,) "description" << Encode.string) stringSchema.description
            , Maybe.map ((,) "enum" << Encode.list << List.map Encode.string) stringSchema.enum
            , Maybe.map ((,) "minLength" << Encode.int) stringSchema.minLength
            , Maybe.map ((,) "maxLength" << Encode.int) stringSchema.maxLength
            , Maybe.map ((,) "pattern" << Encode.string) stringSchema.pattern
            , Maybe.map ((,) "format" << Encode.string << printFormat) stringSchema.format
            ]
                |> Maybe.Extra.values
                |> Encode.object

        Integer integerSchema ->
            [ Just ( "type", Encode.string "integer" )
            , Maybe.map ((,) "title" << Encode.string) integerSchema.title
            , Maybe.map ((,) "description" << Encode.string) integerSchema.description
            , Maybe.map ((,) "enum" << Encode.list << List.map Encode.int) integerSchema.enum
            , Maybe.map ((,) "minimum" << Encode.int) integerSchema.minimum
            , Maybe.map ((,) "maximum" << Encode.int) integerSchema.maximum
            ]
                |> Maybe.Extra.values
                |> Encode.object

        Number numberSchema ->
            [ Just ( "type", Encode.string "number" )
            , Maybe.map ((,) "title" << Encode.string) numberSchema.title
            , Maybe.map ((,) "description" << Encode.string) numberSchema.description
            , Maybe.map ((,) "enum" << Encode.list << List.map Encode.float) numberSchema.enum
            , Maybe.map ((,) "minimum" << Encode.float) numberSchema.minimum
            , Maybe.map ((,) "maximum" << Encode.float) numberSchema.maximum
            ]
                |> Maybe.Extra.values
                |> Encode.object

        Boolean booleanSchema ->
            [ Just ( "type", Encode.string "boolean" )
            , Maybe.map ((,) "title" << Encode.string) booleanSchema.title
            , Maybe.map ((,) "description" << Encode.string) booleanSchema.description
            ]
                |> Maybe.Extra.values
                |> Encode.object

        Null nullSchema ->
            [ Just ( "type", Encode.string "null" )
            , Maybe.map ((,) "title" << Encode.string) nullSchema.title
            , Maybe.map ((,) "description" << Encode.string) nullSchema.description
            ]
                |> Maybe.Extra.values
                |> Encode.object

        OneOf oneOfSchema ->
            [ Maybe.map ((,) "title" << Encode.string) oneOfSchema.title
            , Maybe.map ((,) "description" << Encode.string) oneOfSchema.description
            , List.map encodeValue oneOfSchema.subSchemas
                |> Encode.list
                |> (,) "oneOf"
                |> Just
            ]
                |> Maybe.Extra.values
                |> Encode.object

        AnyOf anyOfSchema ->
            [ Maybe.map ((,) "title" << Encode.string) anyOfSchema.title
            , Maybe.map ((,) "description" << Encode.string) anyOfSchema.description
            , List.map encodeValue anyOfSchema.subSchemas
                |> Encode.list
                |> (,) "anyOf"
                |> Just
            ]
                |> Maybe.Extra.values
                |> Encode.object

        AllOf allOfSchema ->
            [ Maybe.map ((,) "title" << Encode.string) allOfSchema.title
            , Maybe.map ((,) "description" << Encode.string) allOfSchema.description
            , List.map encodeValue allOfSchema.subSchemas
                |> Encode.list
                |> (,) "allOf"
                |> Just
            ]
                |> Maybe.Extra.values
                |> Encode.object


convertProperty : List ObjectProperty -> Encode.Value
convertProperty properties =
    properties
        |> List.map
            (\property ->
                case property of
                    Required name schema ->
                        ( name, encodeValue schema )

                    Optional name schema ->
                        ( name, encodeValue schema )
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


printFormat : StringFormat -> String
printFormat format =
    case format of
        DateTime ->
            "date-time"

        Email ->
            "email"

        Hostname ->
            "hostname"

        Ipv4 ->
            "ipv4"

        Ipv6 ->
            "ipv6"

        Uri ->
            "uri"

        Custom customFormat ->
            customFormat
