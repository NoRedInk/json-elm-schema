module JsonSchema.Decoder exposing (decoder)

{-| Decoding a JSON Schema to an `JsonSchema.Schema`

@docs decoder
-}

import JsonSchema.Model exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Set


{-| Decoder for a JSON Schema
-}
decoder : Decoder Schema
decoder =
    lazy
        (\_ ->
            oneOf
                [ field "type" string
                    |> andThen
                        (\t ->
                            case t of
                                "object" ->
                                    decode objectSchema
                                        |> optional "properties" (keyValuePairs decoder) []
                                        |> optional "required" (list string) []
                                        |> optionalMaybe "title" string
                                        |> optionalMaybe "description" string

                                "array" ->
                                    decode arraySchema
                                        |> optionalMaybe "items" decoder
                                        |> optionalMaybe "minItems" int
                                        |> optionalMaybe "maxItems" int
                                        |> optionalMaybe "title" string
                                        |> optionalMaybe "description" string

                                "string" ->
                                    decode stringSchema
                                        |> optionalMaybe "minLength" int
                                        |> optionalMaybe "maxLength" int
                                        |> optionalMaybe "pattern" string
                                        |> optionalMaybe "format" string
                                        |> optionalMaybe "enum" (list string)
                                        |> optionalMaybe "title" string
                                        |> optionalMaybe "description" string

                                "integer" ->
                                    decode integerSchema
                                        |> optionalMaybe "minimum" int
                                        |> optionalMaybe "maximum" int
                                        |> optionalMaybe "enum" (list int)
                                        |> optionalMaybe "title" string
                                        |> optionalMaybe "description" string

                                "number" ->
                                    decode numberSchema
                                        |> optionalMaybe "minimum" float
                                        |> optionalMaybe "maximum" float
                                        |> optionalMaybe "enum" (list float)
                                        |> optionalMaybe "title" string
                                        |> optionalMaybe "description" string

                                "boolean" ->
                                    decode booleanSchema
                                        |> optionalMaybe "title" string
                                        |> optionalMaybe "description" string

                                "null" ->
                                    decode nullSchema
                                        |> optionalMaybe "title" string
                                        |> optionalMaybe "description" string

                                _ ->
                                    fail ("Unknown object type `" ++ t ++ "`")
                        )
                , decode oneOfSchema
                    |> required "oneOf" (list decoder)
                    |> optionalMaybe "title" string
                    |> optionalMaybe "description" string
                , decode anyOfSchema
                    |> required "anyOf" (list decoder)
                    |> optionalMaybe "title" string
                    |> optionalMaybe "description" string
                , decode allOfSchema
                    |> required "allOf" (list decoder)
                    |> optionalMaybe "title" string
                    |> optionalMaybe "description" string
                ]
        )



-- HELPERS --


optionalMaybe n d =
    optional n (nullable d) Nothing



-- CONSTRUCTORS --


objectSchema propertiesPairs requiredList title description =
    let
        requiredSet =
            Set.fromList requiredList

        objectProperty ( key, schema ) =
            if Set.member key requiredSet then
                Required key schema
            else
                Optional key schema

        properties =
            -- keyValuePairs give keys in reverse order
            List.map objectProperty (List.reverse propertiesPairs)
    in
        Object
            { properties = properties
            , title = title
            , description = description
            }


arraySchema items minItems maxItems title description =
    Array
        { items = items
        , minItems = minItems
        , maxItems = maxItems
        , title = title
        , description = description
        }


stringSchema minLength maxLength pattern format enum title description =
    String
        { minLength = minLength
        , maxLength = maxLength
        , pattern = pattern
        , format = Maybe.map stringFormat format
        , enum = enum
        , title = title
        , description = description
        }


integerSchema minimum maximum enum title description =
    Integer
        { minimum = minimum
        , maximum = maximum
        , enum = enum
        , title = title
        , description = description
        }


numberSchema minimum maximum enum title description =
    Number
        { minimum = minimum
        , maximum = maximum
        , enum = enum
        , title = title
        , description = description
        }


booleanSchema title description =
    Boolean
        { title = title
        , description = description
        }


nullSchema title description =
    Null
        { title = title
        , description = description
        }


oneOfSchema subSchemas title description =
    OneOf
        { subSchemas = subSchemas
        , title = title
        , description = description
        }


anyOfSchema subSchemas title description =
    AnyOf
        { subSchemas = subSchemas
        , title = title
        , description = description
        }


allOfSchema subSchemas title description =
    AllOf
        { subSchemas = subSchemas
        , title = title
        , description = description
        }


stringFormat s =
    case s of
        "date-time" ->
            DateTime

        "email" ->
            Email

        "hostname" ->
            Hostname

        "ipv4" ->
            Ipv4

        "ipv6" ->
            Ipv6

        "uri" ->
            Uri

        _ ->
            Custom s
