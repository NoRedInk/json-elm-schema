module JsonSchema.Encoder exposing (encode, encodeValue, EncoderProgram, encodeSchemaProgram)

{-| Encoding elm json schemas to real json.

@docs encode, encodeValue, EncoderProgram, encodeSchemaProgram
-}

import Dict exposing (Dict)
import Json.Encode as Encode
import JsonSchema.Model exposing (..)
import JsonSchema.Util exposing (hash)
import Maybe.Extra


{-| Type of the encodeSchemaProgram.
-}
type alias EncoderProgram =
    Platform.Program Never () ()


{-| A program to use for encoding a schema.

    main : EncoderProgram
    main = encoderSchemaProgram mySchema emit

    port emit : String -> Cmd a
-}
encodeSchemaProgram : Schema -> (String -> Cmd ()) -> EncoderProgram
encodeSchemaProgram schema emit =
    Platform.program
        { init = ( (), emit (encode schema) )
        , update = (\_ _ -> ( (), Cmd.none ))
        , subscriptions = (\_ -> Sub.none)
        }


{-| Encode an elm json schema into a json string.
-}
encode : Schema -> String
encode schema =
    Encode.encode 2 (encodeValue schema)


{-| Encode an elm json schema into a json value.
-}
encodeValue : Schema -> Encode.Value
encodeValue schema =
    let
        cache : ThunkCache
        cache =
            findThunks schema Dict.empty

        definitions : Maybe ( String, Encode.Value )
        definitions =
            if Dict.isEmpty cache then
                Nothing
            else
                Dict.toList cache
                    |> List.map (Tuple.mapSecond <| encodeSubSchema cache)
                    |> Encode.object
                    |> ((,) "definitions")
                    |> Just
    in
        schema
            |> preEncodeValue cache
            |> ((::) definitions)
            |> Maybe.Extra.values
            |> Encode.object


encodeSubSchema : ThunkCache -> Schema -> Encode.Value
encodeSubSchema cache schema =
    preEncodeValue cache schema
        |> Maybe.Extra.values
        |> Encode.object


preEncodeValue : ThunkCache -> Schema -> List (Maybe ( String, Encode.Value ))
preEncodeValue cache schema =
    case schema of
        Object objectSchema ->
            [ Just ( "type", Encode.string "object" )
            , Maybe.map ((,) "title" << Encode.string) objectSchema.title
            , Maybe.map ((,) "description" << Encode.string) objectSchema.description
            , Just ( "properties", (convertProperty cache objectSchema.properties) )
            , Just ( "required", (findRequiredFields objectSchema.properties) )
            ]

        Array arraySchema ->
            [ Just ( "type", Encode.string "array" )
            , Maybe.map ((,) "title" << Encode.string) arraySchema.title
            , Maybe.map ((,) "description" << Encode.string) arraySchema.description
            , Maybe.map ((,) "items" << encodeSubSchema cache) arraySchema.items
            , Maybe.map ((,) "minItems" << Encode.int) arraySchema.minItems
            , Maybe.map ((,) "maxItems" << Encode.int) arraySchema.maxItems
            ]

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

        Integer integerSchema ->
            [ Just ( "type", Encode.string "integer" )
            , Maybe.map ((,) "title" << Encode.string) integerSchema.title
            , Maybe.map ((,) "description" << Encode.string) integerSchema.description
            , Maybe.map ((,) "enum" << Encode.list << List.map Encode.int) integerSchema.enum
            , Maybe.map ((,) "minimum" << Encode.int) integerSchema.minimum
            , Maybe.map ((,) "maximum" << Encode.int) integerSchema.maximum
            ]

        Number numberSchema ->
            [ Just ( "type", Encode.string "number" )
            , Maybe.map ((,) "title" << Encode.string) numberSchema.title
            , Maybe.map ((,) "description" << Encode.string) numberSchema.description
            , Maybe.map ((,) "enum" << Encode.list << List.map Encode.float) numberSchema.enum
            , Maybe.map ((,) "minimum" << Encode.float) numberSchema.minimum
            , Maybe.map ((,) "maximum" << Encode.float) numberSchema.maximum
            ]

        Boolean booleanSchema ->
            [ Just ( "type", Encode.string "boolean" )
            , Maybe.map ((,) "title" << Encode.string) booleanSchema.title
            , Maybe.map ((,) "description" << Encode.string) booleanSchema.description
            ]

        Null nullSchema ->
            [ Just ( "type", Encode.string "null" )
            , Maybe.map ((,) "title" << Encode.string) nullSchema.title
            , Maybe.map ((,) "description" << Encode.string) nullSchema.description
            ]

        OneOf oneOfSchema ->
            [ Maybe.map ((,) "title" << Encode.string) oneOfSchema.title
            , Maybe.map ((,) "description" << Encode.string) oneOfSchema.description
            , List.map (encodeSubSchema cache) oneOfSchema.subSchemas
                |> Encode.list
                |> (,) "oneOf"
                |> Just
            ]

        AnyOf anyOfSchema ->
            [ Maybe.map ((,) "title" << Encode.string) anyOfSchema.title
            , Maybe.map ((,) "description" << Encode.string) anyOfSchema.description
            , List.map (encodeSubSchema cache) anyOfSchema.subSchemas
                |> Encode.list
                |> (,) "anyOf"
                |> Just
            ]

        AllOf allOfSchema ->
            [ Maybe.map ((,) "title" << Encode.string) allOfSchema.title
            , Maybe.map ((,) "description" << Encode.string) allOfSchema.description
            , List.map (encodeSubSchema cache) allOfSchema.subSchemas
                |> Encode.list
                |> (,) "allOf"
                |> Just
            ]

        Lazy thunk ->
            [ Just ( "$ref", Encode.string <| "#/definitions/" ++ (hash <| thunk ()) ) ]


type alias ThunkCache =
    Dict String Schema


findThunks : Schema -> ThunkCache -> ThunkCache
findThunks schema cache =
    case schema of
        Object { properties } ->
            List.map getPropertySchema properties
                |> List.foldr findThunks cache

        Array { items } ->
            Maybe.Extra.unwrap cache (flip findThunks cache) items

        String _ ->
            cache

        Integer _ ->
            cache

        Number _ ->
            cache

        Boolean _ ->
            cache

        Null _ ->
            cache

        OneOf { subSchemas } ->
            List.foldr findThunks cache subSchemas

        AnyOf { subSchemas } ->
            List.foldr findThunks cache subSchemas

        AllOf { subSchemas } ->
            List.foldr findThunks cache subSchemas

        Lazy thunk ->
            thunkDict thunk cache


getPropertySchema : ObjectProperty -> Schema
getPropertySchema property =
    case property of
        Required _ schema ->
            schema

        Optional _ schema ->
            schema


thunkDict : (() -> Schema) -> ThunkCache -> ThunkCache
thunkDict thunk cache =
    let
        schema : Schema
        schema =
            thunk ()

        key : String
        key =
            hash schema
    in
        if Dict.member key cache then
            cache
        else
            cache
                |> Dict.insert key schema
                |> findThunks schema


convertProperty : ThunkCache -> List ObjectProperty -> Encode.Value
convertProperty cache properties =
    properties
        |> List.map
            (\property ->
                case property of
                    Required name schema ->
                        ( name, encodeSubSchema cache schema )

                    Optional name schema ->
                        ( name, encodeSubSchema cache schema )
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
