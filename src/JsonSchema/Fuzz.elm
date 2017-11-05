module JsonSchema.Fuzz exposing (schemaString, schemaValue)

{-| Fuzzers for json structures corresponding to a certain schema.

@docs schemaString, schemaValue

-}

import Fuzz exposing (Fuzzer)
import Fuzz.Extra
import Json.Encode as Encode exposing (Value)
import JsonSchema.Model exposing (..)
import Maybe.Extra
import Random


{-| Fuzzer that generates json strings.
-}
schemaString : Schema -> Fuzzer String
schemaString schema =
    schemaValue schema
        |> Fuzz.map (Encode.encode 2)


{-| Fuzzer that generates json values.
-}
schemaValue : Schema -> Fuzzer Value
schemaValue schema =
    case toSubSchema schema of
        ( _, subSchema ) ->
            subSchemaValue subSchema


subSchemaValue : SubSchema -> Fuzzer Value
subSchemaValue schema =
    case schema of
        Object objectSchema ->
            objectFuzzer objectSchema

        Array arraySchema ->
            arrayFuzzer arraySchema

        String stringSchema ->
            stringFuzzer stringSchema

        Number numberSchema ->
            numberFuzzer numberSchema

        Integer integerSchema ->
            integerFuzzer integerSchema

        Boolean booleanSchema ->
            booleanFuzzer booleanSchema

        Null _ ->
            nullFuzzer

        Ref _ ->
            Fuzz.invalid "Fuzzing a ref schema is not supported"

        AnyOf anyOfSchema ->
            anyOfFuzzer anyOfSchema

        OneOf oneOfSchema ->
            -- `oneOf` explicitly demands that a value corresponds to one of the provided schema's and none of the others.
            -- Implementing this will probably require merging the subschema's properties together into a single schema.
            Fuzz.invalid "Fuzzing a oneOf schema is currently not supported. anyOf is supported, so perhaps you can use that?"

        AllOf allOfSchema ->
            Fuzz.invalid "Fuzzing an allOf schema is currently not supported"

        Fallback value ->
            Fuzz.invalid "Fuzzing a fallback schema is not supported"


objectFuzzer : ObjectSchema definitions -> Fuzzer Value
objectFuzzer objectSchema =
    let
        unfuzzable =
            Maybe.Extra.isJust objectSchema.minProperties
                || Maybe.Extra.isJust objectSchema.maxProperties
    in
    if unfuzzable then
        Fuzz.invalid "Fuzzing minProperties or maxProperties is currently not supported."
    else
        List.map propertyFuzzer objectSchema.properties
            |> Fuzz.Extra.sequence
            |> Fuzz.map (Maybe.Extra.values >> Encode.object)


propertyFuzzer : ObjectProperty NoDefinitions -> Fuzzer (Maybe ( String, Value ))
propertyFuzzer objectProperty =
    case objectProperty of
        Required key subSchema ->
            subSchemaValue subSchema
                |> Fuzz.map ((,) key >> Just)

        Optional key subSchema ->
            subSchemaValue subSchema
                |> Fuzz.map ((,) key)
                |> Fuzz.maybe


arrayFuzzer : ArraySchema definitions -> Fuzzer Value
arrayFuzzer arraySchema =
    case ( arraySchema.items, arraySchema.minItems, arraySchema.maxItems ) of
        ( Nothing, _, _ ) ->
            -- TODO: do something more interesting in the case the user hasn't defined a schema for the items.
            Fuzz.constant []
                |> Fuzz.map Encode.list

        ( Just subSchema, Nothing, Nothing ) ->
            subSchemaValue subSchema
                |> Fuzz.list
                |> Fuzz.map Encode.list

        ( Just subSchema, Just minItems, Nothing ) ->
            subSchemaValue subSchema
                |> Fuzz.Extra.variableList minItems (minItems + 100)
                |> Fuzz.map Encode.list

        ( Just subSchema, Nothing, Just maxItems ) ->
            subSchemaValue subSchema
                |> Fuzz.Extra.variableList 0 maxItems
                |> Fuzz.map Encode.list

        ( Just subSchema, Just minItems, Just maxItems ) ->
            subSchemaValue subSchema
                |> Fuzz.Extra.variableList minItems maxItems
                |> Fuzz.map Encode.list


stringFuzzer : StringSchema -> Fuzzer Value
stringFuzzer schema =
    -- TODO: Handle `format` and `pattern` properties in some way.
    let
        tooShort : String -> Bool
        tooShort str =
            schema.minLength
                |> Maybe.map ((<=) (String.length str))
                |> Maybe.withDefault False

        expandIfTooShort : String -> String
        expandIfTooShort str =
            if tooShort str then
                (str ++ str ++ "abc")
                    |> expandIfTooShort
            else
                str

        cropIfTooLong : String -> String
        cropIfTooLong str =
            case schema.maxLength of
                Nothing ->
                    str

                Just maxLength ->
                    String.slice 0 maxLength str
    in
    case schema.enum of
        Just enum ->
            enum
                |> Fuzz.Extra.oneOf
                |> Fuzz.map Encode.string

        Nothing ->
            Fuzz.string
                |> Fuzz.map expandIfTooShort
                |> Fuzz.map cropIfTooLong
                |> Fuzz.map Encode.string


numberFuzzer : NumberSchema -> Fuzzer Value
numberFuzzer schema =
    case ( schema.enum, schema.minimum, schema.maximum ) of
        ( Just enum, _, _ ) ->
            enum
                |> Fuzz.Extra.oneOf
                |> Fuzz.map Encode.float

        ( Nothing, Nothing, Nothing ) ->
            Fuzz.float
                |> Fuzz.map Encode.float

        ( Nothing, Just minimum, Just maximum ) ->
            Fuzz.floatRange minimum maximum
                |> Fuzz.map Encode.float

        ( Nothing, Just minimum, Nothing ) ->
            Fuzz.Extra.floatMinimum minimum
                |> Fuzz.map Encode.float

        ( Nothing, Nothing, Just maximum ) ->
            Fuzz.Extra.floatMaximum maximum
                |> Fuzz.map Encode.float


integerFuzzer : IntegerSchema -> Fuzzer Value
integerFuzzer schema =
    case ( schema.enum, schema.minimum, schema.maximum ) of
        ( Just enum, _, _ ) ->
            enum
                |> Fuzz.Extra.oneOf
                |> Fuzz.map Encode.int

        ( Nothing, Nothing, Nothing ) ->
            Fuzz.int
                |> Fuzz.map Encode.int

        ( Nothing, Just minimum, Just maximum ) ->
            Fuzz.intRange minimum maximum
                |> Fuzz.map Encode.int

        ( Nothing, Just minimum, Nothing ) ->
            Fuzz.intRange minimum Random.maxInt
                |> Fuzz.map Encode.int

        ( Nothing, Nothing, Just maximum ) ->
            Fuzz.intRange Random.minInt maximum
                |> Fuzz.map Encode.int


booleanFuzzer : BooleanSchema -> Fuzzer Value
booleanFuzzer schema =
    case schema.enum of
        Just enum ->
            enum
                |> Fuzz.Extra.oneOf
                |> Fuzz.map Encode.bool

        Nothing ->
            Fuzz.bool
                |> Fuzz.map Encode.bool


nullFuzzer : Fuzzer Value
nullFuzzer =
    Encode.null
        |> Fuzz.constant


anyOfFuzzer : BaseCombinatorSchema definitions -> Fuzzer Value
anyOfFuzzer anyOfSchema =
    anyOfSchema.subSchemas
        |> List.map (subSchemaValue >> (,) 1)
        |> Fuzz.frequency
