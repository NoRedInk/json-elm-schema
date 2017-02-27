module JsonSchema.Fuzz exposing (schemaString, schemaValue)

{-| Fuzzers for json structures corresponding to a certain schema.

@docs schemaString, schemaValue
-}

import Fuzz exposing (Fuzzer)
import Fuzz.Extra
import Json.Encode as Encode exposing (Value)
import Maybe.Extra
import JsonSchema.Model exposing (..)
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

        Boolean _ ->
            booleanFuzzer

        Null _ ->
            nullFuzzer

        AnyOf anyOfSchema ->
            anyOfFuzzer anyOfSchema

        OneOf oneOfSchema ->
            Debug.crash "Fuzzing a oneOf schema is currently not supported"

        AllOf allOfSchema ->
            Debug.crash "Fuzzing an allOf schema is currently not supported"


objectFuzzer : ObjectSchema -> Fuzzer Value
objectFuzzer objectSchema =
    List.map propertyFuzzer objectSchema.properties
        |> sequence
        |> Fuzz.map (Maybe.Extra.values >> Encode.object)


sequence : List (Fuzzer a) -> Fuzzer (List a)
sequence fuzzers =
    List.foldl
        (\fuzzer listFuzzer ->
            Fuzz.constant (::)
                |> Fuzz.andMap fuzzer
                |> Fuzz.andMap listFuzzer
        )
        (Fuzz.constant [])
        fuzzers


propertyFuzzer : ObjectProperty -> Fuzzer (Maybe ( String, Value ))
propertyFuzzer objectProperty =
    case objectProperty of
        Required key subSchema ->
            schemaValue subSchema
                |> Fuzz.map ((,) key >> Just)

        Optional key subSchema ->
            schemaValue subSchema
                |> Fuzz.map ((,) key)
                |> Fuzz.maybe


arrayFuzzer : ArraySchema -> Fuzzer Value
arrayFuzzer arraySchema =
    case arraySchema.items of
        Nothing ->
            -- TODO: do something more interesting in the case the user hasn't defined a schema for the items.
            Fuzz.constant []
                |> Fuzz.map Encode.list

        Just subSchema ->
            schemaValue subSchema
                |> Fuzz.list
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
                    |> Fuzz.Extra.anyOrCrash
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
                |> Fuzz.Extra.anyOrCrash
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
                |> Fuzz.Extra.anyOrCrash
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


booleanFuzzer : Fuzzer Value
booleanFuzzer =
    Fuzz.bool
        |> Fuzz.map Encode.bool


nullFuzzer : Fuzzer Value
nullFuzzer =
    Encode.null
        |> Fuzz.constant


anyOfFuzzer : BaseCombinatorSchema -> Fuzzer Value
anyOfFuzzer anyOfSchema =
    anyOfSchema.subSchemas
        |> List.map (schemaValue >> (,) 1)
        |> Fuzz.frequencyOrCrash
