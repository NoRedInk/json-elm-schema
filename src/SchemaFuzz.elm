module SchemaFuzz exposing (schemaValue, schemaString)

import Fuzz exposing (Fuzzer)
import Json.Encode as Encode exposing (Value)
import Model exposing (..)
import Maybe.Extra
import Fuzz.Extra


schemaString : Schema -> Fuzzer String
schemaString schema =
    schemaValue schema
        |> Fuzz.map (Encode.encode 2)


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
stringFuzzer stringSchema =
    -- TODO: handle string constraints.
    Fuzz.string
        |> Fuzz.map Encode.string


numberFuzzer : NumberSchema -> Fuzzer Value
numberFuzzer schema =
    case ( schema.minimum, schema.maximum ) of
        ( Nothing, Nothing ) ->
            Fuzz.float
                |> Fuzz.map Encode.float

        ( Just minimum, Just maximum ) ->
            Fuzz.floatRange minimum maximum
                |> Fuzz.map Encode.float

        ( Just minimum, Nothing ) ->
            Fuzz.Extra.floatMinimum minimum
                |> Fuzz.map Encode.float

        ( Nothing, Just maximum ) ->
            Fuzz.Extra.floatMaximum maximum
                |> Fuzz.map Encode.float


integerFuzzer : IntegerSchema -> Fuzzer Value
integerFuzzer integerSchema =
    -- TODO: handle integer constraints
    Fuzz.int
        |> Fuzz.map Encode.int


booleanFuzzer : Fuzzer Value
booleanFuzzer =
    Fuzz.bool
        |> Fuzz.map Encode.bool


nullFuzzer : Fuzzer Value
nullFuzzer =
    Encode.null
        |> Fuzz.constant
