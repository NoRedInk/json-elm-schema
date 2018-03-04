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
    case schema of
        Object objectSchema ->
            objectFuzzer objectSchema

        Array arraySchema ->
            arrayFuzzer arraySchema

        Tuple tupleSchema ->
            tupleFuzzer tupleSchema
            
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

        Lazy thunk ->
            Fuzz.invalid "Fuzzing a lazy schema is currently not supported"

        Fallback value ->
            Fuzz.invalid "Fuzzing a fallback schema is not supported"


objectFuzzer : ObjectSchema -> Fuzzer Value
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
    case ( arraySchema.items, arraySchema.minItems, arraySchema.maxItems ) of
        ( Nothing, _, _ ) ->
            -- TODO: do something more interesting in the case the user hasn't defined a schema for the items.
            Fuzz.constant []
                |> Fuzz.map Encode.list

        ( Just subSchema, Nothing, Nothing ) ->
            schemaValue subSchema
                |> Fuzz.list
                |> Fuzz.map Encode.list

        ( Just subSchema, Just minItems, Nothing ) ->
            schemaValue subSchema
                |> Fuzz.Extra.variableList minItems (minItems + 100)
                |> Fuzz.map Encode.list

        ( Just subSchema, Nothing, Just maxItems ) ->
            schemaValue subSchema
                |> Fuzz.Extra.variableList 0 maxItems
                |> Fuzz.map Encode.list

        ( Just subSchema, Just minItems, Just maxItems ) ->
            schemaValue subSchema
                |> Fuzz.Extra.variableList minItems maxItems
                |> Fuzz.map Encode.list


tupleFuzzer : TupleSchema -> Fuzzer Value
tupleFuzzer tupleSchema =
    case ( tupleSchema.minItems, tupleSchema.maxItems ) of

        ( Nothing, Nothing ) ->
            tupleWithNoMinOrMaxItemsFuzzer
                tupleSchema.items
                tupleSchema.additionalItems
                    |> Fuzz.map Encode.list
 
        ( Nothing, Just maxItems ) ->
            tupleWithOnlyMaxItemsFuzzer 
                tupleSchema.items 
                tupleSchema.additionalItems 
                maxItems
                    |> Fuzz.map Encode.list

        ( Just minItems, Nothing ) ->
            tupleWithOnlyMinItemsFuzzer 
                tupleSchema.items 
                tupleSchema.additionalItems 
                minItems
                    |> Fuzz.map Encode.list

        ( Just minItems, Just maxItems ) ->
            tupleWithMinAndMaxItemsFuzzer 
                tupleSchema.items 
                tupleSchema.additionalItems 
                minItems 
                maxItems
                    |> Fuzz.map Encode.list


-- Tuple fuzzer helpers

tupleWithNoMinOrMaxItemsFuzzer : Maybe (List Schema) -> Maybe Schema -> Fuzzer (List Value)
tupleWithNoMinOrMaxItemsFuzzer maybeItems maybeAdditionalItems =
    case (maybeItems, maybeAdditionalItems) of

        ( Nothing, Nothing ) ->
            anyValueFuzzer
                |> Fuzz.Extra.variableList 0 100

        ( Nothing, Just additionalItemsSchema ) ->
            schemaValue additionalItemsSchema
                |> Fuzz.Extra.variableList 0 100

        ( Just subSchemas, Nothing ) ->
            fuzzTupleItems_ subSchemas

        ( Just subSchemas, Just additionalItemsSchema ) ->
            Fuzz.map2 (++)           
                ( fuzzTupleItems_ subSchemas )
                ( schemaValue additionalItemsSchema 
                    |> Fuzz.Extra.variableList 0 100
                )


tupleWithOnlyMaxItemsFuzzer : Maybe (List Schema) -> Maybe Schema -> Int -> Fuzzer (List Value)
tupleWithOnlyMaxItemsFuzzer maybeItems maybeAdditionalItems maxItems =
    case (maybeItems, maybeAdditionalItems) of

        ( Nothing, Nothing ) ->
            anyValueFuzzer
                |> Fuzz.Extra.variableList 0 maxItems

        ( Nothing, Just additionalItemsSchema ) ->
            schemaValue additionalItemsSchema
                |> Fuzz.Extra.variableList 0 maxItems

        ( Just subSchemas, Nothing ) ->
            let
                subSchemaCount =
                    List.length subSchemas
            in
            case (compare maxItems subSchemaCount) of
                LT ->   
                    -- number of schemas is greater than max, use only up to max schemas
                    fuzzTupleItems_ (List.take maxItems subSchemas)

                EQ ->   
                    -- number of schemas is equal to max, use all the schemas
                    fuzzTupleItems_ subSchemas

                GT ->   
                    -- number of schemas is less than max, use schemas + up to remainder of max "dummy"
                    Fuzz.map2 (++)
                        ( fuzzTupleItems_ subSchemas )
                        ( anyValueFuzzer
                              |> Fuzz.Extra.variableList 0
                                   (maxItems - subSchemaCount)
                        )
 
        ( Just subSchemas, Just additionalItemsSchema ) ->
            let
                subSchemaCount =
                    List.length subSchemas
            in
            case (compare maxItems subSchemaCount) of
                
                LT ->
                    -- number of schemas is greater than max, use only up to max schemas
                    fuzzTupleItems_ (List.take maxItems subSchemas)

                EQ ->
                    -- number of schemas is equal to max, use all the schemas
                    fuzzTupleItems_ subSchemas

                GT ->
                    -- number of schemas is less than max, use schemas + up to remainder of max additional
                    Fuzz.map2 (++)
                        ( fuzzTupleItems_ subSchemas )
                        ( schemaValue additionalItemsSchema 
                            |> Fuzz.Extra.variableList 0
                                 (maxItems - subSchemaCount)
                        )


tupleWithOnlyMinItemsFuzzer : Maybe (List Schema) -> Maybe Schema -> Int -> Fuzzer (List Value)
tupleWithOnlyMinItemsFuzzer maybeItems maybeAdditionalItems minItems =
    case (maybeItems, maybeAdditionalItems) of

        ( Nothing, Nothing ) ->
            anyValueFuzzer
                |> Fuzz.Extra.variableList minItems (minItems + 100)

        ( Nothing, Just additionalItemsSchema ) ->
            schemaValue additionalItemsSchema
                |> Fuzz.Extra.variableList minItems (minItems + 100)

        ( Just subSchemas, Nothing ) ->
            let
                subSchemaCount =
                    List.length subSchemas
            in
            case (compare minItems subSchemaCount) of

                LT ->
                    -- number of schemas is greater than min, use all the schemas
                    fuzzTupleItems_ subSchemas

                EQ ->
                    -- number of schemas is equal to min, use all the schemas
                    fuzzTupleItems_ subSchemas

                GT ->
                    -- number of schemas is less than min, use schemas + at least up to min "dummy"
                    Fuzz.map2 (++)
                        ( fuzzTupleItems_ subSchemas )
                        ( anyValueFuzzer 
                              |> Fuzz.Extra.variableList 
                                   (minItems - subSchemaCount) 
                                   (minItems - subSchemaCount + 100) 
                        )


        ( Just subSchemas, Just additionalItemsSchema ) ->
            let
                subSchemaCount =
                    List.length subSchemas
            in
            case (compare minItems subSchemaCount) of
                LT ->
                    -- number of schemas is greater than min, use all the schemas + up to 100 additional
                    Fuzz.map2 (++)
                        ( fuzzTupleItems_ subSchemas )
                        ( schemaValue additionalItemsSchema 
                            |> Fuzz.Extra.variableList 0 100
                        )

                EQ ->
                    -- number of schemas is equal to min, use all the schemas + up to 100 additional
                    Fuzz.map2 (++)
                        ( fuzzTupleItems_ subSchemas )
                        ( schemaValue additionalItemsSchema 
                            |> Fuzz.Extra.variableList 0 100
                        )

                GT ->
                    -- number of schemas is less than min, use all the schemas + at least up to min additional
                    Fuzz.map2 (++)
                        ( fuzzTupleItems_ subSchemas )
                        ( schemaValue additionalItemsSchema
                              |> Fuzz.Extra.variableList 
                                   (minItems - subSchemaCount) 
                                   (minItems - subSchemaCount + 100) 
                        )


tupleWithMinAndMaxItemsFuzzer : Maybe (List Schema) -> Maybe Schema -> Int -> Int -> Fuzzer (List Value)
tupleWithMinAndMaxItemsFuzzer maybeItems maybeAdditionalItems minItems maxItems =
    case (maybeItems, maybeAdditionalItems) of
        ( Nothing, Nothing ) ->
            -- no schema or additional specified, fill array between min and max with "dummy"
            anyValueFuzzer
                |> Fuzz.Extra.variableList minItems maxItems

        ( Nothing, Just additionalItemsSchema ) ->
            -- only additional specified, fill array between min and max with additional
            schemaValue additionalItemsSchema
                |> Fuzz.Extra.variableList minItems maxItems

        ( Just subSchemas, Nothing ) ->
            let
                subSchemaCount =
                    List.length subSchemas

                compareMin = 
                    compare minItems subSchemaCount

                compareMax =
                    compare maxItems subSchemaCount

            in
            case (compareMin, compareMax) of
               ( LT, LT ) ->    
                   -- number of schemas is greater than both min and max,
                   -- only use up to max schemas
                   fuzzTupleItems_ (List.take maxItems subSchemas)

               ( LT, EQ ) ->    
                   -- number of schemas is greater than min and equal to max,
                   -- use all the schemas
                   fuzzTupleItems_ subSchemas

               ( LT, GT ) ->    
                   -- number of schemas is between min and max,
                   -- use all the schemas + up to the remainder of max "dummy"
                   Fuzz.map2 (++)
                       ( fuzzTupleItems_ subSchemas )
                       ( anyValueFuzzer
                             |> Fuzz.Extra.variableList 0 
                                  (maxItems - subSchemaCount)
                       )

               ( EQ, LT ) ->    
                   -- nonsensical
                   Fuzz.constant []

               ( EQ, EQ ) ->    
                   -- number of schemas is equal to both min and max,
                   -- use all the schemas
                   fuzzTupleItems_ subSchemas

               ( EQ, GT ) ->    
                   -- number of schemas is equal to min and less than max,
                   -- use all the schemas + up to the remainder of max "dummy"
                   Fuzz.map2 (++)
                       ( fuzzTupleItems_ subSchemas )
                       ( anyValueFuzzer
                             |> Fuzz.Extra.variableList 0 
                                  (maxItems - subSchemaCount)
                       )

               ( GT, LT ) ->    
                   -- nonsensical
                   Fuzz.constant []

               ( GT, EQ ) ->    
                   -- nonsensical
                   Fuzz.constant []

               ( GT, GT ) ->    
                   -- number of schemas is less than both min and max
                   -- use all the schemas + between the remainder to min and the remainder to max "dummy"
                   Fuzz.map2 (++)
                       ( fuzzTupleItems_ subSchemas )
                       ( anyValueFuzzer
                           |> Fuzz.Extra.variableList 
                                (minItems - subSchemaCount)
                                (maxItems - subSchemaCount)
                       )


        ( Just subSchemas, Just additionalItemsSchema ) ->
            let
                subSchemaCount =
                    List.length subSchemas

                compareMin = 
                    compare minItems subSchemaCount

                compareMax =
                    compare maxItems subSchemaCount

            in
            case (compareMin, compareMax) of
               ( LT, LT ) ->    
                   -- number of schemas is greater than both min and max,
                   -- only use up to max schemas
                   fuzzTupleItems_ (List.take maxItems subSchemas)

               ( LT, EQ ) ->    
                   -- number of schemas is greater than min and equal to max,
                   -- use all the schemas
                   fuzzTupleItems_ subSchemas

               ( LT, GT ) ->    
                   -- number of schemas is between min and max,
                   -- use all the schemas + up to the remainder of max additional
                   Fuzz.map2 (++)
                       ( fuzzTupleItems_ subSchemas )
                       ( schemaValue additionalItemsSchema
                             |> Fuzz.Extra.variableList 0 
                                  (maxItems - subSchemaCount)
                       )

               ( EQ, LT ) ->    
                   -- nonsensical
                   Fuzz.constant []

               ( EQ, EQ ) ->    
                   -- number of schemas is equal to min and max,
                   -- use all the schemas
                   fuzzTupleItems_ subSchemas

               ( EQ, GT ) ->    
                   -- number of schemas is equal to min and less than max,
                   -- use all the schemas + up to the remainder of max additional
                   Fuzz.map2 (++)
                       ( fuzzTupleItems_ subSchemas )
                       ( schemaValue additionalItemsSchema
                             |> Fuzz.Extra.variableList 0 
                                  (maxItems - subSchemaCount)
                       )

               ( GT, LT ) ->    
                   -- nonsensical
                   Fuzz.constant []

               ( GT, EQ ) ->    
                   -- nonsensical
                   Fuzz.constant []

               ( GT, GT ) ->    
                   -- number of schemas is less than both min and max,
                   -- use all the schemas + between the remainder to min and the remainder to max additional
                   Fuzz.map2 (++)
                       ( fuzzTupleItems_ subSchemas )
                       ( schemaValue additionalItemsSchema
                           |> Fuzz.Extra.variableList 
                                (minItems - subSchemaCount)
                                (maxItems - subSchemaCount)
                       )


fuzzTupleItems_ : List Schema -> Fuzzer (List Value)
fuzzTupleItems_ schemas =
    schemas
        |> List.map schemaValue
        |> Fuzz.Extra.sequence



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


anyOfFuzzer : BaseCombinatorSchema -> Fuzzer Value
anyOfFuzzer anyOfSchema =
    anyOfSchema.subSchemas
        |> List.map (schemaValue >> (,) 1)
        |> Fuzz.frequency


-- TODO: something more interesting

anyValueFuzzer : Fuzzer Value
anyValueFuzzer = nullFuzzer

