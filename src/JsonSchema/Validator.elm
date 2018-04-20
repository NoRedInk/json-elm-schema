module JsonSchema.Validator exposing (Error, ErrorMessage(..), ValidatedValue(..), getValidatedValue, validate)

{-| Validating a JSON Schema.

It does not yet validate the `format` keyword.

@docs validate, getValidatedValue
@docs Error, ErrorMessage, ValidatedValue

-}

import Array
import Dict exposing (Dict)
import Json.Decode exposing (..)
import Json.Pointer exposing (Pointer)
import JsonSchema.Model exposing (..)
import Regex


{-| The error type from a validation. It contains a JSON Pointer to where
the validation error occured, and an error message
-}
type alias Error =
    ( Pointer, ErrorMessage )


{-| An error message from validation
-}
type ErrorMessage
    = IsShorterThan Int
    | IsLongerThan Int
    | DoesNotMatchPattern String
    | NotInEnumeration
    | RequiredPropertyMissing String
    | HasFewerItemsThan Int
    | HasMoreItemsThan Int
    | IsLessThan Float
    | IsMoreThan Float
    | TooManyMatches
    | TooFewMatches
    | DecodeError String


{-| Either data extracted from validated json, or a list of errors received while validating
-}
type ValidatedValue
    = Invalid (List Error)
    | StringValue String
    | ArrayValue (Array.Array ValidatedValue)
    | TupleValue ( Int, Array.Array ValidatedValue )
    | ObjectValue (Dict String ValidatedValue)
    | BoolValue Bool
    | FloatValue Float
    | IntValue Int
    | LazyValue (List Error)
    | Valid Value


{-| Validate a JSON Value against a schema.

If validation fails, a list of errors is returned, otherwise the list is empty.

It does not yet validate the `format` keyword.

-}
validate : Schema -> Value -> List Error
validate schema value =
    toErrors <| getValidatedValue [] schema value


{-| This function validates your JSON against a schema.

If validation succeeds, you can then gain access to the data you need

If validation fails, you can extract a list of errors

-}
getValidatedValue : Pointer -> Schema -> Value -> ValidatedValue
getValidatedValue pointer schema v =
    case schema of
        Object objectSchema ->
            decodeValue (dict value) v
                |> Result.map (validateObject pointer objectSchema)
                |> getDecodeError pointer

        Array arraySchema ->
            decodeValue (array value) v
                |> Result.map (validateArray pointer arraySchema)
                |> getDecodeError pointer

        Tuple tupleSchema ->
            decodeValue (array value) v
                |> Result.map (validateTuple pointer tupleSchema)
                |> getDecodeError pointer

        String stringSchema ->
            decodeValue string v
                |> Result.map (validateString pointer stringSchema)
                |> getDecodeError pointer

        Integer integerSchema ->
            decodeValue int v
                |> Result.map (validateInteger pointer integerSchema)
                |> getDecodeError pointer

        Number numberSchema ->
            decodeValue float v
                |> Result.map (validateNumber pointer numberSchema)
                |> getDecodeError pointer

        Boolean booleanSchema ->
            decodeValue bool v
                |> Result.map BoolValue
                |> getDecodeError pointer

        Null nullSchema ->
            decodeValue (null []) v
                |> Result.map Invalid
                |> getDecodeError pointer

        OneOf oneOfSchema ->
            oneOfSchema.subSchemas
                |> List.map (flip validate v)
                |> List.filter (not << List.isEmpty)
                |> List.length
                |> (\i ->
                        case i of
                            0 ->
                                [ ( pointer, TooManyMatches ) ]

                            1 ->
                                []

                            _ ->
                                [ ( pointer, TooFewMatches ) ]
                   )
                |> Invalid

        AnyOf anyOfSchema ->
            anyOfSchema.subSchemas
                |> List.map (flip validate v)
                |> List.filter List.isEmpty
                |> List.length
                |> (\i ->
                        case i of
                            0 ->
                                [ ( pointer, TooFewMatches ) ]

                            _ ->
                                []
                   )
                |> Invalid

        AllOf allOfSchema ->
            allOfSchema.subSchemas
                |> List.map (flip validate v)
                |> List.filter (not << List.isEmpty)
                |> List.length
                |> (\i ->
                        case i of
                            0 ->
                                []

                            _ ->
                                [ ( pointer, TooFewMatches ) ]
                   )
                |> Invalid

        Ref _ ->
            Valid v

        Lazy f ->
            LazyValue <| validate (f ()) v

        Fallback a ->
            Valid a


validateObject : Pointer -> ObjectSchema -> Dict String Value -> ValidatedValue
validateObject pointer schema values =
    combineValidations (ObjectValue (Dict.map (\_ v -> Valid v) values))
        (List.map (validateProperty pointer schema values) schema.properties)


validateProperty : Pointer -> ObjectSchema -> Dict String Value -> ObjectProperty -> ValidatedValue
validateProperty pointer objectSchema values property =
    case property of
        Required name schema ->
            case Dict.get name values of
                Nothing ->
                    Invalid [ ( pointer, RequiredPropertyMissing name ) ]

                Just value ->
                    getValidatedValue
                        (List.reverse <| name :: pointer)
                        schema
                        value

        Optional name schema ->
            case Dict.get name values of
                Nothing ->
                    ObjectValue (Dict.map (\_ v -> Valid v) values)

                Just value ->
                    getValidatedValue
                        (List.reverse <| name :: pointer)
                        schema
                        value


validateArray : Pointer -> ArraySchema -> Array.Array Value -> ValidatedValue
validateArray pointer schema values =
    combineValidations (ArrayValue <| Array.map Valid values)
        [ validateItems pointer schema.items values
        , validateMinItems pointer schema.minItems values
        , validateMaxItems pointer schema.maxItems values
        ]


validateItems : Pointer -> Maybe Schema -> Array.Array Value -> ValidatedValue
validateItems pointer items values =
    case items of
        Nothing ->
            ArrayValue <| Array.map Valid values

        Just itemSchema ->
            combineValidations
                (values
                    |> Array.indexedMap (validateIndexedItem pointer itemSchema)
                    |> ArrayValue
                )
                []


validateIndexedItem : Pointer -> Schema -> Int -> Value -> ValidatedValue
validateIndexedItem pointer itemSchema index value =
    getValidatedValue
        (List.reverse <| toString index :: pointer)
        itemSchema
        value


validateMinItems : Pointer -> Maybe Int -> Array.Array Value -> ValidatedValue
validateMinItems pointer int values =
    case int of
        Nothing ->
            ArrayValue <| Array.map Valid values

        Just minItems ->
            if Array.length values >= minItems then
                ArrayValue <| Array.map Valid values
            else
                Invalid [ ( pointer, HasFewerItemsThan minItems ) ]


validateMaxItems : Pointer -> Maybe Int -> Array.Array Value -> ValidatedValue
validateMaxItems pointer int values =
    case int of
        Nothing ->
            ArrayValue <| Array.map Valid values

        Just maxItems ->
            if Array.length values <= maxItems then
                ArrayValue <| Array.map Valid values
            else
                Invalid [ ( pointer, HasMoreItemsThan maxItems ) ]


validateTuple : Pointer -> TupleSchema -> Array.Array Value -> ValidatedValue
validateTuple pointer schema values =
    combineValidations
        (TupleValue ( Array.length values, Array.map Valid values ))
        [ validateTupleItems pointer schema.items schema.additionalItems values
        , validateMinItems pointer schema.minItems values
        , validateMaxItems pointer schema.maxItems values
        ]


validateTupleItems : Pointer -> Maybe (List Schema) -> Maybe Schema -> Array.Array Value -> ValidatedValue
validateTupleItems pointer items additionalItems values =
    let
        -- validateAll_ : Schema -> Array.Array Value -> List Error
        -- validateAll_ schema vals =
        --     List.concat
        --         (List.indexedMap
        --             (\i v ->
        --                 List.map (appendName (toString i)) (validate schema v)
        --             )
        --             (Array.toList vals)
        --         )
        validateAll_ : Schema -> Array.Array Value -> List ValidatedValue
        validateAll_ schema vals =
            List.indexedMap
                (validateIndexedItem pointer schema)
                (Array.toList vals)

        validateMap_ : List Schema -> Array.Array Value -> List ValidatedValue
        validateMap_ schemas vals =
            List.map3 (validateIndexedItem pointer)
                schemas
                (List.range 0 (Array.length vals - 1))
                (Array.toList vals)
    in
    case ( items, additionalItems ) of
        ( Nothing, Nothing ) ->
            TupleValue ( 0, Array.empty )

        ( Nothing, Just additionalItemSchema ) ->
            TupleValue
                ( Array.length values
                , Array.indexedMap (validateIndexedItem pointer additionalItemSchema) values
                )

        ( Just itemSchemas, Nothing ) ->
            {--Note: the spec doesn't specify what should happen if you
             have more schemas than values. If you have less schemas, it
             suggests the extra values won't be validated.

             http://json-schema.org/latest/json-schema-validation.html#rfc.section.6.4.1

             In a strict world it should really be an error to have more
             schemas than values, but to go along with the general
             permissiveness of JSON Schema, we just validate to the lowest
             length.  --}
            TupleValue
                ( Array.length values
                , Array.fromList (validateMap_ itemSchemas values)
                )

        ( Just itemSchemas, Just additionalItemSchema ) ->
            let
                ( tupleValues, remainder ) =
                    arraySplitAt (List.length itemSchemas) values
            in
            TupleValue
                ( Array.length values
                , Array.fromList <|
                    List.concat
                        [ validateMap_ itemSchemas tupleValues
                        , validateAll_ additionalItemSchema remainder
                        ]
                )


{-| -}
validateString : Pointer -> StringSchema -> String -> ValidatedValue
validateString pointer schema string =
    combineValidations (StringValue string)
        [ validateMinLength pointer schema.minLength string
        , validateMaxLength pointer schema.maxLength string
        , validatePattern pointer schema.pattern string
        , validateEnum StringValue pointer schema.enum string
        ]


validateMinLength : Pointer -> Maybe Int -> String -> ValidatedValue
validateMinLength pointer minLength string =
    case minLength of
        Nothing ->
            StringValue string

        Just minLength ->
            if String.length string >= minLength then
                StringValue string
            else
                Invalid [ ( pointer, IsShorterThan minLength ) ]


validateMaxLength : Pointer -> Maybe Int -> String -> ValidatedValue
validateMaxLength pointer maxLength string =
    case maxLength of
        Nothing ->
            StringValue string

        Just maxLength ->
            if String.length string <= maxLength then
                StringValue string
            else
                Invalid [ ( pointer, IsLongerThan maxLength ) ]


validatePattern : Pointer -> Maybe String -> String -> ValidatedValue
validatePattern pointer pattern string =
    case pattern of
        Nothing ->
            StringValue string

        Just pattern ->
            if Regex.contains (Regex.regex pattern) string then
                StringValue string
            else
                Invalid [ ( pointer, DoesNotMatchPattern pattern ) ]


validateEnum : (a -> ValidatedValue) -> Pointer -> Maybe (List a) -> a -> ValidatedValue
validateEnum constructor pointer enum value =
    case enum of
        Nothing ->
            constructor value

        Just enum ->
            if List.member value enum then
                constructor value
            else
                Invalid [ ( pointer, NotInEnumeration ) ]


validateInteger : Pointer -> IntegerSchema -> Int -> ValidatedValue
validateInteger pointer schema integer =
    combineValidations (IntValue integer)
        [ validateEnum IntValue pointer schema.enum integer
        , validateMinimum pointer (Maybe.map toFloat schema.minimum) (toFloat integer)
        , validateMaximum pointer (Maybe.map toFloat schema.maximum) (toFloat integer)
        ]


validateNumber : Pointer -> NumberSchema -> Float -> ValidatedValue
validateNumber pointer schema number =
    combineValidations (FloatValue number)
        [ validateEnum FloatValue pointer schema.enum number
        , validateMinimum pointer schema.minimum number
        , validateMaximum pointer schema.maximum number
        ]


validateMinimum : Pointer -> Maybe Float -> Float -> ValidatedValue
validateMinimum pointer minimum number =
    case minimum of
        Nothing ->
            FloatValue number

        Just minimum ->
            if number >= minimum then
                FloatValue number
            else
                Invalid [ ( pointer, IsLessThan minimum ) ]


validateMaximum : Pointer -> Maybe Float -> Float -> ValidatedValue
validateMaximum pointer maximum number =
    case maximum of
        Nothing ->
            FloatValue number

        Just maximum ->
            if number <= maximum then
                FloatValue number
            else
                Invalid [ ( pointer, IsMoreThan maximum ) ]


getDecodeError : Pointer -> Result String ValidatedValue -> ValidatedValue
getDecodeError pointer res =
    case res of
        Ok validatedValue ->
            validatedValue

        Err e ->
            Invalid [ ( pointer, DecodeError e ) ]


toErrors : ValidatedValue -> List Error
toErrors validatedValue =
    case validatedValue of
        Invalid listErrors ->
            listErrors

        LazyValue listErrors ->
            listErrors

        ArrayValue values ->
            values
                |> Array.toList
                |> List.map toErrors
                |> List.concat

        TupleValue ( length, values ) ->
            values
                |> Array.toList
                |> List.map toErrors
                |> List.concat

        ObjectValue valueDict ->
            valueDict
                |> Dict.values
                |> List.map toErrors
                |> List.concat

        _ ->
            []


combineValidations : ValidatedValue -> List ValidatedValue -> ValidatedValue
combineValidations success validations =
    case List.concatMap toErrors validations of
        [] ->
            success

        errors ->
            Invalid errors


appendName : String -> Error -> Error
appendName name ( pointer, error ) =
    ( name :: pointer, error )


{-| Split an array into two arrays, the first ending at and the second starting at the given index
NOTE: copied from Array.Extra
-}
arraySplitAt : Int -> Array.Array a -> ( Array.Array a, Array.Array a )
arraySplitAt index xs =
    -- TODO: refactor (written this way to help avoid Array bugs)
    let
        len =
            Array.length xs
    in
    case ( index > 0, index < len ) of
        ( True, True ) ->
            ( Array.slice 0 index xs, Array.slice index len xs )

        ( True, False ) ->
            ( xs, Array.empty )

        ( False, True ) ->
            ( Array.empty, xs )

        ( False, False ) ->
            ( Array.empty, Array.empty )
