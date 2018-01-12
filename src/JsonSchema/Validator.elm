module JsonSchema.Validator exposing (Error, ErrorMessage(..), ValidatedValue(..), getValidatedValue, validate)

{-| Validating a JSON Schema.

It does not yet validate the `format` keyword.

@docs validate, getValidatedValue
@docs Error, ErrorMessage, ValidatedValue

-}

import Array
import Dict exposing (Dict)
import Json.Decode exposing (..)
import Json.Pointer
import JsonSchema.Model exposing (..)
import Regex


{-| The error type from a validation. It contains a JSON Pointer to where
the validation error occured, and an error message
-}
type alias Error =
    ( Json.Pointer.Pointer, ErrorMessage )


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


{-| Validate a JSON Value against a schema.

If validation fails, a list of errors is returned, otherwise the list is empty.

It does not yet validate the `format` keyword.

-}
validate : Schema -> Value -> List Error
validate schema value =
    toErrors <| getValidatedValue schema value


{-| -}
getValidatedValue : Schema -> Value -> ValidatedValue
getValidatedValue schema v =
    case schema of
        Object objectSchema ->
            let
                thing : Result String (Dict String Value)
                thing =
                    decodeValue (dict value) v
            in
            thing
                |> Result.map (validateObject objectSchema)
                |> getDecodeError

        Array arraySchema ->
            decodeValue (array value) v
                |> Result.map (validateArray arraySchema)
                |> getDecodeError

        String stringSchema ->
            decodeValue string v
                |> Result.map (validateString stringSchema)
                |> getDecodeError

        Integer integerSchema ->
            decodeValue int v
                |> Result.map (validateInteger integerSchema)
                |> getDecodeError

        Number numberSchema ->
            decodeValue float v
                |> Result.map (validateNumber numberSchema)
                |> getDecodeError

        Boolean booleanSchema ->
            decodeValue bool v
                |> Result.map (always <| Valid)
                |> getDecodeError

        Null nullSchema ->
            decodeValue (null []) v
                |> Result.map Invalid
                |> getDecodeError

        OneOf oneOfSchema ->
            oneOfSchema.subSchemas
                |> List.map (flip validate v)
                |> List.filter (not << List.isEmpty)
                |> List.length
                |> (\i ->
                        case i of
                            0 ->
                                [ ( [], TooManyMatches ) ]

                            1 ->
                                []

                            _ ->
                                [ ( [], TooFewMatches ) ]
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
                                [ ( [], TooFewMatches ) ]

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
                                [ ( [], TooFewMatches ) ]
                   )
                |> Invalid

        Ref _ ->
            Valid

        Lazy f ->
            LazyValue <| validate (f ()) v

        Fallback _ ->
            Valid


validateObject : ObjectSchema -> Dict String Value -> ValidatedValue
validateObject schema values =
    schema.properties
        |> List.map (validateProperty values)
        |> combineValidations (ObjectValue values)


validateArray : ArraySchema -> Array.Array Value -> ValidatedValue
validateArray schema values =
    combineValidations (ArrayValue values)
        [ validateItems schema.items values
        , validateMinItems schema.minItems values
        , validateMaxItems schema.maxItems values
        ]


validateItems : Maybe Schema -> Array.Array Value -> ValidatedValue
validateItems items values =
    case items of
        Nothing ->
            ArrayValue values

        Just itemSchema ->
            Array.toList values
                |> List.indexedMap (\i v -> List.map (appendName (toString i)) (validate itemSchema v))
                |> List.concat
                |> (\a ->
                        case a of
                            [] ->
                                ArrayValue values

                            errorList ->
                                Invalid errorList
                   )


validateMinItems : Maybe Int -> Array.Array Value -> ValidatedValue
validateMinItems int values =
    case int of
        Nothing ->
            ArrayValue values

        Just minItems ->
            if Array.length values >= minItems then
                ArrayValue values
            else
                Invalid [ ( [], HasFewerItemsThan minItems ) ]


validateMaxItems : Maybe Int -> Array.Array Value -> ValidatedValue
validateMaxItems int values =
    case int of
        Nothing ->
            ArrayValue values

        Just maxItems ->
            if Array.length values <= maxItems then
                ArrayValue values
            else
                Invalid [ ( [], HasMoreItemsThan maxItems ) ]


{-| -}
validateString : StringSchema -> String -> ValidatedValue
validateString schema string =
    combineValidations (StringValue string)
        [ validateMinLength schema.minLength string
        , validateMaxLength schema.maxLength string
        , validatePattern schema.pattern string
        , validateEnum schema.enum string
        ]


validateMinLength : Maybe Int -> String -> ValidatedValue
validateMinLength minLength string =
    case minLength of
        Nothing ->
            StringValue string

        Just minLength ->
            if String.length string >= minLength then
                StringValue string
            else
                Invalid [ ( [], IsShorterThan minLength ) ]


validateMaxLength : Maybe Int -> String -> ValidatedValue
validateMaxLength maxLength string =
    case maxLength of
        Nothing ->
            StringValue string

        Just maxLength ->
            if String.length string <= maxLength then
                StringValue string
            else
                Invalid [ ( [], IsLongerThan maxLength ) ]


validatePattern : Maybe String -> String -> ValidatedValue
validatePattern pattern string =
    case pattern of
        Nothing ->
            StringValue string

        Just pattern ->
            if Regex.contains (Regex.regex pattern) string then
                StringValue string
            else
                Invalid [ ( [], DoesNotMatchPattern pattern ) ]


validateEnum : Maybe (List a) -> a -> ValidatedValue
validateEnum enum a =
    case enum of
        Nothing ->
            Valid

        Just enum ->
            if List.member a enum then
                Valid
            else
                Invalid [ ( [], NotInEnumeration ) ]


validateProperty : Dict String Value -> ObjectProperty -> ValidatedValue
validateProperty values property =
    case property of
        Required name schema ->
            case Dict.get name values of
                Nothing ->
                    Invalid [ ( [], RequiredPropertyMissing name ) ]

                Just value ->
                    List.map (appendName name) (validate schema value)
                        |> errorsOrValid (ObjectValue values)

        Optional name schema ->
            case Dict.get name values of
                Nothing ->
                    ObjectValue values

                Just value ->
                    List.map (appendName name) (validate schema value)
                        |> errorsOrValid (ObjectValue values)


validateInteger : IntegerSchema -> Int -> ValidatedValue
validateInteger schema integer =
    combineValidations (IntegerValue integer)
        [ validateEnum schema.enum integer
        , validateMinimum (Maybe.map toFloat schema.minimum) (toFloat integer)
        , validateMaximum (Maybe.map toFloat schema.maximum) (toFloat integer)
        ]


validateNumber : NumberSchema -> Float -> ValidatedValue
validateNumber schema number =
    combineValidations (FloatValue number)
        [ validateEnum schema.enum number
        , validateMinimum schema.minimum number
        , validateMaximum schema.maximum number
        ]


validateMinimum : Maybe Float -> Float -> ValidatedValue
validateMinimum minimum number =
    case minimum of
        Nothing ->
            FloatValue number

        Just minimum ->
            if number >= minimum then
                FloatValue number
            else
                Invalid [ ( [], IsLessThan minimum ) ]


validateMaximum : Maybe Float -> Float -> ValidatedValue
validateMaximum maximum number =
    case maximum of
        Nothing ->
            FloatValue number

        Just maximum ->
            if number <= maximum then
                FloatValue number
            else
                Invalid [ ( [], IsMoreThan maximum ) ]


getDecodeError : Result String ValidatedValue -> ValidatedValue
getDecodeError res =
    case res of
        Ok validatedValue ->
            validatedValue

        Err e ->
            Invalid [ ( [], DecodeError e ) ]


appendName : String -> Error -> Error
appendName name ( pointer, error ) =
    ( name :: pointer, error )


{-| -}
type ValidatedValue
    = Invalid (List Error)
    | StringValue String
    | ArrayValue (Array.Array Value)
    | ObjectValue (Dict String Value)
    | FloatValue Float
    | IntegerValue Int
    | LazyValue (List Error)
    | Valid


toErrors : ValidatedValue -> List Error
toErrors validatedValue =
    case validatedValue of
        Invalid listErrors ->
            listErrors

        LazyValue listErrors ->
            listErrors

        _ ->
            []


combineValidations : ValidatedValue -> List ValidatedValue -> ValidatedValue
combineValidations success validations =
    List.concatMap toErrors validations
        |> errorsOrValid success


errorsOrValid : ValidatedValue -> List Error -> ValidatedValue
errorsOrValid valid errorList =
    case errorList of
        [] ->
            valid

        errors ->
            Invalid errors
