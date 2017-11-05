module JsonSchema.Validator exposing (Error, ErrorMessage(..), validate)

{-| Validating a JSON Schema.

It does not yet validate the `format` keyword.

@docs validate
@docs Error, ErrorMessage

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


validateObject : ObjectSchema NoDefinitions -> Definitions -> Dict String Value -> List Error
validateObject schema definitions values =
    List.concatMap (validateProperty values definitions) schema.properties


validateArray : ArraySchema NoDefinitions -> Definitions -> Array.Array Value -> List Error
validateArray schema definitions values =
    validateItems schema.items definitions values
        ++ validateMinItems schema.minItems values
        ++ validateMaxItems schema.maxItems values


validateItems : Maybe SubSchema -> Definitions -> Array.Array Value -> List Error
validateItems items definitions values =
    case items of
        Nothing ->
            []

        Just itemSchema ->
            List.concat
                (List.indexedMap
                    (\i v ->
                        List.map
                            (appendName (toString i))
                            (validateSubSchema itemSchema definitions v)
                    )
                    (Array.toList values)
                )


validateMinItems : Maybe Int -> Array.Array Value -> List Error
validateMinItems int values =
    case int of
        Nothing ->
            []

        Just minItems ->
            if Array.length values >= minItems then
                []
            else
                [ ( [], HasFewerItemsThan minItems ) ]


validateMaxItems : Maybe Int -> Array.Array Value -> List Error
validateMaxItems int values =
    case int of
        Nothing ->
            []

        Just maxItems ->
            if Array.length values <= maxItems then
                []
            else
                [ ( [], HasMoreItemsThan maxItems ) ]


validateString : StringSchema -> String -> List Error
validateString schema string =
    validateMinLength schema.minLength string
        ++ validateMaxLength schema.maxLength string
        ++ validatePattern schema.pattern string
        ++ validateEnum schema.enum string


validateMinLength : Maybe Int -> String -> List Error
validateMinLength minLength string =
    case minLength of
        Nothing ->
            []

        Just minLength ->
            if String.length string >= minLength then
                []
            else
                [ ( [], IsShorterThan minLength ) ]


validateMaxLength : Maybe Int -> String -> List Error
validateMaxLength maxLength string =
    case maxLength of
        Nothing ->
            []

        Just maxLength ->
            if String.length string <= maxLength then
                []
            else
                [ ( [], IsLongerThan maxLength ) ]


validatePattern : Maybe String -> String -> List Error
validatePattern pattern string =
    case pattern of
        Nothing ->
            []

        Just pattern ->
            if Regex.contains (Regex.regex pattern) string then
                []
            else
                [ ( [], DoesNotMatchPattern pattern ) ]


validateEnum : Maybe (List a) -> a -> List Error
validateEnum enum a =
    case enum of
        Nothing ->
            []

        Just enum ->
            if List.member a enum then
                []
            else
                [ ( [], NotInEnumeration ) ]


validateProperty : Dict String Value -> Definitions -> ObjectProperty NoDefinitions -> List Error
validateProperty values definitions property =
    case property of
        Required name schema ->
            case Dict.get name values of
                Nothing ->
                    [ ( [], RequiredPropertyMissing name ) ]

                Just value ->
                    List.map (appendName name) (validateSubSchema schema definitions value)

        Optional name schema ->
            case Dict.get name values of
                Nothing ->
                    []

                Just value ->
                    List.map (appendName name) (validateSubSchema schema definitions value)


validateInteger : IntegerSchema -> Int -> List Error
validateInteger schema integer =
    validateEnum schema.enum integer
        ++ validateMinimum (Maybe.map toFloat schema.minimum) (toFloat integer)
        ++ validateMaximum (Maybe.map toFloat schema.maximum) (toFloat integer)


validateNumber : NumberSchema -> Float -> List Error
validateNumber schema number =
    validateEnum schema.enum number
        ++ validateMinimum schema.minimum number
        ++ validateMaximum schema.maximum number


validateMinimum : Maybe Float -> Float -> List Error
validateMinimum minimum number =
    case minimum of
        Nothing ->
            []

        Just minimum ->
            if number >= minimum then
                []
            else
                [ ( [], IsLessThan minimum ) ]


validateMaximum : Maybe Float -> Float -> List Error
validateMaximum maximum number =
    case maximum of
        Nothing ->
            []

        Just maximum ->
            if number <= maximum then
                []
            else
                [ ( [], IsMoreThan maximum ) ]


{-| Validate a JSON Value against a schema.

If validation fails, a list of errors is returned, otherwise the list is empty.

It does not yet validate the `format` keyword.

-}
validate : Schema -> Value -> List Error
validate schema v =
    case toSubSchema schema of
        ( definitions, subSchema ) ->
            validateSubSchema subSchema definitions v


validateSubSchema : SubSchema -> Definitions -> Value -> List Error
validateSubSchema schema definitions v =
    case schema of
        Object objectSchema ->
            decodeValue (dict value) v
                |> Result.map (validateObject objectSchema definitions)
                |> getDecodeError

        Array arraySchema ->
            decodeValue (array value) v
                |> Result.map (validateArray arraySchema definitions)
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
                |> Result.map (always [])
                |> getDecodeError

        Null nullSchema ->
            decodeValue (null []) v
                |> getDecodeError

        OneOf oneOfSchema ->
            oneOfSchema.subSchemas
                |> List.map (\s -> validateSubSchema s definitions v)
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

        AnyOf anyOfSchema ->
            anyOfSchema.subSchemas
                |> List.map (\s -> validateSubSchema s definitions v)
                |> List.filter List.isEmpty
                |> List.length
                |> (\i ->
                        case i of
                            0 ->
                                [ ( [], TooFewMatches ) ]

                            _ ->
                                []
                   )

        AllOf allOfSchema ->
            allOfSchema.subSchemas
                |> List.map (\s -> validateSubSchema s definitions v)
                |> List.filter (not << List.isEmpty)
                |> List.length
                |> (\i ->
                        case i of
                            0 ->
                                []

                            _ ->
                                [ ( [], TooFewMatches ) ]
                   )

        Ref refSchema ->
            case Dict.get refSchema.ref definitions of
                Just definition ->
                    validateSubSchema definition definitions v

                Nothing ->
                    []

        Fallback _ ->
            []


getDecodeError : Result String (List Error) -> List Error
getDecodeError res =
    case res of
        Ok e ->
            e

        Err e ->
            [ ( [], DecodeError e ) ]


appendName : String -> Error -> Error
appendName name ( pointer, error ) =
    ( name :: pointer, error )
