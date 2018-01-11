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


validateObject : ObjectSchema -> Dict String Value -> List Error
validateObject schema values =
    List.concatMap (validateProperty values) schema.properties


validateArray : ArraySchema -> Array.Array Value -> List Error
validateArray schema values =
    validateItems schema.items values
        ++ validateMinItems schema.minItems values
        ++ validateMaxItems schema.maxItems values


validateTuple : TupleSchema -> Array.Array Value -> List Error
validateTuple schema values =
    validateTupleItems schema.items schema.additionalItems values
        ++ validateMinItems schema.minItems values
        ++ validateMaxItems schema.maxItems values

validateItems : Maybe Schema -> Array.Array Value -> List Error
validateItems items values =
    case items of
        Nothing ->
            []

        Just itemSchema ->
            List.concat (List.indexedMap (\i v -> List.map (appendName (toString i)) (validate itemSchema v)) (Array.toList values))

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


validateTupleItems : Maybe (List Schema) -> Maybe Schema -> Array.Array Value -> List Error
validateTupleItems items additionalItems values =
    let
        validateAll_ : Schema -> Array.Array Value -> List Error
        validateAll_ schema vals =
            List.concat 
                ( List.indexedMap (\i v -> 
                    List.map (appendName (toString i)) (validate schema v)) 
                    (Array.toList vals)
                )

        validateMap_ : List Schema -> Array.Array Value -> List Error
        validateMap_ schemas vals =
            List.map3 validateWithIndex_
                ( List.range 0 ((Array.length vals) - 1))
                schemas
                ( Array.toList vals )
                   |> List.concat

        validateWithIndex_ i schema v =
            validate schema v
                |> List.map (appendName (toString i))
    in
    case ( items, additionalItems ) of
        ( Nothing, Nothing ) ->
            []

        ( Nothing, Just additionalItemSchema ) ->
            validateAll_ additionalItemSchema values

        ( Just itemSchemas, Nothing ) ->
            {-- Note: the spec doesn't specify what should happen if you
             have more schemas than values. If you have less schemas, it
             suggests the extra values won't be validated.
             
             http://json-schema.org/latest/json-schema-validation.html#rfc.section.6.4.1 
             
             In a strict world it should really be an error to have more
             schemas than values, but to go along with the general 
             permissiveness of JSON Schema, we just validate to the lowest 
             length.  --}
                 
            validateMap_ itemSchemas values

        ( Just itemSchemas, Just additionalItemSchema ) ->
            let
                ( tupleValues, remainder ) = 
                    arraySplitAt (List.length itemSchemas) values
            in
                ( validateMap_ itemSchemas tupleValues )
                    ++ ( validateAll_ additionalItemSchema remainder )


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


validateProperty : Dict String Value -> ObjectProperty -> List Error
validateProperty values property =
    case property of
        Required name schema ->
            case Dict.get name values of
                Nothing ->
                    [ ( [], RequiredPropertyMissing name ) ]

                Just value ->
                    List.map (appendName name) (validate schema value)

        Optional name schema ->
            case Dict.get name values of
                Nothing ->
                    []

                Just value ->
                    List.map (appendName name) (validate schema value)


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
    case schema of
        Object objectSchema ->
            decodeValue (dict value) v
                |> Result.map (validateObject objectSchema)
                |> getDecodeError

        Array arraySchema ->
            decodeValue (array value) v
                |> Result.map (validateArray arraySchema)
                |> getDecodeError

        Tuple tupleSchema ->
            decodeValue (array value) v
                |> Result.map (validateTuple tupleSchema)
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

        Ref _ ->
            []

        Lazy f ->
            validate (f ()) v

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
