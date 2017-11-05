module ValidatorSpec exposing (..)

import Expect
import Json.Encode as Encode
import JsonSchema exposing (..)
import JsonSchema.Model as Model
import JsonSchema.Validator as Validator
import Test exposing (..)


objectSchemaSpec : Test
objectSchemaSpec =
    let
        objectSchema : Schema
        objectSchema =
            object
                [ title "object schema title"
                , description "object schema description"
                , properties
                    [ optional "firstName" <| string []
                    , required "lastName" <| string []
                    ]
                ]
    in
    describe "object schema"
        [ test "validate valid" <|
            \() ->
                Encode.object [ ( "firstName", Encode.string "foo" ), ( "lastName", Encode.string "bar" ) ]
                    |> Validator.validate objectSchema
                    |> Expect.equal []
        , test "validate valid optional field missing" <|
            \() ->
                Encode.object [ ( "lastName", Encode.string "bar" ) ]
                    |> Validator.validate objectSchema
                    |> Expect.equal []
        , test "validate valid additionalItems" <|
            \() ->
                Encode.object [ ( "unknown", Encode.int 1 ), ( "firstName", Encode.string "foo" ), ( "lastName", Encode.string "bar" ) ]
                    |> Validator.validate objectSchema
                    |> Expect.equal []
        , test "validate optional field wrong type" <|
            \() ->
                Encode.object [ ( "firstName", Encode.int 1 ), ( "lastName", Encode.string "bar" ) ]
                    |> Validator.validate objectSchema
                    |> Expect.equal [ ( [ "firstName" ], Validator.DecodeError "Expecting a String but instead got: 1" ) ]
        , test "validate required field missing" <|
            \() ->
                Encode.object [ ( "firstName", Encode.string "bar" ) ]
                    |> Validator.validate objectSchema
                    |> Expect.equal [ ( [], Validator.RequiredPropertyMissing "lastName" ) ]
        , test "validate multiple errors" <|
            \() ->
                Encode.object [ ( "firstName", Encode.int 1 ) ]
                    |> Validator.validate objectSchema
                    |> Expect.equal
                        [ ( [ "firstName" ], Validator.DecodeError "Expecting a String but instead got: 1" )
                        , ( [], Validator.RequiredPropertyMissing "lastName" )
                        ]
        ]


arraySchemaSpec : Test
arraySchemaSpec =
    let
        arraySchema : Schema
        arraySchema =
            array
                [ title "array schema title"
                , description "array schema description"
                , items <| string []
                , minItems 3
                , maxItems 6
                ]
    in
    describe "array schema"
        [ test "validate valid" <|
            \() ->
                Encode.list [ Encode.string "foo", Encode.string "bar", Encode.string "baz" ]
                    |> Validator.validate arraySchema
                    |> Expect.equal []
        , test "validate too short" <|
            \() ->
                Encode.list [ Encode.string "foo", Encode.string "bar" ]
                    |> Validator.validate arraySchema
                    |> Expect.equal [ ( [], Validator.HasFewerItemsThan 3 ) ]
        , test "validate too long" <|
            \() ->
                Encode.list
                    [ Encode.string "foo"
                    , Encode.string "bar"
                    , Encode.string "baz"
                    , Encode.string "foo"
                    , Encode.string "bar"
                    , Encode.string "baz"
                    , Encode.string "foo"
                    ]
                    |> Validator.validate arraySchema
                    |> Expect.equal [ ( [], Validator.HasMoreItemsThan 6 ) ]
        , test "validate wrong item type" <|
            \() ->
                Encode.list
                    [ Encode.string "foo"
                    , Encode.string "bar"
                    , Encode.string "baz"
                    , Encode.int 1
                    , Encode.string "bar"
                    , Encode.string "baz"
                    ]
                    |> Validator.validate arraySchema
                    |> Expect.equal
                        [ ( [ "3" ]
                          , Validator.DecodeError "Expecting a String but instead got: 1"
                          )
                        ]
        , test "validate multiple errors" <|
            \() ->
                Encode.list
                    [ Encode.string "foo"
                    , Encode.int 1
                    ]
                    |> Validator.validate arraySchema
                    |> Expect.equal
                        [ ( [ "1" ]
                          , Validator.DecodeError "Expecting a String but instead got: 1"
                          )
                        , ( [], Validator.HasFewerItemsThan 3 )
                        ]
        ]


stringSchemaSpec : Test
stringSchemaSpec =
    let
        stringSchema : Schema
        stringSchema =
            string
                [ title "string schema title"
                , description "string schema description"
                , minLength 2
                , maxLength 8
                , pattern "f"
                , format dateTime
                ]
    in
    describe "string schema"
        [ test "validate valid" <|
            \() ->
                Encode.string "foo"
                    |> Validator.validate stringSchema
                    |> Expect.equal []
        , test "validate wrong pattern" <|
            \() ->
                Encode.string "goo"
                    |> Validator.validate stringSchema
                    |> Expect.equal [ ( [], Validator.DoesNotMatchPattern "f" ) ]
        , test "validate too short" <|
            \() ->
                Encode.string "f"
                    |> Validator.validate stringSchema
                    |> Expect.equal [ ( [], Validator.IsShorterThan 2 ) ]
        , test "validate too long" <|
            \() ->
                Encode.string "foooooooo"
                    |> Validator.validate stringSchema
                    |> Expect.equal [ ( [], Validator.IsLongerThan 8 ) ]
        , test "validate multiple errors" <|
            \() ->
                Encode.string "goooooooo"
                    |> Validator.validate stringSchema
                    |> Expect.equal [ ( [], Validator.IsLongerThan 8 ), ( [], Validator.DoesNotMatchPattern "f" ) ]
        ]


stringEnumSchemaSpec : Test
stringEnumSchemaSpec =
    let
        stringEnumSchema : Schema
        stringEnumSchema =
            string
                [ title "string schema title"
                , enum [ "a", "b" ]
                ]
    in
    describe "string enum schema"
        [ test "validate valid 1" <|
            \() ->
                Encode.string "a"
                    |> Validator.validate stringEnumSchema
                    |> Expect.equal []
        , test "validate valid 2" <|
            \() ->
                Encode.string "b"
                    |> Validator.validate stringEnumSchema
                    |> Expect.equal []
        , test "validate invalid" <|
            \() ->
                Encode.string "c"
                    |> Validator.validate stringEnumSchema
                    |> Expect.equal [ ( [], Validator.NotInEnumeration ) ]
        ]


integerEnumSchemaSpec : Test
integerEnumSchemaSpec =
    let
        integerEnumSchema : Schema
        integerEnumSchema =
            integer
                [ title "integer schema title"
                , enum [ 1, 2 ]
                ]
    in
    describe "integer enum schema"
        [ test "validate valid 1" <|
            \() ->
                Encode.int 1
                    |> Validator.validate integerEnumSchema
                    |> Expect.equal []
        , test "validate valid 2" <|
            \() ->
                Encode.int 2
                    |> Validator.validate integerEnumSchema
                    |> Expect.equal []
        , test "validate invalid" <|
            \() ->
                Encode.int 3
                    |> Validator.validate integerEnumSchema
                    |> Expect.equal [ ( [], Validator.NotInEnumeration ) ]
        ]


integerSchemaSpec : Test
integerSchemaSpec =
    let
        integerSchema : Schema
        integerSchema =
            integer
                [ title "integer schema title"
                , description "integer schema description"
                , minimum 2
                , maximum 8
                ]
    in
    describe "integer schema"
        [ test "validate valid" <|
            \() ->
                Encode.int 4
                    |> Validator.validate integerSchema
                    |> Expect.equal []
        , test "validate too small" <|
            \() ->
                Encode.int 1
                    |> Validator.validate integerSchema
                    |> Expect.equal [ ( [], Validator.IsLessThan 2 ) ]
        , test "validate too large" <|
            \() ->
                Encode.int 9
                    |> Validator.validate integerSchema
                    |> Expect.equal [ ( [], Validator.IsMoreThan 8 ) ]
        ]


numberSchemaSpec : Test
numberSchemaSpec =
    let
        numberSchema : Schema
        numberSchema =
            number
                [ title "number schema title"
                , description "number schema description"
                , minimum 2.5
                , maximum 8.3
                ]
    in
    describe "number schema"
        [ test "validate valid" <|
            \() ->
                Encode.float 4
                    |> Validator.validate numberSchema
                    |> Expect.equal []
        , test "validate too small" <|
            \() ->
                Encode.float 2.4
                    |> Validator.validate numberSchema
                    |> Expect.equal [ ( [], Validator.IsLessThan 2.5 ) ]
        , test "validate too large" <|
            \() ->
                Encode.float 8.4
                    |> Validator.validate numberSchema
                    |> Expect.equal [ ( [], Validator.IsMoreThan 8.3 ) ]
        ]


numberEnumSchemaSpec : Test
numberEnumSchemaSpec =
    let
        numberEnumSchema : Schema
        numberEnumSchema =
            number
                [ title "number schema title"
                , enum [ 1.2, 3.4 ]
                ]
    in
    describe "number enum schema"
        [ test "validate valid 1" <|
            \() ->
                Encode.float 1.2
                    |> Validator.validate numberEnumSchema
                    |> Expect.equal []
        , test "validate valid 2" <|
            \() ->
                Encode.float 3.4
                    |> Validator.validate numberEnumSchema
                    |> Expect.equal []
        , test "validate invalid" <|
            \() ->
                Encode.float 2.3
                    |> Validator.validate numberEnumSchema
                    |> Expect.equal [ ( [], Validator.NotInEnumeration ) ]
        ]


booleanSchemaSpec : Test
booleanSchemaSpec =
    let
        booleanSchema : Schema
        booleanSchema =
            boolean
                [ title "boolean schema title"
                , description "boolean schema description"
                ]
    in
    describe "boolean schema"
        [ test "validate valid" <|
            \() ->
                Encode.bool True
                    |> Validator.validate booleanSchema
                    |> Expect.equal []
        , test "validate invalid type" <|
            \() ->
                Encode.int 1
                    |> Validator.validate booleanSchema
                    |> Expect.equal [ ( [], Validator.DecodeError "Expecting a Bool but instead got: 1" ) ]
        ]


nullSchemaSpec : Test
nullSchemaSpec =
    let
        nullSchema : Schema
        nullSchema =
            null
                [ title "null schema title"
                , description "null schema description"
                ]
    in
    describe "null schema"
        [ test "validate valid" <|
            \() ->
                Encode.null
                    |> Validator.validate nullSchema
                    |> Expect.equal []
        , test "validate invalid type" <|
            \() ->
                Encode.int 1
                    |> Validator.validate nullSchema
                    |> Expect.equal [ ( [], Validator.DecodeError "Expecting null but instead got: 1" ) ]
        ]


oneOfSpec : Test
oneOfSpec =
    let
        oneOfSchema =
            oneOf
                [ title "oneOf schema title"
                , description "oneOf schema description"
                ]
                [ string [ pattern "a" ], string [ pattern "b" ] ]
    in
    describe "oneOf schema"
        [ test "validate valid 1" <|
            \() ->
                Encode.string "a"
                    |> Validator.validate oneOfSchema
                    |> Expect.equal []
        , test "validate valid 2" <|
            \() ->
                Encode.string "b"
                    |> Validator.validate oneOfSchema
                    |> Expect.equal []
        , test "validate no match" <|
            \() ->
                Encode.string "c"
                    |> Validator.validate oneOfSchema
                    |> Expect.equal [ ( [], Validator.TooFewMatches ) ]
        , test "validate too many" <|
            \() ->
                Encode.string "ab"
                    |> Validator.validate oneOfSchema
                    |> Expect.equal [ ( [], Validator.TooManyMatches ) ]
        ]


anyOfSpec : Test
anyOfSpec =
    let
        anyOfSchema =
            anyOf
                [ title "anyOf schema title"
                , description "anyOf schema description"
                ]
                [ string [ pattern "a" ], string [ pattern "b" ] ]
    in
    describe "anyOf schema"
        [ test "validate valid 1" <|
            \() ->
                Encode.string "a"
                    |> Validator.validate anyOfSchema
                    |> Expect.equal []
        , test "validate valid 2" <|
            \() ->
                Encode.string "b"
                    |> Validator.validate anyOfSchema
                    |> Expect.equal []
        , test "validate valid 3" <|
            \() ->
                Encode.string "ab"
                    |> Validator.validate anyOfSchema
                    |> Expect.equal []
        , test "validate no match" <|
            \() ->
                Encode.string "c"
                    |> Validator.validate anyOfSchema
                    |> Expect.equal [ ( [], Validator.TooFewMatches ) ]
        ]


allOfSpec : Test
allOfSpec =
    let
        allOfSchema =
            allOf
                [ title "allOf schema title"
                , description "allOf schema description"
                ]
                [ string [ pattern "a" ], string [ pattern "b" ] ]
    in
    describe "allOf schema"
        [ test "validate valid" <|
            \() ->
                Encode.string "ab"
                    |> Validator.validate allOfSchema
                    |> Expect.equal []
        , test "validate invalid 1" <|
            \() ->
                Encode.string "a"
                    |> Validator.validate allOfSchema
                    |> Expect.equal [ ( [], Validator.TooFewMatches ) ]
        , test "validate invalid 2" <|
            \() ->
                Encode.string "b"
                    |> Validator.validate allOfSchema
                    |> Expect.equal [ ( [], Validator.TooFewMatches ) ]
        , test "validate invalid 3" <|
            \() ->
                Encode.string "c"
                    |> Validator.validate allOfSchema
                    |> Expect.equal [ ( [], Validator.TooFewMatches ) ]
        ]


recursiveSchemaSpec : Test
recursiveSchemaSpec =
    let
        recursiveSchema : Schema
        recursiveSchema =
            recurse "recursive"
                (\ref ->
                    oneOf []
                        [ array
                            [ items ref
                            ]
                        , integer [ maximum 2 ]
                        ]
                )
    in
    describe "lazy schema"
        [ test "validate valid 1" <|
            \() ->
                Encode.list [ Encode.list [] ]
                    |> Validator.validate recursiveSchema
                    |> Expect.equal []
        , test "validate valid 2" <|
            \() ->
                Encode.list [ Encode.list [ Encode.int 1 ] ]
                    |> Validator.validate recursiveSchema
                    |> Expect.equal []
        , test "validate invalid" <|
            \() ->
                Encode.list [ Encode.list [ Encode.int 3 ] ]
                    |> Validator.validate recursiveSchema
                    |> Expect.equal [ ( [], Validator.TooFewMatches ) ]
        ]
