module SchemaFuzzSpec exposing (spec)

import Expect exposing (pass)
import Json.Decode as Decode
import JsonSchema exposing (..)
import Model exposing (Schema)
import SchemaFuzz exposing (schemaString, schemaValue)
import Test exposing (..)


spec : Test
spec =
    describe "SchemaFuzz"
        [ simpleNumberFuzzSpec
        , minimumNumberFuzzSpec
        , maximumNumberFuzzSpec
        , minimumAndMaximumNumberFuzzSpec
        , simpleIntegerFuzzSpec
        , minimumIntegerFuzzSpec
        , maximumIntegerFuzzSpec
        , minimumAndMaximumIntegerFuzzSpec
        ]


simpleNumberFuzzSpec : Test
simpleNumberFuzzSpec =
    let
        simpleNumberSchema : Schema
        simpleNumberSchema =
            number []
    in
        fuzz (schemaValue simpleNumberSchema) "it generates numbers" <|
            \value ->
                case Decode.decodeValue Decode.float value of
                    Ok _ ->
                        Expect.pass

                    Err _ ->
                        Expect.fail <| "Expected a number but got: " ++ (toString value)


minimumNumberFuzzSpec : Test
minimumNumberFuzzSpec =
    let
        minimumFloat : Float
        minimumFloat =
            3.14

        minimumNumberSchema : Schema
        minimumNumberSchema =
            number
                [ minimum minimumFloat ]
    in
        fuzz (schemaValue minimumNumberSchema) "it generates numbers larger or equal than a certain minimum" <|
            \value ->
                case Decode.decodeValue Decode.float value of
                    Ok result ->
                        result
                            |> Expect.atLeast minimumFloat

                    Err _ ->
                        Expect.fail <| "Expected a number but got: " ++ (toString value)


maximumNumberFuzzSpec : Test
maximumNumberFuzzSpec =
    let
        maximumFloat : Float
        maximumFloat =
            3.14

        maximumNumberSchema : Schema
        maximumNumberSchema =
            number
                [ maximum maximumFloat ]
    in
        fuzz (schemaValue maximumNumberSchema) "it generates numbers larger or equal than a certain maximum" <|
            \value ->
                case Decode.decodeValue Decode.float value of
                    Ok result ->
                        result
                            |> Expect.atMost maximumFloat

                    Err _ ->
                        Expect.fail <| "Expected a number but got: " ++ (toString value)


minimumAndMaximumNumberFuzzSpec : Test
minimumAndMaximumNumberFuzzSpec =
    let
        minimumFloat : Float
        minimumFloat =
            -3.14

        maximumFloat : Float
        maximumFloat =
            3.14

        minimumAndMaximumNumberSchema : Schema
        minimumAndMaximumNumberSchema =
            number
                [ minimum minimumFloat
                , maximum maximumFloat
                ]
    in
        fuzz (schemaValue minimumAndMaximumNumberSchema) "it generates numbers between a minimum and a maximum" <|
            \value ->
                case Decode.decodeValue Decode.float value of
                    Ok result ->
                        Expect.all
                            [ Expect.atMost maximumFloat
                            , Expect.atLeast minimumFloat
                            ]
                            result

                    Err _ ->
                        Expect.fail <| "Expected a number but got: " ++ (toString value)


simpleIntegerFuzzSpec : Test
simpleIntegerFuzzSpec =
    let
        simpleIntegerSchema : Schema
        simpleIntegerSchema =
            integer []
    in
        fuzz (schemaValue simpleIntegerSchema) "it generates integers" <|
            \value ->
                case Decode.decodeValue Decode.int value of
                    Ok _ ->
                        Expect.pass

                    Err _ ->
                        Expect.fail <| "Expected a integer but got: " ++ (toString value)


minimumIntegerFuzzSpec : Test
minimumIntegerFuzzSpec =
    let
        minimumInteger : Int
        minimumInteger =
            3

        minimumIntegerSchema : Schema
        minimumIntegerSchema =
            integer
                [ minimum minimumInteger ]
    in
        fuzz (schemaValue minimumIntegerSchema) "it generates integers larger or equal than a certain minimum" <|
            \value ->
                case Decode.decodeValue Decode.int value of
                    Ok result ->
                        result
                            |> Expect.atLeast minimumInteger

                    Err _ ->
                        Expect.fail <| "Expected a integer but got: " ++ (toString value)


maximumIntegerFuzzSpec : Test
maximumIntegerFuzzSpec =
    let
        maximumInteger : Int
        maximumInteger =
            3

        maximumIntegerSchema : Schema
        maximumIntegerSchema =
            integer
                [ maximum maximumInteger ]
    in
        fuzz (schemaValue maximumIntegerSchema) "it generates integers larger or equal than a certain maximum" <|
            \value ->
                case Decode.decodeValue Decode.int value of
                    Ok result ->
                        result
                            |> Expect.atMost maximumInteger

                    Err _ ->
                        Expect.fail <| "Expected a integer but got: " ++ (toString value)


minimumAndMaximumIntegerFuzzSpec : Test
minimumAndMaximumIntegerFuzzSpec =
    let
        minimumInteger : Int
        minimumInteger =
            -3

        maximumInteger : Int
        maximumInteger =
            3

        minimumAndMaximumIntegerSchema : Schema
        minimumAndMaximumIntegerSchema =
            integer
                [ minimum minimumInteger
                , maximum maximumInteger
                ]
    in
        fuzz (schemaValue minimumAndMaximumIntegerSchema) "it generates integers between a minimum and a maximum" <|
            \value ->
                case Decode.decodeValue Decode.int value of
                    Ok result ->
                        Expect.all
                            [ Expect.atMost maximumInteger
                            , Expect.atLeast minimumInteger
                            ]
                            result

                    Err _ ->
                        Expect.fail <| "Expected a integer but got: " ++ (toString value)
