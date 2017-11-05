module Spec.JsonSchema.Generate exposing (..)

import Expect
import Fixtures
import JsonSchema exposing (Schema, array)
import JsonSchema.Generate as Generate exposing (ElmDecoder(..), elmDecoderToString, toElmDecoder)
import Test exposing (..)


testToElmDecoder =
    describe "toElmDecoder"
        [ test "string" <|
            \() ->
                Fixtures.stringSchema
                    |> toElmDecoder
                    |> Expect.equal (Ok StringDecoder)
        , test "int" <|
            \() ->
                Fixtures.integerSchema
                    |> toElmDecoder
                    |> Expect.equal (Ok IntDecoder)
        , test "number" <|
            \() ->
                Fixtures.numberSchema
                    |> toElmDecoder
                    |> Expect.equal (Ok FloatDecoder)
        , test "array of strings" <|
            \() ->
                Fixtures.arraySchema
                    |> toElmDecoder
                    |> Expect.equal (Ok (ArrayDecoder StringDecoder))
        , test "array of json values" <|
            \() ->
                array []
                    |> toElmDecoder
                    |> Expect.equal (Ok (ArrayDecoder JsonDecoder))
        ]


testElmDecoderToString =
    describe "elmDecoderToString"
        [ test "String" <|
            \() ->
                StringDecoder
                    |> elmDecoderToString
                    |> Expect.equal "Json.Decode.string"
        , test "IntDecoder" <|
            \() ->
                IntDecoder
                    |> elmDecoderToString
                    |> Expect.equal "Json.Decode.int"
        , test "FloatDecoder" <|
            \() ->
                FloatDecoder
                    |> elmDecoderToString
                    |> Expect.equal "Json.Decode.float"
        , describe "ArrayDecoder"
            [ test "with raw json inside" <|
                \() ->
                    ArrayDecoder JsonDecoder
                        |> elmDecoderToString
                        |> Expect.equal "(Json.Decode.list Json.Decode.value)"
            , test "with strings inside" <|
                \() ->
                    ArrayDecoder StringDecoder
                        |> elmDecoderToString
                        |> Expect.equal "(Json.Decode.list Json.Decode.string)"
            ]
        ]
