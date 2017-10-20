module Test.Generate exposing (..)

import Expect
import Fixtures
import JsonSchema exposing (Schema, array)
import JsonSchema.Decoder exposing (PreSchema(..))
import JsonSchema.Generate as Generate exposing (ElmDecoder(..), elmDecoderToString, toElmDecoder)
import Test exposing (..)


stringPreSchema =
    String
        { title = Just "string schema title"
        , description = Just "string schema description"
        , minLength = Just 2
        , maxLength = Just 8
        , format = Nothing
        , enum = Nothing
        , examples = []
        , pattern = Just "^foo$"
        }


testToElmDecoder =
    describe "toElmDecoder"
        [ test "string" <|
            \() ->
                stringPreSchema
                    |> toElmDecoder
                    |> Expect.equal (Ok StringDecoder)
        , test "int" <|
            \() ->
                { title = Just "integer schema title"
                , description = Just "integer schema description"
                , minimum = Just 2
                , maximum = Just 8
                , enum = Nothing
                , examples = []
                }
                    |> Integer
                    |> toElmDecoder
                    |> Expect.equal (Ok IntDecoder)
        , test "number" <|
            \() ->
                { title = Just "number schema title"
                , description = Just "number schema description"
                , minimum = Just 2.5
                , maximum = Just 8.3
                , enum = Nothing
                , examples = []
                }
                    |> Number
                    |> toElmDecoder
                    |> Expect.equal (Ok FloatDecoder)
        , test "array of strings" <|
            \() ->
                { title = Just "array schema title"
                , description = Just "array schema description"
                , items = Just stringPreSchema
                , minItems = Just 3
                , maxItems = Just 6
                , examples = []
                }
                    |> Array
                    |> toElmDecoder
                    |> Expect.equal (Ok (ArrayDecoder StringDecoder))
        , test "array of json values" <|
            \() ->
                { title = Just "array schema title"
                , description = Just "array schema description"
                , items = Nothing
                , minItems = Just 3
                , maxItems = Just 6
                , examples = []
                }
                    |> Array
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
