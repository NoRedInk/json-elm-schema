module Test.Generate exposing (..)

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
        , test "object" <|
            \() ->
                Fixtures.objectSchema
                    |> toElmDecoder
                    |> Expect.equal
                        (Ok
                            (ObjectDecoder
                                [ ( "firstName", False, StringDecoder )
                                , ( "lastName", True, StringDecoder )
                                ]
                            )
                        )
        ]


testElmDecoderToString : Test
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
        , test "ObjectDecoder" <|
            \() ->
                (ObjectDecoder
                    [ ( "firstName", False, StringDecoder )
                    , ( "lastName", True, StringDecoder )
                    ]
                )
                    |> elmDecoderToString
                    |> Expect.equal "(Decode.Pipeline.decode (\\firstName lastName -> { firstName = firstName, lastName = lastName }) |> optional firstName (Json.Decode.map Just Json.Decode.string) Nothing |> required lastName Json.Decode.string)"
        ]



--
-- generateStringSchema : Fuzzer (List ( String -> Schema, ElmDecoder -> Expectation ))
-- generateStringSchema =
--     Fuzz.constant
--         [ --(title "string schema title", Expect.equal )
--           --, (description "string schema description", Expect.equal 5)
--           ( minLength 2, Expect.atLeast 2 )
--         , ( maxLength 8, Expect.atMost 8 )
--         , ( pattern "^foo$", Expect.equal 888 {- TODO -} )
--
--         -- should not be able to generate format since we already used pattern
--         , ( format dateTime, Expect.equal 5 {- TODO -} )
--         ]
--
