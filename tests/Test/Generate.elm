module Test.Generate exposing (..)

import Expect
import Fixtures
import JsonSchema exposing (Schema)
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
