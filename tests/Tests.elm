module Tests exposing (..)

import Test exposing (..)
import Expect
import Encoder exposing (encoder)
import Json.Decode as Decode
import Fixture
import Json.Decode.Pipeline as Pipeline


all : Test
all =
    describe "JsonSchema"
        [ test "basics" <|
            \() ->
                encoder Fixture.testSchema
                    |> expectAt
                        [ "properties", "firstName", "type" ]
                        ( Decode.string, "string" )
        , test "basics" <|
            \() ->
                encoder Fixture.testSchema
                    |> expectAt
                        [ "properties", "age", "minimum" ]
                        ( Decode.int, 0 )
        , test "basics" <|
            \() ->
                encoder Fixture.testSchema
                    |> expectAt
                        [ "properties", "height", "type" ]
                        ( Decode.string, "number" )
        ]


expectAt : List String -> ( Decode.Decoder a, a ) -> String -> Expect.Expectation
expectAt path ( decoder, expected ) actual =
    let
        result =
            Decode.decodeString
                (Pipeline.decode identity
                    |> Pipeline.requiredAt path decoder
                )
                actual
    in
        case result of
            Ok decoded ->
                Expect.equal expected decoded

            Err error ->
                Expect.fail ("Couldn't decode schema: " ++ error)


testDecoder : String -> Result String String
testDecoder =
    Decode.decodeString
        (Pipeline.decode identity
            |> Pipeline.required "title" Decode.string
        )
