port module Main exposing (..)

import Tests
import Test
import SchemaFuzzSpec
import DecoderSpec
import ValidatorSpec
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)


main : TestProgram
main =
    [ Tests.spec
    , SchemaFuzzSpec.spec
    , DecoderSpec.spec
    , ValidatorSpec.spec
    ]
        |> Test.concat
        |> run emit


port emit : ( String, Value ) -> Cmd msg
