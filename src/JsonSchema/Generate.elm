module JsonSchema.Generate exposing (ElmDecoder(..), elmDecoderToString, generate, toElmDecoder)

import JsonSchema.Model exposing (Schema(..))


generate : Schema -> Result String String
generate schema =
    toElmDecoder schema
        |> Result.map elmDecoderToString


type ElmDecoder
    = StringDecoder
    | IntDecoder
    | FloatDecoder
    | BoolDecoder
    | JsonDecoder
    | NullDecoder
    | ArrayDecoder ElmDecoder


toElmDecoder : Schema -> Result String ElmDecoder
toElmDecoder schema =
    case schema of
        Object objectSchema ->
            Debug.crash "TODO"

        Array arraySchema ->
            -- TODO incorporate the extra info in the argument
            Ok (ArrayDecoder JsonDecoder)

        String stringSchema ->
            -- TODO incorporate the extra info in the argument
            Ok StringDecoder

        Integer integerSchema ->
            -- TODO incorporate the extra info in the argument
            Ok IntDecoder

        Number numberSchema ->
            -- TODO incorporate the extra info in the argument
            Ok FloatDecoder

        Boolean booleanSchema ->
            -- TODO incorporate the extra info in the argument
            Ok BoolDecoder

        Null nullSchema ->
            -- TODO incorporate the extra info in the argument
            Ok NullDecoder

        Ref refSchema ->
            Debug.crash "TODO"

        OneOf baseCombinatorSchema ->
            Debug.crash "TODO"

        AnyOf baseCombinatorSchema ->
            Debug.crash "TODO"

        AllOf baseCombinatorSchema ->
            Debug.crash "TODO"

        Lazy function ->
            Debug.crash "TODO"

        Fallback valueDecodeJson ->
            Debug.crash "TODO"


elmDecoderToString : ElmDecoder -> String
elmDecoderToString decoder =
    case decoder of
        StringDecoder ->
            "Json.Decode.string"

        IntDecoder ->
            "Json.Decode.int"

        FloatDecoder ->
            "Json.Decode.float"

        BoolDecoder ->
            "Json.Decode.bool"

        JsonDecoder ->
            "Json.Decode.value"

        NullDecoder ->
            "(Json.Decode.null ())"

        ArrayDecoder decoder ->
            "(Json.Decode.list " ++ elmDecoderToString decoder ++ ")"
