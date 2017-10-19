module JsonSchema.Generate exposing (ElmDecoder(..), elmDecoderToString, generate, toElmDecoder)

import Result.Extra
import JsonSchema.Model exposing (ObjectProperty(..), Schema(..))


generate : Schema -> Result String String
generate schema =
    let
        wrapInModule : String -> String
        wrapInModule generatedDecoder =
            [ "module Decoder exposing (decoder)"
            , "import Json.Decode"
            , "import Decode.Pipeline"
            , "decoder = " ++ generatedDecoder
            ]
                |> String.join "\n"
    in
        toElmDecoder schema
            |> Result.map (elmDecoderToString >> wrapInModule)


type ElmDecoder
    = StringDecoder
    | IntDecoder
    | FloatDecoder
    | BoolDecoder
    | JsonDecoder
    | NullDecoder
    | ArrayDecoder ElmDecoder
    | ObjectDecoder (List ( String, Bool, ElmDecoder ))


toElmDecoder : Schema -> Result String ElmDecoder
toElmDecoder schema =
    case schema of
        Object { properties } ->
            let
                toFieldDecoder : ObjectProperty -> Result String ( String, Bool, ElmDecoder )
                toFieldDecoder property =
                    case property of
                        Required fieldName fieldSchema ->
                            toElmDecoder fieldSchema
                                |> Result.map ((,,) fieldName True)

                        Optional fieldName fieldSchema ->
                            toElmDecoder fieldSchema
                                |> Result.map ((,,) fieldName False)
            in
                List.map toFieldDecoder properties
                    |> Result.Extra.combine
                    |> Result.map ObjectDecoder

        Array { items } ->
            -- TODO incorporate the extra info in the argument
            items
                |> Maybe.map toElmDecoder
                |> Maybe.withDefault (Ok JsonDecoder)
                |> Result.map ArrayDecoder

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

        ObjectDecoder fieldDecoders ->
            let
                fieldNames : List String
                fieldNames =
                    List.map (\( fieldName, _, _ ) -> fieldName) fieldDecoders

                initFn : String
                initFn =
                    [ "(\\"
                    , (String.join " " fieldNames)
                    , " -> { "
                    , List.map fieldAssignment fieldNames |> String.join ", "
                    , " })"
                    ]
                        |> String.concat

                fieldAssignment : String -> String
                fieldAssignment fieldName =
                    fieldName ++ " = " ++ fieldName

                pipeline : String
                pipeline =
                    List.map pipelineSegment fieldDecoders
                        |> String.join " "

                pipelineSegment : ( String, Bool, ElmDecoder ) -> String
                pipelineSegment ( fieldName, required, fieldDecoder ) =
                    if required then
                        [ "|> required "
                        , fieldName
                        , " "
                        , elmDecoderToString fieldDecoder
                        ]
                            |> String.concat
                    else
                        [ "|> optional "
                        , fieldName
                        , " (Json.Decode.map Just "
                        , elmDecoderToString fieldDecoder
                        , ") Nothing"
                        ]
                            |> String.concat
            in
                "(Decode.Pipeline.decode " ++ initFn ++ " " ++ pipeline ++ ")"
