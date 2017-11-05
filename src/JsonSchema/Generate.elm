module JsonSchema.Generate exposing (ElmDecoder(..), elmDecoderToString, generate, toElmDecoder)

import Dict exposing (Dict)
import JsonSchema.Model exposing (..)
import Result.Extra
import Set exposing (Set)


generate : Schema -> Result String String
generate schema =
    let
        wrapInModule : String -> String
        wrapInModule generatedDecoder =
            [ "module Decoder exposing (decoder)"
            , "\n"
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


type Type
    = RecordType (Dict String Type)
    | UnionType (Dict String Type)
    | StringType
    | IntType
    | FloatType
    | MaybeType Type
    | ListType Type
    | BoolType
    | UnitType
    | JsonValueType


toElmDecoder : Schema -> Result String ElmDecoder
toElmDecoder schema =
    case toSubSchema schema of
        ( _, subSchema ) ->
            subSchemaToElmDecoder subSchema


subSchemaToElmDecoder : SubSchema -> Result String ElmDecoder
subSchemaToElmDecoder schema =
    case schema of
        Object { properties } ->
            let
                toFieldDecoder : ObjectProperty NoDefinitions -> Result String ( String, Bool, ElmDecoder )
                toFieldDecoder property =
                    case property of
                        Required fieldName propertySchema ->
                            subSchemaToElmDecoder propertySchema
                                |> Result.map ((,,) fieldName True)

                        Optional fieldName propertySchema ->
                            subSchemaToElmDecoder propertySchema
                                |> Result.map ((,,) fieldName False)
            in
            properties
                |> List.map toFieldDecoder
                |> Result.Extra.combine
                |> Result.map ObjectDecoder

        Array { items } ->
            -- TODO incorporate the extra info in the argument
            items
                |> Maybe.map subSchemaToElmDecoder
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
                    , String.join " " fieldNames
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


toType : SubSchema -> Type
toType preSchema =
    case preSchema of
        Object { properties } ->
            let
                typeOfProperty : ObjectProperty NoDefinitions -> ( String, Type )
                typeOfProperty property =
                    case property of
                        Required fieldName fieldSchema ->
                            ( fieldName, toType fieldSchema )

                        Optional fieldName fieldSchema ->
                            ( fieldName, MaybeType (toType fieldSchema) )
            in
            List.map typeOfProperty properties
                |> Dict.fromList
                |> RecordType

        Array { items } ->
            items
                |> Maybe.map toType
                |> Maybe.withDefault JsonValueType
                |> ListType

        String preStringSchema ->
            StringType

        Integer preIntegerSchema ->
            IntType

        Number preNumberSchema ->
            FloatType

        Boolean preBooleanSchema ->
            BoolType

        Null preBaseSchema ->
            UnitType

        Ref preRefSchema ->
            Debug.crash "TODO"

        OneOf preBaseCombinatorSchema ->
            Debug.crash "TODO"

        AnyOf preBaseCombinatorSchema ->
            Debug.crash "TODO"

        AllOf preBaseCombinatorSchema ->
            Debug.crash "TODO"

        Fallback value ->
            Debug.crash "TODO"