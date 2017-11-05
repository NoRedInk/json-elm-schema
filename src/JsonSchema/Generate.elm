module JsonSchema.Generate exposing (ElmDecoder(..), elmDecoderToString, generate, toElmDecoder)

import Char
import Dict exposing (Dict)
import JsonSchema.Model exposing (..)
import Regex exposing (Regex)
import Result.Extra


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
    | OtherDecoder String


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
        ( definitions, subSchema ) ->
            subSchemaToElmDecoder definitions subSchema


subSchemaToElmDecoder : Definitions -> SubSchema -> Result String ElmDecoder
subSchemaToElmDecoder definitions schema =
    case schema of
        Object { properties } ->
            let
                toFieldDecoder : ObjectProperty NoDefinitions -> Result String ( String, Bool, ElmDecoder )
                toFieldDecoder property =
                    case property of
                        Required fieldName propertySchema ->
                            subSchemaToElmDecoder definitions propertySchema
                                |> Result.map ((,,) fieldName True)

                        Optional fieldName propertySchema ->
                            subSchemaToElmDecoder definitions propertySchema
                                |> Result.map ((,,) fieldName False)
            in
            properties
                |> List.map toFieldDecoder
                |> Result.Extra.combine
                |> Result.map ObjectDecoder

        Array { items } ->
            -- TODO incorporate the extra info in the argument
            items
                |> Maybe.map (subSchemaToElmDecoder definitions)
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
            Ok (OtherDecoder <| asDecoderName refSchema.ref)

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

        OtherDecoder name ->
            name


toType : Definitions -> SubSchema -> Type
toType definitions preSchema =
    case preSchema of
        Object { properties } ->
            let
                typeOfProperty : ObjectProperty NoDefinitions -> ( String, Type )
                typeOfProperty property =
                    case property of
                        Required fieldName fieldSchema ->
                            ( fieldName, toType definitions fieldSchema )

                        Optional fieldName fieldSchema ->
                            ( fieldName, MaybeType (toType definitions fieldSchema) )
            in
            List.map typeOfProperty properties
                |> Dict.fromList
                |> RecordType

        Array { items } ->
            items
                |> Maybe.map (toType definitions)
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


asDecoderName : String -> String
asDecoderName str =
    asVarName str ++ "Decoder"


separator : Regex
separator =
    Regex.regex "[^\\w]+(\\w)"


asVarName : String -> String
asVarName str =
    let
        replacer : Regex.Match -> String
        replacer { submatches } =
            List.head submatches
                |> Maybe.andThen identity
                |> Maybe.map String.toUpper
                |> Maybe.withDefault ""
    in
    Regex.replace Regex.All separator replacer str
        -- First character needs to be small
        |> String.uncons
        |> Maybe.map (Tuple.mapFirst Char.toLower >> uncurry String.cons)
        |> Maybe.withDefault ""
