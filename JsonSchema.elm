module JsonSchema exposing (..)

import Json.Encode as Encode
import Maybe.Extra


type Schema
    = Object (List ObjectSchema)
    | Array (List ArraySchema)
    | String (List StringSchema)
    | Number (List NumberSchema)
    | Integer (List IntegerSchema)
    | Null


type ObjectSchema
    = ObjectProperties (List Property)
    | ObjectTitle String
    | ObjectDescription String


type Property
    = Required String Schema
    | NotRequired String Schema


type ArraySchema
    = ArrayTitle String
    | ArrayDescription String


type StringSchema
    = StringTitle String
    | StringDescription String


type NumberSchema
    = NumberTitle String
    | NumberDescription String


type IntegerSchema
    = IntegerTitle String
    | IntegerDescription String
    | IntegerMaximum Int
    | IntegerMinimum Int


encoder : Schema -> String
encoder schema =
    schema
        |> convert
        |> Encode.encode 4


convert : Schema -> Encode.Value
convert schema =
    case schema of
        Object objectSchema ->
            (( "type", Encode.string "object" )
                :: List.concatMap objectSchemaDecoder objectSchema
            )
                |> Encode.object

        Array arraySchema ->
            (( "type", Encode.string "array" )
                :: List.concatMap arraySchemaDecoder arraySchema
            )
                |> Encode.object

        String stringSchema ->
            (( "type", Encode.string "string" )
                :: List.concatMap stringSchemaDecoder stringSchema
            )
                |> Encode.object

        Number numberSchema ->
            (( "type", Encode.string "number" )
                :: List.concatMap numberSchemaDecoder numberSchema
            )
                |> Encode.object

        Integer integerSchema ->
            (( "type", Encode.string "integer" )
                :: List.concatMap integerSchemaDecoder integerSchema
            )
                |> Encode.object

        Null ->
            Encode.null


objectSchemaDecoder : ObjectSchema -> List ( String, Encode.Value )
objectSchemaDecoder value =
    case value of
        ObjectTitle title ->
            [ ( "title", Encode.string title ) ]

        ObjectDescription description ->
            [ ( "description", Encode.string description ) ]

        ObjectProperties properties ->
            [ ( "properties", convertProperty properties )
            , ( "required", findRequiredFields properties )
            ]


arraySchemaDecoder : ArraySchema -> List ( String, Encode.Value )
arraySchemaDecoder value =
    case value of
        ArrayTitle title ->
            [ ( "title", Encode.string title ) ]

        ArrayDescription description ->
            [ ( "description", Encode.string description ) ]


stringSchemaDecoder : StringSchema -> List ( String, Encode.Value )
stringSchemaDecoder value =
    case value of
        StringTitle title ->
            [ ( "title", Encode.string title ) ]

        StringDescription description ->
            [ ( "description", Encode.string description ) ]


numberSchemaDecoder : NumberSchema -> List ( String, Encode.Value )
numberSchemaDecoder value =
    case value of
        NumberTitle title ->
            [ ( "title", Encode.string title ) ]

        NumberDescription description ->
            [ ( "description", Encode.string description ) ]


integerSchemaDecoder : IntegerSchema -> List ( String, Encode.Value )
integerSchemaDecoder value =
    case value of
        IntegerTitle title ->
            [ ( "title", Encode.string title ) ]

        IntegerDescription description ->
            [ ( "description", Encode.string description ) ]

        IntegerMaximum value ->
            [ ( "maximum", Encode.int value ) ]

        IntegerMinimum value ->
            [ ( "minimum", Encode.int value ) ]


convertProperty : List Property -> Encode.Value
convertProperty properties =
    properties
        |> List.map
            (\property ->
                case property of
                    Required name schema ->
                        ( name, convert schema )

                    NotRequired name schema ->
                        ( name, convert schema )
            )
        |> Encode.object


findRequiredFields : List Property -> Encode.Value
findRequiredFields properties =
    properties
        |> List.map
            (\property ->
                case property of
                    Required name _ ->
                        Just name

                    NotRequired _ _ ->
                        Nothing
            )
        |> Maybe.Extra.values
        |> List.map Encode.string
        |> Encode.list



-- object [ ("name", object [("something", null)])
--      , ("age", int 42)
--      ]
