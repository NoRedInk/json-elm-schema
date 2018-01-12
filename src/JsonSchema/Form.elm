module JsonSchema.Form exposing (..)

import Html
import Html.Attributes
import Json.Decode exposing (Value)
import Json.Pointer
import JsonSchema
import JsonSchema.Model
import JsonSchema.Validator


type alias Model =
    { schema : JsonSchema.Schema
    , json : Json
    }


type Json
    = Valid Value
    | Error


init : JsonSchema.Schema -> String -> Model
init schema maybeJson =
    let
        json =
            Json.Decode.decodeString Json.Decode.value maybeJson
                |> (\result ->
                        case result of
                            Ok thing ->
                                Valid thing

                            Err s ->
                                Error
                   )
    in
    Model schema json


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []


rootPointer : Json.Pointer.Pointer
rootPointer =
    []


view : Model -> Html.Html msg
view model =
    Html.div []
        [ Html.text <| toString model
        , Html.div []
            [ case model.json of
                Valid value ->
                    schemaView rootPointer model.schema value

                Error ->
                    Html.text "It's an error!"
            ]
        ]


schemaView : Json.Pointer.Pointer -> JsonSchema.Schema -> Value -> Html.Html msg
schemaView pointer schema value =
    case ( JsonSchema.Validator.getValidatedValue schema value, schema ) of
        ( JsonSchema.Validator.Invalid errorList, _ ) ->
            Html.text "notImplemented"

        ( JsonSchema.Validator.StringValue string, JsonSchema.Model.String stringSchema ) ->
            stringView stringSchema string pointer

        ( JsonSchema.Validator.StringValue string, _ ) ->
            Html.text "WTF?"

        ( JsonSchema.Validator.ArrayValue arrayOfValues, _ ) ->
            Html.text "notImplemented"

        ( JsonSchema.Validator.ObjectValue objectOfValues, _ ) ->
            Html.text "notImplemented"

        ( JsonSchema.Validator.FloatValue float, _ ) ->
            Html.text "notImplemented"

        ( JsonSchema.Validator.IntegerValue int, _ ) ->
            Html.text "notImplemented"

        ( JsonSchema.Validator.LazyValue lazy, _ ) ->
            Html.text "notImplemented"

        ( JsonSchema.Validator.Valid, _ ) ->
            Html.text "notImplemented"


stringView : JsonSchema.Model.StringSchema -> String -> Json.Pointer.Pointer -> Html.Html msg
stringView stringSchema string pointer =
    Html.div []
        [ viewJust label stringSchema.title
        , Html.input [ Html.Attributes.value string ] []
        ]


label : String -> Html.Html msg
label string =
    Html.label [] [ Html.text string ]


viewJust fn a =
    case a of
        Just a_ ->
            fn a_

        Nothing ->
            Html.span [] []
