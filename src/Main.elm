module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onInput, onClick)
import Platform exposing (Program)
import Json.Decode
import JsonSchema.Model as Schema exposing (Schema)
import JsonSchema.Decoder


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }


model : Model
model =
    { rawSchema = ""
    , schema = Err "Click generate to parse a json schema"
    }


type alias Model =
    { rawSchema : String
    , schema : Result String Schema
    }


type Msg
    = Generate
    | SetRawSchema String


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetRawSchema newRawSchema ->
            { model | rawSchema = newRawSchema }

        Generate ->
            { model
                | schema =
                    Json.Decode.decodeString JsonSchema.Decoder.decoder model.rawSchema
            }


view : Model -> Html Msg
view model =
    section []
        [ h1 [] [ text "Generate a Decoder from a JSON schema!" ]
        , textarea [ onInput SetRawSchema ] []
        , button [ onClick Generate ] [ text "Generate!" ]
        , section [] [ text (toString model.schema) ]
        ]
