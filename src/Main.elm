module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onInput, onClick)
import Platform exposing (Program)
import Json.Decode
import JsonSchema.Decoder
import JsonSchema.Generate


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
    , generatedCode = Err "Click generate to parse a json schema"
    }


type alias Model =
    { rawSchema : String
    , generatedCode : Result String String
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
                | generatedCode =
                    Json.Decode.decodeString JsonSchema.Decoder.decoder model.rawSchema
                        |> Result.andThen JsonSchema.Generate.generate
            }


view : Model -> Html Msg
view model =
    section []
        [ h1 [] [ text "Generate a Decoder from a JSON schema!" ]
        , textarea [ onInput SetRawSchema ] []
        , button [ onClick Generate ] [ text "Generate!" ]
        , h2 [] [ text "Generated Decoder" ]
        , case model.generatedCode of
            Err message ->
                section [] [ text ("Error: " ++ message) ]

            Ok generatedCode ->
                code []
                    (String.split "\n" generatedCode
                        |> List.map (\line -> p [] [ text line ])
                    )
        ]
