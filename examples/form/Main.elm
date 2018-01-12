module Main exposing (..)

import Html
import JsonSchema
import JsonSchema.Form


main : Program Never Model Msg
main =
    Html.program
        { init = ( init, Cmd.none )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { form : JsonSchema.Form.Model
    }


type Msg
    = SchemaFormMsg JsonSchema.Form.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SchemaFormMsg schemaMsg ->
            let
                ( newForm, cmd ) =
                    JsonSchema.Form.update schemaMsg model.form
            in
            { model | form = newForm } ! [ Cmd.map SchemaFormMsg cmd ]


view : Model -> Html.Html Msg
view model =
    JsonSchema.Form.view model.form


init : Model
init =
    { form = JsonSchema.Form.init exampleSchema exampleJson }


exampleSchema : JsonSchema.Schema
exampleSchema =
    JsonSchema.string [ JsonSchema.title "sup" ]


exampleJson : String
exampleJson =
    """
     "an example string"
     """
