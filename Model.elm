module Model exposing (..)


type Schema
    = Object ObjectSchema
    | String StringSchema
    | Integer IntegerSchema
    | Null


type alias BaseSchema extras =
    { extras
        | title : Maybe String
        , description : Maybe String
    }


type alias ObjectSchema =
    BaseSchema
        { properties : List ObjectProperty
        }


type alias IntegerSchema =
    BaseSchema
        { minimum : Maybe Int
        , maximum : Maybe Int
        }


type ObjectProperty
    = Required String Schema
    | Optional String Schema


type alias StringSchema =
    BaseSchema {}
