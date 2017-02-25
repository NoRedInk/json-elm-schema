module Model exposing (..)


type Schema
    = Object ObjectSchema
    | Array ArraySchema
    | String StringSchema
    | Integer IntegerSchema
    | Number NumberSchema
    | Boolean (BaseSchema {})
    | Null (BaseSchema {})
    | OneOf BaseCombinatorSchema
    | AnyOf BaseCombinatorSchema
    | AllOf BaseCombinatorSchema


type alias BaseSchema extras =
    { extras
        | title : Maybe String
        , description : Maybe String
    }


type alias ObjectSchema =
    BaseSchema
        { properties : List ObjectProperty
        }


type alias ArraySchema =
    BaseSchema
        { items : Maybe Schema
        }


type alias BaseNumberSchema num =
    BaseSchema
        { minimum : Maybe num
        , maximum : Maybe num
        }


type alias IntegerSchema =
    BaseNumberSchema Int


type alias NumberSchema =
    BaseNumberSchema Float


type ObjectProperty
    = Required String Schema
    | Optional String Schema


type alias StringSchema =
    BaseSchema
        { minLength : Maybe Int
        , maxLength : Maybe Int
        , pattern : Maybe String
        , format : Maybe StringFormat
        }


type alias BaseCombinatorSchema =
    BaseSchema
        { subSchemas : List Schema
        }


{-| One of the built-in string formats defined by the json schema specification,
    or a custom format your schema validator understands.
-}
type StringFormat
    = DateTime
    | Email
    | Hostname
    | Ipv4
    | Ipv6
    | Uri
    | Custom String
