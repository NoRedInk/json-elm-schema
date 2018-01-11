module JsonSchema.Model exposing (..)

import Json.Decode
import Json.Encode as Encode


type Schema
    = Object ObjectSchema
    | Array ArraySchema
    | Tuple TupleSchema
    | String StringSchema
    | Integer IntegerSchema
    | Number NumberSchema
    | Boolean BooleanSchema
    | Null NullSchema
    | Ref RefSchema
    | OneOf BaseCombinatorSchema
    | AnyOf BaseCombinatorSchema
    | AllOf BaseCombinatorSchema
    | Lazy (() -> Schema)
    | Fallback Json.Decode.Value


type alias BaseSchema extras =
    { extras
        | title : Maybe String
        , description : Maybe String
        , examples : List Encode.Value
    }


type alias WithEnumSchema primitive extras =
    { extras
        | enum : Maybe (List primitive)
    }


type alias ObjectSchema =
    BaseSchema
        { properties : List ObjectProperty
        , minProperties : Maybe Int
        , maxProperties : Maybe Int
        }


type alias ArraySchema =
    BaseSchema
        { items : Maybe Schema
        , minItems : Maybe Int
        , maxItems : Maybe Int
        }

type alias TupleSchema =
    BaseSchema
        { items : Maybe (List Schema)
        , minItems : Maybe Int
        , maxItems : Maybe Int
        , additionalItems: Maybe Schema
        }


type alias BaseNumberSchema num =
    WithEnumSchema num
        (BaseSchema
            { minimum : Maybe num
            , maximum : Maybe num
            }
        )


type alias IntegerSchema =
    BaseNumberSchema Int


type alias NumberSchema =
    BaseNumberSchema Float


type ObjectProperty
    = Required String Schema
    | Optional String Schema


type alias StringSchema =
    WithEnumSchema String
        (BaseSchema
            { minLength : Maybe Int
            , maxLength : Maybe Int
            , pattern : Maybe String
            , format : Maybe StringFormat
            }
        )


type alias BooleanSchema =
    WithEnumSchema Bool (BaseSchema {})


type alias NullSchema =
    BaseSchema {}


type alias RefSchema =
    BaseSchema
        { ref : String
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
