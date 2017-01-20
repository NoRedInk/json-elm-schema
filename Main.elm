module Main exposing (..)


type Schema
    = Object ObjectSchema
    | Array ArraySchema
    | String StringSchema
    | Number NumberSchema
    | Integer IntegerSchema
    | Null


type alias ObjectSchema =
    { properties : List Property
    , title : String
    , description : String
    }


type Property
    = Required String Schema
    | NotRequired String Schema


type alias ArraySchema =
    { title : String
    , description : String
    }


type alias StringSchema =
    { title : String
    , description : String
    }


type alias NumberSchema =
    { title : String
    , description : String
    }


type alias IntegerSchema =
    { title : String
    , description : String
    , maximum : Maybe Int
    , minimum : Maybe Int
    }
