module JsonSchema.Util exposing (..)

import JsonSchema.Model exposing (Schema)
import Murmur3


hash : Schema -> String
hash schema =
    toString schema
        |> Murmur3.hashString 1234
        |> toString
