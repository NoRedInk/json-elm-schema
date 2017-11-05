module JsonSchema.Util exposing (..)

import JsonSchema.Model exposing (SubSchema)
import Murmur3


hash : SubSchema -> String
hash schema =
    toString schema
        |> Murmur3.hashString 1234
        |> toString
