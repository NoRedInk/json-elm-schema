module Fuzz.Extra exposing (..)

import Fuzz exposing (..)
import Random.Pcg as Random
import Shrink exposing (Shrinker)


{-| A fuzzer for float values with a given minimum, inclusive.
Shrunken values will also be within the range.
-}
floatMinimum : Float -> Fuzzer Float
floatMinimum lo =
    custom
        (Random.frequency
            [ ( 4, Random.float lo (lo + 100) )
            , ( 1, Random.constant lo )
            , ( 8, Random.float lo (toFloat <| Random.maxInt - Random.minInt) )
            ]
        )
        (Shrink.keepIf (\i -> i >= lo) Shrink.float)


{-| A fuzzer for float values with a given maximum, inclusive.
Shrunken values will also be within the range.
-}
floatMaximum : Float -> Fuzzer Float
floatMaximum hi =
    custom
        (Random.frequency
            [ ( 4, Random.float (hi - 100) hi )
            , ( 1, Random.constant hi )
            , ( 8, Random.float (toFloat <| Random.minInt - Random.maxInt) hi )
            ]
        )
        (Shrink.keepIf (\i -> i <= hi) Shrink.float)


{-| A fuzzer that randomly selects elements in a list.
-}
oneOf : List a -> Fuzzer a
oneOf values =
    values
        |> List.map (Fuzz.constant >> (,) 1)
        |> Fuzz.frequency


{-| A fuzzer that creates lists with bounded lengths.
-}
variableList : Int -> Int -> Fuzzer a -> Fuzzer (List a)
variableList min max item =
    intRange min max
        |> Fuzz.andThen (\length -> List.repeat length item |> sequence)


{-| Sequence a list of fuzzers into a fuzzer of a list
-}
sequence : List (Fuzzer a) -> Fuzzer (List a)
sequence fuzzers =
    List.foldl
        (\fuzzer listFuzzer ->
            Fuzz.constant (::)
                |> Fuzz.andMap fuzzer
                |> Fuzz.andMap listFuzzer
        )
        (Fuzz.constant [])
        fuzzers
