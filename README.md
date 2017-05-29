# Elm JSON Schema
It's very easy to write a [JSON Schema](http://json-schema.org/) that is valid JSON but not a valid JSON Schema.
Such faulty schema's still work, they'll just be less strict then you think they are.
Usually you'll learn about this a bit later than you'd prefer.

Elm-json-schema allows you to write your JSON schema's in Elm, ensuring a valid result.

## Example
```elm
personSchema : Schema
personSchema =
    object
        [ title "person"
        , properties
            [ required "firstName" <| string []
            , required "lastName" <| string []
            , optional "age" <|
                integer
                    [ description "Age in years"
                    , minimum 0
                    ]
            , optional "height" <|
                number
                    [ description "Height in meters"
                    , minimum 0
                    ]
            , required "tags" <|
                array 
                    [ items <| string []
                    , minItems 1
                    ]
            ]
        ]
```

## Links
- [A fully working example](https://github.com/NoRedInk/json-elm-schema/tree/master/example)
- [A CLI tool for compiling your elm json schemas to regular json schemas](https://github.com/NoRedInk/json-elm-schema/tree/master/cli)
