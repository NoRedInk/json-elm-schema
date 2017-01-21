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
            ]
        ]
```

## Plans
- [ ] Add a cli that takes a schema written in Elm and outputs that schema in JSON.
- [ ] Increase coverage of json schema spec.
- [ ] Create an elm-test Schema Fuzzer that generates random json adhering to a schema.
  This could be used for testing decoders.
