#!/bin/sh
test_dir="$(pwd)/$(dirname $0)"
ajv_dir=$test_dir/../node_modules/ajv
cd $test_dir/..
ELM_JSON_SCHEMA_AJV_PATH=$ajv_dir ./node_modules/.bin/elm-test
