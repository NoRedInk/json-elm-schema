#!/usr/bin/env node
var LineByLine = require('n-readlines')
var elmSchemaFile = process.argv[2]

// Get the name and exports of the file containing the Elm JSON schemas.
// We're going to assume each export is a Schema type.
var elmModule = readModule(elmSchemaFile)

// Generate a Main module that compiles the schemas and sends them over a port.
var portModule = generatePortModule(elmModule.name, elmModule.exports)

// TODO: Write the port module to a file in a tmp directory.
// TODO: Generate a elm-package.json in the tmp directory referencing the project containing the schema files.
// TODO: compile and run the port module in the tmp directory and send the output to stdout.

// DEBUG: testing the script so far.
console.log(portModule)

function readModule (filePath) {
  var liner = new LineByLine(filePath)
  var MODULE_REGEX = /module (\w+) exposing \(([ .,\w]+)\)/
  var moduleName, exports, line
  do {
    line = liner.next()
    var match = line && line.toString().match(MODULE_REGEX)
    moduleName = match && match[1]
    exports = match && match[1]
  } while (line && !moduleName)
  if (!moduleName) {
    throw new Error('No module declaration found in elm file: ' + filePath)
  }

  var exportsAll = exports.trim() === '..'
  return {
    name: moduleName,
    exports: exportsAll ? exports.split(',') : readDefinitions(liner)
  }
}

function readDefinitions (liner) {
  var DEFINITION_REGEX = /^(\w+) :/
  var definitions = []
  do {
    var line = liner.next()
    var definitionMatch = line && line.toString().match(DEFINITION_REGEX)
    var singleDefinition = definitionMatch && definitionMatch[1]
    if (singleDefinition) definitions.push(singleDefinition)
  } while (line)

  return definitions
}

function generatePortModule (moduleName, exports) {
  return [
    'port module Main exposing(..)',
    'import Encoder',
    'import ' + moduleName,
    'port files : List String -> Cmd msg'
  ].join('\n')
}
