#!/usr/bin/env node
var LineByLine = require('n-readlines')
var elmSchemaFile = process.argv[2]

// We're assuming all exports from the file passed in are schema's.
var elmExports = readExports(elmSchemaFile)
console.log(elmExports)

function readExports (filePath) {
  var liner = new LineByLine(filePath)
  var EXPORTS_REGEX = /module (?:\w+) exposing \(([ .,\w]+)\)/
  var elmExports, line
  do {
    line = liner.next()
    var moduleMatch = line && line.toString().match(EXPORTS_REGEX)
    elmExports = moduleMatch && moduleMatch[1]
  } while (line && !elmExports)
  if (!elmExports) {
    throw new Error('No module declaration found in elm file: ' + filePath)
  }

  if (!elmExports.trim() === '..') {
    return elmExports.split(',')
  }

  var DEFINITION_REGEX = /^(\w+) :/
  var definitions = []
  do {
    line = liner.next()
    var definitionMatch = line && line.toString().match(DEFINITION_REGEX)
    var singleDefinition = definitionMatch && definitionMatch[1]
    if (singleDefinition) definitions.push(singleDefinition)
  } while (line)

  return definitions
}
