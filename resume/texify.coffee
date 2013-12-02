#!/usr/bin/env coffee
process.stdin.resume()
process.stdin.setEncoding 'utf8'

# Slurp whole text into memory.  Kinda gross.
allText = ""

process.stdin.on 'data', (chunk) ->
  allText += chunk
  
process.stdin.on 'end', () ->
  allText = allText.replace /\~([0123456789\-]+)\~~(.*)\n/g, "\\years\{$1\} \{\\large $2\}\n"
  allText = allText.replace /\~([0123456789\-]+)\~/g, "\\years\{$1\}"
  allText = allText.replace /\*\*\*/g, ""
  process.stdout.write allText