#!/usr/bin/env node

const fs = require('fs')
const path = require('path')

var listing = {}

function camelCasePath (str) {
  var parts = str.split(/[^A-Za-z0-9]/)
  if (parts.length === 1) return str
  var rest = parts.slice(1)
    .filter(function (word) {
      return !word.match(/^([a-f0-9]{20}|[a-f0-9]{32})$/) // remove webpack `[hash]`
    }).map(function (word) {
      if (word.length === 1) return word.toUpperCase()
      return word.charAt(0).toUpperCase() + word.substr(1)
    })
  return [parts[0].toLowerCase(), rest.join('')].join('')
}

function onfiles (trim, basedir, files) {
  files.forEach(function (file) {
    var fullpath = path.join(basedir, file)
    var stat = fs.statSync(fullpath)
    if (stat.isDirectory()) {
      return onfiles(trim, fullpath, fs.readdirSync(fullpath))
    }
    listing[camelCasePath(fullpath.substr(trim.length + 1))] = fs.readFileSync(fullpath, 'utf-8')
  })
}

process.argv.slice(2).forEach(function (basedir, index) {
  onfiles(basedir, basedir, fs.readdirSync(basedir))
})

console.log('module Templates exposing')
Object.keys(listing).forEach(function (key, index) {
  var prefix = '    ,'
  if (index === 0) prefix = '    ('
  console.log(prefix, key)
})
console.log('    )')
Object.keys(listing).forEach(function (key, index) {
  console.log('')
  console.log('')
  console.log(key, ': String')
  console.log(key, '=')
  console.log('    ' + JSON.stringify(listing[key]))
})
