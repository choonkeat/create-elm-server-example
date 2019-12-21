function camelize (str) {
  let parts = str.replace(/_/g, ' ').toLowerCase().split(/\W+/)
    .reduce(function (sum, s) {
      if (typeof sum === 'string') sum = [sum]
      if (sum[0]) return [...sum, s[0].toUpperCase() + s.slice(1)]
      return [s]
    })
  return (typeof parts === 'string' ? parts : parts.join(''))
}
console.log(`module Env exposing (..)

`)
for (k in process.env) {
  let variable = camelize(k)
  if (variable) { console.log(`{-| environment variable ${k}\n-}\n` + variable, '=\n    ', JSON.stringify(process.env[k]), '\n\n') }
}
