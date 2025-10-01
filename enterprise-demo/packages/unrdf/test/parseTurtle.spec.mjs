import { given, when, then } from 'citty-test-utils'
import { parseTurtle } from '../src/index.mjs'

given('a Turtle string with one triple', ctx => {
  ctx.ttl = '@prefix ex: <http://example.org/> . ex:a ex:b "c" .'
})

when('we parse the Turtle', ctx => {
  ctx.store = parseTurtle(ctx.ttl)
})

then('the store has one quad', ctx => {
  const quads = ctx.store.getQuads(null, null, null, null)
  expect(quads).toHaveLength(1)
})