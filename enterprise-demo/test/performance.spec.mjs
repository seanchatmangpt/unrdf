import { test, expect } from 'vitest'
import { performance } from 'perf_hooks'
import { parseTurtle } from 'unrdf'

test('parseTurtle performance for 10000 triples', () => {
  const ttlHeader = '@prefix ex: <http://example.org/> . '
  const triple = 'ex:a ex:b ex:c . '
  const ttl = ttlHeader + triple.repeat(10000)

  const start = performance.now()
  const store = parseTurtle(ttl)
  const duration = performance.now() - start
  console.log(`parseTurtle processed 10000 triples in ${duration.toFixed(2)}ms`)

  expect(store.getQuads(null, null, null, null)).toHaveLength(10000)
  expect(duration).toBeLessThan(500)
})