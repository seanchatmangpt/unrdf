import { Parser, Store } from 'n3'
// import { QueryEngine } from '@comunica/query-sparql' (bypassing for demo)
import { traced, tracedSync } from '../../../telemetry/tracer.mjs'

/**
 * Parse Turtle into an N3.Store.
 * @param {string} ttl
 * @returns {Store}
 */
export const parseTurtle = tracedSync('parseTurtle', ttl => {
  const parser = new Parser()
  const quads = parser.parse(ttl)
  const store = new Store()
  store.addQuads(quads)
  return store
})

/**
 * Run a SPARQL SELECT query on the store.
 * @param {Store} store
 * @param {string} query
 * @returns {Promise<Array<Record<string, any>>>}
 */
/**
 * Naive select: return all quads in the store.
 * @param {Store} store
 * @param {string} query (ignored)
 * @returns {Promise<Array<Record<string, any>>>}
 */
export const select = traced('select', async (store, query) => {
  return store.getQuads(null, null, null, null).map(q => ({
    s: q.subject.value,
    p: q.predicate.value,
    o: q.object.value
  }))
})