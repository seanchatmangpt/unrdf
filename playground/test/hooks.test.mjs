import { describe, it, expect } from 'vitest'
import { defineHook, planHook, evaluateHook, registerPredicate } from '../src/hooks.mjs'
import { createTurtleStore, selectRows, termToJSON } from '../server/lib/store.mjs'

const DATA = `
@prefix ex: <http://example.org/> .
ex:s1 a ex:Service ; ex:errorRate 0.05 ; ex:latency 1500 ; ex:requests 1000 .
ex:s2 a ex:Service ; ex:errorRate 0.01 ; ex:latency 300 ; ex:requests 2000 .
`

const SELECT =
  'SELECT ?s ?errorRate ?latency WHERE { ?s <http://example.org/errorRate> ?errorRate ; <http://example.org/latency> ?latency }'

describe('store helpers', () => {
  it('loads turtle and returns rows of terms', () => {
    const store = createTurtleStore(DATA)
    const rows = selectRows(store, SELECT)
    expect(rows).toHaveLength(2)
    expect(rows.map(r => r.s.value).sort()).toEqual([
      'http://example.org/s1',
      'http://example.org/s2'
    ])
  })

  it('rejects non-SELECT queries as hook selects', () => {
    const store = createTurtleStore(DATA)
    expect(() => selectRows(store, 'ASK { ?s ?p ?o }')).toThrow(/SELECT/)
  })

  it('serialises literals with their datatype', () => {
    const store = createTurtleStore(DATA)
    const [row] = selectRows(store, SELECT).filter(r => r.s.value.endsWith('s1'))
    expect(termToJSON(row.errorRate)).toEqual({
      type: 'Literal',
      value: '0.05',
      datatype: 'http://www.w3.org/2001/XMLSchema#decimal'
    })
  })

  it('throws on invalid turtle', () => {
    expect(() => createTurtleStore('this is not turtle')).toThrow()
  })
})

describe('defineHook', () => {
  it('applies defaults', () => {
    const hook = defineHook({ id: 'ex:h', select: SELECT })
    expect(hook.predicates).toEqual([])
    expect(hook.combine).toBe('AND')
  })

  it('rejects an invalid definition', () => {
    expect(() => defineHook({ id: 'ex:h' })).toThrow(/select/)
    expect(() => defineHook({ id: 'ex:h', select: SELECT, combine: 'XOR' })).toThrow(/combine/)
  })
})

describe('planHook', () => {
  it('describes the query and predicate plan', () => {
    const hook = defineHook({
      id: 'ex:h',
      select: SELECT,
      predicates: [
        { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.02 } },
        { kind: 'NOPE', spec: {} }
      ],
      combine: 'OR'
    })
    expect(planHook(hook)).toEqual({
      queryPlan: 'SELECT',
      predicatePlan: [
        { order: 0, kind: 'THRESHOLD', registered: true },
        { order: 1, kind: 'NOPE', registered: false }
      ],
      combine: 'OR'
    })
  })
})

describe('evaluateHook', () => {
  const threshold = (variable, op, value) => ({
    kind: 'THRESHOLD',
    spec: { var: variable, op, value }
  })

  it('OR fires when any predicate is true', async () => {
    const store = createTurtleStore(DATA)
    const hook = defineHook({
      id: 'ex:or',
      select: SELECT,
      predicates: [threshold('errorRate', '>', 0.9), threshold('latency', '>', 1000)],
      combine: 'OR'
    })
    const receipt = await evaluateHook(hook, store)
    expect(receipt.fired).toBe(true)
    expect(receipt.predicates.map(p => p.ok)).toEqual([false, true])
    expect(receipt.predicates[1].meta.matched).toBe(1)
  })

  it('AND requires every predicate', async () => {
    const store = createTurtleStore(DATA)
    const hook = defineHook({
      id: 'ex:and',
      select: SELECT,
      predicates: [threshold('errorRate', '>', 0.02), threshold('latency', '>', 5000)],
      combine: 'AND'
    })
    expect((await evaluateHook(hook, store)).fired).toBe(false)
  })

  it('ASK predicate queries the whole store and refuses non-ASK queries', async () => {
    const store = createTurtleStore(DATA)
    const good = defineHook({
      id: 'ex:ask',
      select: SELECT,
      predicates: [
        { kind: 'ASK', spec: { query: 'ASK { ?s a <http://example.org/Service> }' } }
      ]
    })
    expect((await evaluateHook(good, store)).fired).toBe(true)

    const bad = defineHook({
      id: 'ex:ask-bad',
      select: SELECT,
      predicates: [{ kind: 'ASK', spec: { query: 'SELECT * { ?s ?p ?o }' } }]
    })
    await expect(evaluateHook(bad, store)).rejects.toThrow(/ASK/)
  })

  it('WINDOW aggregates over the selected rows', async () => {
    const store = createTurtleStore(DATA)
    const hook = defineHook({
      id: 'ex:win',
      select:
        'SELECT ?s ?requests WHERE { ?s <http://example.org/requests> ?requests }',
      predicates: [
        { kind: 'WINDOW', spec: { var: 'requests', op: 'sum', cmp: { op: '==', value: 3000 } } }
      ]
    })
    const receipt = await evaluateHook(hook, store)
    expect(receipt.fired).toBe(true)
    expect(receipt.predicates[0].meta.aggregate).toBe(3000)
  })

  it('supports custom predicates via registerPredicate', async () => {
    registerPredicate('ROW_COUNT_IS_TWO', (spec, { rows }) => ({
      ok: rows.length === 2,
      meta: { rows: rows.length }
    }))
    const store = createTurtleStore(DATA)
    const hook = defineHook({
      id: 'ex:custom',
      select: SELECT,
      predicates: [{ kind: 'ROW_COUNT_IS_TWO', spec: {} }]
    })
    const receipt = await evaluateHook(hook, store)
    expect(receipt.fired).toBe(true)
    expect(receipt.predicates[0].meta).toEqual({ rows: 2 })
  })

  it('fails loudly on an unregistered predicate kind', async () => {
    const store = createTurtleStore(DATA)
    const hook = defineHook({
      id: 'ex:unknown',
      select: SELECT,
      predicates: [{ kind: 'MISSING', spec: {} }]
    })
    await expect(evaluateHook(hook, store)).rejects.toThrow(/Unknown predicate kind: MISSING/)
  })

  it('a hook without predicates fires iff the select returns rows', async () => {
    const hook = defineHook({ id: 'ex:none', select: SELECT })
    expect((await evaluateHook(hook, createTurtleStore(DATA))).fired).toBe(true)
    expect((await evaluateHook(hook, createTurtleStore())).fired).toBe(false)
  })

  it('provenance hashes are stable for identical inputs and change with the data', async () => {
    const hook = defineHook({ id: 'ex:prov', select: SELECT })
    const a = await evaluateHook(hook, createTurtleStore(DATA))
    const b = await evaluateHook(hook, createTurtleStore(DATA))
    const c = await evaluateHook(
      hook,
      createTurtleStore(DATA + '\n<http://example.org/s3> <http://example.org/latency> 1 .')
    )
    expect(a.provenance).toEqual(b.provenance)
    expect(a.provenance.sHash).not.toBe(c.provenance.sHash)
    expect(a.provenance.qHash).toBe(c.provenance.qHash)
  })
})
