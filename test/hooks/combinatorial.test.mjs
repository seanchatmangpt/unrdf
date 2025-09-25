/**
 * @fileoverview Comprehensive combinatorial test for Knowledge Hooks
 * 
 * Tests all possible combinations of predicates, operators, and logic to ensure
 * the hooks system works correctly in every scenario.
 */

import { describe, it, expect, beforeEach } from 'vitest'
import { initStore } from '../../src/context/index.mjs'
import { useTurtle } from '../../src/composables/use-turtle.mjs'
import { defineHook, evaluateHook, registerPredicate } from '../../src/hooks.mjs'

// Comprehensive RDF data for testing all scenarios
const comprehensiveTurtle = `
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Services with various metrics
ex:service1 a ex:Service ;
  ex:errorRate 0.01 ;
  ex:latency 500 ;
  ex:requests 1000 ;
  ex:status ex:active ;
  ex:version "1.0.0" .

ex:service2 a ex:Service ;
  ex:errorRate 0.03 ;
  ex:latency 1200 ;
  ex:requests 2000 ;
  ex:status ex:active ;
  ex:version "1.1.0" .

ex:service3 a ex:Service ;
  ex:errorRate 0.005 ;
  ex:latency 300 ;
  ex:requests 500 ;
  ex:status ex:maintenance ;
  ex:version "0.9.0" .

ex:service4 a ex:Service ;
  ex:errorRate 0.05 ;
  ex:latency 2000 ;
  ex:requests 3000 ;
  ex:status ex:degraded ;
  ex:version "1.2.0" .

# People for FOAF testing
ex:person1 a foaf:Person ;
  foaf:name "Alice" ;
  foaf:age 30 ;
  foaf:mbox "alice@example.org" .

ex:person2 a foaf:Person ;
  foaf:name "Bob" ;
  foaf:age 25 ;
  foaf:mbox "bob@example.org" .

ex:person3 a foaf:Person ;
  foaf:name "Charlie" ;
  foaf:age 35 ;
  foaf:mbox "charlie@example.org" .

# SHACL shapes
ex:ServiceShape a sh:NodeShape ;
  sh:targetClass ex:Service ;
  sh:property [
    sh:path ex:errorRate ;
    sh:datatype xsd:decimal ;
    sh:minInclusive 0 ;
    sh:maxInclusive 1
  ] ;
  sh:property [
    sh:path ex:latency ;
    sh:datatype xsd:integer ;
    sh:minInclusive 0
  ] .

ex:PersonShape a sh:NodeShape ;
  sh:targetClass foaf:Person ;
  sh:property [
    sh:path foaf:name ;
    sh:datatype xsd:string ;
    sh:minLength 1
  ] ;
  sh:property [
    sh:path foaf:age ;
    sh:datatype xsd:integer ;
    sh:minInclusive 0 ;
    sh:maxInclusive 150
  ] .
`

describe('Combinatorial Knowledge Hooks Tests', () => {
  let runApp

  beforeEach(async () => {
    runApp = initStore()
    await runApp(async () => {
      const turtle = await useTurtle()
      await turtle.parse(comprehensiveTurtle)
    })
  })

  describe('All Predicate Types Combinatorially', () => {
    const predicateTypes = ['ASK', 'THRESHOLD', 'DELTA', 'SHACL', 'WINDOW']
    const operators = ['AND', 'OR']
    const thresholdOps = ['>', '>=', '<', '<=', '==', '!=']
    const windowOps = ['count', 'sum', 'avg']

    it('should test every predicate type individually', async () => {
      await runApp(async () => {
        for (const predicateType of predicateTypes) {
          const hook = defineHook({
            id: `test:single-${predicateType}`,
            select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }',
            predicates: [createPredicateSpec(predicateType, 0)]
          })

          const receipt = await evaluateHook(hook)
          expect(receipt.predicates).toHaveLength(1)
          expect(receipt.predicates[0].kind).toBe(predicateType)
          expect(typeof receipt.predicates[0].ok).toBe('boolean')
        }
      })
    })

    it('should test all threshold operators', async () => {
      await runApp(async () => {
        for (const op of thresholdOps) {
          const hook = defineHook({
            id: `test:threshold-${op}`,
            select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }',
            predicates: [
              { kind: 'THRESHOLD', spec: { var: 'errorRate', op, value: 0.02 } }
            ]
          })

          const receipt = await evaluateHook(hook)
          expect(receipt.predicates[0].kind).toBe('THRESHOLD')
          expect(typeof receipt.predicates[0].ok).toBe('boolean')
        }
      })
    })

    it('should test all window operations', async () => {
      await runApp(async () => {
        for (const op of windowOps) {
          const hook = defineHook({
            id: `test:window-${op}`,
            select: 'SELECT ?service ?requests WHERE { ?service <http://example.org/requests> ?requests }',
            predicates: [
              { 
                kind: 'WINDOW', 
                spec: { 
                  var: 'requests', 
                  size: '5m', 
                  op, 
                  cmp: { op: '>', value: 0 } 
                } 
              }
            ]
          })

          const receipt = await evaluateHook(hook)
          expect(receipt.predicates[0].kind).toBe('WINDOW')
          expect(receipt.predicates[0].meta.op).toBe(op)
          expect(typeof receipt.predicates[0].ok).toBe('boolean')
        }
      })
    })

    it('should test all combination operators', async () => {
      await runApp(async () => {
        for (const combine of operators) {
          const hook = defineHook({
            id: `test:combine-${combine}`,
            select: 'SELECT ?service ?errorRate ?latency WHERE { ?service <http://example.org/errorRate> ?errorRate ; <http://example.org/latency> ?latency }',
            predicates: [
              { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.01 } },
              { kind: 'THRESHOLD', spec: { var: 'latency', op: '>', value: 400 } }
            ],
            combine
          })

          const receipt = await evaluateHook(hook)
          expect(receipt.predicates).toHaveLength(2)
          expect(typeof receipt.fired).toBe('boolean')
          
          if (combine === 'AND') {
            // AND should be true only if both predicates are true
            const bothTrue = receipt.predicates.every(p => p.ok)
            expect(receipt.fired).toBe(bothTrue)
          } else if (combine === 'OR') {
            // OR should be true if any predicate is true
            const anyTrue = receipt.predicates.some(p => p.ok)
            expect(receipt.fired).toBe(anyTrue)
          }
        }
      })
    })
  })

  describe('Two-Predicate Combinations', () => {
    const predicatePairs = [
      ['ASK', 'THRESHOLD'],
      ['ASK', 'DELTA'],
      ['ASK', 'SHACL'],
      ['ASK', 'WINDOW'],
      ['THRESHOLD', 'DELTA'],
      ['THRESHOLD', 'SHACL'],
      ['THRESHOLD', 'WINDOW'],
      ['DELTA', 'SHACL'],
      ['DELTA', 'WINDOW'],
      ['SHACL', 'WINDOW']
    ]

    it('should test all two-predicate combinations with AND', async () => {
      await runApp(async () => {
        for (const [pred1, pred2] of predicatePairs) {
          const hook = defineHook({
            id: `test:${pred1}-${pred2}-and`,
            select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }',
            predicates: [
              createPredicateSpec(pred1, 0),
              createPredicateSpec(pred2, 1)
            ],
            combine: 'AND'
          })

          const receipt = await evaluateHook(hook)
          expect(receipt.predicates).toHaveLength(2)
          expect(receipt.predicates[0].kind).toBe(pred1)
          expect(receipt.predicates[1].kind).toBe(pred2)
          expect(typeof receipt.fired).toBe('boolean')
        }
      })
    })

    it('should test all two-predicate combinations with OR', async () => {
      await runApp(async () => {
        for (const [pred1, pred2] of predicatePairs) {
          const hook = defineHook({
            id: `test:${pred1}-${pred2}-or`,
            select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }',
            predicates: [
              createPredicateSpec(pred1, 0),
              createPredicateSpec(pred2, 1)
            ],
            combine: 'OR'
          })

          const receipt = await evaluateHook(hook)
          expect(receipt.predicates).toHaveLength(2)
          expect(receipt.predicates[0].kind).toBe(pred1)
          expect(receipt.predicates[1].kind).toBe(pred2)
          expect(typeof receipt.fired).toBe('boolean')
        }
      })
    })
  })

  describe('Three-Predicate Combinations', () => {
    const threePredicateCombos = [
      ['ASK', 'THRESHOLD', 'DELTA'],
      ['ASK', 'THRESHOLD', 'SHACL'],
      ['ASK', 'THRESHOLD', 'WINDOW'],
      ['ASK', 'DELTA', 'SHACL'],
      ['ASK', 'DELTA', 'WINDOW'],
      ['ASK', 'SHACL', 'WINDOW'],
      ['THRESHOLD', 'DELTA', 'SHACL'],
      ['THRESHOLD', 'DELTA', 'WINDOW'],
      ['THRESHOLD', 'SHACL', 'WINDOW'],
      ['DELTA', 'SHACL', 'WINDOW']
    ]

    it('should test all three-predicate combinations', async () => {
      await runApp(async () => {
        for (const [pred1, pred2, pred3] of threePredicateCombos) {
          const hook = defineHook({
            id: `test:${pred1}-${pred2}-${pred3}`,
            select: 'SELECT ?service ?errorRate ?latency WHERE { ?service <http://example.org/errorRate> ?errorRate ; <http://example.org/latency> ?latency }',
            predicates: [
              createPredicateSpec(pred1, 0),
              createPredicateSpec(pred2, 1),
              createPredicateSpec(pred3, 2)
            ],
            combine: 'AND'
          })

          const receipt = await evaluateHook(hook)
          expect(receipt.predicates).toHaveLength(3)
          expect(receipt.predicates[0].kind).toBe(pred1)
          expect(receipt.predicates[1].kind).toBe(pred2)
          expect(receipt.predicates[2].kind).toBe(pred3)
          expect(typeof receipt.fired).toBe('boolean')
        }
      })
    })
  })

  describe('All-Four-Predicate Combinations', () => {
    it('should test all four predicate types together', async () => {
      await runApp(async () => {
        const hook = defineHook({
          id: 'test:all-four-and',
          select: 'SELECT ?service ?errorRate ?latency ?requests WHERE { ?service <http://example.org/errorRate> ?errorRate ; <http://example.org/latency> ?latency ; <http://example.org/requests> ?requests }',
          predicates: [
            { kind: 'ASK', spec: { query: 'ASK WHERE { ?s a <http://example.org/Service> }' } },
            { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.001 } },
            { kind: 'DELTA', spec: { key: ['service'], prev: [] } },
            { kind: 'SHACL', spec: { shapes: 'ex:ServiceShape' } }
          ],
          combine: 'AND'
        })

        const receipt = await evaluateHook(hook)
        expect(receipt.predicates).toHaveLength(4)
        expect(receipt.predicates[0].kind).toBe('ASK')
        expect(receipt.predicates[1].kind).toBe('THRESHOLD')
        expect(receipt.predicates[2].kind).toBe('DELTA')
        expect(receipt.predicates[3].kind).toBe('SHACL')
        expect(typeof receipt.fired).toBe('boolean')
      })
    })

    it('should test all four predicate types with OR', async () => {
      await runApp(async () => {
        const hook = defineHook({
          id: 'test:all-four-or',
          select: 'SELECT ?service ?errorRate ?latency ?requests WHERE { ?service <http://example.org/errorRate> ?errorRate ; <http://example.org/latency> ?latency ; <http://example.org/requests> ?requests }',
          predicates: [
            { kind: 'ASK', spec: { query: 'ASK WHERE { ?s a <http://example.org/NonExistent> }' } }, // This will be false
            { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 1.0 } }, // This will be false
            { kind: 'DELTA', spec: { key: ['service'], prev: [] } }, // This will be true
            { kind: 'SHACL', spec: { shapes: 'ex:ServiceShape' } } // This will be true
          ],
          combine: 'OR'
        })

        const receipt = await evaluateHook(hook)
        expect(receipt.predicates).toHaveLength(4)
        expect(receipt.fired).toBe(true) // OR should be true because DELTA and SHACL are true
      })
    })
  })

  describe('All-Five-Predicate Combinations', () => {
    it('should test all five predicate types together', async () => {
      await runApp(async () => {
        const hook = defineHook({
          id: 'test:all-five',
          select: 'SELECT ?service ?errorRate ?latency ?requests WHERE { ?service <http://example.org/errorRate> ?errorRate ; <http://example.org/latency> ?latency ; <http://example.org/requests> ?requests }',
          predicates: [
            { kind: 'ASK', spec: { query: 'ASK WHERE { ?s a <http://example.org/Service> }' } },
            { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.001 } },
            { kind: 'DELTA', spec: { key: ['service'], prev: [] } },
            { kind: 'SHACL', spec: { shapes: 'ex:ServiceShape' } },
            { 
              kind: 'WINDOW', 
              spec: { 
                var: 'requests', 
                size: '5m', 
                op: 'count', 
                cmp: { op: '>', value: 2 } 
              } 
            }
          ],
          combine: 'AND'
        })

        const receipt = await evaluateHook(hook)
        expect(receipt.predicates).toHaveLength(5)
        expect(receipt.predicates[0].kind).toBe('ASK')
        expect(receipt.predicates[1].kind).toBe('THRESHOLD')
        expect(receipt.predicates[2].kind).toBe('DELTA')
        expect(receipt.predicates[3].kind).toBe('SHACL')
        expect(receipt.predicates[4].kind).toBe('WINDOW')
        expect(typeof receipt.fired).toBe('boolean')
      })
    })
  })

  describe('Edge Case Combinations', () => {
    it('should handle empty predicate arrays', async () => {
      await runApp(async () => {
        const hook = defineHook({
          id: 'test:empty-predicates',
          select: 'SELECT ?service WHERE { ?service a <http://example.org/Service> }',
          predicates: [],
          combine: 'AND'
        })

        const receipt = await evaluateHook(hook)
        expect(receipt.fired).toBe(true) // AND with empty array is true
        expect(receipt.predicates).toHaveLength(0)
      })
    })

    it('should handle single predicate arrays', async () => {
      await runApp(async () => {
        const hook = defineHook({
          id: 'test:single-predicate',
          select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }',
          predicates: [
            { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.02 } }
          ],
          combine: 'AND'
        })

        const receipt = await evaluateHook(hook)
        expect(receipt.predicates).toHaveLength(1)
        expect(receipt.fired).toBe(receipt.predicates[0].ok)
      })
    })

    it('should handle mixed true/false predicates', async () => {
      await runApp(async () => {
        const hook = defineHook({
          id: 'test:mixed-true-false',
          select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }',
          predicates: [
            { kind: 'ASK', spec: { query: 'ASK WHERE { ?s a <http://example.org/Service> }' } }, // True
            { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 1.0 } }, // False
            { kind: 'DELTA', spec: { key: ['service'], prev: [] } }, // True
            { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.1 } } // False
          ],
          combine: 'AND'
        })

        const receipt = await evaluateHook(hook)
        expect(receipt.predicates).toHaveLength(4)
        expect(receipt.fired).toBe(false) // AND with any false should be false
      })
    })
  })

  describe('Custom Predicate Combinations', () => {
    beforeEach(async () => {
      // Register custom predicates for testing
      registerPredicate('CUSTOM_COUNT', async (spec, ctx) => {
        const { rows } = ctx
        const count = rows.length
        return { 
          ok: count >= spec.threshold, 
          meta: { count, threshold: spec.threshold, kind: 'CUSTOM_COUNT' } 
        }
      })

      registerPredicate('CUSTOM_RANGE', async (spec, ctx) => {
        const { rows } = ctx
        const values = rows.map(r => Number(r[spec.var] ?? 0))
        const inRange = values.every(v => v >= spec.min && v <= spec.max)
        return { 
          ok: inRange, 
          meta: { min: spec.min, max: spec.max, inRange, kind: 'CUSTOM_RANGE' } 
        }
      })
    })

    it('should combine custom predicates with built-in predicates', async () => {
      await runApp(async () => {
        const hook = defineHook({
          id: 'test:custom-builtin',
          select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }',
          predicates: [
            { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.001 } },
            { kind: 'CUSTOM_COUNT', spec: { threshold: 2 } },
            { kind: 'CUSTOM_RANGE', spec: { var: 'errorRate', min: 0, max: 0.1 } }
          ],
          combine: 'AND'
        })

        const receipt = await evaluateHook(hook)
        expect(receipt.predicates).toHaveLength(3)
        expect(receipt.predicates[0].kind).toBe('THRESHOLD')
        expect(receipt.predicates[1].kind).toBe('CUSTOM_COUNT')
        expect(receipt.predicates[2].kind).toBe('CUSTOM_RANGE')
        expect(typeof receipt.fired).toBe('boolean')
      })
    })
  })

  describe('Performance Under Load', () => {
    it('should handle many predicate combinations efficiently', async () => {
      await runApp(async () => {
        const hooks = []
        
        // Create many different hook combinations
        for (let i = 0; i < 20; i++) {
          const hook = defineHook({
            id: `test:load-${i}`,
            select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }',
            predicates: [
              { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.001 + i * 0.001 } },
              { kind: 'DELTA', spec: { key: ['service'], prev: [] } }
            ],
            combine: i % 2 === 0 ? 'AND' : 'OR'
          })
          hooks.push(hook)
        }

        // Evaluate all hooks concurrently
        const receipts = await Promise.all(hooks.map(hook => evaluateHook(hook)))
        
        expect(receipts).toHaveLength(20)
        receipts.forEach((receipt, i) => {
          expect(receipt.id).toBe(`test:load-${i}`)
          expect(receipt.predicates).toHaveLength(2)
          expect(typeof receipt.fired).toBe('boolean')
        })
      })
    })
  })
})

/**
 * Helper function to create predicate specifications
 */
function createPredicateSpec(type, index) {
  switch (type) {
    case 'ASK':
      return { kind: 'ASK', spec: { query: 'ASK WHERE { ?s a <http://example.org/Service> }' } }
    case 'THRESHOLD':
      return { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.01 + index * 0.01 } }
    case 'DELTA':
      return { kind: 'DELTA', spec: { key: ['service'], prev: [] } }
    case 'SHACL':
      return { kind: 'SHACL', spec: { shapes: 'ex:ServiceShape' } }
    case 'WINDOW':
      return { 
        kind: 'WINDOW', 
        spec: { 
          var: 'errorRate', 
          size: '5m', 
          op: 'count', 
          cmp: { op: '>', value: index } 
        } 
      }
    default:
      throw new Error(`Unknown predicate type: ${type}`)
  }
}
