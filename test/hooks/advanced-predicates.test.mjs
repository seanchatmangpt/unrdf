/**
 * @fileoverview Test advanced Knowledge Hooks predicates (SHACL, WINDOW)
 * 
 * Tests the more complex predicate types including validation and windowing.
 */

import { describe, it, expect, beforeEach } from 'vitest'
import { initStore } from '../../src/context/index.mjs'
import { useTurtle } from '../../src/composables/use-turtle.mjs'
import { defineHook, evaluateHook } from '../../src/hooks.mjs'

// Sample RDF data for testing
const sampleTurtle = `
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix sh: <http://www.w3.org/ns/shacl#> .

ex:service1 a ex:Service ;
  ex:errorRate 0.01 ;
  ex:latency 500 ;
  ex:requests 1000 ;
  ex:status ex:active .

ex:service2 a ex:Service ;
  ex:errorRate 0.03 ;
  ex:latency 1200 ;
  ex:requests 2000 ;
  ex:status ex:active .

ex:service3 a ex:Service ;
  ex:errorRate 0.005 ;
  ex:latency 300 ;
  ex:requests 500 ;
  ex:status ex:maintenance .

ex:person1 a foaf:Person ;
  foaf:name "Alice" ;
  foaf:age 30 .

ex:person2 a foaf:Person ;
  foaf:name "Bob" ;
  foaf:age 25 .

# Sample SHACL shapes
ex:ServiceShape a sh:NodeShape ;
  sh:targetClass ex:Service ;
  sh:property [
    sh:path ex:errorRate ;
    sh:datatype xsd:decimal ;
    sh:minInclusive 0 ;
    sh:maxInclusive 1
  ] .
`

describe('Advanced Knowledge Hooks Predicates', () => {
  let runApp

  beforeEach(async () => {
    runApp = initStore()
    await runApp(async () => {
      const turtle = await useTurtle()
      await turtle.parse(sampleTurtle)
    })
  })

  describe('SHACL Predicate', () => {
    it('should handle SHACL validation stub', async () => {
      await runApp(async () => {
        const hook = defineHook({
          id: 'test:shacl',
          select: 'SELECT ?service WHERE { ?service a <http://example.org/Service> }',
          predicates: [
            { kind: 'SHACL', spec: { shapes: 'ex:ServiceShape', strict: true } }
          ]
        })

        const receipt = await evaluateHook(hook)
        expect(receipt.fired).toBe(true)
        expect(receipt.predicates[0].kind).toBe('SHACL')
        expect(receipt.predicates[0].ok).toBe(true)
        expect(receipt.predicates[0].meta.note).toContain('SHACL stub')
      })
    })

    it('should handle different SHACL configurations', async () => {
      await runApp(async () => {
        const strictHook = defineHook({
          id: 'test:shacl-strict',
          select: 'SELECT ?service WHERE { ?service a <http://example.org/Service> }',
          predicates: [
            { kind: 'SHACL', spec: { shapes: 'ex:ServiceShape', strict: true } }
          ]
        })

        const lenientHook = defineHook({
          id: 'test:shacl-lenient',
          select: 'SELECT ?service WHERE { ?service a <http://example.org/Service> }',
          predicates: [
            { kind: 'SHACL', spec: { shapes: 'ex:ServiceShape', strict: false } }
          ]
        })

        const strictReceipt = await evaluateHook(strictHook)
        const lenientReceipt = await evaluateHook(lenientHook)

        expect(strictReceipt.predicates[0].ok).toBe(true)
        expect(lenientReceipt.predicates[0].ok).toBe(true)
      })
    })
  })

  describe('WINDOW Predicate', () => {
    it('should handle count aggregation', async () => {
      await runApp(async () => {
        const hook = defineHook({
          id: 'test:window-count',
          select: 'SELECT ?service ?requests WHERE { ?service <http://example.org/requests> ?requests }',
          predicates: [
            { 
              kind: 'WINDOW', 
              spec: { 
                var: 'requests', 
                size: '5m', 
                op: 'count', 
                cmp: { op: '>', value: 2 } 
              } 
            }
          ]
        })

        const receipt = await evaluateHook(hook)
        expect(receipt.fired).toBe(true)
        expect(receipt.predicates[0].kind).toBe('WINDOW')
        expect(receipt.predicates[0].ok).toBe(true)
        expect(receipt.predicates[0].meta.agg).toBe(3) // 3 services
        expect(receipt.predicates[0].meta.op).toBe('count')
      })
    })

    it('should handle sum aggregation', async () => {
      await runApp(async () => {
        const hook = defineHook({
          id: 'test:window-sum',
          select: 'SELECT ?service ?requests WHERE { ?service <http://example.org/requests> ?requests }',
          predicates: [
            { 
              kind: 'WINDOW', 
              spec: { 
                var: 'requests', 
                size: '5m', 
                op: 'sum', 
                cmp: { op: '>', value: 3000 } 
              } 
            }
          ]
        })

        const receipt = await evaluateHook(hook)
        expect(receipt.fired).toBe(true)
        expect(receipt.predicates[0].meta.op).toBe('sum')
        expect(receipt.predicates[0].meta.agg).toBe(3500) // 1000 + 2000 + 500
      })
    })

    it('should handle average aggregation', async () => {
      await runApp(async () => {
        const hook = defineHook({
          id: 'test:window-avg',
          select: 'SELECT ?service ?requests WHERE { ?service <http://example.org/requests> ?requests }',
          predicates: [
            { 
              kind: 'WINDOW', 
              spec: { 
                var: 'requests', 
                size: '5m', 
                op: 'avg', 
                cmp: { op: '>', value: 1000 } 
              } 
            }
          ]
        })

        const receipt = await evaluateHook(hook)
        expect(receipt.fired).toBe(true)
        expect(receipt.predicates[0].meta.op).toBe('avg')
        expect(receipt.predicates[0].meta.agg).toBeCloseTo(1166.67, 2) // (1000 + 2000 + 500) / 3
      })
    })

    it('should handle different comparison operators', async () => {
      await runApp(async () => {
        const operators = ['>', '>=', '<', '<=', '==', '!=']
        
        for (const op of operators) {
          const hook = defineHook({
            id: `test:window-${op}`,
            select: 'SELECT ?service ?requests WHERE { ?service <http://example.org/requests> ?requests }',
            predicates: [
              { 
                kind: 'WINDOW', 
                spec: { 
                  var: 'requests', 
                  size: '5m', 
                  op: 'count', 
                  cmp: { op, value: 2 } 
                } 
              }
            ]
          })

          const receipt = await evaluateHook(hook)
          expect(receipt.predicates[0].kind).toBe('WINDOW')
          expect(typeof receipt.predicates[0].ok).toBe('boolean')
        }
      })
    })

    it('should handle missing comparison (always true)', async () => {
      await runApp(async () => {
        const hook = defineHook({
          id: 'test:window-no-cmp',
          select: 'SELECT ?service ?requests WHERE { ?service <http://example.org/requests> ?requests }',
          predicates: [
            { 
              kind: 'WINDOW', 
              spec: { 
                var: 'requests', 
                size: '5m', 
                op: 'count'
                // No cmp specified
              } 
            }
          ]
        })

        const receipt = await evaluateHook(hook)
        expect(receipt.fired).toBe(true)
        expect(receipt.predicates[0].ok).toBe(true)
      })
    })

    it('should handle empty result sets', async () => {
      await runApp(async () => {
        const hook = defineHook({
          id: 'test:window-empty',
          select: 'SELECT ?service ?requests WHERE { ?service <http://example.org/nonexistent> ?requests }',
          predicates: [
            { 
              kind: 'WINDOW', 
              spec: { 
                var: 'requests', 
                size: '5m', 
                op: 'count', 
                cmp: { op: '>', value: 0 } 
              } 
            }
          ]
        })

        const receipt = await evaluateHook(hook)
        expect(receipt.fired).toBe(false)
        expect(receipt.predicates[0].meta.agg).toBe(0)
      })
    })

    it('should handle non-numeric values gracefully', async () => {
      await runApp(async () => {
        const hook = defineHook({
          id: 'test:window-non-numeric',
          select: 'SELECT ?service ?status WHERE { ?service <http://example.org/status> ?status }',
          predicates: [
            { 
              kind: 'WINDOW', 
              spec: { 
                var: 'status', 
                size: '5m', 
                op: 'count', 
                cmp: { op: '>', value: 0 } 
              } 
            }
          ]
        })

        const receipt = await evaluateHook(hook)
        expect(receipt.fired).toBe(true)
        expect(receipt.predicates[0].meta.agg).toBe(3) // Count of non-numeric values
      })
    })
  })

  describe('Edge Cases', () => {
    it('should handle malformed predicate specifications', async () => {
      await runApp(async () => {
        // This should throw an error for unknown predicate kind
        const hook = defineHook({
          id: 'test:malformed',
          select: 'SELECT ?service WHERE { ?service a <http://example.org/Service> }',
          predicates: [
            { kind: 'UNKNOWN', spec: { some: 'value' } }
          ]
        })

        await expect(evaluateHook(hook)).rejects.toThrow('Unknown predicate kind: UNKNOWN')
      })
    })

    it('should handle empty predicate arrays', async () => {
      await runApp(async () => {
        const hook = defineHook({
          id: 'test:empty-predicates',
          select: 'SELECT ?service WHERE { ?service a <http://example.org/Service> }',
          predicates: []
        })

        const receipt = await evaluateHook(hook)
        expect(receipt.fired).toBe(true) // AND with empty array is true
        expect(receipt.predicates).toHaveLength(0)
      })
    })
  })
})
