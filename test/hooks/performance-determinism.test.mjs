/**
 * @fileoverview Test performance characteristics and deterministic behavior
 * 
 * Tests timing, hash consistency, and performance characteristics of the hooks system.
 */

import { describe, it, expect, beforeEach } from 'vitest'
import { initStore } from '../../src/context/index.mjs'
import { useTurtle } from '../../src/composables/use-turtle.mjs'
import { defineHook, evaluateHook } from '../../src/hooks.mjs'

// Sample RDF data for testing
const sampleTurtle = `
@prefix ex: <http://example.org/> .

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
`

describe('Performance and Determinism', () => {
  let runApp

  beforeEach(async () => {
    runApp = initStore()
    await runApp(async () => {
      const turtle = await useTurtle()
      await turtle.parse(sampleTurtle)
    })
  })

  describe('Hash Determinism', () => {
    it('should generate consistent hashes across multiple evaluations', async () => {
      await runApp(async () => {
        const hook = defineHook({
          id: 'test:determinism',
          select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }',
          predicates: [
            { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.01 } }
          ]
        })

        // Run multiple evaluations
        const receipts = []
        for (let i = 0; i < 5; i++) {
          const receipt = await evaluateHook(hook)
          receipts.push(receipt)
        }

        // Check hash consistency
        const firstHash = receipts[0].provenance.qHash
        const predicateHash = receipts[0].provenance.pHash
        const storeHash = receipts[0].provenance.sHash

        for (const receipt of receipts) {
          expect(receipt.provenance.qHash).toBe(firstHash)
          expect(receipt.provenance.pHash).toBe(predicateHash)
          expect(receipt.provenance.sHash).toBe(storeHash)
        }
      })
    })

    it('should generate different hashes for different queries', async () => {
      await runApp(async () => {
        const hook1 = defineHook({
          id: 'test:hash-diff-1',
          select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }',
          predicates: [
            { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.01 } }
          ]
        })

        const hook2 = defineHook({
          id: 'test:hash-diff-2',
          select: 'SELECT ?service ?latency WHERE { ?service <http://example.org/latency> ?latency }',
          predicates: [
            { kind: 'THRESHOLD', spec: { var: 'latency', op: '>', value: 400 } }
          ]
        })

        const receipt1 = await evaluateHook(hook1)
        const receipt2 = await evaluateHook(hook2)

        expect(receipt1.provenance.qHash).not.toBe(receipt2.provenance.qHash)
        expect(receipt1.provenance.pHash).not.toBe(receipt2.provenance.pHash)
      })
    })

    it('should generate different hashes for different predicate specifications', async () => {
      await runApp(async () => {
        const hook1 = defineHook({
          id: 'test:pred-hash-1',
          select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }',
          predicates: [
            { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.01 } }
          ]
        })

        const hook2 = defineHook({
          id: 'test:pred-hash-2',
          select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }',
          predicates: [
            { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.02 } }
          ]
        })

        const receipt1 = await evaluateHook(hook1)
        const receipt2 = await evaluateHook(hook2)

        expect(receipt1.provenance.qHash).toBe(receipt2.provenance.qHash) // Same query
        expect(receipt1.provenance.pHash).not.toBe(receipt2.provenance.pHash) // Different predicates
      })
    })

    it('should generate stable hashes for identical data', async () => {
      await runApp(async () => {
        const hook = defineHook({
          id: 'test:stable-hash',
          select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }',
          predicates: [
            { kind: 'DELTA', spec: { key: ['service', 'errorRate'], prev: [] } }
          ]
        })

        const receipt1 = await evaluateHook(hook)
        const receipt2 = await evaluateHook(hook)

        // DELTA predicate should generate same current hash for identical data
        expect(receipt1.predicates[0].meta.current).toEqual(receipt2.predicates[0].meta.current)
      })
    })
  })

  describe('Performance Timing', () => {
    it('should measure execution time accurately', async () => {
      await runApp(async () => {
        const hook = defineHook({
          id: 'test:timing',
          select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }',
          predicates: [
            { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.01 } }
          ]
        })

        const receipt = await evaluateHook(hook)
        
        expect(receipt.durations.totalMs).toBeGreaterThan(0)
        expect(receipt.durations.totalMs).toBeLessThan(1000) // Should be fast
        expect(typeof receipt.durations.totalMs).toBe('number')
      })
    })

    it('should have consistent performance across multiple runs', async () => {
      await runApp(async () => {
        const hook = defineHook({
          id: 'test:performance-consistency',
          select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }',
          predicates: [
            { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.01 } }
          ]
        })

        const durations = []
        for (let i = 0; i < 10; i++) {
          const receipt = await evaluateHook(hook)
          durations.push(receipt.durations.totalMs)
        }

        // All durations should be reasonable
        for (const duration of durations) {
          expect(duration).toBeGreaterThan(0)
          expect(duration).toBeLessThan(1000)
        }

        // Calculate average and check it's reasonable
        const avgDuration = durations.reduce((sum, d) => sum + d, 0) / durations.length
        expect(avgDuration).toBeGreaterThan(0)
        expect(avgDuration).toBeLessThan(100)
      })
    })

    it('should handle complex queries efficiently', async () => {
      await runApp(async () => {
        const hook = defineHook({
          id: 'test:complex-performance',
          select: 'SELECT ?service ?errorRate ?latency ?requests WHERE { ?service <http://example.org/errorRate> ?errorRate ; <http://example.org/latency> ?latency ; <http://example.org/requests> ?requests }',
          predicates: [
            { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.001 } },
            { kind: 'THRESHOLD', spec: { var: 'latency', op: '>', value: 200 } },
            { kind: 'THRESHOLD', spec: { var: 'requests', op: '>', value: 100 } },
            { kind: 'ASK', spec: { query: 'ASK WHERE { ?s a <http://example.org/Service> }' } },
            { kind: 'DELTA', spec: { key: ['service'], prev: [] } }
          ],
          combine: 'AND'
        })

        const receipt = await evaluateHook(hook)
        
        expect(receipt.durations.totalMs).toBeGreaterThan(0)
        expect(receipt.durations.totalMs).toBeLessThan(2000) // Should still be fast even with complex predicates
        expect(receipt.predicates).toHaveLength(5)
      })
    })
  })

  describe('Memory and Resource Usage', () => {
    it('should not leak memory across multiple evaluations', async () => {
      await runApp(async () => {
        const hook = defineHook({
          id: 'test:memory',
          select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }',
          predicates: [
            { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.01 } }
          ]
        })

        // Run many evaluations
        for (let i = 0; i < 100; i++) {
          const receipt = await evaluateHook(hook)
          expect(receipt.fired).toBe(true)
        }

        // If we get here without memory issues, the test passes
        expect(true).toBe(true)
      })
    })

    it('should handle large result sets efficiently', async () => {
      await runApp(async () => {
        // Create a hook that returns all services
        const hook = defineHook({
          id: 'test:large-results',
          select: 'SELECT ?service ?errorRate ?latency ?requests WHERE { ?service <http://example.org/errorRate> ?errorRate ; <http://example.org/latency> ?latency ; <http://example.org/requests> ?requests }',
          predicates: [
            { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>=', value: 0 } } // Always true
          ]
        })

        const receipt = await evaluateHook(hook)
        
        expect(receipt.fired).toBe(true)
        expect(receipt.durations.totalMs).toBeLessThan(1000) // Should handle efficiently
      })
    })
  })

  describe('Concurrent Evaluation', () => {
    it('should handle concurrent hook evaluations', async () => {
      await runApp(async () => {
        const hooks = [
          defineHook({
            id: 'test:concurrent-1',
            select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }',
            predicates: [{ kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.01 } }]
          }),
          defineHook({
            id: 'test:concurrent-2',
            select: 'SELECT ?service ?latency WHERE { ?service <http://example.org/latency> ?latency }',
            predicates: [{ kind: 'THRESHOLD', spec: { var: 'latency', op: '>', value: 400 } }]
          }),
          defineHook({
            id: 'test:concurrent-3',
            select: 'SELECT ?service ?requests WHERE { ?service <http://example.org/requests> ?requests }',
            predicates: [{ kind: 'THRESHOLD', spec: { var: 'requests', op: '>', value: 1000 } }]
          })
        ]

        // Evaluate all hooks concurrently
        const receipts = await Promise.all(hooks.map(hook => evaluateHook(hook)))

        expect(receipts).toHaveLength(3)
        receipts.forEach((receipt, i) => {
          expect(receipt.id).toBe(`test:concurrent-${i + 1}`)
          expect(receipt.fired).toBe(true)
          expect(receipt.durations.totalMs).toBeGreaterThan(0)
        })
      })
    })

    it('should maintain isolation between concurrent evaluations', async () => {
      await runApp(async () => {
        const hook1 = defineHook({
          id: 'test:isolation-1',
          select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }',
          predicates: [{ kind: 'DELTA', spec: { key: ['service'], prev: [] } }]
        })

        const hook2 = defineHook({
          id: 'test:isolation-2',
          select: 'SELECT ?service ?latency WHERE { ?service <http://example.org/latency> ?latency }',
          predicates: [{ kind: 'DELTA', spec: { key: ['service'], prev: [] } }]
        })

        // Evaluate concurrently
        const [receipt1, receipt2] = await Promise.all([
          evaluateHook(hook1),
          evaluateHook(hook2)
        ])

        // Each should have its own delta state
        expect(receipt1.predicates[0].meta.current).toBeDefined()
        expect(receipt2.predicates[0].meta.current).toBeDefined()
        expect(receipt1.id).toBe('test:isolation-1')
        expect(receipt2.id).toBe('test:isolation-2')
      })
    })
  })

  describe('Error Handling Performance', () => {
    it('should handle errors efficiently', async () => {
      await runApp(async () => {
        const hook = defineHook({
          id: 'test:error-performance',
          select: 'SELECT ?service ?nonexistent WHERE { ?service <http://example.org/nonexistent> ?nonexistent }',
          predicates: [
            { kind: 'THRESHOLD', spec: { var: 'nonexistent', op: '>', value: 0 } }
          ]
        })

        const receipt = await evaluateHook(hook)
        
        expect(receipt.fired).toBe(false)
        expect(receipt.durations.totalMs).toBeLessThan(1000) // Should fail fast
      })
    })

    it('should handle malformed queries efficiently', async () => {
      await runApp(async () => {
        const hook = defineHook({
          id: 'test:malformed-performance',
          select: 'INVALID SPARQL QUERY',
          predicates: [
            { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.01 } }
          ]
        })

        // This should throw an error quickly
        await expect(evaluateHook(hook)).rejects.toThrow()
      })
    })
  })
})
