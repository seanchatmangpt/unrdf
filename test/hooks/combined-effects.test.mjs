/**
 * @fileoverview Test combined predicates and effect functions
 * 
 * Tests the combination logic (AND/OR) and side effect functions.
 */

import { describe, it, expect, beforeEach, vi } from 'vitest'
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
`

describe('Combined Predicates and Effects', () => {
  let runApp

  beforeEach(async () => {
    runApp = initStore()
    await runApp(async () => {
      const turtle = await useTurtle()
      await turtle.parse(sampleTurtle)
    })
  })

  describe('AND Combination', () => {
    it('should fire when all predicates are true', async () => {
      await runApp(async () => {
        const hook = defineHook({
          id: 'test:and-all-true',
          select: 'SELECT ?service ?errorRate ?latency WHERE { ?service <http://example.org/errorRate> ?errorRate ; <http://example.org/latency> ?latency }',
          predicates: [
            { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.001 } },
            { kind: 'THRESHOLD', spec: { var: 'latency', op: '>', value: 200 } }
          ],
          combine: 'AND'
        })

        const receipt = await evaluateHook(hook)
        expect(receipt.fired).toBe(true)
        expect(receipt.predicates).toHaveLength(2)
        expect(receipt.predicates[0].ok).toBe(true)
        expect(receipt.predicates[1].ok).toBe(true)
      })
    })

    it('should not fire when any predicate is false', async () => {
      await runApp(async () => {
        const hook = defineHook({
          id: 'test:and-one-false',
          select: 'SELECT ?service ?errorRate ?latency WHERE { ?service <http://example.org/errorRate> ?errorRate ; <http://example.org/latency> ?latency }',
          predicates: [
            { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.001 } },
            { kind: 'THRESHOLD', spec: { var: 'latency', op: '>', value: 2000 } } // This will be false
          ],
          combine: 'AND'
        })

        const receipt = await evaluateHook(hook)
        expect(receipt.fired).toBe(false)
        expect(receipt.predicates[0].ok).toBe(true)
        expect(receipt.predicates[1].ok).toBe(false)
      })
    })

    it('should default to AND when combine is not specified', async () => {
      await runApp(async () => {
        const hook = defineHook({
          id: 'test:and-default',
          select: 'SELECT ?service ?errorRate ?latency WHERE { ?service <http://example.org/errorRate> ?errorRate ; <http://example.org/latency> ?latency }',
          predicates: [
            { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.001 } },
            { kind: 'THRESHOLD', spec: { var: 'latency', op: '>', value: 2000 } }
          ]
          // No combine specified - should default to AND
        })

        const receipt = await evaluateHook(hook)
        expect(receipt.fired).toBe(false) // AND with one false = false
      })
    })
  })

  describe('OR Combination', () => {
    it('should fire when any predicate is true', async () => {
      await runApp(async () => {
        const hook = defineHook({
          id: 'test:or-any-true',
          select: 'SELECT ?service ?errorRate ?latency WHERE { ?service <http://example.org/errorRate> ?errorRate ; <http://example.org/latency> ?latency }',
          predicates: [
            { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.001 } },
            { kind: 'THRESHOLD', spec: { var: 'latency', op: '>', value: 2000 } } // This will be false
          ],
          combine: 'OR'
        })

        const receipt = await evaluateHook(hook)
        expect(receipt.fired).toBe(true)
        expect(receipt.predicates[0].ok).toBe(true)
        expect(receipt.predicates[1].ok).toBe(false)
      })
    })

    it('should not fire when all predicates are false', async () => {
      await runApp(async () => {
        const hook = defineHook({
          id: 'test:or-all-false',
          select: 'SELECT ?service ?errorRate ?latency WHERE { ?service <http://example.org/errorRate> ?errorRate ; <http://example.org/latency> ?latency }',
          predicates: [
            { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 1.0 } }, // This will be false
            { kind: 'THRESHOLD', spec: { var: 'latency', op: '>', value: 2000 } } // This will be false
          ],
          combine: 'OR'
        })

        const receipt = await evaluateHook(hook)
        expect(receipt.fired).toBe(false)
        expect(receipt.predicates[0].ok).toBe(false)
        expect(receipt.predicates[1].ok).toBe(false)
      })
    })
  })

  describe('Effect Functions', () => {
    it('should execute effect when hook fires', async () => {
      await runApp(async () => {
        const mockEffect = vi.fn()
        
        const hook = defineHook({
          id: 'test:effect-fired',
          select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }',
          predicates: [
            { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.02 } }
          ],
          effect: mockEffect
        })

        const receipt = await evaluateHook(hook)
        expect(receipt.fired).toBe(true)
        expect(mockEffect).toHaveBeenCalledTimes(1)
        expect(mockEffect).toHaveBeenCalledWith({
          rows: expect.any(Array),
          receipt: expect.any(Object),
          store: expect.any(Object),
          graph: expect.any(Object)
        })
      })
    })

    it('should not execute effect when hook does not fire', async () => {
      await runApp(async () => {
        const mockEffect = vi.fn()
        
        const hook = defineHook({
          id: 'test:effect-not-fired',
          select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }',
          predicates: [
            { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 1.0 } } // This will be false
          ],
          effect: mockEffect
        })

        const receipt = await evaluateHook(hook)
        expect(receipt.fired).toBe(false)
        expect(mockEffect).not.toHaveBeenCalled()
      })
    })

    it('should handle async effect functions', async () => {
      await runApp(async () => {
        const mockEffect = vi.fn().mockResolvedValue(undefined)
        
        const hook = defineHook({
          id: 'test:async-effect',
          select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }',
          predicates: [
            { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.02 } }
          ],
          effect: mockEffect
        })

        const receipt = await evaluateHook(hook)
        expect(receipt.fired).toBe(true)
        expect(mockEffect).toHaveBeenCalledTimes(1)
      })
    })

    it('should handle effect function errors gracefully', async () => {
      await runApp(async () => {
        const mockEffect = vi.fn().mockRejectedValue(new Error('Effect failed'))
        
        const hook = defineHook({
          id: 'test:effect-error',
          select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }',
          predicates: [
            { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.02 } }
          ],
          effect: mockEffect
        })

        await expect(evaluateHook(hook)).rejects.toThrow('Effect failed')
      })
    })

    it('should provide correct context to effect function', async () => {
      await runApp(async () => {
        let capturedContext = null
        
        const hook = defineHook({
          id: 'test:effect-context',
          select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }',
          predicates: [
            { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.02 } }
          ],
          effect: (ctx) => {
            capturedContext = ctx
          }
        })

        const receipt = await evaluateHook(hook)
        expect(receipt.fired).toBe(true)
        expect(capturedContext).toBeDefined()
        expect(capturedContext.rows).toBeInstanceOf(Array)
        expect(capturedContext.receipt).toBe(receipt)
        expect(capturedContext.store).toBeDefined()
        expect(capturedContext.graph).toBeDefined()
      })
    })
  })

  describe('Complex Combinations', () => {
    it('should handle multiple predicate types with AND', async () => {
      await runApp(async () => {
        const hook = defineHook({
          id: 'test:complex-and',
          select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }',
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
        expect(receipt.fired).toBe(true) // All should be true
      })
    })

    it('should handle multiple predicate types with OR', async () => {
      await runApp(async () => {
        const hook = defineHook({
          id: 'test:complex-or',
          select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }',
          predicates: [
            { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 1.0 } }, // This will be false
            { kind: 'ASK', spec: { query: 'ASK WHERE { ?s a <http://example.org/Service> }' } }, // This will be true
            { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 1.0 } } // This will be false
          ],
          combine: 'OR'
        })

        const receipt = await evaluateHook(hook)
        expect(receipt.predicates).toHaveLength(3)
        expect(receipt.predicates[0].ok).toBe(false)
        expect(receipt.predicates[1].ok).toBe(true)
        expect(receipt.predicates[2].ok).toBe(false)
        expect(receipt.fired).toBe(true) // OR with one true = true
      })
    })
  })
})
