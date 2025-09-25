/**
 * @fileoverview Test basic Knowledge Hooks predicates (ASK, THRESHOLD, DELTA)
 * 
 * Tests the core predicate types that form the foundation of the hooks system.
 */

import { describe, it, expect, beforeEach } from 'vitest'
import { initStore } from '../../src/context/index.mjs'
import { useTurtle } from '../../src/composables/use-turtle.mjs'
import { defineHook, evaluateHook, planHook } from '../../src/hooks.mjs'

// Sample RDF data for testing
const sampleTurtle = `
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:service1 a ex:Service ;
  ex:errorRate 0.01 ;
  ex:latency 500 ;
  ex:status ex:active ;
  ex:requests 1000 .

ex:service2 a ex:Service ;
  ex:errorRate 0.03 ;
  ex:latency 1200 ;
  ex:status ex:active ;
  ex:requests 2000 .

ex:service3 a ex:Service ;
  ex:errorRate 0.005 ;
  ex:latency 300 ;
  ex:status ex:maintenance ;
  ex:requests 500 .

ex:person1 a foaf:Person ;
  foaf:name "Alice" ;
  foaf:age 30 .

ex:person2 a foaf:Person ;
  foaf:name "Bob" ;
  foaf:age 25 .
`

describe('Basic Knowledge Hooks Predicates', () => {
  let runApp

  beforeEach(async () => {
    runApp = initStore()
    await runApp(async () => {
      const turtle = await useTurtle()
      await turtle.parse(sampleTurtle)
    })
  })

  describe('ASK Predicate', () => {
    it('should evaluate ASK queries correctly', async () => {
      await runApp(async () => {
        const hook = defineHook({
          id: 'test:ask',
          ask: 'ASK WHERE { ?s a <http://example.org/Service> }',
          predicates: [
            { kind: 'ASK', spec: { query: 'ASK WHERE { ?s a <http://example.org/Service> }' } }
          ]
        })

        const plan = planHook(hook)
        expect(plan.queryPlan).toBe('ASK')
        expect(plan.predicatePlan).toHaveLength(1)
        expect(plan.predicatePlan[0].kind).toBe('ASK')

        const receipt = await evaluateHook(hook)
        expect(receipt.fired).toBe(true)
        expect(receipt.predicates).toHaveLength(1)
        expect(receipt.predicates[0].kind).toBe('ASK')
        expect(receipt.predicates[0].ok).toBe(true)
        expect(receipt.predicates[0].meta.kind).toBe('ASK')
      })
    })

    it('should handle false ASK queries', async () => {
      await runApp(async () => {
        const hook = defineHook({
          id: 'test:ask-false',
          ask: 'ASK WHERE { ?s a <http://example.org/NonExistent> }',
          predicates: [
            { kind: 'ASK', spec: { query: 'ASK WHERE { ?s a <http://example.org/NonExistent> }' } }
          ]
        })

        const receipt = await evaluateHook(hook)
        expect(receipt.fired).toBe(false)
        expect(receipt.predicates[0].ok).toBe(false)
      })
    })
  })

  describe('THRESHOLD Predicate', () => {
    it('should evaluate numeric thresholds correctly', async () => {
      await runApp(async () => {
        const hook = defineHook({
          id: 'test:threshold',
          select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }',
          predicates: [
            { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.02 } }
          ]
        })

        const receipt = await evaluateHook(hook)
        expect(receipt.fired).toBe(true)
        expect(receipt.predicates[0].kind).toBe('THRESHOLD')
        expect(receipt.predicates[0].ok).toBe(true)
        expect(receipt.predicates[0].meta.matched).toBeGreaterThan(0)
      })
    })

    it('should handle different comparison operators', async () => {
      await runApp(async () => {
        const operators = ['>', '>=', '<', '<=', '==', '!=']
        
        for (const op of operators) {
          const hook = defineHook({
            id: `test:threshold-${op}`,
            select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }',
            predicates: [
              { kind: 'THRESHOLD', spec: { var: 'errorRate', op, value: 0.01 } }
            ]
          })

          const receipt = await evaluateHook(hook)
          expect(receipt.predicates[0].kind).toBe('THRESHOLD')
          expect(typeof receipt.predicates[0].ok).toBe('boolean')
        }
      })
    })

    it('should handle missing variables gracefully', async () => {
      await runApp(async () => {
        const hook = defineHook({
          id: 'test:threshold-missing',
          select: 'SELECT ?service WHERE { ?service a <http://example.org/Service> }',
          predicates: [
            { kind: 'THRESHOLD', spec: { var: 'nonexistent', op: '>', value: 0 } }
          ]
        })

        const receipt = await evaluateHook(hook)
        expect(receipt.predicates[0].ok).toBe(false)
      })
    })
  })

  describe('DELTA Predicate', () => {
    it('should detect changes in key variables', async () => {
      await runApp(async () => {
        const hook = defineHook({
          id: 'test:delta',
          select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }',
          predicates: [
            { kind: 'DELTA', spec: { key: ['service', 'errorRate'], prev: [] } }
          ]
        })

        const receipt = await evaluateHook(hook)
        expect(receipt.fired).toBe(true)
        expect(receipt.predicates[0].kind).toBe('DELTA')
        expect(receipt.predicates[0].ok).toBe(true)
        expect(receipt.predicates[0].meta.current).toBeDefined()
        expect(receipt.predicates[0].meta.previous).toEqual([])
      })
    })

    it('should detect no change when data is identical', async () => {
      await runApp(async () => {
        // First evaluation to establish baseline
        const hook1 = defineHook({
          id: 'test:delta-baseline',
          select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }',
          predicates: [
            { kind: 'DELTA', spec: { key: ['service', 'errorRate'], prev: [] } }
          ]
        })

        const receipt1 = await evaluateHook(hook1)
        const currentHash = receipt1.predicates[0].meta.current

        // Second evaluation with same data
        const hook2 = defineHook({
          id: 'test:delta-same',
          select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }',
          predicates: [
            { kind: 'DELTA', spec: { key: ['service', 'errorRate'], prev: currentHash } }
          ]
        })

        const receipt2 = await evaluateHook(hook2)
        expect(receipt2.fired).toBe(false)
        expect(receipt2.predicates[0].ok).toBe(false)
      })
    })

    it('should generate stable hashes for consistent data', async () => {
      await runApp(async () => {
        const hook = defineHook({
          id: 'test:delta-stable',
          select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }',
          predicates: [
            { kind: 'DELTA', spec: { key: ['service', 'errorRate'], prev: [] } }
          ]
        })

        const receipt1 = await evaluateHook(hook)
        const receipt2 = await evaluateHook(hook)

        expect(receipt1.predicates[0].meta.current).toEqual(receipt2.predicates[0].meta.current)
      })
    })
  })

  describe('Hook Planning', () => {
    it('should generate correct execution plans', async () => {
      const hook = defineHook({
        id: 'test:planning',
        select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }',
        predicates: [
          { kind: 'ASK', spec: { query: 'ASK WHERE { ?s a <http://example.org/Service> }' } },
          { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.02 } },
          { kind: 'DELTA', spec: { key: ['service'], prev: [] } }
        ],
        combine: 'OR'
      })

      const plan = planHook(hook)
      expect(plan.id).toBe('test:planning')
      expect(plan.queryPlan).toBe('SELECT')
      expect(plan.predicatePlan).toHaveLength(3)
      expect(plan.combine).toBe('OR')
      expect(plan.predicatePlan[0].kind).toBe('ASK')
      expect(plan.predicatePlan[1].kind).toBe('THRESHOLD')
      expect(plan.predicatePlan[2].kind).toBe('DELTA')
    })
  })
})
