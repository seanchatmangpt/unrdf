/**
 * @fileoverview Test custom predicates and front-matter hook loading
 * 
 * Tests the extensibility features and markdown front-matter integration.
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest'
import { initStore } from '../../src/context/index.mjs'
import { useTurtle } from '../../src/composables/use-turtle.mjs'
import { defineHook, evaluateHook, registerPredicate, loadFrontmatterHook } from '../../src/hooks.mjs'
import fs from 'node:fs/promises'
import { tmpdir } from 'node:os'
import { join } from 'node:path'

// Sample RDF data for testing
const sampleTurtle = `
@prefix ex: <http://example.org/> .

ex:service1 a ex:Service ;
  ex:errorRate 0.01 ;
  ex:latency 500 ;
  ex:requests 1000 .

ex:service2 a ex:Service ;
  ex:errorRate 0.03 ;
  ex:latency 1200 ;
  ex:requests 2000 .

ex:service3 a ex:Service ;
  ex:errorRate 0.005 ;
  ex:latency 300 ;
  ex:requests 500 .
`

describe('Custom Predicates and Front-matter', () => {
  let runApp
  let tempFiles = []

  beforeEach(async () => {
    runApp = initStore()
    await runApp(async () => {
      const turtle = await useTurtle()
      await turtle.parse(sampleTurtle)
    })
  })

  afterEach(async () => {
    // Clean up temporary files
    for (const file of tempFiles) {
      try {
        await fs.unlink(file)
      } catch (error) {
        // Ignore cleanup errors
      }
    }
    tempFiles = []
  })

  describe('Custom Predicate Registration', () => {
    it('should register and use custom predicates', async () => {
      await runApp(async () => {
        // Register a custom predicate
        registerPredicate('CUSTOM_COUNT', async (spec, ctx) => {
          const { rows } = ctx
          const count = rows.length
          const threshold = spec.threshold || 0
          return { 
            ok: count >= threshold, 
            meta: { count, threshold, kind: 'CUSTOM_COUNT' } 
          }
        })

        const hook = defineHook({
          id: 'test:custom-count',
          select: 'SELECT ?service WHERE { ?service a <http://example.org/Service> }',
          predicates: [
            { kind: 'CUSTOM_COUNT', spec: { threshold: 2 } }
          ]
        })

        const receipt = await evaluateHook(hook)
        expect(receipt.fired).toBe(true)
        expect(receipt.predicates[0].kind).toBe('CUSTOM_COUNT')
        expect(receipt.predicates[0].ok).toBe(true)
        expect(receipt.predicates[0].meta.count).toBe(3)
        expect(receipt.predicates[0].meta.threshold).toBe(2)
      })
    })

    it('should handle custom predicates with different configurations', async () => {
      await runApp(async () => {
        registerPredicate('CUSTOM_RANGE', async (spec, ctx) => {
          const { rows } = ctx
          const values = rows.map(r => Number(r[spec.var] ?? 0))
          const min = Math.min(...values)
          const max = Math.max(...values)
          const inRange = min >= spec.min && max <= spec.max
          return { 
            ok: inRange, 
            meta: { min, max, range: { min: spec.min, max: spec.max }, kind: 'CUSTOM_RANGE' } 
          }
        })

        const hook = defineHook({
          id: 'test:custom-range',
          select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }',
          predicates: [
            { kind: 'CUSTOM_RANGE', spec: { var: 'errorRate', min: 0, max: 0.1 } }
          ]
        })

        const receipt = await evaluateHook(hook)
        expect(receipt.fired).toBe(true)
        expect(receipt.predicates[0].meta.min).toBeLessThanOrEqual(0.1)
        expect(receipt.predicates[0].meta.max).toBeLessThanOrEqual(0.1)
      })
    })

    it('should handle custom predicates that access store and graph', async () => {
      await runApp(async () => {
        registerPredicate('CUSTOM_STORE_CHECK', async (spec, ctx) => {
          const { store, graph } = ctx
          const storeSize = store.size
          const hasServices = await graph.ask('ASK WHERE { ?s a <http://example.org/Service> }')
          return { 
            ok: storeSize > 0 && hasServices, 
            meta: { storeSize, hasServices, kind: 'CUSTOM_STORE_CHECK' } 
          }
        })

        const hook = defineHook({
          id: 'test:custom-store',
          select: 'SELECT ?service WHERE { ?service a <http://example.org/Service> }',
          predicates: [
            { kind: 'CUSTOM_STORE_CHECK', spec: {} }
          ]
        })

        const receipt = await evaluateHook(hook)
        expect(receipt.fired).toBe(true)
        expect(receipt.predicates[0].meta.storeSize).toBeGreaterThan(0)
        expect(receipt.predicates[0].meta.hasServices).toBe(true)
      })
    })

    it('should handle custom predicate errors gracefully', async () => {
      await runApp(async () => {
        registerPredicate('CUSTOM_ERROR', async (spec, ctx) => {
          throw new Error('Custom predicate error')
        })

        const hook = defineHook({
          id: 'test:custom-error',
          select: 'SELECT ?service WHERE { ?service a <http://example.org/Service> }',
          predicates: [
            { kind: 'CUSTOM_ERROR', spec: {} }
          ]
        })

        await expect(evaluateHook(hook)).rejects.toThrow('Custom predicate error')
      })
    })
  })

  describe('Front-matter Hook Loading', () => {
    it('should load hooks from markdown files', async () => {
      const markdownContent = `---
hook:
  id: 'ex:HealthMonitor'
  name: 'Service Health Monitor'
  description: 'Monitors service error rates and latency thresholds'
  select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }'
  predicates:
    - kind: 'THRESHOLD'
      spec:
        var: 'errorRate'
        op: '>'
        value: 0.02
  combine: 'OR'
---

# Service Health Monitor

This hook monitors service error rates and alerts when they exceed 2%.

## Configuration

- **Threshold**: 2% error rate
- **Action**: Alert operations team
- **Schedule**: Every 5 minutes
`

      const tempFile = join(tmpdir(), `test-hook-${Date.now()}.md`)
      await fs.writeFile(tempFile, markdownContent)
      tempFiles.push(tempFile)

      const hook = await loadFrontmatterHook(tempFile)
      expect(hook.id).toBe('ex:HealthMonitor')
      expect(hook.select).toContain('SELECT ?service ?errorRate')
      expect(hook.predicates).toHaveLength(1)
      expect(hook.predicates[0].kind).toBe('THRESHOLD')
      expect(hook.predicates[0].spec.var).toBe('errorRate')
      expect(hook.predicates[0].spec.op).toBe('>')
      expect(hook.predicates[0].spec.value).toBe(0.02)
      expect(hook.combine).toBe('OR')
    })

    it('should evaluate loaded hooks correctly', async () => {
      await runApp(async () => {
        const markdownContent = `---
hook:
  id: 'ex:TestHook'
  select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }'
  predicates:
    - kind: 'THRESHOLD'
      spec:
        var: 'errorRate'
        op: '>'
        value: 0.02
  combine: 'OR'
---

# Test Hook

This is a test hook.
`

        const tempFile = join(tmpdir(), `test-hook-eval-${Date.now()}.md`)
        await fs.writeFile(tempFile, markdownContent)
        tempFiles.push(tempFile)

        const hook = await loadFrontmatterHook(tempFile)
        const receipt = await evaluateHook(hook)
        
        expect(receipt.id).toBe('ex:TestHook')
        expect(receipt.fired).toBe(true) // Should fire because 0.03 > 0.02
        expect(receipt.predicates[0].kind).toBe('THRESHOLD')
        expect(receipt.predicates[0].ok).toBe(true)
      })
    })

    it('should handle hooks with multiple predicates', async () => {
      const markdownContent = `---
hook:
  id: 'ex:MultiPredicateHook'
  select: 'SELECT ?service ?errorRate ?latency WHERE { ?service <http://example.org/errorRate> ?errorRate ; <http://example.org/latency> ?latency }'
  predicates:
    - kind: 'THRESHOLD'
      spec:
        var: 'errorRate'
        op: '>'
        value: 0.01
    - kind: 'THRESHOLD'
      spec:
        var: 'latency'
        op: '>'
        value: 400
    - kind: 'ASK'
      spec:
        query: 'ASK WHERE { ?s a <http://example.org/Service> }'
  combine: 'AND'
---

# Multi-Predicate Hook

This hook has multiple predicates combined with AND logic.
`

      const tempFile = join(tmpdir(), `test-multi-hook-${Date.now()}.md`)
      await fs.writeFile(tempFile, markdownContent)
      tempFiles.push(tempFile)

      const hook = await loadFrontmatterHook(tempFile)
      expect(hook.predicates).toHaveLength(3)
      expect(hook.predicates[0].kind).toBe('THRESHOLD')
      expect(hook.predicates[1].kind).toBe('THRESHOLD')
      expect(hook.predicates[2].kind).toBe('ASK')
      expect(hook.combine).toBe('AND')
    })

    it('should handle hooks with effect functions', async () => {
      const markdownContent = `---
hook:
  id: 'ex:EffectHook'
  select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }'
  predicates:
    - kind: 'THRESHOLD'
      spec:
        var: 'errorRate'
        op: '>'
        value: 0.02
  combine: 'OR'
  effect: |
    async ({ rows, receipt, store, graph }) => {
      console.log('Effect executed for', rows.length, 'rows');
    }
---

# Effect Hook

This hook has an effect function that executes when it fires.
`

      const tempFile = join(tmpdir(), `test-effect-hook-${Date.now()}.md`)
      await fs.writeFile(tempFile, markdownContent)
      tempFiles.push(tempFile)

      const hook = await loadFrontmatterHook(tempFile)
      expect(hook.effect).toBeDefined()
      expect(typeof hook.effect).toBe('string') // YAML loads it as a string, not a function
    })

    it('should throw error for missing hook.id', async () => {
      const markdownContent = `---
hook:
  name: 'Invalid Hook'
  select: 'SELECT ?service WHERE { ?service a <http://example.org/Service> }'
---

# Invalid Hook

This hook is missing the required id field.
`

      const tempFile = join(tmpdir(), `test-invalid-hook-${Date.now()}.md`)
      await fs.writeFile(tempFile, markdownContent)
      tempFiles.push(tempFile)

      await expect(loadFrontmatterHook(tempFile)).rejects.toThrow('Missing hook.id')
    })

    it('should handle malformed YAML gracefully', async () => {
      const markdownContent = `---
hook:
  id: 'ex:MalformedHook'
  select: 'SELECT ?service WHERE { ?service a <http://example.org/Service> }'
  predicates:
    - kind: 'THRESHOLD'
      spec:
        var: 'errorRate'
        op: '>'
        value: 0.02
  # Missing closing bracket
---

# Malformed Hook

This hook has malformed YAML.
`

      const tempFile = join(tmpdir(), `test-malformed-hook-${Date.now()}.md`)
      await fs.writeFile(tempFile, markdownContent)
      tempFiles.push(tempFile)

      // This should either work or throw a YAML parsing error
      try {
        const hook = await loadFrontmatterHook(tempFile)
        expect(hook.id).toBe('ex:MalformedHook')
      } catch (error) {
        expect(error.message).toContain('YAML')
      }
    })
  })

  describe('Integration Tests', () => {
    it('should combine custom predicates with built-in predicates', async () => {
      await runApp(async () => {
        registerPredicate('CUSTOM_VALIDATION', async (spec, ctx) => {
          const { rows } = ctx
          const validCount = rows.filter(r => {
            const value = Number(r[spec.var] ?? 0)
            return value >= spec.min && value <= spec.max
          }).length
          return { 
            ok: validCount >= spec.minValid, 
            meta: { validCount, minValid: spec.minValid, kind: 'CUSTOM_VALIDATION' } 
          }
        })

        const hook = defineHook({
          id: 'test:integrated',
          select: 'SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }',
          predicates: [
            { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.001 } },
            { kind: 'CUSTOM_VALIDATION', spec: { var: 'errorRate', min: 0, max: 0.1, minValid: 2 } },
            { kind: 'ASK', spec: { query: 'ASK WHERE { ?s a <http://example.org/Service> }' } }
          ],
          combine: 'AND'
        })

        const receipt = await evaluateHook(hook)
        expect(receipt.fired).toBe(true)
        expect(receipt.predicates).toHaveLength(3)
        expect(receipt.predicates[0].kind).toBe('THRESHOLD')
        expect(receipt.predicates[1].kind).toBe('CUSTOM_VALIDATION')
        expect(receipt.predicates[2].kind).toBe('ASK')
      })
    })
  })
})
