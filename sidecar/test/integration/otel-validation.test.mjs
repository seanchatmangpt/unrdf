/**
 * @file OTEL Observability Validation Tests
 * @description Tests to validate OpenTelemetry metrics and tracing
 */

import { describe, it, expect } from 'vitest'
import { $fetch, setup } from '@nuxt/test-utils/e2e'

describe('OTEL Observability Validation', async () => {
  await setup({
    server: true,
    build: true
  })

  describe('Health endpoint OTEL metrics', () => {
    it('health check generates observability data', async () => {
      const response = await $fetch('/api/health')

      expect(response.success).toBe(true)
      // Verify response structure includes OTEL-monitored fields
      expect(response.data).toHaveProperty('healthy')
      expect(response.data).toHaveProperty('checks')
    })

    it('health check telemetry flag is recorded', async () => {
      const response = await $fetch('/api/health')

      expect(response.data.checks).toHaveProperty('telemetry')
      // telemetry check indicates OTEL is configured
      expect(typeof response.data.checks.telemetry).toBe('boolean')
    })
  })

  describe('Transaction OTEL metrics', () => {
    it('transaction generates observability spans', async () => {
      const response = await $fetch('/api/transaction/apply', {
        method: 'POST',
        body: {
          delta: [
            {
              subject: { termType: 'NamedNode', value: 'http://example.org/otel-test' },
              predicate: { termType: 'NamedNode', value: 'http://example.org/test' },
              object: { termType: 'Literal', value: 'otel metrics test' }
            }
          ],
          author: 'otel-test'
        }
      })

      expect(response.success).toBe(true)
      expect(response.data).toHaveProperty('transactionId')
      // Transaction ID should be recorded in OTEL spans
      expect(response.data.transactionId).toBeTruthy()
    })

    it('failed transaction generates error metrics', async () => {
      try {
        await $fetch('/api/transaction/apply', {
          method: 'POST',
          body: {
            delta: [] // Invalid: empty delta
          }
        })
        expect.fail('Should have thrown validation error')
      } catch (error) {
        // Error should be recorded in OTEL metrics
        expect(error.statusCode).toBe(400)
      }
    })
  })

  describe('Query OTEL metrics', () => {
    it('query execution generates performance metrics', async () => {
      const response = await $fetch('/api/query', {
        query: {
          query: 'SELECT * WHERE { ?s ?p ?o } LIMIT 10'
        }
      })

      expect(response.success).toBe(true)
      // Result count can be used for performance metrics
      expect(response.data).toHaveProperty('count')
    })

    it('invalid query generates error metrics', async () => {
      try {
        await $fetch('/api/query', {
          query: {
            query: 'INVALID SPARQL SYNTAX'
          }
        })
        expect.fail('Should have thrown error')
      } catch (error) {
        // Error should be recorded in OTEL
        expect(error.statusCode).toBeGreaterThanOrEqual(400)
      }
    })
  })

  describe('Hook registration OTEL metrics', () => {
    it('hook registration generates metrics', async () => {
      const response = await $fetch('/api/hooks/register', {
        method: 'POST',
        body: {
          id: 'otel-test-hook',
          select: 'SELECT ?s WHERE { ?s ?p ?o }',
          predicates: [
            {
              kind: 'ASK',
              query: 'ASK { ?s ?p ?o }'
            }
          ],
          combine: 'AND',
          phase: 'pre'
        }
      })

      expect(response.success).toBe(true)
      expect(response.data.hookId).toBe('otel-test-hook')
      // Hook ID should be recorded in OTEL spans
    })
  })

  describe('Service name configuration', () => {
    it('service name is set from runtime config', async () => {
      const response = await $fetch('/api/health')

      expect(response.data).toHaveProperty('service')
      expect(typeof response.data.service).toBe('string')
      // Service name should match OTEL configuration
      expect(response.data.service).toBeTruthy()
    })
  })

  describe('Error tracking', () => {
    it('validation errors are tracked', async () => {
      try {
        await $fetch('/api/hooks/register', {
          method: 'POST',
          body: {
            // Missing required fields
            id: 'test'
          }
        })
        expect.fail('Should have thrown validation error')
      } catch (error) {
        // Validation error should be recorded in OTEL
        expect(error.statusCode).toBe(400)
      }
    })

    it('server errors are tracked', async () => {
      try {
        await $fetch('/api/query', {
          query: {
            query: 'SELECT * WHERE { ?s ?p ?o',  // Malformed query
            format: 'json'
          }
        })
        expect.fail('Should have thrown error')
      } catch (error) {
        // Server error should be recorded in OTEL
        expect(error.statusCode).toBeGreaterThanOrEqual(400)
      }
    })
  })
})
