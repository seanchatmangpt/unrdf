/**
 * @file Query Endpoint Integration Tests
 * @description London BDD tests for SPARQL query API route
 */

import { describe, it, expect } from 'vitest'
import { $fetch, setup } from '@nuxt/test-utils/e2e'

describe('GET /api/query (Integration)', async () => {
  await setup({
    server: true,
    build: true
  })

  it('executes valid SPARQL SELECT query', async () => {
    const response = await $fetch('/api/query', {
      query: {
        query: 'SELECT * WHERE { ?s ?p ?o } LIMIT 10'
      }
    })

    expect(response.success).toBe(true)
    expect(response.data).toHaveProperty('results')
    expect(response.data).toHaveProperty('format')
    expect(response.data.format).toBe('json')
  })

  it('returns count for array results', async () => {
    const response = await $fetch('/api/query', {
      query: {
        query: 'SELECT * WHERE { ?s ?p ?o } LIMIT 5'
      }
    })

    expect(response.success).toBe(true)
    expect(response.data).toHaveProperty('count')
    expect(Array.isArray(response.data.results)).toBe(true)
  })

  it('rejects query without required parameter', async () => {
    try {
      await $fetch('/api/query', {
        query: {}
      })
      expect.fail('Should have thrown validation error')
    } catch (error) {
      expect(error.statusCode).toBe(400)
    }
  })

  it('rejects invalid SPARQL syntax', async () => {
    try {
      await $fetch('/api/query', {
        query: {
          query: 'INVALID SPARQL'
        }
      })
      expect.fail('Should have thrown error')
    } catch (error) {
      expect(error.statusCode).toBeGreaterThanOrEqual(400)
    }
  })

  it('supports format parameter', async () => {
    const response = await $fetch('/api/query', {
      query: {
        query: 'SELECT * WHERE { ?s ?p ?o } LIMIT 1',
        format: 'turtle'
      }
    })

    expect(response.success).toBe(true)
    expect(response.data.format).toBe('turtle')
  })

  it('handles empty result sets', async () => {
    const response = await $fetch('/api/query', {
      query: {
        query: 'SELECT * WHERE { <urn:test:nonexistent> ?p ?o }'
      }
    })

    expect(response.success).toBe(true)
    expect(response.data.results).toBeDefined()
  })

  it('rejects unsupported format', async () => {
    try {
      await $fetch('/api/query', {
        query: {
          query: 'SELECT * WHERE { ?s ?p ?o }',
          format: 'xml'
        }
      })
      expect.fail('Should have thrown validation error')
    } catch (error) {
      expect(error.statusCode).toBe(400)
    }
  })
})
