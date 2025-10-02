/**
 * @file Hooks Endpoint Integration Tests
 * @description London BDD tests for hook registration API route
 */

import { describe, it, expect } from 'vitest'
import { $fetch, setup } from '@nuxt/test-utils/e2e'

describe('POST /api/hooks/register (Integration)', async () => {
  await setup({
    server: true,
    build: true
  })

  it('registers hook with ASK predicate', async () => {
    const response = await $fetch('/api/hooks/register', {
      method: 'POST',
      body: {
        id: 'test-ask-hook',
        select: 'SELECT ?s ?p ?o WHERE { ?s ?p ?o }',
        predicates: [
          {
            kind: 'ASK',
            query: 'ASK { ?s a <http://example.org/Person> }'
          }
        ],
        combine: 'AND',
        phase: 'pre'
      }
    })

    expect(response.success).toBe(true)
    expect(response.data).toHaveProperty('hookId')
    expect(response.data.hookId).toBe('test-ask-hook')
  })

  it('registers hook with THRESHOLD predicate', async () => {
    const response = await $fetch('/api/hooks/register', {
      method: 'POST',
      body: {
        id: 'test-threshold-hook',
        select: 'SELECT (COUNT(?s) as ?count) WHERE { ?s ?p ?o }',
        predicates: [
          {
            kind: 'THRESHOLD',
            variable: 'count',
            operator: '>',
            value: 10
          }
        ],
        combine: 'AND',
        phase: 'post'
      }
    })

    expect(response.success).toBe(true)
    expect(response.data.hookId).toBe('test-threshold-hook')
  })

  it('registers hook with COUNT predicate', async () => {
    const response = await $fetch('/api/hooks/register', {
      method: 'POST',
      body: {
        id: 'test-count-hook',
        select: 'SELECT ?s WHERE { ?s a <http://example.org/Person> }',
        predicates: [
          {
            kind: 'COUNT',
            countVariable: 's'
          }
        ],
        combine: 'AND',
        phase: 'pre'
      }
    })

    expect(response.success).toBe(true)
    expect(response.data.hookId).toBe('test-count-hook')
  })

  it('registers hook with multiple predicates and OR combine', async () => {
    const response = await $fetch('/api/hooks/register', {
      method: 'POST',
      body: {
        id: 'test-multi-hook',
        select: 'SELECT ?s ?count WHERE { ?s ?p ?o }',
        predicates: [
          {
            kind: 'ASK',
            query: 'ASK { ?s a <http://example.org/Person> }'
          },
          {
            kind: 'THRESHOLD',
            variable: 'count',
            operator: '>=',
            value: 5
          }
        ],
        combine: 'OR',
        phase: 'pre'
      }
    })

    expect(response.success).toBe(true)
    expect(response.data.hookId).toBe('test-multi-hook')
  })

  it('rejects hook without required id', async () => {
    try {
      await $fetch('/api/hooks/register', {
        method: 'POST',
        body: {
          select: 'SELECT ?s WHERE { ?s ?p ?o }',
          predicates: [{ kind: 'ASK', query: 'ASK { ?s ?p ?o }' }],
          combine: 'AND',
          phase: 'pre'
        }
      })
      expect.fail('Should have thrown validation error')
    } catch (error) {
      expect(error.statusCode).toBe(400)
    }
  })

  it('rejects hook with empty predicates array', async () => {
    try {
      await $fetch('/api/hooks/register', {
        method: 'POST',
        body: {
          id: 'test-hook',
          select: 'SELECT ?s WHERE { ?s ?p ?o }',
          predicates: [],
          combine: 'AND',
          phase: 'pre'
        }
      })
      expect.fail('Should have thrown validation error')
    } catch (error) {
      expect(error.statusCode).toBe(400)
    }
  })

  it('rejects hook with invalid phase', async () => {
    try {
      await $fetch('/api/hooks/register', {
        method: 'POST',
        body: {
          id: 'test-hook',
          select: 'SELECT ?s WHERE { ?s ?p ?o }',
          predicates: [{ kind: 'ASK', query: 'ASK { ?s ?p ?o }' }],
          combine: 'AND',
          phase: 'invalid'
        }
      })
      expect.fail('Should have thrown validation error')
    } catch (error) {
      expect(error.statusCode).toBe(400)
    }
  })

  it('rejects THRESHOLD predicate without required fields', async () => {
    try {
      await $fetch('/api/hooks/register', {
        method: 'POST',
        body: {
          id: 'test-hook',
          select: 'SELECT ?count WHERE { }',
          predicates: [
            {
              kind: 'THRESHOLD',
              variable: 'count'
              // Missing operator and value
            }
          ],
          combine: 'AND',
          phase: 'pre'
        }
      })
      expect.fail('Should have thrown validation error')
    } catch (error) {
      expect(error.statusCode).toBe(400)
    }
  })
})
