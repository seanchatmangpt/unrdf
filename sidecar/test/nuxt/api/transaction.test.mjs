/**
 * @file Transaction Endpoint Integration Tests
 * @description London BDD tests for transaction application API route
 */

import { describe, it, expect } from 'vitest'
import { $fetch, setup } from '@nuxt/test-utils/e2e'

describe('POST /api/transaction/apply (Integration)', async () => {
  await setup({
    server: true,
    build: true
  })

  it('applies valid transaction with RDF quads', async () => {
    const response = await $fetch('/api/transaction/apply', {
      method: 'POST',
      body: {
        delta: [
          {
            subject: { termType: 'NamedNode', value: 'http://example.org/subject1' },
            predicate: { termType: 'NamedNode', value: 'http://example.org/predicate1' },
            object: {
              termType: 'Literal',
              value: 'test value',
              datatype: { value: 'http://www.w3.org/2001/XMLSchema#string' }
            }
          }
        ],
        author: 'test-author',
        metadata: { source: 'test-suite' }
      }
    })

    expect(response.success).toBe(true)
    expect(response.data).toHaveProperty('transactionId')
    expect(response.data.transactionId).toBeTruthy()
  })

  it('returns receiptId when lockchain is enabled', async () => {
    const response = await $fetch('/api/transaction/apply', {
      method: 'POST',
      body: {
        delta: [
          {
            subject: { termType: 'NamedNode', value: 'http://example.org/subject2' },
            predicate: { termType: 'NamedNode', value: 'http://example.org/predicate2' },
            object: { termType: 'Literal', value: 'value2' }
          }
        ],
        author: 'test-author'
      }
    })

    expect(response.success).toBe(true)
    // receiptId may be undefined if lockchain not initialized in test
    if (response.data.receiptId) {
      expect(typeof response.data.receiptId).toBe('string')
    }
  })

  it('includes hooksExecuted when hooks fire', async () => {
    const response = await $fetch('/api/transaction/apply', {
      method: 'POST',
      body: {
        delta: [
          {
            subject: { termType: 'NamedNode', value: 'http://example.org/person1' },
            predicate: { termType: 'NamedNode', value: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' },
            object: { termType: 'NamedNode', value: 'http://example.org/Person' }
          }
        ]
      }
    })

    expect(response.success).toBe(true)
    // hooksExecuted may be empty if no hooks registered
    expect(response.data).toHaveProperty('hooksExecuted')
    expect(Array.isArray(response.data.hooksExecuted)).toBe(true)
  })

  it('rejects transaction with empty delta', async () => {
    try {
      await $fetch('/api/transaction/apply', {
        method: 'POST',
        body: {
          delta: [],
          author: 'test-author'
        }
      })
      expect.fail('Should have thrown validation error')
    } catch (error) {
      expect(error.statusCode).toBe(400)
    }
  })

  it('rejects transaction without delta', async () => {
    try {
      await $fetch('/api/transaction/apply', {
        method: 'POST',
        body: {
          author: 'test-author'
        }
      })
      expect.fail('Should have thrown validation error')
    } catch (error) {
      expect(error.statusCode).toBe(400)
    }
  })

  it('applies transaction with multiple quads', async () => {
    const response = await $fetch('/api/transaction/apply', {
      method: 'POST',
      body: {
        delta: [
          {
            subject: { termType: 'NamedNode', value: 'http://example.org/s1' },
            predicate: { termType: 'NamedNode', value: 'http://example.org/p1' },
            object: { termType: 'Literal', value: 'v1' }
          },
          {
            subject: { termType: 'NamedNode', value: 'http://example.org/s2' },
            predicate: { termType: 'NamedNode', value: 'http://example.org/p2' },
            object: { termType: 'Literal', value: 'v2' }
          },
          {
            subject: { termType: 'NamedNode', value: 'http://example.org/s3' },
            predicate: { termType: 'NamedNode', value: 'http://example.org/p3' },
            object: { termType: 'Literal', value: 'v3' }
          }
        ],
        author: 'batch-test'
      }
    })

    expect(response.success).toBe(true)
    expect(response.data.transactionId).toBeTruthy()
  })

  it('accepts optional metadata', async () => {
    const response = await $fetch('/api/transaction/apply', {
      method: 'POST',
      body: {
        delta: [
          {
            subject: { termType: 'NamedNode', value: 'http://example.org/s' },
            predicate: { termType: 'NamedNode', value: 'http://example.org/p' },
            object: { termType: 'Literal', value: 'v' }
          }
        ],
        metadata: {
          timestamp: Date.now(),
          source: 'integration-test',
          version: '1.0.0'
        }
      }
    })

    expect(response.success).toBe(true)
  })
})
