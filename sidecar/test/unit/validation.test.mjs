/**
 * @file Validation Schema Unit Tests
 * @description London BDD tests for Zod validation schemas
 */

import { describe, it, expect } from 'vitest'
import {
  registerHookSchema,
  applyTransactionSchema,
  registerPolicySchema,
  registerEffectSchema,
  initLockchainSchema,
  registerAgentSchema,
  querySchema
} from '../../server/utils/validation.mjs'

describe('Validation Schemas (Unit)', () => {
  describe('registerHookSchema', () => {
    it('validates valid hook registration', () => {
      const validHook = {
        id: 'test-hook',
        select: 'SELECT ?s ?p ?o WHERE { ?s ?p ?o }',
        predicates: [
          { kind: 'ASK', query: 'ASK { ?s a :Person }' }
        ],
        combine: 'AND',
        phase: 'pre'
      }

      const result = registerHookSchema.safeParse(validHook)
      expect(result.success).toBe(true)
    })

    it('rejects hook without id', () => {
      const invalidHook = {
        select: 'SELECT ?s ?p ?o WHERE { ?s ?p ?o }',
        predicates: [{ kind: 'ASK', query: 'ASK { ?s a :Person }' }],
        combine: 'AND',
        phase: 'pre'
      }

      const result = registerHookSchema.safeParse(invalidHook)
      expect(result.success).toBe(false)
      expect(result.error.errors[0].path).toEqual(['id'])
    })

    it('rejects hook with empty predicates array', () => {
      const invalidHook = {
        id: 'test-hook',
        select: 'SELECT ?s ?p ?o WHERE { ?s ?p ?o }',
        predicates: [],
        combine: 'AND',
        phase: 'pre'
      }

      const result = registerHookSchema.safeParse(invalidHook)
      expect(result.success).toBe(false)
    })

    it('validates THRESHOLD predicate', () => {
      const validHook = {
        id: 'threshold-hook',
        select: 'SELECT ?count WHERE { }',
        predicates: [
          {
            kind: 'THRESHOLD',
            variable: 'count',
            operator: '>',
            value: 100
          }
        ],
        combine: 'AND',
        phase: 'pre'
      }

      const result = registerHookSchema.safeParse(validHook)
      expect(result.success).toBe(true)
    })
  })

  describe('applyTransactionSchema', () => {
    it('validates valid transaction', () => {
      const validTransaction = {
        delta: [
          {
            subject: { termType: 'NamedNode', value: 'http://example.org/s' },
            predicate: { termType: 'NamedNode', value: 'http://example.org/p' },
            object: { termType: 'Literal', value: 'test', datatype: { value: 'http://www.w3.org/2001/XMLSchema#string' } }
          }
        ],
        author: 'test-author',
        metadata: { source: 'test' }
      }

      const result = applyTransactionSchema.safeParse(validTransaction)
      expect(result.success).toBe(true)
    })

    it('rejects transaction with empty delta', () => {
      const invalidTransaction = {
        delta: [],
        author: 'test-author'
      }

      const result = applyTransactionSchema.safeParse(invalidTransaction)
      expect(result.success).toBe(false)
    })
  })

  describe('registerPolicySchema', () => {
    it('validates valid policy registration', () => {
      const validPolicy = {
        id: 'test-policy',
        shapes: '@prefix sh: <http://www.w3.org/ns/shacl#> .',
        priority: 10
      }

      const result = registerPolicySchema.safeParse(validPolicy)
      expect(result.success).toBe(true)
    })

    it('accepts optional priority', () => {
      const validPolicy = {
        id: 'test-policy',
        shapes: '@prefix sh: <http://www.w3.org/ns/shacl#> .'
      }

      const result = registerPolicySchema.safeParse(validPolicy)
      expect(result.success).toBe(true)
    })
  })

  describe('registerEffectSchema', () => {
    it('validates valid effect registration', () => {
      const validEffect = {
        id: 'test-effect',
        code: 'console.log("test")',
        timeout: 5000,
        memoryLimit: 1024 * 1024
      }

      const result = registerEffectSchema.safeParse(validEffect)
      expect(result.success).toBe(true)
    })

    it('rejects negative timeout', () => {
      const invalidEffect = {
        id: 'test-effect',
        code: 'console.log("test")',
        timeout: -1000
      }

      const result = registerEffectSchema.safeParse(invalidEffect)
      expect(result.success).toBe(false)
    })
  })

  describe('initLockchainSchema', () => {
    it('validates valid lockchain initialization', () => {
      const validInit = {
        repoUrl: 'http://gitea:3000/kgc/lockchain.git',
        branch: 'main',
        credentials: { username: 'test', password: 'test' }
      }

      const result = initLockchainSchema.safeParse(validInit)
      expect(result.success).toBe(true)
    })

    it('rejects invalid URL', () => {
      const invalidInit = {
        repoUrl: 'not-a-url',
        branch: 'main'
      }

      const result = initLockchainSchema.safeParse(invalidInit)
      expect(result.success).toBe(false)
    })
  })

  describe('querySchema', () => {
    it('validates valid SPARQL query', () => {
      const validQuery = {
        query: 'SELECT * WHERE { ?s ?p ?o }',
        format: 'json'
      }

      const result = querySchema.safeParse(validQuery)
      expect(result.success).toBe(true)
    })

    it('accepts optional format', () => {
      const validQuery = {
        query: 'SELECT * WHERE { ?s ?p ?o }'
      }

      const result = querySchema.safeParse(validQuery)
      expect(result.success).toBe(true)
    })

    it('rejects unsupported format', () => {
      const invalidQuery = {
        query: 'SELECT * WHERE { ?s ?p ?o }',
        format: 'xml'
      }

      const result = querySchema.safeParse(invalidQuery)
      expect(result.success).toBe(false)
    })
  })
})
