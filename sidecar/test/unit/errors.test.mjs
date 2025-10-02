/**
 * @file Error Classes Unit Tests
 * @description London BDD tests for custom error classes
 */

import { describe, it, expect } from 'vitest'
import {
  ApiError,
  ValidationError,
  NotFoundError,
  HookExecutionError,
  PolicyViolationError,
  EffectTimeoutError,
  LockchainError,
  InternalError
} from '../../server/utils/errors.mjs'

describe('Error Classes (Unit)', () => {
  describe('ApiError', () => {
    it('creates error with message and status code', () => {
      const error = new ApiError('Test error', 400, 'TEST_ERROR')

      expect(error.message).toBe('Test error')
      expect(error.statusCode).toBe(400)
      expect(error.code).toBe('TEST_ERROR')
      expect(error.name).toBe('ApiError')
    })

    it('defaults to API_ERROR code if not provided', () => {
      const error = new ApiError('Test error', 500)

      expect(error.code).toBe('API_ERROR')
    })
  })

  describe('ValidationError', () => {
    it('creates validation error with issues', () => {
      const issues = [
        { path: 'id', message: 'Required', code: 'required' }
      ]
      const error = new ValidationError('Validation failed', issues)

      expect(error.statusCode).toBe(400)
      expect(error.code).toBe('VALIDATION_ERROR')
      expect(error.issues).toEqual(issues)
    })
  })

  describe('NotFoundError', () => {
    it('creates not found error with resource and id', () => {
      const error = new NotFoundError('Hook', 'test-hook-id')

      expect(error.statusCode).toBe(404)
      expect(error.code).toBe('NOT_FOUND')
      expect(error.message).toBe("Hook with ID 'test-hook-id' not found")
    })

    it('creates not found error without id', () => {
      const error = new NotFoundError('Policy')

      expect(error.message).toBe('Policy not found')
    })
  })

  describe('HookExecutionError', () => {
    it('creates hook execution error with id and reason', () => {
      const error = new HookExecutionError('my-hook', 'Predicate failed')

      expect(error.statusCode).toBe(422)
      expect(error.code).toBe('HOOK_EXECUTION_ERROR')
      expect(error.hookId).toBe('my-hook')
      expect(error.message).toContain('my-hook')
      expect(error.message).toContain('Predicate failed')
    })
  })

  describe('PolicyViolationError', () => {
    it('creates policy violation error with violations', () => {
      const violations = [
        { path: 'ex:name', message: 'Required property missing' }
      ]
      const error = new PolicyViolationError('my-policy', violations)

      expect(error.statusCode).toBe(422)
      expect(error.code).toBe('POLICY_VIOLATION')
      expect(error.policyId).toBe('my-policy')
      expect(error.violations).toEqual(violations)
    })
  })

  describe('EffectTimeoutError', () => {
    it('creates effect timeout error with timeout value', () => {
      const error = new EffectTimeoutError('my-effect', 30000)

      expect(error.statusCode).toBe(408)
      expect(error.code).toBe('EFFECT_TIMEOUT')
      expect(error.effectId).toBe('my-effect')
      expect(error.timeout).toBe(30000)
      expect(error.message).toContain('30000ms')
    })
  })

  describe('LockchainError', () => {
    it('creates lockchain error with cause', () => {
      const cause = new Error('Git push failed')
      const error = new LockchainError('Failed to write receipt', cause)

      expect(error.statusCode).toBe(500)
      expect(error.code).toBe('LOCKCHAIN_ERROR')
      expect(error.cause).toBe(cause)
    })
  })

  describe('InternalError', () => {
    it('creates internal error with cause', () => {
      const cause = new Error('Database connection failed')
      const error = new InternalError('Unexpected error occurred', cause)

      expect(error.statusCode).toBe(500)
      expect(error.code).toBe('INTERNAL_ERROR')
      expect(error.cause).toBe(cause)
    })
  })
})
