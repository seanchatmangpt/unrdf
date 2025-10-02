/**
 * @file Response Builders Unit Tests
 * @description London BDD tests for response formatting utilities
 */

import { describe, it, expect, vi } from 'vitest'
import { sendSuccess, sendError, sendValidationError } from '../../server/utils/response.mjs'
import { ValidationError, HookExecutionError } from '../../server/utils/errors.mjs'
import { z } from 'zod'

describe('Response Builders (Unit)', () => {
  describe('sendSuccess', () => {
    it('formats success response with data', () => {
      const mockEvent = {
        node: { res: { statusCode: 200 } }
      }

      const result = sendSuccess(mockEvent, { id: 'test-123' })

      expect(mockEvent.node.res.statusCode).toBe(200)
      expect(result).toEqual({
        success: true,
        data: { id: 'test-123' }
      })
    })

    it('sets custom status code', () => {
      const mockEvent = {
        node: { res: { statusCode: 200 } }
      }

      const result = sendSuccess(mockEvent, { created: true }, 201)

      expect(mockEvent.node.res.statusCode).toBe(201)
      expect(result.success).toBe(true)
    })
  })

  describe('sendError', () => {
    it('formats error response with code and message', () => {
      const mockEvent = {
        node: { res: { statusCode: 200 } }
      }
      const error = new ValidationError('Validation failed', [])

      const result = sendError(mockEvent, error)

      expect(mockEvent.node.res.statusCode).toBe(400)
      expect(result).toEqual({
        success: false,
        error: {
          code: 'VALIDATION_ERROR',
          message: 'Validation failed',
          issues: []
        }
      })
    })

    it('includes hook id for hook execution errors', () => {
      const mockEvent = {
        node: { res: { statusCode: 200 } }
      }
      const error = new HookExecutionError('test-hook', 'Failed')

      const result = sendError(mockEvent, error)

      expect(result.error.hookId).toBe('test-hook')
    })

    it('defaults to 500 for unknown errors', () => {
      const mockEvent = {
        node: { res: { statusCode: 200 } }
      }
      const error = new Error('Unknown error')

      const result = sendError(mockEvent, error)

      expect(mockEvent.node.res.statusCode).toBe(500)
      expect(result.error.code).toBe('INTERNAL_ERROR')
    })
  })

  describe('sendValidationError', () => {
    it('formats Zod validation errors', () => {
      const mockEvent = {
        node: { res: { statusCode: 200 } }
      }

      const schema = z.object({
        id: z.string().min(1),
        count: z.number().positive()
      })

      const validation = schema.safeParse({ id: '', count: -1 })

      const result = sendValidationError(mockEvent, validation.error)

      expect(mockEvent.node.res.statusCode).toBe(400)
      expect(result.success).toBe(false)
      expect(result.error.code).toBe('VALIDATION_ERROR')
      expect(result.error.issues).toHaveLength(2)
      expect(result.error.issues[0]).toHaveProperty('path')
      expect(result.error.issues[0]).toHaveProperty('message')
    })
  })
})
