/**
 * @file Update Hook Endpoint
 * @description PUT /api/hooks/:id - Update an existing hook
 */

import { defineEventHandler, getRouterParam, readBody } from '#imports'
import { getManagers } from '../../utils/managers.mjs'
import { sendSuccess, sendValidationError, asyncHandler } from '../../utils/response.mjs'
import { registerHookSchema } from '../../utils/validation.mjs'
import { NotFoundError, ValidationError } from '../../utils/errors.mjs'

/**
 * Update hook handler
 * @param {Object} event - H3 event
 * @returns {Promise<Object>} Updated hook
 */
export default defineEventHandler(asyncHandler(async (event) => {
  const hookId = getRouterParam(event, 'id')

  if (!hookId) {
    throw new NotFoundError('Hook ID is required')
  }

  const body = await readBody(event)

  // Validate request
  const validation = registerHookSchema.safeParse(body)
  if (!validation.success) {
    return sendValidationError(event, validation.error)
  }

  const { hookManager } = getManagers()

  // Check if hook exists
  const existingHook = await hookManager.getHook(hookId)
  if (!existingHook) {
    throw new NotFoundError(`Hook not found: ${hookId}`)
  }

  const { id, select, predicates, combine, phase } = validation.data

  // Convert predicates to hook format
  const hookPredicates = predicates.map(p => {
    switch (p.kind) {
      case 'ASK':
        return { kind: 'ASK', query: p.query }
      case 'SHACL':
        return { kind: 'SHACL', shapes: p.shapes }
      case 'DELTA':
        return { kind: 'DELTA' }
      case 'THRESHOLD':
        return {
          kind: 'THRESHOLD',
          variable: p.variable,
          operator: p.operator,
          value: p.value
        }
      case 'COUNT':
        return {
          kind: 'COUNT',
          countVariable: p.countVariable
        }
      case 'WINDOW':
        return {
          kind: 'WINDOW',
          windowSize: p.windowSize
        }
      default:
        throw new ValidationError(`Unknown predicate kind: ${p.kind}`)
    }
  })

  // Update hook
  const updatedHook = await hookManager.updateHook(hookId, {
    id,
    select,
    predicates: hookPredicates,
    combine,
    phase
  })

  return sendSuccess(event, {
    hookId: updatedHook.id,
    phase: updatedHook.phase,
    predicateCount: updatedHook.predicates.length,
    updated: true,
    timestamp: new Date().toISOString()
  })
}))
