/**
 * @file Register Hook Endpoint
 * @description POST /api/hooks/register - Register knowledge hook
 */

import { defineEventHandler, readBody } from '#imports'
import { getManagers } from '../../utils/managers.mjs'
import { sendSuccess, sendValidationError, asyncHandler } from '../../utils/response.mjs'
import { registerHookSchema } from '../../utils/validation.mjs'
import { ValidationError } from '../../utils/errors.mjs'

/**
 * Register hook handler
 * @param {Object} event - H3 event
 * @returns {Promise<Object>} Registration response
 */
export default defineEventHandler(asyncHandler(async (event) => {
  const body = await readBody(event)

  // Validate request
  const validation = registerHookSchema.safeParse(body)
  if (!validation.success) {
    return sendValidationError(event, validation.error)
  }

  const { hookManager } = getManagers()
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

  // Register hook
  const hook = await hookManager.defineHook({
    id,
    select,
    predicates: hookPredicates,
    combine,
    phase
  })

  return sendSuccess(event, {
    hookId: hook.id,
    phase: hook.phase,
    predicateCount: hook.predicates.length
  }, 201)
}))
