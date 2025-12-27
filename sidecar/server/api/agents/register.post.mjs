/**
 * @file Register Agent Endpoint
 * @description POST /api/agents/register - Register multi-agent entity
 */

import { defineEventHandler, readBody, createError } from '#imports'
import { getManagers } from '../../utils/managers.mjs'
import { sendSuccess, sendValidationError, asyncHandler } from '../../utils/response.mjs'
import { registerAgentSchema } from '../../utils/validation.mjs'

/**
 * Register agent handler
 * @param {Object} event - H3 event
 * @returns {Promise<Object>} Registration response
 */
export default defineEventHandler(asyncHandler(async (event) => {
  // Check authentication
  if (!event.context.auth?.authenticated) {
    throw createError({
      statusCode: 401,
      statusMessage: 'Unauthorized',
      message: 'Authentication required'
    })
  }

  const body = await readBody(event)

  // Validate request
  const validation = registerAgentSchema.safeParse(body)
  if (!validation.success) {
    return sendValidationError(event, validation.error)
  }

  const { resolutionLayer } = getManagers()
  const { id, endpoint, priority } = validation.data

  // Register agent
  await resolutionLayer.registerAgent({
    id,
    endpoint,
    priority: priority || 0
  })

  return sendSuccess(event, {
    agentId: id,
    endpoint,
    priority: priority || 0,
    registeredBy: event.context.auth.userId
  }, 201)
}))
