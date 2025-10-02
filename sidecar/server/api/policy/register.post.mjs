/**
 * @file Register Policy Endpoint
 * @description POST /api/policy/register - Register SHACL policy pack
 */
import { defineEventHandler, readBody } from '#imports'

import { getManagers } from '../../utils/managers.mjs'
import { sendSuccess, sendValidationError, asyncHandler } from '../../utils/response.mjs'
import { registerPolicySchema } from '../../utils/validation.mjs'

/**
 * Register policy handler
 * @param {Object} event - H3 event
 * @returns {Promise<Object>} Registration response
 */
export default defineEventHandler(asyncHandler(async (event) => {
  const body = await readBody(event)

  // Validate request
  const validation = registerPolicySchema.safeParse(body)
  if (!validation.success) {
    return sendValidationError(event, validation.error)
  }

  const { policyPack } = getManagers()
  const { id, shapes, priority } = validation.data

  // Register policy
  await policyPack.registerPolicy({
    id,
    shapes,
    priority: priority || 0
  })

  return sendSuccess(event, {
    policyId: id,
    priority: priority || 0
  }, 201)
}))
