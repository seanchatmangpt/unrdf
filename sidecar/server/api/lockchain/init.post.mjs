/**
 * @file Initialize Lockchain Endpoint
 * @description POST /api/lockchain/init - Initialize Git lockchain
 */
import { defineEventHandler, readBody } from '#imports'

import { getManagers } from '../../utils/managers.mjs'
import { sendSuccess, sendValidationError, asyncHandler } from '../../utils/response.mjs'
import { initLockchainSchema } from '../../utils/validation.mjs'
import { LockchainError } from '../../utils/errors.mjs'

/**
 * Initialize lockchain handler
 * @param {Object} event - H3 event
 * @returns {Promise<Object>} Initialization response
 */
export default defineEventHandler(asyncHandler(async (event) => {
  const body = await readBody(event)

  // Validate request
  const validation = initLockchainSchema.safeParse(body)
  if (!validation.success) {
    return sendValidationError(event, validation.error)
  }

  const { lockchainWriter } = getManagers()

  if (!lockchainWriter) {
    throw new LockchainError('Lockchain writer not configured. Set KGC_GIT_REPO_URL.')
  }

  const { repoUrl, branch, credentials } = validation.data

  // Initialize lockchain
  await lockchainWriter.initialize({
    repoUrl,
    branch: branch || 'main',
    credentials
  })

  return sendSuccess(event, {
    repoUrl,
    branch: branch || 'main',
    initialized: true
  }, 201)
}))
