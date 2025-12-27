/**
 * @file Apply Transaction Endpoint
 * @description POST /api/transaction/apply - Apply RDF transaction
 */

import { defineEventHandler, readBody } from '#imports'
import { getManagers } from '../../utils/managers.mjs'
import { sendSuccess, sendValidationError, asyncHandler } from '../../utils/response.mjs'
import { applyTransactionSchema } from '../../utils/validation.mjs'
import { HookExecutionError, PolicyViolationError } from '../../utils/errors.mjs'

/**
 * Apply transaction handler
 * @param {Object} event - H3 event
 * @returns {Promise<Object>} Transaction response
 */
export default defineEventHandler(asyncHandler(async (event) => {
  const body = await readBody(event)

  // Validate request
  const validation = applyTransactionSchema.safeParse(body)
  if (!validation.success) {
    return sendValidationError(event, validation.error)
  }

  const { transactionManager, hookManager, policyPack, lockchainWriter, observability } = getManagers()
  const { delta, author, metadata } = validation.data

  const startTime = Date.now()
  const hooksExecuted = []

  try {
    // Execute pre-hooks
    const preHooks = hookManager.getHooksByPhase('pre')
    for (const hook of preHooks) {
      const result = await hookManager.executeHook(hook.id, delta)
      if (!result.passed) {
        throw new HookExecutionError(hook.id, result.reason)
      }
      hooksExecuted.push(hook.id)
    }

    // Apply transaction
    const txResult = await transactionManager.applyTransaction(delta, {
      author,
      metadata
    })

    // Validate with policy pack
    if (policyPack.hasPolicies()) {
      const validationResult = await policyPack.validate(delta)
      if (!validationResult.conforms) {
        throw new PolicyViolationError('default', validationResult.violations)
      }
    }

    // Execute post-hooks
    const postHooks = hookManager.getHooksByPhase('post')
    for (const hook of postHooks) {
      const result = await hookManager.executeHook(hook.id, delta)
      // Post-hooks don't block, just log
      if (!result.passed) {
        console.warn(`Post-hook ${hook.id} failed:`, result.reason)
      }
      hooksExecuted.push(hook.id)
    }

    // Write lockchain receipt
    let receiptId
    if (lockchainWriter) {
      receiptId = await lockchainWriter.writeReceipt({
        transactionId: txResult.id,
        delta,
        author,
        timestamp: new Date().toISOString(),
        hooksExecuted
      })
    }

    // Record metrics
    const duration = Date.now() - startTime
    await observability.recordMetric('transaction.duration', duration)
    await observability.recordMetric('transaction.success', 1)

    return sendSuccess(event, {
      transactionId: txResult.id,
      receiptId,
      hooksExecuted,
      duration: `${duration}ms`
    }, 201)

  } catch (error) {
    // Record error metric
    await observability.recordMetric('transaction.error', 1)
    throw error
  }
}))
