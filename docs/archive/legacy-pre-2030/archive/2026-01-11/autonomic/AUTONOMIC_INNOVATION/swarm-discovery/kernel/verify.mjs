/**
 * @file verify.mjs - Receipt verification and tampering detection
 * @description Implements Q3 (Receipts) and Q5 (SLA honesty) invariants
 */

import { hashData, hashContent } from './hash.mjs'
import { stableStringify } from './stable-json.mjs'

/**
 * Create a verifiable receipt
 * @param {object} inputs - Experiment inputs
 * @param {object} outputs - Experiment outputs
 * @param {string} codeHash - Hash of experiment code
 * @param {object} env - Environment metadata
 * @returns {object} - Receipt with verify() method
 */
export function createReceipt(inputs, outputs, codeHash, env = {}) {
  const canonical = {
    inputs: sortObjectKeys(inputs),
    outputs: sortObjectKeys(outputs),
    codeHash,
    env: sortObjectKeys(env),
    timestamp: new Date().toISOString()
  }

  const hash = hashData(canonical)

  return {
    hash,
    canonical,
    codeHash,
    timestamp: canonical.timestamp,

    /**
     * Verify receipt authenticity
     * @param {object} checkInputs - Inputs to verify against
     * @param {object} checkOutputs - Outputs to verify against
     * @returns {boolean} - True if receipt is valid
     */
    verify(checkInputs, checkOutputs) {
      const recomputed = {
        inputs: sortObjectKeys(checkInputs),
        outputs: sortObjectKeys(checkOutputs),
        codeHash,
        env: sortObjectKeys(env),
        timestamp: canonical.timestamp
      }

      const recomputedHash = hashData(recomputed)
      return recomputedHash === this.hash
    },

    /**
     * Export receipt as JSON
     * @returns {string} - JSON representation
     */
    toJSON() {
      return stableStringify({
        hash: this.hash,
        codeHash: this.codeHash,
        timestamp: this.timestamp,
        inputKeys: Object.keys(canonical.inputs).sort(),
        outputKeys: Object.keys(canonical.outputs).sort()
      })
    }
  }
}

/**
 * Verify a stored receipt against re-run data
 * @param {object} storedReceipt - Stored receipt object
 * @param {object} newInputs - New experiment inputs
 * @param {object} newOutputs - New experiment outputs
 * @param {string} codeHash - Code hash
 * @returns {object} - {valid, witness?}
 */
export function verifyReceipt(storedReceipt, newInputs, newOutputs, codeHash) {
  if (!storedReceipt || !storedReceipt.hash) {
    return { valid: false, reason: 'Invalid receipt object' }
  }

  // Recompute hash with same parameters
  const receipt = createReceipt(newInputs, newOutputs, codeHash, storedReceipt.env)

  if (receipt.hash !== storedReceipt.hash) {
    return {
      valid: false,
      reason: 'Receipt hash mismatch (tampered data)',
      stored: storedReceipt.hash,
      computed: receipt.hash
    }
  }

  return { valid: true }
}

/**
 * Create SLA measurement receipt
 * @param {object} measurements - Performance measurements
 * @param {object} claims - SLA claims
 * @returns {object} - Measurement receipt
 */
export function createMeasurementReceipt(measurements, claims) {
  const canonical = {
    measurements: sortObjectKeys(measurements),
    claims: sortObjectKeys(claims),
    timestamp: new Date().toISOString()
  }

  return {
    hash: hashData(canonical),
    measurements,
    claims,
    timestamp: canonical.timestamp,

    verify() {
      // Check measurements support claims
      const failures = []

      for (const [key, claim] of Object.entries(claims)) {
        if (!(key in measurements)) {
          failures.push(`Claim ${key} has no measurement`)
        } else {
          const measured = measurements[key]
          if (claim.maxMs && measured.totalMs > claim.maxMs) {
            failures.push(`${key} exceeded SLA: ${measured.totalMs}ms > ${claim.maxMs}ms`)
          }
          if (claim.maxErrorRate && measured.errorRate > claim.maxErrorRate) {
            failures.push(`${key} error rate exceeded: ${measured.errorRate} > ${claim.maxErrorRate}`)
          }
        }
      }

      return failures.length === 0 ? { valid: true } : { valid: false, failures }
    }
  }
}

/**
 * Sort object keys recursively
 * @private
 */
function sortObjectKeys(obj) {
  if (obj === null || typeof obj !== 'object') {
    return obj
  }

  if (Array.isArray(obj)) {
    return obj.map(sortObjectKeys)
  }

  const sorted = {}
  for (const key of Object.keys(obj).sort()) {
    sorted[key] = sortObjectKeys(obj[key])
  }

  return sorted
}
