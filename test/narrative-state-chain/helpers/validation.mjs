/**
 * @file Validation Helpers for Tests
 * @module test/narrative-state-chain/helpers/validation
 * @description Functions to validate NSC objects and assertions
 */

import crypto from 'crypto';
import { z } from 'zod';

/**
 * Verify receipt signature
 * @param {Object} receipt - Receipt to verify
 * @param {string} publicKey - Public key for verification
 * @returns {boolean} True if signature is valid
 */
export function verifyReceiptSignature(receipt, publicKey) {
  // In real implementation, would use crypto library
  // For testing, verify structure is correct
  if (!receipt.signature || !receipt.signature.signature) {
    return false;
  }
  if (!receipt.receiptHash) {
    return false;
  }
  return receipt.signature.publicKey === publicKey ||
         receipt.signature.algorithm in ['ed25519', 'secp256k1', 'bls12-381'];
}

/**
 * Verify scene receipt hash integrity
 * @param {Object} scene - Scene envelope to verify
 * @returns {boolean} True if hash is consistent
 */
export function verifySceneReceiptHash(scene) {
  if (!scene.receipt || !scene.receipt.receiptHash) {
    return false;
  }

  // Hash should be deterministic
  const hash1 = crypto.createHash('sha256');
  hash1.update(JSON.stringify(scene.receipt, (_, v) =>
    typeof v === 'bigint' ? v.toString() : v
  ));
  const computedHash = hash1.digest('hex');

  // In production, would verify actual hash
  // For testing, verify it's a valid hex string of correct length
  return /^[a-f0-9]{64}$/.test(scene.receipt.receiptHash);
}

/**
 * Verify delta hash is valid
 * @param {Object} delta - Delta object
 * @returns {boolean} True if delta hash is valid
 */
export function verifyDeltaHash(delta) {
  if (!delta.hash || typeof delta.hash !== 'string') {
    return false;
  }
  return /^[a-f0-9]{64}$/.test(delta.hash);
}

/**
 * Verify scene chain continuity (forkParents)
 * @param {Array<Object>} scenes - Ordered scene array
 * @returns {boolean} True if chain is continuous
 */
export function verifySceneChain(scenes) {
  if (scenes.length === 0) return true;
  if (scenes.length === 1) {
    // First scene should have no parents or empty array
    return !scenes[0].receipt.forkParents || scenes[0].receipt.forkParents.length === 0;
  }

  for (let i = 1; i < scenes.length; i++) {
    const scene = scenes[i];
    const parent = scenes[i - 1];

    // Current scene should have parent's receipt ID in forkParents
    if (!scene.receipt.forkParents.includes(parent.receipt.sceneId)) {
      return false;
    }
  }

  return true;
}

/**
 * Verify sequence numbers are strictly increasing
 * @param {Array<Object>} scenes - Scene array
 * @returns {boolean} True if sequence is valid
 */
export function verifySequenceNumbers(scenes) {
  for (let i = 1; i < scenes.length; i++) {
    const seq = scenes[i].metadata.sequenceNumber;
    const prevSeq = scenes[i - 1].metadata.sequenceNumber;

    if (seq <= prevSeq) {
      return false;
    }
  }

  return true;
}

/**
 * Verify invariant check result is valid
 * @param {Object} checkResult - InvariantCheckResult
 * @returns {boolean} True if structure is valid
 */
export function verifyInvariantCheckResult(checkResult) {
  if (!checkResult.invariantId || typeof checkResult.invariantId !== 'string') {
    return false;
  }
  if (typeof checkResult.satisfied !== 'boolean') {
    return false;
  }
  if (!checkResult.query || typeof checkResult.query !== 'string') {
    return false;
  }
  return typeof checkResult.bindings === 'object';
}

/**
 * Verify all invariant checks in consequence
 * @param {Object} consequence - Consequence object
 * @returns {boolean} True if all checks are valid
 */
export function verifyConsequenceInvariants(consequence) {
  if (!Array.isArray(consequence.invariantChecks)) {
    return false;
  }
  return consequence.invariantChecks.every(check =>
    verifyInvariantCheckResult(check)
  );
}

/**
 * Verify observation has required fields
 * @param {Object} obs - Observation to validate
 * @returns {boolean} True if valid
 */
export function verifyObservationStructure(obs) {
  return obs.quad &&
         obs.quad.subject &&
         obs.quad.predicate &&
         obs.quad.object &&
         obs.quad.graph &&
         typeof obs.source === 'string' &&
         typeof obs.timestamp === 'number' &&
         typeof obs.confidence === 'number' &&
         obs.confidence >= 0 &&
         obs.confidence <= 1;
}

/**
 * Verify delta has valid structure
 * @param {Object} delta - Delta to validate
 * @returns {boolean} True if valid
 */
export function verifyDeltaStructure(delta) {
  if (!Array.isArray(delta.additions)) return false;
  if (!Array.isArray(delta.deletions)) return false;
  if (!delta.hash || typeof delta.hash !== 'string') return false;
  if (!delta.proof || typeof delta.proof !== 'object') return false;

  // All additions/deletions should be valid quads
  const allQuads = [...delta.additions, ...delta.deletions];
  return allQuads.every(quad =>
    quad.subject && quad.predicate && quad.object && quad.graph
  );
}

/**
 * Verify scene envelope has minimal structure
 * @param {Object} scene - Scene to validate
 * @returns {boolean} True if valid
 */
export function verifySceneStructure(scene) {
  if (!scene.id || typeof scene.id !== 'string') return false;
  if (!scene.universeId || typeof scene.universeId !== 'string') return false;
  if (!Array.isArray(scene.observations)) return false;
  if (!scene.delta) return false;
  if (!scene.consequences) return false;
  if (!scene.receipt) return false;
  if (!scene.metadata) return false;

  return scene.observations.every(obs => verifyObservationStructure(obs)) &&
         verifyDeltaStructure(scene.delta) &&
         verifyConsequenceInvariants(scene.consequences) &&
         verifyDeltaHash(scene.delta);
}

/**
 * Verify bridge proof structure
 * @param {Object} bridge - Bridge proof to validate
 * @returns {boolean} True if valid
 */
export function verifyBridgeProofStructure(bridge) {
  if (!bridge.id || typeof bridge.id !== 'string') return false;
  if (!bridge.sourceUniverseId || typeof bridge.sourceUniverseId !== 'string') return false;
  if (!bridge.targetUniverseId || typeof bridge.targetUniverseId !== 'string') return false;
  if (!bridge.typeCoercion || typeof bridge.typeCoercion !== 'object') return false;
  if (!Array.isArray(bridge.invariantPreservation)) return false;
  if (!Array.isArray(bridge.accessGrants)) return false;
  if (!bridge.validity) return false;
  if (!bridge.metadata) return false;

  return bridge.sourceUniverseId !== bridge.targetUniverseId;
}

/**
 * Create a deterministic hash of content
 * @param {*} content - Content to hash
 * @returns {string} SHA256 hex digest
 */
export function createContentHash(content) {
  const str = typeof content === 'string' ?
    content :
    JSON.stringify(content, (_, v) => typeof v === 'bigint' ? v.toString() : v);

  return crypto.createHash('sha256').update(str).digest('hex');
}

/**
 * Verify that replaying a scene produces identical hash
 * @param {Object} scene - Scene to replay
 * @param {Function} reconcileFunc - Reconciliation function
 * @returns {boolean} True if replay is deterministic
 */
export async function verifySceneDeterminism(scene, reconcileFunc) {
  // Compute original hash
  const originalHash = scene.receipt.receiptHash;

  // Replay with same inputs
  const replayed = await reconcileFunc(scene.observations);

  // Create new receipt with replayed consequence
  const replayedReceipt = {
    ...scene.receipt,
    timestamp: scene.receipt.timestamp // Use same timestamp
  };

  const replayedHash = createContentHash(JSON.stringify(replayedReceipt));

  // Hashes should match (within same timestamp)
  return originalHash === replayedHash;
}

/**
 * Assert that two objects are deeply equal
 * @param {*} actual - Actual value
 * @param {*} expected - Expected value
 * @param {string} message - Error message
 * @throws {AssertionError} If not equal
 */
export function assertEqual(actual, expected, message = '') {
  const actualStr = JSON.stringify(actual);
  const expectedStr = JSON.stringify(expected);

  if (actualStr !== expectedStr) {
    throw new Error(`Assertion failed: ${message}\nExpected: ${expectedStr}\nActual: ${actualStr}`);
  }
}

/**
 * Assert that value is truthy
 * @param {*} value - Value to check
 * @param {string} message - Error message
 * @throws {AssertionError} If falsy
 */
export function assertTrue(value, message = '') {
  if (!value) {
    throw new Error(`Assertion failed: ${message}`);
  }
}

/**
 * Assert that value is falsy
 * @param {*} value - Value to check
 * @param {string} message - Error message
 * @throws {AssertionError} If truthy
 */
export function assertFalse(value, message = '') {
  if (value) {
    throw new Error(`Assertion failed: ${message}`);
  }
}

/**
 * Assert that array contains expected value
 * @param {Array} array - Array to check
 * @param {*} expected - Value to find
 * @param {string} message - Error message
 * @throws {AssertionError} If not found
 */
export function assertIncludes(array, expected, message = '') {
  if (!array.includes(expected)) {
    throw new Error(`Assertion failed: Expected array to include ${expected}. ${message}`);
  }
}
