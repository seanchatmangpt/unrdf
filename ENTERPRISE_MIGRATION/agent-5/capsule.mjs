/**
 * @fileoverview Capsule data structure for proof system.
 * A capsule contains metadata, delta (change), and receipt (hash chain).
 */

import { canonicalize } from './canonicalize.mjs';

/**
 * @typedef {Object} CapsuleMeta
 * @property {string} id - Unique capsule identifier
 * @property {number} timestamp - Unix timestamp in milliseconds
 * @property {string} agentId - ID of agent that created capsule
 * @property {string} phase - Phase identifier (e.g., 'analyze', 'migrate')
 */

/**
 * @typedef {Object} Capsule
 * @property {CapsuleMeta} meta - Capsule metadata
 * @property {any} delta - The actual change/operation data
 * @property {Object|null} receipt - Hash chain receipt linking to previous capsule
 */

/**
 * Create a new capsule.
 *
 * @param {any} delta - The change/operation to encapsulate
 * @param {Partial<CapsuleMeta>} meta - Metadata for the capsule
 * @returns {Capsule} New capsule instance
 * @throws {Error} If required metadata is missing
 */
export function createCapsule(delta, meta = {}) {
  if (!meta.agentId) {
    throw new Error('Capsule requires meta.agentId');
  }

  if (!meta.phase) {
    throw new Error('Capsule requires meta.phase');
  }

  const timestamp = meta.timestamp || Date.now();
  const id = meta.id || generateCapsuleId(meta.agentId, timestamp);

  return {
    meta: {
      id,
      timestamp,
      agentId: meta.agentId,
      phase: meta.phase,
    },
    delta,
    receipt: null, // Receipt added later by receipt generator
  };
}

/**
 * Generate a unique capsule ID.
 *
 * @param {string} agentId - Agent identifier
 * @param {number} timestamp - Unix timestamp in milliseconds
 * @returns {string} Capsule ID in format: capsule_{timestamp}_{agentId}
 */
function generateCapsuleId(agentId, timestamp) {
  const timestampSec = Math.floor(timestamp / 1000);
  const agentIdShort = agentId.replace(/^agent-/, '').slice(0, 8);
  return `capsule_${timestampSec}_${agentIdShort}`;
}

/**
 * Validate capsule structure.
 *
 * @param {any} capsule - Capsule to validate
 * @returns {boolean} True if valid
 * @throws {Error} If capsule structure is invalid
 */
export function validateCapsule(capsule) {
  if (!capsule || typeof capsule !== 'object') {
    throw new Error('Capsule must be an object');
  }

  if (!capsule.meta || typeof capsule.meta !== 'object') {
    throw new Error('Capsule requires meta object');
  }

  const { meta } = capsule;

  if (!meta.id || typeof meta.id !== 'string') {
    throw new Error('Capsule meta requires string id');
  }

  if (!meta.timestamp || typeof meta.timestamp !== 'number') {
    throw new Error('Capsule meta requires numeric timestamp');
  }

  if (!meta.agentId || typeof meta.agentId !== 'string') {
    throw new Error('Capsule meta requires string agentId');
  }

  if (!meta.phase || typeof meta.phase !== 'string') {
    throw new Error('Capsule meta requires string phase');
  }

  if (!('delta' in capsule)) {
    throw new Error('Capsule requires delta property');
  }

  if (!('receipt' in capsule)) {
    throw new Error('Capsule requires receipt property');
  }

  return true;
}

/**
 * Serialize capsule to deterministic JSON.
 * Uses canonical JSON for cryptographic hashing.
 *
 * @param {Capsule} capsule - Capsule to serialize
 * @returns {string} Canonical JSON string
 * @throws {Error} If capsule is invalid
 */
export function serializeCapsule(capsule) {
  validateCapsule(capsule);
  return canonicalize(capsule);
}

/**
 * Deserialize capsule from JSON string.
 *
 * @param {string} json - JSON string to parse
 * @returns {Capsule} Parsed capsule
 * @throws {Error} If JSON is invalid or capsule structure is invalid
 */
export function deserializeCapsule(json) {
  if (typeof json !== 'string') {
    throw new Error('deserializeCapsule requires a string');
  }

  let capsule;
  try {
    capsule = JSON.parse(json);
  } catch (error) {
    throw new Error(`Invalid JSON: ${error.message}`);
  }

  validateCapsule(capsule);
  return capsule;
}

/**
 * Extract capsule content for hashing (without receipt).
 * Receipt must be excluded when computing capsule hash to avoid circular dependency.
 *
 * @param {Capsule} capsule - Capsule to extract content from
 * @returns {Object} Capsule content without receipt
 */
export function extractCapsuleContent(capsule) {
  const { meta, delta } = capsule;
  return { meta, delta };
}

/**
 * Clone a capsule with optional modifications.
 *
 * @param {Capsule} capsule - Capsule to clone
 * @param {Object} modifications - Properties to modify
 * @returns {Capsule} Cloned capsule
 */
export function cloneCapsule(capsule, modifications = {}) {
  return {
    meta: { ...capsule.meta, ...(modifications.meta || {}) },
    delta: modifications.delta !== undefined ? modifications.delta : capsule.delta,
    receipt: modifications.receipt !== undefined ? modifications.receipt : capsule.receipt,
  };
}

/**
 * Attach receipt to capsule.
 * Creates a new capsule instance with the receipt attached.
 *
 * @param {Capsule} capsule - Capsule to attach receipt to
 * @param {Object} receipt - Receipt to attach
 * @returns {Capsule} New capsule with receipt
 */
export function attachReceipt(capsule, receipt) {
  return {
    ...capsule,
    receipt,
  };
}

/**
 * Get capsule summary for logging/debugging.
 *
 * @param {Capsule} capsule - Capsule to summarize
 * @returns {Object} Capsule summary
 */
export function getCapsuleSummary(capsule) {
  return {
    id: capsule.meta.id,
    agentId: capsule.meta.agentId,
    phase: capsule.meta.phase,
    timestamp: capsule.meta.timestamp,
    timestampISO: new Date(capsule.meta.timestamp).toISOString(),
    hasReceipt: capsule.receipt !== null,
    deltaType: typeof capsule.delta,
  };
}
