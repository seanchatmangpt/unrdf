/**
 * @file stable-ids.mjs
 * @description Deterministic IRI and Skolem identifier generation using SHA-256
 * @module agent-3/stable-ids
 */

import { createHash } from 'node:crypto';

/**
 * Generate a stable IRI from domain, entity, and attribute components
 *
 * Deterministic: same (domain, entity, attr) → same IRI always
 * Uses SHA-256 hash to ensure collision resistance and stability
 *
 * @param {string} domain - Domain namespace (e.g., "kgc-facade")
 * @param {string} entity - Entity type (e.g., "customer")
 * @param {string} attr - Attribute or identifier (e.g., "customer-123")
 * @returns {string} Stable IRI with format: http://kgc.internal/{domain}/{entity}/{attr}#{hash}
 *
 * @example
 * stableIRI("kgc-facade", "customer", "customer-123")
 * // → "http://kgc.internal/kgc-facade/customer/customer-123#a3f2c1..."
 */
export function stableIRI(domain, entity, attr) {
  // Input validation
  if (!domain || typeof domain !== 'string') {
    throw new Error('stableIRI: domain must be a non-empty string');
  }
  if (!entity || typeof entity !== 'string') {
    throw new Error('stableIRI: entity must be a non-empty string');
  }
  if (!attr || typeof attr !== 'string') {
    throw new Error('stableIRI: attr must be a non-empty string');
  }

  // Create deterministic hash of the composite key
  const composite = `${domain}:${entity}:${attr}`;
  const hash = createHash('sha256')
    .update(composite)
    .digest('hex')
    .substring(0, 16); // First 16 chars for readability

  // Construct stable IRI
  return `http://kgc.internal/${domain}/${entity}/${attr}#${hash}`;
}

/**
 * Generate a stable Skolem blank node identifier from a template and values
 *
 * For attributes without stable IRIs or when blank nodes are required
 * Uses SHA-256 hash of template + values for determinism
 *
 * @param {string} template - Template string with placeholders (e.g., "customer-{id}-{attr}")
 * @param {Object} values - Key-value pairs to fill template (e.g., { id: "123", attr: "address" })
 * @returns {string} Stable blank node with format: _:skolem-{hash}
 *
 * @example
 * stableSkolem("customer-{id}-{attr}", { id: "123", attr: "address" })
 * // → "_:skolem-7f3a9e2b1c4d5e6f"
 */
export function stableSkolem(template, values) {
  // Input validation
  if (!template || typeof template !== 'string') {
    throw new Error('stableSkolem: template must be a non-empty string');
  }
  if (!values || typeof values !== 'object') {
    throw new Error('stableSkolem: values must be an object');
  }

  // Create deterministic hash of template + sorted values
  // Sort keys for consistent hashing regardless of property order
  const sortedValues = JSON.stringify(values, Object.keys(values).sort());
  const composite = `${template}${sortedValues}`;

  const hash = createHash('sha256')
    .update(composite)
    .digest('hex')
    .substring(0, 16); // First 16 chars for readability

  // Construct Skolem blank node identifier
  return `_:skolem-${hash}`;
}

/**
 * Utility: Generate a short stable hash for any input
 * Used internally for additional identifier generation
 *
 * @param {string} input - Input string to hash
 * @param {number} [length=16] - Hash length (default 16)
 * @returns {string} Hexadecimal hash string
 * @private
 */
export function stableHash(input, length = 16) {
  if (!input || typeof input !== 'string') {
    throw new Error('stableHash: input must be a non-empty string');
  }

  return createHash('sha256')
    .update(input)
    .digest('hex')
    .substring(0, length);
}
