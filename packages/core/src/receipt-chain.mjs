/** Deterministic, tamper-evident execution receipt chain. */
import { createHash, randomUUID } from 'node:crypto';

/**
 * Recursively sort object keys and convert bigints to strings so serialization is deterministic.
 * @param {*} value - Value to canonicalize.
 * @returns {*} Canonical copy of the value.
 */
export function canonicalizeJSON(value) {
  if (Array.isArray(value)) return value.map(canonicalizeJSON);
  if (value && typeof value === 'object')
    return Object.fromEntries(
      Object.keys(value)
        .sort()
        .map(k => [k, canonicalizeJSON(value[k])])
    );
  if (typeof value === 'bigint') return value.toString();
  return value;
}

/**
 * SHA-256 of the canonical JSON of a value.
 * @param {*} value - Value to hash.
 * @returns {string} Hex digest.
 */
export function hashCanonical(value) {
  return createHash('sha256')
    .update(JSON.stringify(canonicalizeJSON(value)))
    .digest('hex');
}

/**
 * Append-only chain of execution receipts linked by digest.
 */
export class ReceiptChain {
  #receipts = [];

  /**
   * Create a chain.
   * @param {Object} options - Chain identity.
   * @param {string} options.subject - Subject the chain is about.
   * @param {string} options.source - Source of the receipts.
   * @param {string|null} [options.authority] - Issuing authority.
   * @throws {TypeError} If subject or source is missing.
   */
  constructor({ subject, source, authority = null } = {}) {
    if (!subject || !source) throw new TypeError('subject and source are required');
    this.subject = subject;
    this.source = source;
    this.authority = authority;
  }

  /**
   * Append a receipt.
   * @param {Object} entry - Receipt content.
   * @param {string} entry.action - Action performed.
   * @param {Object} [entry.inputs] - Action inputs.
   * @param {Object} [entry.outputs] - Action outputs.
   * @param {string} entry.result - Result label.
   * @param {string|null} [entry.verifier] - Verifier name.
   * @param {Object} [entry.environment] - Environment description.
   * @param {string[]} [entry.exclusions] - Known exclusions or errors.
   * @returns {Object} A copy of the frozen receipt with id, sequence, previous and digest.
   * @throws {TypeError} If action or result is missing.
   */
  append({
    action,
    inputs = {},
    outputs = {},
    result,
    verifier = null,
    environment = {},
    exclusions = [],
  }) {
    if (!action || !result) throw new TypeError('action and result are required');
    const previous = this.#receipts.at(-1)?.digest ?? null;
    const body = canonicalizeJSON({
      schema: 'unrdf.execution-receipt/1',
      id: randomUUID(),
      sequence: this.#receipts.length + 1,
      subject: this.subject,
      source: this.source,
      authority: this.authority,
      previous,
      action,
      inputs,
      outputs,
      result,
      verifier,
      environment,
      exclusions,
    });
    const receipt = Object.freeze({ ...body, digest: hashCanonical(body) });
    this.#receipts.push(receipt);
    return structuredClone(receipt);
  }

  /**
   * List all receipts.
   * @returns {Object[]} Copies of the receipts in order.
   */
  list() {
    return this.#receipts.map(receipt => structuredClone(receipt));
  }
  /**
   * Get the latest receipt.
   * @returns {Object|null} A copy of the last receipt, or null if empty.
   */
  head() {
    return this.#receipts.length ? structuredClone(this.#receipts.at(-1)) : null;
  }

  /**
   * Check each receipt's digest, previous link and sequence number.
   * @returns {{valid: boolean, count: number, head: string|null, failures: Object[]}} Verification result.
   */
  verify() {
    const failures = [];
    for (let i = 0; i < this.#receipts.length; i++) {
      const receipt = this.#receipts[i];
      const { digest, ...body } = receipt;
      if (hashCanonical(body) !== digest)
        failures.push({ sequence: i + 1, code: 'DIGEST_MISMATCH' });
      const expected = i === 0 ? null : this.#receipts[i - 1].digest;
      if (receipt.previous !== expected)
        failures.push({ sequence: i + 1, code: 'PREVIOUS_MISMATCH' });
      if (receipt.sequence !== i + 1) failures.push({ sequence: i + 1, code: 'SEQUENCE_MISMATCH' });
    }
    return {
      valid: failures.length === 0,
      count: this.#receipts.length,
      head: this.head()?.digest ?? null,
      failures,
    };
  }

  /**
   * Export the chain with its receipts and verification result.
   * @returns {Object} Canonical export object.
   */
  export() {
    return canonicalizeJSON({
      schema: 'unrdf.receipt-chain/1',
      subject: this.subject,
      source: this.source,
      authority: this.authority,
      receipts: this.list(),
      verification: this.verify(),
    });
  }
}

/**
 * Compare two values by canonical digest, ignoring selected keys at any depth.
 * @param {*} first - First value.
 * @param {*} second - Second value.
 * @param {Object} [options] - Options.
 * @param {string[]} [options.ignore] - Keys omitted before hashing (default ['id']).
 * @returns {{match: boolean, firstDigest: string, secondDigest: string, state: string}} Comparison with state REPLAY_MATCH or REPLAY_DIFFERENCE.
 */
export function compareReplay(first, second, { ignore = ['id'] } = {}) {
  const omit = value => {
    if (Array.isArray(value)) return value.map(omit);
    if (value && typeof value === 'object')
      return Object.fromEntries(
        Object.entries(value)
          .filter(([k]) => !ignore.includes(k))
          .map(([k, v]) => [k, omit(v)])
      );
    return value;
  };
  const firstDigest = hashCanonical(omit(first));
  const secondDigest = hashCanonical(omit(second));
  return {
    match: firstDigest === secondDigest,
    firstDigest,
    secondDigest,
    state: firstDigest === secondDigest ? 'REPLAY_MATCH' : 'REPLAY_DIFFERENCE',
  };
}

/**
 * Create a ReceiptChain.
 * @param {Object} options - Options forwarded to the constructor.
 * @returns {ReceiptChain} A new chain.
 */
export function createReceiptChain(options) {
  return new ReceiptChain(options);
}
