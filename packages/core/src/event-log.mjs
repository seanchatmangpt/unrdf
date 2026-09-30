/** Append-only hash-chained event log. */
import { hashCanonical, canonicalizeJSON } from './receipt-chain.mjs';

/**
 * Append-only log whose events each carry the digest of their predecessor.
 */
export class EventLog {
  #events = [];

  /**
   * Append an event.
   * @param {string} type - Event type.
   * @param {*} payload - Event payload.
   * @param {Object} [metadata] - Event metadata.
   * @returns {Object} A copy of the frozen event including sequence, previous and digest.
   * @throws {TypeError} If `type` is missing.
   */
  append(type, payload, metadata = {}) {
    if (!type) throw new TypeError('event type is required');
    const previous = this.#events.at(-1)?.digest ?? null;
    const body = canonicalizeJSON({
      schema: 'unrdf.event/1',
      sequence: this.#events.length + 1,
      type,
      payload,
      metadata,
      previous,
    });
    const event = Object.freeze({ ...body, digest: hashCanonical(body) });
    this.#events.push(event);
    return structuredClone(event);
  }

  /**
   * Read events, optionally filtered.
   * @param {Object} [filter] - Filter options.
   * @param {number} [filter.from] - Minimum sequence number (inclusive).
   * @param {string|null} [filter.type] - Only events of this type.
   * @returns {Object[]} Copies of matching events.
   */
  read({ from = 1, type = null } = {}) {
    return this.#events
      .filter(event => event.sequence >= from && (type === null || event.type === type))
      .map(event => structuredClone(event));
  }

  /**
   * Recompute digests and check the chain links.
   * @returns {{valid: boolean, count: number, head: string|null, failures: Object[]}} Verification result.
   */
  verify() {
    const failures = [];
    for (let index = 0; index < this.#events.length; index++) {
      const event = this.#events[index];
      const { digest, ...body } = event;
      if (digest !== hashCanonical(body))
        failures.push({ sequence: index + 1, code: 'EVENT_DIGEST_MISMATCH' });
      if (event.previous !== (index === 0 ? null : this.#events[index - 1].digest))
        failures.push({ sequence: index + 1, code: 'EVENT_CHAIN_BROKEN' });
    }
    return {
      valid: failures.length === 0,
      count: this.#events.length,
      head: this.#events.at(-1)?.digest ?? null,
      failures,
    };
  }
}

/**
 * Create an empty event log.
 * @returns {EventLog} A new log.
 */
export function createEventLog() {
  return new EventLog();
}
