/** Append-only hash-chained event log. */
import { hashCanonical, canonicalizeJSON } from './receipt-chain.mjs';

export class EventLog {
  #events = [];

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

  read({ from = 1, type = null } = {}) {
    return this.#events
      .filter(event => event.sequence >= from && (type === null || event.type === type))
      .map(event => structuredClone(event));
  }

  verify() {
    const failures = [];
    for (let index = 0; index < this.#events.length; index++) {
      const event = this.#events[index];
      const { digest, ...body } = event;
      if (digest !== hashCanonical(body)) failures.push({ sequence: index + 1, code: 'EVENT_DIGEST_MISMATCH' });
      if (event.previous !== (index === 0 ? null : this.#events[index - 1].digest)) failures.push({ sequence: index + 1, code: 'EVENT_CHAIN_BROKEN' });
    }
    return { valid: failures.length === 0, count: this.#events.length, head: this.#events.at(-1)?.digest ?? null, failures };
  }
}

export function createEventLog() { return new EventLog(); }
