/** Deterministic, tamper-evident execution receipt chain. */
import { createHash, randomUUID } from 'node:crypto';

export function canonicalizeJSON(value) {
  if (Array.isArray(value)) return value.map(canonicalizeJSON);
  if (value && typeof value === 'object') return Object.fromEntries(Object.keys(value).sort().map(k => [k, canonicalizeJSON(value[k])]));
  if (typeof value === 'bigint') return value.toString();
  return value;
}

export function hashCanonical(value) {
  return createHash('sha256').update(JSON.stringify(canonicalizeJSON(value))).digest('hex');
}

export class ReceiptChain {
  #receipts = [];

  constructor({ subject, source, authority = null } = {}) {
    if (!subject || !source) throw new TypeError('subject and source are required');
    this.subject = subject;
    this.source = source;
    this.authority = authority;
  }

  append({ action, inputs = {}, outputs = {}, result, verifier = null, environment = {}, exclusions = [] }) {
    if (!action || !result) throw new TypeError('action and result are required');
    const previous = this.#receipts.at(-1)?.digest ?? null;
    const body = canonicalizeJSON({
      schema: 'unrdf.execution-receipt/1', id: randomUUID(), sequence: this.#receipts.length + 1,
      subject: this.subject, source: this.source, authority: this.authority, previous,
      action, inputs, outputs, result, verifier, environment, exclusions,
    });
    const receipt = Object.freeze({ ...body, digest: hashCanonical(body) });
    this.#receipts.push(receipt);
    return structuredClone(receipt);
  }

  list() { return this.#receipts.map(receipt => structuredClone(receipt)); }
  head() { return this.#receipts.length ? structuredClone(this.#receipts.at(-1)) : null; }

  verify() {
    const failures = [];
    for (let i = 0; i < this.#receipts.length; i++) {
      const receipt = this.#receipts[i];
      const { digest, ...body } = receipt;
      if (hashCanonical(body) !== digest) failures.push({ sequence: i + 1, code: 'DIGEST_MISMATCH' });
      const expected = i === 0 ? null : this.#receipts[i - 1].digest;
      if (receipt.previous !== expected) failures.push({ sequence: i + 1, code: 'PREVIOUS_MISMATCH' });
      if (receipt.sequence !== i + 1) failures.push({ sequence: i + 1, code: 'SEQUENCE_MISMATCH' });
    }
    return { valid: failures.length === 0, count: this.#receipts.length, head: this.head()?.digest ?? null, failures };
  }

  export() {
    return canonicalizeJSON({ schema: 'unrdf.receipt-chain/1', subject: this.subject, source: this.source, authority: this.authority, receipts: this.list(), verification: this.verify() });
  }
}

export function compareReplay(first, second, { ignore = ['id'] } = {}) {
  const omit = value => {
    if (Array.isArray(value)) return value.map(omit);
    if (value && typeof value === 'object') return Object.fromEntries(Object.entries(value).filter(([k]) => !ignore.includes(k)).map(([k, v]) => [k, omit(v)]));
    return value;
  };
  const firstDigest = hashCanonical(omit(first));
  const secondDigest = hashCanonical(omit(second));
  return { match: firstDigest === secondDigest, firstDigest, secondDigest, state: firstDigest === secondDigest ? 'REPLAY_MATCH' : 'REPLAY_DIFFERENCE' };
}

export function createReceiptChain(options) { return new ReceiptChain(options); }
