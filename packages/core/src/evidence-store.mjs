/** Content-addressed evidence store. */
import { hashCanonical, canonicalizeJSON } from './receipt-chain.mjs';

export class EvidenceStore {
  #records = new Map();
  #byClaim = new Map();
  #bySubject = new Map();

  add(record) {
    const normalized = canonicalizeJSON(record ?? {});
    if (!normalized.claim || !normalized.subject || !normalized.source) throw new TypeError('evidence requires claim, subject, and source');
    const digest = hashCanonical(normalized);
    const stored = Object.freeze({ ...normalized, digest });
    if (!this.#records.has(digest)) {
      this.#records.set(digest, stored);
      this.#index(this.#byClaim, stored.claim, digest);
      this.#index(this.#bySubject, stored.subject, digest);
    }
    return structuredClone(stored);
  }

  get(digest) {
    const record = this.#records.get(digest);
    return record ? structuredClone(record) : null;
  }

  find({ claim = null, subject = null, state = null } = {}) {
    let digests = new Set(this.#records.keys());
    if (claim) digests = this.#intersect(digests, this.#byClaim.get(claim) ?? new Set());
    if (subject) digests = this.#intersect(digests, this.#bySubject.get(subject) ?? new Set());
    return [...digests]
      .map(digest => this.#records.get(digest))
      .filter(record => state === null || record.state === state)
      .sort((a, b) => a.digest.localeCompare(b.digest))
      .map(record => structuredClone(record));
  }

  verify() {
    const failures = [];
    for (const [digest, record] of this.#records) {
      const { digest: ignored, ...body } = record;
      if (hashCanonical(body) !== digest) failures.push({ digest, code: 'EVIDENCE_DIGEST_MISMATCH' });
    }
    return { valid: failures.length === 0, count: this.#records.size, failures, root: this.root() };
  }

  root() { return hashCanonical([...this.#records.keys()].sort()); }
  export() { return canonicalizeJSON({ schema: 'unrdf.evidence-store/1', records: [...this.#records.values()], verification: this.verify() }); }
  #index(index, key, digest) { if (!index.has(key)) index.set(key, new Set()); index.get(key).add(digest); }
  #intersect(left, right) { return new Set([...left].filter(value => right.has(value))); }
}

export function createEvidenceStore() { return new EvidenceStore(); }
