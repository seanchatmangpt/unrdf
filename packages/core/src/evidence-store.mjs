/** Content-addressed evidence store. */
import { hashCanonical, canonicalizeJSON } from './receipt-chain.mjs';

/**
 * Store of evidence records keyed by digest, indexed by claim and subject.
 */
export class EvidenceStore {
  #records = new Map();
  #byClaim = new Map();
  #bySubject = new Map();

  /**
   * Add an evidence record; identical records are stored once.
   * @param {Object} record - Evidence with claim, subject and source (plus any other fields).
   * @returns {Object} A copy of the stored, digested record.
   * @throws {TypeError} If claim, subject or source is missing.
   */
  add(record) {
    const normalized = canonicalizeJSON(record ?? {});
    if (!normalized.claim || !normalized.subject || !normalized.source)
      throw new TypeError('evidence requires claim, subject, and source');
    const digest = hashCanonical(normalized);
    const stored = Object.freeze({ ...normalized, digest });
    if (!this.#records.has(digest)) {
      this.#records.set(digest, stored);
      this.#index(this.#byClaim, stored.claim, digest);
      this.#index(this.#bySubject, stored.subject, digest);
    }
    return structuredClone(stored);
  }

  /**
   * Fetch a record by digest.
   * @param {string} digest - Record digest.
   * @returns {Object|null} A copy of the record, or null.
   */
  get(digest) {
    const record = this.#records.get(digest);
    return record ? structuredClone(record) : null;
  }

  /**
   * Find records matching all given criteria.
   * @param {Object} [criteria] - Criteria.
   * @param {string|null} [criteria.claim] - Claim to match.
   * @param {string|null} [criteria.subject] - Subject to match.
   * @param {string|null} [criteria.state] - State to match.
   * @returns {Object[]} Copies of matching records sorted by digest.
   */
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

  /**
   * Recompute each record's digest.
   * @returns {{valid: boolean, count: number, failures: Object[], root: string}} Verification result.
   */
  verify() {
    const failures = [];
    for (const [digest, record] of this.#records) {
      const { digest: _ignored, ...body } = record;
      if (hashCanonical(body) !== digest)
        failures.push({ digest, code: 'EVIDENCE_DIGEST_MISMATCH' });
    }
    return { valid: failures.length === 0, count: this.#records.size, failures, root: this.root() };
  }

  /**
   * Hash of the sorted record digests.
   * @returns {string} Hex digest.
   */
  root() {
    return hashCanonical([...this.#records.keys()].sort());
  }
  /**
   * Export all records with a verification result.
   * @returns {Object} Canonical export object.
   */
  export() {
    return canonicalizeJSON({
      schema: 'unrdf.evidence-store/1',
      records: [...this.#records.values()],
      verification: this.verify(),
    });
  }
  #index(index, key, digest) {
    if (!index.has(key)) index.set(key, new Set());
    index.get(key).add(digest);
  }
  #intersect(left, right) {
    return new Set([...left].filter(value => right.has(value)));
  }
}

/**
 * Create an empty evidence store.
 * @returns {EvidenceStore} A new store.
 */
export function createEvidenceStore() {
  return new EvidenceStore();
}
