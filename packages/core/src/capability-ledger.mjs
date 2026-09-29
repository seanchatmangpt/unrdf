/**
 * Deterministic capability ledger with explicit standing and disposition law.
 */
import { createHash } from 'node:crypto';

export const Standing = Object.freeze({
  UNKNOWN: 'UNKNOWN',
  PARTIAL_ALIVE: 'PARTIAL_ALIVE',
  ALIVE: 'ALIVE',
  BLOCKED: 'BLOCKED',
  BUILD_BROKEN: 'BUILD_BROKEN',
  UNSUPPORTED: 'UNSUPPORTED',
});

export const Disposition = Object.freeze({
  PRESERVED: 'PRESERVED',
  SUBSUMED: 'SUBSUMED',
  REPLACED: 'REPLACED',
  ARCHIVED: 'ARCHIVED',
  REFUSED: 'REFUSED',
});

const transitions = new Map([
  [Standing.UNKNOWN, new Set([Standing.UNKNOWN, Standing.PARTIAL_ALIVE, Standing.BLOCKED, Standing.BUILD_BROKEN, Standing.UNSUPPORTED])],
  [Standing.PARTIAL_ALIVE, new Set([Standing.PARTIAL_ALIVE, Standing.ALIVE, Standing.BLOCKED, Standing.BUILD_BROKEN])],
  [Standing.ALIVE, new Set([Standing.ALIVE, Standing.PARTIAL_ALIVE, Standing.BLOCKED, Standing.BUILD_BROKEN])],
  [Standing.BLOCKED, new Set([Standing.BLOCKED, Standing.UNKNOWN, Standing.PARTIAL_ALIVE, Standing.BUILD_BROKEN, Standing.UNSUPPORTED])],
  [Standing.BUILD_BROKEN, new Set([Standing.BUILD_BROKEN, Standing.UNKNOWN, Standing.PARTIAL_ALIVE, Standing.BLOCKED])],
  [Standing.UNSUPPORTED, new Set([Standing.UNSUPPORTED, Standing.UNKNOWN])],
]);

function canonical(value) {
  if (Array.isArray(value)) return value.map(canonical);
  if (value && typeof value === 'object') {
    return Object.fromEntries(Object.keys(value).sort().map(key => [key, canonical(value[key])]));
  }
  return value;
}

function digest(value) {
  return createHash('sha256').update(JSON.stringify(canonical(value))).digest('hex');
}

function assertText(value, name) {
  if (typeof value !== 'string' || value.trim() === '') throw new TypeError(`${name} must be a non-empty string`);
}

/**
 * Ledger of capabilities with standing, disposition, evidence and an append-only history.
 */
export class CapabilityLedger {
  #entries = new Map();
  #history = [];

  /**
   * Create a ledger.
   * @param {Object} options - Ledger identity.
   * @param {string} options.subject - Non-empty subject the ledger describes.
   * @param {string|null} [options.source] - Source of the assessment.
   * @param {string|null} [options.authority] - Authority issuing the ledger.
   * @throws {TypeError} If `subject` is not a non-empty string.
   */
  constructor({ subject, source = null, authority = null } = {}) {
    assertText(subject, 'subject');
    this.subject = subject;
    this.source = source;
    this.authority = authority;
  }

  /**
   * Admit a new capability into the ledger.
   * @param {Object} capability - Capability definition (id, owner, contract, verifier, falsifier, disposition, standing, exclusions, metadata).
   * @returns {Object} A copy of the stored entry.
   * @throws {TypeError} If required fields, standing or disposition are invalid.
   * @throws {Error} If the id is already admitted.
   */
  admit(capability) {
    const { id, owner, contract, verifier, falsifier, disposition = null, standing = Standing.UNKNOWN } = capability ?? {};
    assertText(id, 'capability.id');
    assertText(owner, 'capability.owner');
    assertText(contract, 'capability.contract');
    if (!Object.values(Standing).includes(standing)) throw new TypeError(`invalid standing: ${standing}`);
    if (disposition !== null && !Object.values(Disposition).includes(disposition)) throw new TypeError(`invalid disposition: ${disposition}`);
    if (this.#entries.has(id)) throw new Error(`CAPABILITY_DUPLICATE:${id}`);
    const entry = {
      id, owner, contract,
      verifier: verifier ?? null,
      falsifier: falsifier ?? null,
      disposition,
      standing,
      evidence: [],
      exclusions: [...(capability.exclusions ?? [])],
      metadata: canonical(capability.metadata ?? {}),
    };
    this.#entries.set(id, entry);
    this.#record('ADMIT', id, { standing, disposition });
    return structuredClone(entry);
  }

  /**
   * Move a capability to a new standing according to the transition table. ALIVE requires a verifier, falsifier and evidence.
   * @param {string} id - Capability id.
   * @param {string} standing - Target Standing value.
   * @param {Object|null} [evidence] - Evidence recorded with the transition.
   * @returns {Object} A copy of the updated entry.
   * @throws {Error} If the capability is unknown, the transition is illegal, or ALIVE prerequisites are missing.
   * @throws {TypeError} If the standing is invalid.
   */
  transition(id, standing, evidence = null) {
    const entry = this.#require(id);
    if (!Object.values(Standing).includes(standing)) throw new TypeError(`invalid standing: ${standing}`);
    if (!transitions.get(entry.standing)?.has(standing)) {
      throw new Error(`ILLEGAL_STANDING_TRANSITION:${entry.standing}->${standing}:${id}`);
    }
    if (standing === Standing.ALIVE) {
      if (!entry.verifier) throw new Error(`ALIVE_WITHOUT_VERIFIER:${id}`);
      if (!entry.falsifier) throw new Error(`ALIVE_WITHOUT_FALSIFIER:${id}`);
      if (!evidence) throw new Error(`ALIVE_WITHOUT_EVIDENCE:${id}`);
    }
    const previous = entry.standing;
    entry.standing = standing;
    if (evidence) entry.evidence.push(canonical(evidence));
    this.#record('TRANSITION', id, { previous, standing, evidence: evidence ? canonical(evidence) : null });
    return structuredClone(entry);
  }

  /**
   * Set a capability's disposition with a rationale. REFUSED requires a falsifier.
   * @param {string} id - Capability id.
   * @param {string} disposition - A Disposition value.
   * @param {string} rationale - Non-empty explanation.
   * @returns {Object} A copy of the updated entry.
   * @throws {Error} If the capability is unknown or REFUSED lacks a falsifier.
   * @throws {TypeError} If the disposition or rationale is invalid.
   */
  setDisposition(id, disposition, rationale) {
    const entry = this.#require(id);
    if (!Object.values(Disposition).includes(disposition)) throw new TypeError(`invalid disposition: ${disposition}`);
    assertText(rationale, 'rationale');
    if (disposition === Disposition.REFUSED && !entry.falsifier) throw new Error(`REFUSAL_WITHOUT_FALSIFIER:${id}`);
    entry.disposition = disposition;
    entry.dispositionRationale = rationale;
    this.#record('DISPOSITION', id, { disposition, rationale });
    return structuredClone(entry);
  }

  /**
   * Attach an evidence object to a capability.
   * @param {string} id - Capability id.
   * @param {Object} evidence - Evidence to record.
   * @returns {Object} A copy of the updated entry.
   * @throws {TypeError} If evidence is not an object.
   * @throws {Error} If the capability is unknown.
   */
  attachEvidence(id, evidence) {
    if (!evidence || typeof evidence !== 'object') throw new TypeError('evidence must be an object');
    const entry = this.#require(id);
    entry.evidence.push(canonical(evidence));
    this.#record('EVIDENCE', id, canonical(evidence));
    return structuredClone(entry);
  }

  /**
   * Get a capability entry.
   * @param {string} id - Capability id.
   * @returns {Object} A copy of the entry.
   * @throws {Error} If the capability is unknown.
   */
  get(id) { return structuredClone(this.#require(id)); }
  /**
   * List all entries sorted by id.
   * @returns {Object[]} Copies of the entries.
   */
  list() { return [...this.#entries.values()].sort((a,b) => a.id.localeCompare(b.id)).map(entry => structuredClone(entry)); }

  /**
   * Count entries by standing and disposition and count missing verifiers, falsifiers and dispositions.
   * @returns {Object} Summary counts.
   */
  summary() {
    const byStanding = Object.fromEntries(Object.values(Standing).map(x => [x, 0]));
    const byDisposition = Object.fromEntries(Object.values(Disposition).map(x => [x, 0]));
    let missingVerifier = 0, missingFalsifier = 0, missingDisposition = 0;
    for (const entry of this.#entries.values()) {
      byStanding[entry.standing]++;
      if (entry.disposition) byDisposition[entry.disposition]++; else missingDisposition++;
      if (!entry.verifier) missingVerifier++;
      if (!entry.falsifier) missingFalsifier++;
    }
    return { count: this.#entries.size, byStanding, byDisposition, missingVerifier, missingFalsifier, missingDisposition };
  }

  /**
   * Assess the ledger overall: ALIVE only when no blocking reasons remain, otherwise PARTIAL_ALIVE.
   * @returns {{standing: string, reasons: string[], summary: Object, digest: string}} Verdict with reasons and ledger digest.
   */
  crown() {
    const summary = this.summary();
    const reasons = [];
    if (summary.byStanding.UNKNOWN) reasons.push('UNKNOWN_CAPABILITIES');
    if (summary.byStanding.BLOCKED) reasons.push('BLOCKED_CAPABILITIES');
    if (summary.byStanding.BUILD_BROKEN) reasons.push('BUILD_BROKEN_CAPABILITIES');
    if (summary.byStanding.PARTIAL_ALIVE) reasons.push('PARTIAL_CAPABILITIES');
    if (summary.missingVerifier) reasons.push('MISSING_VERIFIERS');
    if (summary.missingFalsifier) reasons.push('MISSING_FALSIFIERS');
    if (summary.missingDisposition) reasons.push('MISSING_DISPOSITIONS');
    return { standing: reasons.length === 0 ? Standing.ALIVE : Standing.PARTIAL_ALIVE, reasons, summary, digest: this.digest() };
  }

  /**
   * Serialize the ledger canonically (identity, entries and history).
   * @returns {Object} Canonical JSON-safe object.
   */
  toJSON() {
    return canonical({ schema: 'unrdf.capability-ledger/1', subject: this.subject, source: this.source, authority: this.authority, entries: this.list(), history: this.#history });
  }
  /**
   * SHA-256 digest of the canonical serialization.
   * @returns {string} Hex digest.
   */
  digest() { return digest(this.toJSON()); }

  #require(id) { const entry = this.#entries.get(id); if (!entry) throw new Error(`CAPABILITY_NOT_FOUND:${id}`); return entry; }
  #record(type, capability, detail) { this.#history.push({ sequence: this.#history.length + 1, type, capability, detail: canonical(detail) }); }
}

/**
 * Create a CapabilityLedger.
 * @param {Object} options - Options forwarded to the CapabilityLedger constructor.
 * @returns {CapabilityLedger} A new ledger.
 */
export function createCapabilityLedger(options) { return new CapabilityLedger(options); }
