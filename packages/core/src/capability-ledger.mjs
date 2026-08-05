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

export class CapabilityLedger {
  #entries = new Map();
  #history = [];

  constructor({ subject, source = null, authority = null } = {}) {
    assertText(subject, 'subject');
    this.subject = subject;
    this.source = source;
    this.authority = authority;
  }

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

  attachEvidence(id, evidence) {
    if (!evidence || typeof evidence !== 'object') throw new TypeError('evidence must be an object');
    const entry = this.#require(id);
    entry.evidence.push(canonical(evidence));
    this.#record('EVIDENCE', id, canonical(evidence));
    return structuredClone(entry);
  }

  get(id) { return structuredClone(this.#require(id)); }
  list() { return [...this.#entries.values()].sort((a,b) => a.id.localeCompare(b.id)).map(entry => structuredClone(entry)); }

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

  toJSON() {
    return canonical({ schema: 'unrdf.capability-ledger/1', subject: this.subject, source: this.source, authority: this.authority, entries: this.list(), history: this.#history });
  }
  digest() { return digest(this.toJSON()); }

  #require(id) { const entry = this.#entries.get(id); if (!entry) throw new Error(`CAPABILITY_NOT_FOUND:${id}`); return entry; }
  #record(type, capability, detail) { this.#history.push({ sequence: this.#history.length + 1, type, capability, detail: canonical(detail) }); }
}

export function createCapabilityLedger(options) { return new CapabilityLedger(options); }
