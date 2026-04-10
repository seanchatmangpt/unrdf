/**
 * @file Proof Gate Module
 * @module manufacturing/gate
 * @description Proof gate logic for validating manufactured artifacts (A = μ(O))
 */

import { GateError } from '../error.mjs';

/**
 * A named predicate gate that validates an artifact.
 */
export class ProofGate {
  /**
   * @param {string} name - Gate identifier (e.g., 'schema-valid')
   * @param {Function} predicate - (artifact) => boolean | Promise<boolean>
   * @param {'critical'|'major'|'minor'} severity - Failure severity
   */
  constructor(name, predicate, severity = 'major') {
    this.name = name;
    this.predicate = predicate;
    this.severity = severity;
  }

  /**
   * Run the gate against an artifact.
   * @param {object} artifact
   * @returns {Promise<{ passed: boolean, gate: string, severity: string }>}
   */
  async run(artifact) {
    const passed = Boolean(await this.predicate(artifact));
    return { passed, gate: this.name, severity: this.severity };
  }
}

/**
 * Factory for creating a ProofGate.
 * @param {string} name
 * @param {Function} predicate
 * @param {'critical'|'major'|'minor'} [severity='major']
 * @returns {ProofGate}
 */
export function createGate(name, predicate, severity = 'major') {
  return new ProofGate(name, predicate, severity);
}

/**
 * Run a single gate against an artifact.
 * @param {ProofGate} gate
 * @param {object} artifact
 * @returns {Promise<{ passed: boolean, gate: string, severity: string }>}
 */
export function runGate(gate, artifact) {
  return gate.run(artifact);
}

/**
 * Run all gates against an artifact. Throws GateError if any fail.
 * @param {ProofGate[]} gates
 * @param {object} artifact
 * @returns {Promise<Array<{ passed: boolean, gate: string, severity: string }>>}
 * @throws {GateError} if one or more gates fail
 */
export async function runGates(gates, artifact) {
  const results = await Promise.all(gates.map(g => g.run(artifact)));
  const failed = results.filter(r => !r.passed);

  if (failed.length > 0) {
    const names = failed.map(r => r.gate).join(', ');
    const severity = failed[0].severity;
    throw new GateError(names, `${failed.length} gate(s) failed: ${names}`, severity);
  }

  return results;
}
