/**
 * @file Gate Module Tests — Chicago TDD
 * @description Behavior verification for proof gates
 *
 * Core Chicago TDD tests focused on gate bypass detection:
 * - Gate evaluates correctly (true/false)
 * - Async predicates supported
 * - Severity preserved in result
 */

import { describe, it, expect } from 'vitest';
import { ProofGate, createGate } from '../../src/gate/index.mjs';

describe('ProofGate — Gate Bypass Detection', () => {

  it('returns passed:true when predicate returns true', async () => {
    const gate = new ProofGate('valid', () => true);
    const result = await gate.run({ type: 'powl', content: 'data' });
    expect(result.passed).toBe(true);
    expect(result.gate).toBe('valid');
  });

  it('returns passed:false when predicate returns false', async () => {
    const gate = new ProofGate('has-content', artifact => artifact.content !== '');
    const result = await gate.run({ type: 'powl', content: '' });
    expect(result.passed).toBe(false);
    expect(result.gate).toBe('has-content');
  });

  it('supports async predicates', async () => {
    const gate = new ProofGate('async-gate', async () => true);
    const result = await gate.run({});
    expect(result.passed).toBe(true);
  });

  it('preserves severity in result', async () => {
    const gate = new ProofGate('critical-check', () => false, 'critical');
    const result = await gate.run({});
    expect(result.severity).toBe('critical');
  });

  it('defaults severity to major', () => {
    const gate = new ProofGate('my-gate', () => true);
    expect(gate.severity).toBe('major');
  });
});
