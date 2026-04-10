/**
 * @file Gate Module Tests
 * @description Chicago TDD — behavior verification for proof gates
 */

import { describe, it, expect } from 'vitest';
import { ProofGate, createGate, runGate, runGates } from '../../src/gate/index.mjs';
import { GateError } from '../../src/error.mjs';

describe('ProofGate', () => {
  it('stores name, predicate, and severity', () => {
    const pred = () => true;
    const gate = new ProofGate('schema-valid', pred, 'critical');
    expect(gate.name).toBe('schema-valid');
    expect(gate.predicate).toBe(pred);
    expect(gate.severity).toBe('critical');
  });

  it('defaults severity to major', () => {
    const gate = new ProofGate('my-gate', () => true);
    expect(gate.severity).toBe('major');
  });

  it('run returns passed:true when predicate returns true', async () => {
    const gate = new ProofGate('valid', () => true);
    const result = await gate.run({ type: 'powl', content: 'data' });
    expect(result.passed).toBe(true);
    expect(result.gate).toBe('valid');
  });

  it('run returns passed:false when predicate returns false', async () => {
    const gate = new ProofGate('has-content', artifact => artifact.content !== '');
    const result = await gate.run({ type: 'powl', content: '' });
    expect(result.passed).toBe(false);
    expect(result.gate).toBe('has-content');
  });

  it('run supports async predicates', async () => {
    const gate = new ProofGate('async-gate', async () => true);
    const result = await gate.run({});
    expect(result.passed).toBe(true);
  });

  it('run result includes severity', async () => {
    const gate = new ProofGate('critical-check', () => false, 'critical');
    const result = await gate.run({});
    expect(result.severity).toBe('critical');
  });
});

describe('createGate', () => {
  it('creates a ProofGate with given arguments', () => {
    const gate = createGate('receipt-valid', () => true, 'minor');
    expect(gate).toBeInstanceOf(ProofGate);
    expect(gate.name).toBe('receipt-valid');
    expect(gate.severity).toBe('minor');
  });

  it('defaults severity to major', () => {
    const gate = createGate('g', () => true);
    expect(gate.severity).toBe('major');
  });
});

describe('runGate', () => {
  it('delegates to gate.run', async () => {
    const gate = createGate('test', artifact => artifact.ok === true);
    const result = await runGate(gate, { ok: true });
    expect(result.passed).toBe(true);
    expect(result.gate).toBe('test');
  });

  it('returns passed:false for failing predicate', async () => {
    const gate = createGate('test', () => false);
    const result = await runGate(gate, {});
    expect(result.passed).toBe(false);
  });
});

describe('runGates', () => {
  it('returns all results when all gates pass', async () => {
    const gates = [
      createGate('g1', () => true),
      createGate('g2', () => true),
    ];
    const results = await runGates(gates, { type: 'powl', content: 'x' });
    expect(results).toHaveLength(2);
    expect(results.every(r => r.passed)).toBe(true);
  });

  it('throws GateError when any gate fails', async () => {
    const gates = [
      createGate('g1', () => true),
      createGate('g2', () => false),
    ];
    await expect(runGates(gates, {})).rejects.toBeInstanceOf(GateError);
  });

  it('GateError message names the failing gate', async () => {
    const gates = [createGate('schema-valid', () => false)];
    try {
      await runGates(gates, {});
      expect.fail('should have thrown');
    } catch (err) {
      expect(err.message).toContain('schema-valid');
    }
  });

  it('throws GateError listing all failing gates', async () => {
    const gates = [
      createGate('gate-a', () => false),
      createGate('gate-b', () => false),
      createGate('gate-c', () => true),
    ];
    try {
      await runGates(gates, {});
      expect.fail('should have thrown');
    } catch (err) {
      expect(err.message).toContain('gate-a');
      expect(err.message).toContain('gate-b');
    }
  });

  it('runs gates on empty array without error', async () => {
    const results = await runGates([], {});
    expect(results).toHaveLength(0);
  });
});
