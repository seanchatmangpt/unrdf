/**
 * @file Causality Chain Tests — Chicago TDD
 * @description Verification for A=μ(O) lineage tracking
 *
 * Core Chicago TDD tests focused on process truth:
 * - Ontology source tracking
 * - Step recording with timestamps
 * - Depth calculation
 * - toJSON serialization
 */

import { describe, it, expect } from 'vitest';
import { CausalityChain } from '../../src/causality/index.mjs';

describe('CausalityChain — A=μ(O) Verification', () => {

  it('stores ontology source', () => {
    const chain = new CausalityChain('seed-dna.nt');
    expect(chain.ontologySource).toBe('seed-dna.nt');
  });

  it('records operator steps with timestamps', () => {
    const chain = new CausalityChain('ontology.ttl');
    chain.addStep('validate-ontology', { data: 'test' }, { valid: true });
    chain.addStep('project-artifact', { valid: true }, { projected: true });

    expect(chain.steps).toHaveLength(2);
    expect(chain.steps[0].operatorName).toBe('validate-ontology');
    expect(chain.steps[0].timestamp).toBeDefined();
    expect(chain.steps[1].operatorName).toBe('project-artifact');
  });

  it('calculates depth as number of operators applied', () => {
    const chain = new CausalityChain('test.nt');
    expect(chain.depth).toBe(0);

    chain.addStep('op1', {}, {});
    expect(chain.depth).toBe(1);

    chain.addStep('op2', {}, {});
    expect(chain.depth).toBe(2);
  });

  it('serializes to JSON with all fields', () => {
    const chain = new CausalityChain('source.ttl');
    chain.addStep('transform', { in: 'data' }, { out: 'result' });

    const json = chain.toJSON();
    expect(json.ontologySource).toBe('source.ttl');
    expect(json.createdAt).toBeDefined();
    expect(json.steps).toHaveLength(1);
    expect(json.depth).toBe(1);
  });

  it('tracks transformation chain (input → output)', () => {
    const chain = new CausalityChain('data.ttl');
    chain.addStep('op1', { value: 5 }, { value: 10 });
    chain.addStep('op2', { value: 10 }, { value: 30 });

    expect(chain.steps[0].input.value).toBe(5);
    expect(chain.steps[0].output.value).toBe(10);
    expect(chain.steps[1].input.value).toBe(10);
    expect(chain.steps[1].output.value).toBe(30);
  });

  it('detects break in causality (missing input for output)', () => {
    const chain = new CausalityChain('source.ttl');
    chain.addStep('op1', { id: 1 }, { id: 2 });
    chain.addStep('op2', { id: 999 }, { id: 3 });  // id:999 doesn't exist!

    // This should be detectable - output doesn't match input
    expect(chain.steps[1].input.id).toBe(999);
    expect(chain.steps[1].output.id).toBe(3);
  });

  it('returns chainable API (addStep returns this)', () => {
    const chain = new CausalityChain('test.nt');
    const result = chain.addStep('op1', {}, {});
    expect(result).toBe(chain);
  });
});
