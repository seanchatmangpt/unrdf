/**
 * @file Causality Module Tests
 * @description Chicago TDD — behavior verification for A=μ(O) lineage tracking
 */

import { describe, it, expect } from 'vitest';
import { CausalityChain } from '../../src/causality/index.mjs';

describe('CausalityChain', () => {
  it('stores the ontology source', () => {
    const chain = new CausalityChain('seed-dna.nt');
    expect(chain.ontologySource).toBe('seed-dna.nt');
  });

  it('starts with zero steps', () => {
    const chain = new CausalityChain('onto.nt');
    expect(chain.steps).toHaveLength(0);
    expect(chain.depth).toBe(0);
  });

  it('records a step with operator, input, and output', () => {
    const chain = new CausalityChain('onto.nt');
    chain.addStep('validate', { data: 'x' }, { valid: true });
    expect(chain.steps).toHaveLength(1);
    expect(chain.steps[0].operatorName).toBe('validate');
    expect(chain.steps[0].input).toEqual({ data: 'x' });
    expect(chain.steps[0].output).toEqual({ valid: true });
  });

  it('each step has a timestamp', () => {
    const chain = new CausalityChain('onto.nt');
    chain.addStep('transform', {}, {});
    expect(chain.steps[0].timestamp).toBeDefined();
    expect(typeof chain.steps[0].timestamp).toBe('string');
  });

  it('depth reflects number of steps added', () => {
    const chain = new CausalityChain('onto.nt');
    chain.addStep('validate', {}, {});
    chain.addStep('transform', {}, {});
    chain.addStep('enrich', {}, {});
    expect(chain.depth).toBe(3);
  });

  it('addStep is chainable', () => {
    const chain = new CausalityChain('onto.nt');
    const returned = chain.addStep('validate', {}, {});
    expect(returned).toBe(chain);
  });

  it('allows chaining multiple addStep calls', () => {
    const chain = new CausalityChain('onto.nt');
    chain.addStep('validate', {}, {}).addStep('transform', {}, {});
    expect(chain.depth).toBe(2);
  });

  it('toJSON includes ontologySource and steps', () => {
    const chain = new CausalityChain('seed.nt');
    chain.addStep('validate', { data: 1 }, { valid: true });
    const json = chain.toJSON();
    expect(json.ontologySource).toBe('seed.nt');
    expect(json.steps).toHaveLength(1);
    expect(json.depth).toBe(1);
    expect(json.createdAt).toBeDefined();
  });

  it('toJSON steps include operator name', () => {
    const chain = new CausalityChain('onto.nt');
    chain.addStep('enrich', {}, {});
    const json = chain.toJSON();
    expect(json.steps[0].operatorName).toBe('enrich');
  });

  it('records createdAt timestamp on construction', () => {
    const chain = new CausalityChain('onto.nt');
    expect(chain.createdAt).toBeDefined();
    expect(typeof chain.createdAt).toBe('string');
  });

  it('steps are independent between chain instances', () => {
    const chain1 = new CausalityChain('a.nt');
    const chain2 = new CausalityChain('b.nt');
    chain1.addStep('validate', {}, {});
    expect(chain2.depth).toBe(0);
  });
});
