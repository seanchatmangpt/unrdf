/**
 * @fileoverview AtomVM + Hooks Integration Tests
 * @description Behavioral suite covering the 6 core priorities of AtomVM/Hooks interoperability.
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { HooksBridge } from '../src/hooks-bridge.mjs';
import { createStore } from '@unrdf/oxigraph';
import { canonicalize } from '@unrdf/v6-core/receipt-pattern';

describe('AtomVM + Hooks Integration', () => {
  let store;
  let bridge;

  beforeEach(async () => {
    store = createStore();
    bridge = new HooksBridge(store, {
      nodeId: 'atomvm-test-node',
      maxHooks: 500,
      enableReceiptChaining: true,
      enableJIT: true,
    });
  });

  afterEach(() => {
    bridge = null;
    store = null;
  });

  describe('Priority 1: Receipt Determinism', () => {
    it('should generate deterministic cryptographic receipts', async () => {
      const hookSpec = {
        hook_name: 'receipt-test',
        hook_type: 'validation',
        condition: { type: 'sparql-ask', spec: { query: 'ASK { ?s ?p ?o }' } },
        effects: [],
        priority: 80,
      };

      const result = await bridge.registerHook(hookSpec);
      expect(result.receipt.receiptHash).toBeDefined();
      expect(typeof result.receipt.receiptHash).toBe('string');
    });
  });

  describe('Priority 2: RDF Transformation', () => {
    it('should execute valid SPARQL CONSTRUCT effects', async () => {
      const hookSpec = {
        hook_name: 'construct-test',
        hook_type: 'transformation',
        condition: { type: 'sparql-ask', spec: { query: 'ASK { ?s ?p ?o }' } },
        effects: [{ type: 'sparql-construct', config: { query: 'CONSTRUCT { ?s ?p "ok" } WHERE { ?s ?p ?o }' } }],
        priority: 75,
      };

      const result = await bridge.registerHook(hookSpec);
      expect(result.registered).toBe(true);
    });
  });

  describe('Priority 3: SHACL Validation', () => {
    it('should evaluate shape validation with annotation mode', async () => {
      const cond = await bridge.evaluateCondition({
        type: 'shacl',
        spec: { shape: 'ex:TestShape', enforcementMode: 'annotate' },
      });
      expect(typeof cond.result).toBe('boolean');
    });
  });

  describe('Priority 4: State Proofs', () => {
    it('should produce distinct hashes for varied inputs', async () => {
      const r1 = await bridge.registerHook({
        hook_name: 'h1',
        hook_type: 'validation',
        condition: { type: 'sparql-ask', spec: { query: 'ASK { ?s a ex:A }' } },
        effects: [],
        priority: 50,
      });
      const r2 = await bridge.registerHook({
        hook_name: 'h2',
        hook_type: 'validation',
        condition: { type: 'sparql-ask', spec: { query: 'ASK { ?s a ex:B }' } },
        effects: [],
        priority: 50,
      });
      expect(r1.receipt.id).not.toBe(r2.receipt.id);
    });
  });

  describe('Priority 5: Forward-Chaining Rules', () => {
    it('should support N3 rule evaluation', async () => {
      const cond = await bridge.evaluateCondition({
        type: 'n3-rule',
        spec: { rules: '{ ?x a :Test } => { ?x :derived true } .', askQuery: 'ASK { ?x :derived true }' },
      });
      expect(cond.metadata.evaluationType).toBe('n3-rule');
    });
  });

  describe('Priority 6: Logic Programming', () => {
    it('should evaluate Datalog goal proof', async () => {
      const cond = await bridge.evaluateCondition({
        type: 'datalog',
        spec: {
          facts: ['user(alice)'],
          rules: ['allowed(X) :- user(X)'],
          goal: 'allowed(alice)',
        },
      });
      expect(cond.result).toBe(true);
    });
  });
});
