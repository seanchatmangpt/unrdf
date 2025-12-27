/**
 * @file Policy Engine Tests
 * @module @unrdf/fusion/test/policy-engine
 *
 * Tests unified policy/hooks/conditions API:
 * 1. SPARQL condition evaluation (pass/fail)
 * 2. Hook execution on policy decisions
 * 3. Deterministic receipt emission
 */

import { describe, it, expect } from 'vitest';
import { createPolicyRegistry } from '../src/policy-engine.mjs';
import { createStore, dataFactory } from '@unrdf/oxigraph';
import { defineHook } from '@unrdf/hooks';

describe('Policy Engine - Unified API', () => {
  describe('SPARQL Condition Evaluation', () => {
    it('should evaluate SPARQL ASK condition - PASS case', async () => {
      const registry = await createPolicyRegistry();
      const store = createStore();

      // Add test data
      const foaf = 'http://xmlns.com/foaf/0.1/';
      store.add(
        dataFactory.quad(
          dataFactory.namedNode('http://example.org/alice'),
          dataFactory.namedNode(`${foaf}age`),
          dataFactory.literal('25', dataFactory.namedNode('http://www.w3.org/2001/XMLSchema#integer'))
        )
      );

      // Register policy with age >= 18 condition
      registry.registerPolicy({
        name: 'AdultOnly',
        conditions: [
          {
            sparql: `
              PREFIX foaf: <http://xmlns.com/foaf/0.1/>
              ASK {
                <http://example.org/alice> foaf:age ?age .
                FILTER (?age >= 18)
              }
            `,
          },
        ],
        actions: [],
      });

      // Evaluate
      const result = await registry.evaluatePolicy(
        store,
        { subject: 'http://example.org/alice' },
        'AdultOnly'
      );

      expect(result.decision).toBe('allow');
      expect(result.conditionResults).toEqual([true]);
      expect(result.policy).toBe('AdultOnly');
    });

    it('should evaluate SPARQL ASK condition - FAIL case', async () => {
      const registry = await createPolicyRegistry();
      const store = createStore();

      // Add test data - age < 18
      const foaf = 'http://xmlns.com/foaf/0.1/';
      store.add(
        dataFactory.quad(
          dataFactory.namedNode('http://example.org/bob'),
          dataFactory.namedNode(`${foaf}age`),
          dataFactory.literal('16', dataFactory.namedNode('http://www.w3.org/2001/XMLSchema#integer'))
        )
      );

      // Register policy with age >= 18 condition
      registry.registerPolicy({
        name: 'AdultOnly',
        conditions: [
          {
            sparql: `
              PREFIX foaf: <http://xmlns.com/foaf/0.1/>
              ASK {
                <http://example.org/bob> foaf:age ?age .
                FILTER (?age >= 18)
              }
            `,
          },
        ],
        actions: [],
      });

      // Evaluate
      const result = await registry.evaluatePolicy(
        store,
        { subject: 'http://example.org/bob' },
        'AdultOnly'
      );

      expect(result.decision).toBe('deny');
      expect(result.conditionResults).toEqual([false]);
      expect(result.message).toContain('Condition 1 failed');
    });

    it('should short-circuit on first failed condition', async () => {
      const registry = await createPolicyRegistry();
      const store = createStore();

      // Empty store - first condition will fail
      registry.registerPolicy({
        name: 'MultiCondition',
        conditions: [
          { sparql: 'ASK { ?s ?p ?o }' }, // Will fail on empty store
          { sparql: 'ASK { ?x ?y ?z }' }, // Should not execute
        ],
        actions: [],
      });

      const result = await registry.evaluatePolicy(store, {}, 'MultiCondition');

      expect(result.decision).toBe('deny');
      expect(result.conditionResults).toHaveLength(1); // Only first condition executed
    });
  });

  describe('Hook Execution on Decisions', () => {
    it('should execute hook action when conditions pass', async () => {
      const registry = await createPolicyRegistry();
      const store = createStore();

      let hookExecuted = false;
      let hookContext = null;

      // Define and register hook
      const auditHook = defineHook({
        id: 'auditLog',
        trigger: 'before-add',
        validate: (quad, options) => {
          hookExecuted = true;
          hookContext = options;
          return { valid: true };
        },
      });
      registry.registerHook('auditLog', auditHook);

      // Register policy
      registry.registerPolicy({
        name: 'AuditPolicy',
        conditions: [{ sparql: 'ASK { }' }], // Always passes (empty ASK)
        actions: [{ hook: 'auditLog', args: { event: 'test_audit' } }],
      });

      // Create test quad
      const testQuad = dataFactory.quad(
        dataFactory.namedNode('http://example.org/s'),
        dataFactory.namedNode('http://example.org/p'),
        dataFactory.literal('o')
      );

      // Evaluate
      const result = await registry.evaluatePolicy(store, testQuad, 'AuditPolicy');

      expect(result.decision).toBe('allow');
      expect(hookExecuted).toBe(true);
      expect(hookContext).toEqual({ event: 'test_audit' });
      expect(result.actionResults).toHaveLength(1);
      expect(result.actionResults[0].valid).toBe(true);
    });

    it('should deny when hook validation fails', async () => {
      const registry = await createPolicyRegistry();
      const store = createStore();

      // Hook that always fails validation
      const rejectHook = defineHook({
        id: 'rejectAll',
        trigger: 'before-add',
        validate: () => {
          return { valid: false };
        },
      });
      registry.registerHook('rejectAll', rejectHook);

      registry.registerPolicy({
        name: 'RejectPolicy',
        conditions: [], // No conditions
        actions: [{ hook: 'rejectAll' }],
      });

      const testQuad = dataFactory.quad(
        dataFactory.namedNode('http://example.org/s'),
        dataFactory.namedNode('http://example.org/p'),
        dataFactory.literal('o')
      );

      const result = await registry.evaluatePolicy(store, testQuad, 'RejectPolicy');

      expect(result.decision).toBe('deny');
      expect(result.message).toContain('validation failed');
    });

    it('should handle hook transformation', async () => {
      const registry = await createPolicyRegistry();
      const store = createStore();

      // Hook that transforms the quad
      const transformHook = defineHook({
        id: 'addPrefix',
        trigger: 'before-add',
        validate: () => ({ valid: true }),
        transform: (quad) => {
          return dataFactory.quad(
            dataFactory.namedNode('http://transformed.org/' + quad.subject.value),
            quad.predicate,
            quad.object
          );
        },
      });
      registry.registerHook('addPrefix', transformHook);

      registry.registerPolicy({
        name: 'TransformPolicy',
        conditions: [],
        actions: [{ hook: 'addPrefix' }],
      });

      const testQuad = dataFactory.quad(
        dataFactory.namedNode('http://example.org/s'),
        dataFactory.namedNode('http://example.org/p'),
        dataFactory.literal('o')
      );

      const result = await registry.evaluatePolicy(store, testQuad, 'TransformPolicy');

      expect(result.decision).toBe('allow');
      expect(result.actionResults[0].quad).toBeDefined();
      expect(result.actionResults[0].quad.subject.value).toContain('transformed.org');
    });
  });

  describe('Deterministic Receipt Emission', () => {
    it('should emit identical receipts for identical decisions', async () => {
      const registry = await createPolicyRegistry();
      const store = createStore();

      registry.registerPolicy({
        name: 'DeterministicPolicy',
        conditions: [],
        actions: [],
      });

      const resource = { id: 'test-resource', value: 42 };

      // First decision
      const decision1 = await registry.routeDecision(store, resource, 'DeterministicPolicy');
      const receipt1 = decision1.receipt;

      // Second decision with same inputs (but different timestamp)
      // Need to ensure same timestamp for true determinism
      const decision2 = await registry.evaluatePolicy(store, resource, 'DeterministicPolicy');
      decision2.timestamp = decision1.timestamp; // Force same timestamp
      const receipt2 = await registry.emitPolicyReceipt(decision2, resource);

      expect(receipt1.hash).toBe(receipt2.hash);
      expect(receipt1.canonical).toBe(receipt2.canonical);
    });

    it('should produce different hashes for different decisions', async () => {
      const registry = await createPolicyRegistry();
      const store = createStore();

      registry.registerPolicy({
        name: 'Policy1',
        conditions: [],
        actions: [],
      });

      const resource1 = { id: 'resource-1' };
      const resource2 = { id: 'resource-2' };

      const decision1 = await registry.routeDecision(store, resource1, 'Policy1');
      const decision2 = await registry.routeDecision(store, resource2, 'Policy1');

      // Different resources should produce different receipts
      expect(decision1.receiptHash).not.toBe(decision2.receiptHash);
    });

    it('should include all decision metadata in receipt', async () => {
      const registry = await createPolicyRegistry();
      const store = createStore();

      const testHook = defineHook({
        id: 'testHook',
        trigger: 'before-add',
        validate: () => ({ valid: true }),
      });
      registry.registerHook('testHook', testHook);

      registry.registerPolicy({
        name: 'CompletePolicy',
        conditions: [{ sparql: 'ASK { }' }],
        actions: [{ hook: 'testHook' }],
      });

      const testQuad = dataFactory.quad(
        dataFactory.namedNode('http://example.org/s'),
        dataFactory.namedNode('http://example.org/p'),
        dataFactory.literal('o')
      );

      const decision = await registry.routeDecision(store, testQuad, 'CompletePolicy');

      expect(decision.receipt).toBeDefined();
      expect(decision.receipt.hash).toBeDefined();
      expect(decision.receipt.policy).toBe('CompletePolicy');
      expect(decision.receipt.decision).toBe('allow');
      expect(decision.receipt.timestamp).toBeTypeOf('number');
      expect(decision.receipt.conditionResults).toEqual([true]);
      expect(decision.receipt.actionResults).toHaveLength(1);
      expect(decision.receipt.canonical).toBeDefined();
    });

    it('should verify receipt hash matches content', async () => {
      const registry = await createPolicyRegistry();
      const store = createStore();

      registry.registerPolicy({
        name: 'VerifyPolicy',
        conditions: [],
        actions: [],
      });

      const decision = await registry.routeDecision(store, {}, 'VerifyPolicy');
      const receipt = decision.receipt;

      // Re-compute hash from canonical form
      const { blake3 } = await import('hash-wasm');
      const recomputedHash = await blake3(receipt.canonical);

      expect(receipt.hash).toBe(recomputedHash);
    });
  });

  describe('Registry Management', () => {
    it('should get all registered policies', async () => {
      const registry = await createPolicyRegistry();

      registry.registerPolicy({ name: 'Policy1', conditions: [], actions: [] });
      registry.registerPolicy({ name: 'Policy2', conditions: [], actions: [] });

      const policies = registry.getPolicies();

      expect(policies).toHaveLength(2);
      expect(policies.map((p) => p.name)).toContain('Policy1');
      expect(policies.map((p) => p.name)).toContain('Policy2');
    });

    it('should get specific policy by name', async () => {
      const registry = await createPolicyRegistry();

      registry.registerPolicy({
        name: 'TargetPolicy',
        description: 'Test policy',
        conditions: [],
        actions: [],
      });

      const policy = registry.getPolicy('TargetPolicy');

      expect(policy).toBeDefined();
      expect(policy.name).toBe('TargetPolicy');
      expect(policy.description).toBe('Test policy');
    });

    it('should return null for non-existent policy', async () => {
      const registry = await createPolicyRegistry();

      const policy = registry.getPolicy('NonExistent');

      expect(policy).toBeNull();
    });

    it('should track registry statistics', async () => {
      const registry = await createPolicyRegistry();

      const hook1 = defineHook({ id: 'hook1', trigger: 'before-add', validate: () => ({ valid: true }) });
      const hook2 = defineHook({ id: 'hook2', trigger: 'before-add', validate: () => ({ valid: true }) });

      registry.registerHook('hook1', hook1);
      registry.registerHook('hook2', hook2);

      registry.registerPolicy({ name: 'P1', conditions: [], actions: [], enabled: true });
      registry.registerPolicy({ name: 'P2', conditions: [], actions: [], enabled: false });

      const stats = registry.getStats();

      expect(stats.policies.total).toBe(2);
      expect(stats.policies.enabled).toBe(1);
      expect(stats.hooks.total).toBe(2);
      expect(stats.hooks.registered).toContain('hook1');
      expect(stats.hooks.registered).toContain('hook2');
    });

    it('should validate policy schema on registration', async () => {
      const registry = await createPolicyRegistry();

      // Invalid policy - missing name
      expect(() => {
        registry.registerPolicy({
          conditions: [],
          actions: [],
        });
      }).toThrow();

      // Invalid policy - bad priority
      expect(() => {
        registry.registerPolicy({
          name: 'BadPolicy',
          priority: 150, // Max is 100
          conditions: [],
          actions: [],
        });
      }).toThrow();
    });
  });

  describe('Integration Tests', () => {
    it('should integrate SPARQL conditions + hooks + receipts', async () => {
      const registry = await createPolicyRegistry();
      const store = createStore();

      // Setup: Add user data
      const foaf = 'http://xmlns.com/foaf/0.1/';
      store.add(
        dataFactory.quad(
          dataFactory.namedNode('http://example.org/user1'),
          dataFactory.namedNode(`${foaf}age`),
          dataFactory.literal('22', dataFactory.namedNode('http://www.w3.org/2001/XMLSchema#integer'))
        )
      );

      // Register audit hook
      const auditLog = [];
      const auditHook = defineHook({
        id: 'audit',
        trigger: 'before-add',
        validate: (quad, options) => {
          auditLog.push({ quad, options });
          return { valid: true };
        },
      });
      registry.registerHook('audit', auditHook);

      // Register policy with condition + action
      registry.registerPolicy({
        name: 'CustomerCredit',
        conditions: [
          {
            sparql: `
              PREFIX foaf: <http://xmlns.com/foaf/0.1/>
              ASK {
                <http://example.org/user1> foaf:age ?age .
                FILTER (?age >= 18)
              }
            `,
          },
        ],
        actions: [{ hook: 'audit', args: { event: 'credit_approved', limit: 1000 } }],
      });

      // Execute policy
      const testQuad = dataFactory.quad(
        dataFactory.namedNode('http://example.org/purchase1'),
        dataFactory.namedNode('http://example.org/amount'),
        dataFactory.literal('500')
      );

      const result = await registry.routeDecision(store, testQuad, 'CustomerCredit');

      // Verify complete flow
      expect(result.decision).toBe('allow');
      expect(result.conditionResults).toEqual([true]);
      expect(result.actionResults).toHaveLength(1);
      expect(result.actionResults[0].valid).toBe(true);
      expect(result.receiptHash).toBeDefined();
      expect(result.receipt.hash).toBe(result.receiptHash);
      expect(auditLog).toHaveLength(1);
      expect(auditLog[0].options).toEqual({ event: 'credit_approved', limit: 1000 });
    });
  });
});
