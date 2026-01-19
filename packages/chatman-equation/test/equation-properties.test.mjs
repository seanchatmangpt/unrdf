/**
 * @file equation-properties.test.mjs
 * @description Tests mathematical properties of the Chatman equation A = μ(O ⊔ Δ)
 * - Determinism: Same O, Δ, μ → Same A
 * - Idempotence: μ(μ(O)) ≈ μ(O)
 * - Composability: μ₁ ∘ μ₂ works
 * - Provability: Receipt generation and verification
 */

import { describe, it, expect } from 'vitest';
import { createHash, randomUUID } from 'crypto';
import { mu, compose } from '../src/mu-transform.mjs';

// Helper functions
function createObservation(state = {}, domain = 'market') {
  return {
    id: randomUUID(),
    timestamp: new Date().toISOString(),
    domain,
    state,
  };
}

function createDelta(operations = [], domain = 'market') {
  return {
    id: randomUUID(),
    timestamp: new Date().toISOString(),
    domain,
    operations,
  };
}

function createOperator(domain = 'market', name = 'test_op') {
  return {
    type: 'merge',
    name,
    domain,
    conflict_resolution: 'delta_wins',
  };
}

describe('Equation Properties: A = μ(O ⊔ Δ)', () => {
  describe('Determinism: Same O, Δ, μ → Same A', () => {
    it('should produce identical result state for identical inputs', () => {
      const obs = createObservation({ value: 100 });
      const delta = createDelta([{ op: 'update', field: 'value', value: 200 }]);
      const operator = createOperator();

      const result1 = mu(obs, delta, operator, { includeReceipt: false });
      const result2 = mu(obs, delta, operator, { includeReceipt: false });

      expect(result1.artifact.result).toEqual(result2.artifact.result);
    });

    it('should produce identical hash for identical inputs', () => {
      const obs = createObservation({ data: 'test' });
      const delta = createDelta([{ op: 'update', field: 'data', value: 'updated' }]);
      const operator = createOperator();

      const result1 = mu(obs, delta, operator, { includeReceipt: true });
      const result2 = mu(obs, delta, operator, { includeReceipt: true });

      expect(result1.artifact.receipt.observationHash).toBe(result2.artifact.receipt.observationHash);
      expect(result1.artifact.receipt.deltaHash).toBe(result2.artifact.receipt.deltaHash);
    });

    it('should be deterministic across multiple runs (N=10)', () => {
      const obs = createObservation({ counter: 0 });
      const delta = createDelta([{ op: 'update', field: 'counter', value: 10 }]);
      const operator = createOperator();

      const results = Array.from({ length: 10 }, () =>
        mu(obs, delta, operator, { includeReceipt: false })
      );

      const firstResult = results[0].artifact.result;
      results.forEach(result => {
        expect(result.artifact.result).toEqual(firstResult);
      });
    });

    it('should produce different result for different observation', () => {
      const obs1 = createObservation({ value: 1 });
      const obs2 = createObservation({ value: 2 });
      const delta = createDelta([{ op: 'update', field: 'value', value: 100 }]);
      const operator = createOperator();

      const result1 = mu(obs1, delta, operator);
      const result2 = mu(obs2, delta, operator);

      // Results should be the same (both update to 100)
      expect(result1.artifact.result.value).toBe(100);
      expect(result2.artifact.result.value).toBe(100);

      // But source observations are different
      expect(result1.metadata.observationId).not.toBe(result2.metadata.observationId);
    });

    it('should produce different result for different delta', () => {
      const obs = createObservation({ value: 50 });
      const delta1 = createDelta([{ op: 'update', field: 'value', value: 100 }]);
      const delta2 = createDelta([{ op: 'update', field: 'value', value: 200 }]);
      const operator = createOperator();

      const result1 = mu(obs, delta1, operator);
      const result2 = mu(obs, delta2, operator);

      expect(result1.artifact.result.value).toBe(100);
      expect(result2.artifact.result.value).toBe(200);
    });

    it('should maintain determinism with complex state', () => {
      const obs = createObservation({
        metrics: { count: 100, sum: 5000 },
        tags: ['a', 'b'],
      });
      const delta = createDelta([
        { op: 'update', field: 'metrics', value: { count: 200, sum: 10000 } },
        { op: 'add', field: 'status', value: 'active' },
      ]);
      const operator = createOperator();

      const result1 = mu(obs, delta, operator, { includeReceipt: false });
      const result2 = mu(obs, delta, operator, { includeReceipt: false });

      expect(result1.artifact.result).toEqual(result2.artifact.result);
    });

    it('should canonicalize Turtle output deterministically', () => {
      const obs = createObservation({ test: 'value' });
      const delta = createDelta([{ op: 'update', field: 'test', value: 'new' }]);
      const operator = createOperator();

      const result1 = mu(obs, delta, operator, { deterministic: true });
      const result2 = mu(obs, delta, operator, { deterministic: true });

      // Proof content should be identical (ignoring generated IDs)
      const proof1Lines = result1.artifact.proof.content.split('\n').filter(l => l.startsWith('@prefix'));
      const proof2Lines = result2.artifact.proof.content.split('\n').filter(l => l.startsWith('@prefix'));

      expect(proof1Lines).toEqual(proof2Lines);
    });
  });

  describe('Idempotence: μ(μ(O)) ≈ μ(O)', () => {
    it('should allow chaining transformations', () => {
      const obs1 = createObservation({ value: 100 });
      const delta1 = createDelta([{ op: 'update', field: 'value', value: 200 }]);
      const operator = createOperator();

      const result1 = mu(obs1, delta1, operator);

      // Create new observation from result
      const obs2 = {
        id: randomUUID(),
        timestamp: new Date().toISOString(),
        domain: obs1.domain,
        state: result1.artifact.result,
      };

      const delta2 = createDelta([{ op: 'update', field: 'value', value: 300 }]);
      const result2 = mu(obs2, delta2, operator);

      expect(result1.artifact.result.value).toBe(200);
      expect(result2.artifact.result.value).toBe(300);
    });

    it('should preserve structure through transformation', () => {
      const obs = createObservation({ field1: 1, field2: 2 });
      const delta = createDelta([{ op: 'update', field: 'field1', value: 10 }]);
      const operator = createOperator();

      const result = mu(obs, delta, operator);

      // Result should have same structure
      expect(Object.keys(result.artifact.result).sort()).toEqual(['field1', 'field2']);
      expect(result.artifact.result.field2).toBe(2); // Unchanged field preserved
    });

    it('should maintain artifact properties through chain', () => {
      const obs1 = createObservation({ x: 1 });
      const delta1 = createDelta([{ op: 'update', field: 'x', value: 2 }]);
      const operator = createOperator();

      const result1 = mu(obs1, delta1, operator);

      const obs2 = {
        id: randomUUID(),
        timestamp: new Date().toISOString(),
        domain: obs1.domain,
        state: result1.artifact.result,
      };
      const delta2 = createDelta([{ op: 'update', field: 'x', value: 3 }]);
      const result2 = mu(obs2, delta2, operator);

      // Both should be valid artifacts
      expect(result1.artifact.id).toBeDefined();
      expect(result2.artifact.id).toBeDefined();
      expect(result1.artifact.operator).toBe(operator.name);
      expect(result2.artifact.operator).toBe(operator.name);
    });
  });

  describe('Composability: μ₁ ∘ μ₂ works', () => {
    it('should compose two transformations', () => {
      const obs = createObservation({ value: 1 });
      const delta = createDelta([{ op: 'update', field: 'value', value: 2 }]);
      const operator = createOperator();

      const mu1 = (o, d, op, opts) => mu(o, d, op, opts);
      const mu2 = (o, d, op, opts) => mu(o, d, op, opts);

      const composed = compose(mu1, mu2);
      const result = composed(obs, delta, operator);

      expect(result).toBeDefined();
      expect(result.artifact).toBeDefined();
    });

    it('should support sequential transformations', () => {
      const obs = createObservation({ count: 10 });
      const delta1 = createDelta([{ op: 'update', field: 'count', value: 20 }]);
      const delta2 = createDelta([{ op: 'update', field: 'count', value: 30 }]);
      const operator = createOperator();

      const step1 = mu(obs, delta1, operator);

      const obs2 = {
        id: randomUUID(),
        timestamp: new Date().toISOString(),
        domain: obs.domain,
        state: step1.artifact.result,
      };

      const step2 = mu(obs2, delta2, operator);

      expect(step1.artifact.result.count).toBe(20);
      expect(step2.artifact.result.count).toBe(30);
    });

    it('should support parallel transformations', () => {
      const observations = [
        createObservation({ id: 1, value: 10 }),
        createObservation({ id: 2, value: 20 }),
        createObservation({ id: 3, value: 30 }),
      ];

      const deltas = observations.map(obs =>
        createDelta([{ op: 'update', field: 'value', value: obs.state.value * 2 }])
      );

      const operator = createOperator();

      const results = observations.map((obs, i) =>
        mu(obs, deltas[i], operator, { includeReceipt: false })
      );

      expect(results).toHaveLength(3);
      expect(results[0].artifact.result.value).toBe(20);
      expect(results[1].artifact.result.value).toBe(40);
      expect(results[2].artifact.result.value).toBe(60);
    });

    it('should support transformation pipelines', () => {
      const obs = createObservation({ stage: 1 });
      const operator = createOperator();

      const stages = [
        createDelta([{ op: 'update', field: 'stage', value: 2 }]),
        createDelta([{ op: 'update', field: 'stage', value: 3 }]),
        createDelta([{ op: 'update', field: 'stage', value: 4 }]),
      ];

      let currentObs = obs;
      const results = [];

      for (const delta of stages) {
        const result = mu(currentObs, delta, operator, { includeReceipt: false });
        results.push(result);

        currentObs = {
          id: randomUUID(),
          timestamp: new Date().toISOString(),
          domain: currentObs.domain,
          state: result.artifact.result,
        };
      }

      expect(results).toHaveLength(3);
      expect(results[0].artifact.result.stage).toBe(2);
      expect(results[1].artifact.result.stage).toBe(3);
      expect(results[2].artifact.result.stage).toBe(4);
    });

    it('should maintain receipts through composition', () => {
      const obs = createObservation({ x: 1 });
      const delta1 = createDelta([{ op: 'update', field: 'x', value: 2 }]);
      const delta2 = createDelta([{ op: 'update', field: 'x', value: 3 }]);
      const operator = createOperator();

      const result1 = mu(obs, delta1, operator, { includeReceipt: true });

      const obs2 = {
        id: randomUUID(),
        timestamp: new Date().toISOString(),
        domain: obs.domain,
        state: result1.artifact.result,
      };

      const result2 = mu(obs2, delta2, operator, { includeReceipt: true });

      // Both should have receipts
      expect(result1.artifact.receipt).toBeDefined();
      expect(result2.artifact.receipt).toBeDefined();

      // Receipts should be unique
      expect(result1.artifact.receipt.observationHash).not.toBe(result2.artifact.receipt.observationHash);
    });
  });

  describe('Provability: Receipt Generation', () => {
    it('should generate receipt with all required fields', () => {
      const obs = createObservation({ test: 'value' });
      const delta = createDelta([{ op: 'update', field: 'test', value: 'updated' }]);
      const operator = createOperator();

      const result = mu(obs, delta, operator, { includeReceipt: true });

      expect(result.artifact.receipt).toBeDefined();
      expect(result.artifact.receipt.observationHash).toBeDefined();
      expect(result.artifact.receipt.deltaHash).toBeDefined();
      expect(result.artifact.receipt.artifactHash).toBeDefined();
      expect(result.artifact.receipt.timestamp).toBeDefined();
      expect(result.artifact.receipt.equation).toBe('A = μ(O ⊔ Δ)');
    });

    it('should generate SHA-256 hashes', () => {
      const obs = createObservation({ value: 1 });
      const delta = createDelta([{ op: 'update', field: 'value', value: 2 }]);
      const operator = createOperator();

      const result = mu(obs, delta, operator, { includeReceipt: true });

      // SHA-256 produces 64 hex characters
      expect(result.artifact.receipt.observationHash).toHaveLength(64);
      expect(result.artifact.receipt.deltaHash).toHaveLength(64);
      expect(result.artifact.receipt.artifactHash).toHaveLength(64);
      expect(result.artifact.receipt.observationHash).toMatch(/^[0-9a-f]{64}$/);
    });

    it('should generate unique hashes for different inputs', () => {
      const obs1 = createObservation({ value: 1 });
      const obs2 = createObservation({ value: 2 });
      const delta = createDelta([{ op: 'update', field: 'value', value: 100 }]);
      const operator = createOperator();

      const result1 = mu(obs1, delta, operator, { includeReceipt: true });
      const result2 = mu(obs2, delta, operator, { includeReceipt: true });

      expect(result1.artifact.receipt.observationHash).not.toBe(result2.artifact.receipt.observationHash);
    });

    it('should allow receipt verification', () => {
      const obs = createObservation({ verify: 'test' });
      const delta = createDelta([{ op: 'update', field: 'verify', value: 'passed' }]);
      const operator = createOperator();

      const result = mu(obs, delta, operator, { includeReceipt: true });

      // Verify observation hash
      const expectedObsHash = createHash('sha256')
        .update(JSON.stringify(obs))
        .digest('hex');

      expect(result.artifact.receipt.observationHash).toBe(expectedObsHash);

      // Verify delta hash
      const expectedDeltaHash = createHash('sha256')
        .update(JSON.stringify(delta))
        .digest('hex');

      expect(result.artifact.receipt.deltaHash).toBe(expectedDeltaHash);
    });

    it('should include ISO timestamp in receipt', () => {
      const obs = createObservation({ time: 'test' });
      const delta = createDelta([{ op: 'update', field: 'time', value: 'now' }]);
      const operator = createOperator();

      const result = mu(obs, delta, operator, { includeReceipt: true });

      // Should be valid ISO 8601 timestamp
      const timestamp = new Date(result.artifact.receipt.timestamp);
      expect(timestamp.toISOString()).toBe(result.artifact.receipt.timestamp);
      expect(timestamp.getTime()).toBeLessThanOrEqual(Date.now());
    });

    it('should skip receipt when includeReceipt=false', () => {
      const obs = createObservation({ no: 'receipt' });
      const delta = createDelta([{ op: 'update', field: 'no', value: 'none' }]);
      const operator = createOperator();

      const result = mu(obs, delta, operator, { includeReceipt: false });

      expect(result.artifact.receipt).toBeUndefined();
    });

    it('should support receipt chain across multiple transforms', () => {
      const observations = [
        createObservation({ chain: 1 }),
        createObservation({ chain: 2 }),
        createObservation({ chain: 3 }),
      ];

      const deltas = observations.map((obs, i) =>
        createDelta([{ op: 'update', field: 'chain', value: i + 10 }])
      );

      const operator = createOperator();

      const results = observations.map((obs, i) =>
        mu(obs, deltas[i], operator, { includeReceipt: true })
      );

      // Each should have receipt
      results.forEach(result => {
        expect(result.artifact.receipt).toBeDefined();
      });

      // All receipts should be unique
      const obsHashes = results.map(r => r.artifact.receipt.observationHash);
      const uniqueHashes = new Set(obsHashes);
      expect(uniqueHashes.size).toBe(obsHashes.length);
    });
  });

  describe('Mathematical Correctness', () => {
    it('should satisfy: μ(O₁, Δ) ≠ μ(O₂, Δ) when O₁ ≠ O₂ (generally)', () => {
      const O1 = createObservation({ initial: 1 });
      const O2 = createObservation({ initial: 2 });
      const delta = createDelta([{ op: 'add', field: 'new', value: 100 }]);
      const operator = createOperator();

      const A1 = mu(O1, delta, operator);
      const A2 = mu(O2, delta, operator);

      // Both should have 'new' field with value 100
      expect(A1.artifact.result.new).toBe(100);
      expect(A2.artifact.result.new).toBe(100);

      // But 'initial' fields differ
      expect(A1.artifact.result.initial).toBe(1);
      expect(A2.artifact.result.initial).toBe(2);
    });

    it('should be total function (defined for all valid inputs)', () => {
      const validCases = [
        { obs: createObservation({}), delta: createDelta([{ op: 'add', field: 'x', value: 1 }]) },
        { obs: createObservation({ a: 1 }), delta: createDelta([{ op: 'update', field: 'a', value: 2 }]) },
        { obs: createObservation({ a: 1, b: 2 }), delta: createDelta([{ op: 'delete', field: 'b' }]) },
      ];

      const operator = createOperator();

      validCases.forEach(({ obs, delta }) => {
        const result = mu(obs, delta, operator);
        expect(result).toBeDefined();
        expect(result.artifact).toBeDefined();
      });
    });

    it('should preserve information (no unexpected data loss)', () => {
      const obs = createObservation({
        important: 'data',
        critical: 'info',
        metadata: { version: '1.0' },
      });

      const delta = createDelta([
        { op: 'add', field: 'new', value: 'field' },
      ]);

      const operator = createOperator();
      const result = mu(obs, delta, operator);

      // Original fields preserved
      expect(result.artifact.result.important).toBe('data');
      expect(result.artifact.result.critical).toBe('info');
      expect(result.artifact.result.metadata).toEqual({ version: '1.0' });

      // New field added
      expect(result.artifact.result.new).toBe('field');
    });

    it('should respect closure operator properties', () => {
      const obs = createObservation({ value: 1 });
      const delta = createDelta([{ op: 'update', field: 'value', value: 2 }]);

      // Different operator strategies
      const operators = [
        { ...createOperator(), conflict_resolution: 'delta_wins' },
        { ...createOperator(), conflict_resolution: 'current_wins' },
        { ...createOperator(), conflict_resolution: 'merge' },
      ];

      operators.forEach(operator => {
        const result = mu(obs, delta, operator);
        expect(result).toBeDefined();
        expect(result.artifact.operator).toBe(operator.name);
      });
    });
  });

  describe('Performance Properties', () => {
    it('should transform in reasonable time for typical inputs', () => {
      const obs = createObservation({ count: 1 });
      const delta = createDelta([{ op: 'update', field: 'count', value: 2 }]);
      const operator = createOperator();

      const durations = Array.from({ length: 100 }, () => {
        const result = mu(obs, delta, operator);
        return result.metadata.duration;
      });

      const avgDuration = durations.reduce((a, b) => a + b, 0) / durations.length;

      // Should be consistently fast
      expect(avgDuration).toBeLessThan(20); // <20ms average
    });

    it('should handle multiple delta operations efficiently', () => {
      const obs = createObservation({});
      const operations = Array.from({ length: 50 }, (_, i) => ({
        op: 'add',
        field: `field${i}`,
        value: i,
      }));

      const delta = createDelta(operations);
      const operator = createOperator();

      const start = Date.now();
      const result = mu(obs, delta, operator);
      const duration = Date.now() - start;

      expect(duration).toBeLessThan(100); // Still fast with 50 operations
      expect(Object.keys(result.artifact.result)).toHaveLength(50);
    });

    it('should scale linearly with observation size', () => {
      const operator = createOperator();
      const delta = createDelta([{ op: 'add', field: 'new', value: 'value' }]);

      const sizes = [10, 100, 500];
      const durations = sizes.map(size => {
        const state = {};
        for (let i = 0; i < size; i++) {
          state[`field${i}`] = i;
        }

        const obs = createObservation(state);
        const result = mu(obs, delta, operator);
        return result.metadata.duration;
      });

      // Should scale reasonably (not exponentially)
      // Add 1ms baseline to avoid division by zero
      expect(durations[2]).toBeLessThan((durations[0] + 1) * 10);
    });
  });
});
