/**
 * Admission Gate & Receipt Chain Tests
 * Comprehensive test suite covering all admission gate features
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { AdmissionGate } from '../src/admission-gate.mjs';

describe('AdmissionGate', () => {
  let gate;
  let mockTime = 1000000n;

  beforeEach(() => {
    // Create gate with mock time source for deterministic tests
    gate = new AdmissionGate({
      timeSource: () => mockTime++,
    });
  });

  describe('Basic Admit/Deny', () => {
    it('should admit valid operation within bounds', async () => {
      const delta = {
        operation: 'add',
        triples: [{ s: 'subject', p: 'predicate', o: 'object' }],
      };
      const bounds = { maxTriples: 10 };

      const receipt = await gate.admit(delta, bounds, []);

      expect(receipt.result).toBe('admit');
      expect(receipt.reason).toBe('All checks passed');
      expect(receipt.operation).toBe('add');
      expect(receipt.timestamp_ns).toBe(1000000n);
      expect(receipt.hash).toBeTruthy();
      expect(receipt.hash).toMatch(/^[a-f0-9]{64}$/); // BLAKE3 produces 64 hex chars
    });

    it('should deny operation exceeding triple bounds', async () => {
      const delta = {
        operation: 'add',
        triples: [
          { s: 's1', p: 'p1', o: 'o1' },
          { s: 's2', p: 'p2', o: 'o2' },
          { s: 's3', p: 'p3', o: 'o3' },
        ],
      };
      const bounds = { maxTriples: 2 };

      const receipt = await gate.admit(delta, bounds, []);

      expect(receipt.result).toBe('deny');
      expect(receipt.reason).toContain('Triple count 3 exceeds bound 2');
    });

    it('should deny operation exceeding query bounds', async () => {
      const bounds = { maxQueries: 2 };

      // First query - admit
      const receipt1 = await gate.admit({ operation: 'query', query: 'SELECT * WHERE { ?s ?p ?o }' }, bounds, []);
      expect(receipt1.result).toBe('admit');

      // Second query - admit
      const receipt2 = await gate.admit({ operation: 'query', query: 'SELECT * WHERE { ?s ?p ?o }' }, bounds, []);
      expect(receipt2.result).toBe('admit');

      // Third query - deny (exceeds maxQueries)
      const receipt3 = await gate.admit({ operation: 'query', query: 'SELECT * WHERE { ?s ?p ?o }' }, bounds, []);
      expect(receipt3.result).toBe('deny');
      expect(receipt3.reason).toContain('exceeds bound 2');
    });

    it('should validate predicate checks', async () => {
      const delta = { operation: 'add', triples: [] };
      const predicates = [
        {
          name: 'hasTriples',
          check: (d) => d.triples && d.triples.length > 0,
          description: 'must have at least one triple',
        },
      ];

      const receipt = await gate.admit(delta, {}, predicates);

      expect(receipt.result).toBe('deny');
      expect(receipt.reason).toContain("Predicate 'hasTriples' failed");
    });

    it('should handle predicate errors gracefully', async () => {
      const delta = { operation: 'add', triples: [] };
      const predicates = [
        {
          name: 'throwsError',
          check: () => {
            throw new Error('Intentional test error');
          },
        },
      ];

      const receipt = await gate.admit(delta, {}, predicates);

      expect(receipt.result).toBe('deny');
      expect(receipt.reason).toContain('threw error');
      expect(receipt.reason).toContain('Intentional test error');
    });
  });

  describe('Receipt Chaining', () => {
    it('should create first receipt with null parent', async () => {
      // Create fresh gate for this test
      const freshGate = new AdmissionGate({
        timeSource: () => 2000000n,
      });
      const delta = { operation: 'add', triples: [] };
      const receipt = await freshGate.admit(delta, {}, []);

      expect(receipt.parent_receipt_id).toBeNull();
      expect(receipt.id).toBeTruthy();
      expect(receipt.timestamp_ns).toBe(2000000n);
    });

    it('should chain receipts with parent references', async () => {
      const delta1 = { operation: 'add', triples: [] };
      const delta2 = { operation: 'query', query: 'SELECT *' };

      const receipt1 = await gate.admit(delta1, {}, []);
      const receipt2 = await gate.admit(delta2, {}, []);

      expect(receipt1.parent_receipt_id).toBeNull();
      expect(receipt2.parent_receipt_id).toBe(receipt1.id);
      expect(receipt2.timestamp_ns).toBeGreaterThan(receipt1.timestamp_ns);
    });

    it('should maintain monotonic timestamps in chain', async () => {
      const deltas = [{ operation: 'add', triples: [] }, { operation: 'add', triples: [] }, { operation: 'add', triples: [] }];

      const receipts = [];
      for (const delta of deltas) {
        receipts.push(await gate.admit(delta, {}, []));
      }

      // Verify monotonic increase
      for (let i = 1; i < receipts.length; i++) {
        expect(receipts[i].timestamp_ns).toBeGreaterThan(receipts[i - 1].timestamp_ns);
      }
    });

    it('should build valid receipt chain', async () => {
      // Create multiple operations
      await gate.admit({ operation: 'add', triples: [] }, {}, []);
      await gate.admit({ operation: 'query', query: 'SELECT *' }, {}, []);
      await gate.admit({ operation: 'add', triples: [] }, {}, []);

      const chain = gate.getReceiptChain();

      expect(chain).toHaveLength(3);
      expect(chain[0].parent_receipt_id).toBeNull();
      expect(chain[1].parent_receipt_id).toBe(chain[0].id);
      expect(chain[2].parent_receipt_id).toBe(chain[1].id);
    });
  });

  describe('Bounds Violation Detection', () => {
    it('should detect triple count violations', async () => {
      const delta = {
        operation: 'add',
        triples: [
          { s: 's1', p: 'p1', o: 'o1' },
          { s: 's2', p: 'p2', o: 'o2' },
          { s: 's3', p: 'p3', o: 'o3' },
          { s: 's4', p: 'p4', o: 'o4' },
          { s: 's5', p: 'p5', o: 'o5' },
        ],
      };
      const bounds = { maxTriples: 3 };

      const receipt = await gate.admit(delta, bounds, []);

      expect(receipt.result).toBe('deny');
      expect(receipt.reason).toBe('Triple count 5 exceeds bound 3');
      expect(receipt.bounds_used.maxTriples).toBe(3);
    });

    it('should track bounds in receipt', async () => {
      const delta = { operation: 'add', triples: [] };
      const bounds = { maxTriples: 100, maxMemoryMB: 512, maxTimeMs: 1000 };

      const receipt = await gate.admit(delta, bounds, []);

      expect(receipt.bounds_used).toEqual(bounds);
    });

    it('should allow operation at exact bound', async () => {
      const delta = {
        operation: 'add',
        triples: [
          { s: 's1', p: 'p1', o: 'o1' },
          { s: 's2', p: 'p2', o: 'o2' },
        ],
      };
      const bounds = { maxTriples: 2 };

      const receipt = await gate.admit(delta, bounds, []);

      expect(receipt.result).toBe('admit');
    });

    it('should work with no bounds specified', async () => {
      const delta = { operation: 'add', triples: [{ s: 's', p: 'p', o: 'o' }] };

      const receipt = await gate.admit(delta, {}, []);

      expect(receipt.result).toBe('admit');
      expect(receipt.bounds_used).toEqual({});
    });
  });

  describe('Forbidden Operations Blocking', () => {
    it('should block forbidden update operation without receipt', async () => {
      const delta = { operation: 'update', triples: [] };

      const receipt = await gate.admit(delta, {}, []);

      expect(receipt.result).toBe('deny');
      expect(receipt.reason).toContain("Forbidden operation 'update' without valid receipt");
    });

    it('should block forbidden delete operation without receipt', async () => {
      const delta = { operation: 'delete', triples: [] };

      const receipt = await gate.admit(delta, {}, []);

      expect(receipt.result).toBe('deny');
      expect(receipt.reason).toContain("Forbidden operation 'delete' without valid receipt");
    });

    it('should allow update operation with prior admitted update receipt', async () => {
      // First, admit an update to establish authorization
      const delta1 = { operation: 'update', triples: [] };
      const receipt1 = await gate.admit(delta1, {}, []);
      expect(receipt1.result).toBe('deny'); // First one denied

      // Manually add an admitted update receipt to chain (simulating authorization)
      gate.receiptChain.push({
        id: 'auth_receipt',
        timestamp_ns: mockTime++,
        operation: 'update',
        result: 'admit',
        reason: 'Authorized',
        bounds_used: {},
        parent_receipt_id: receipt1.id,
        hash: 'mock_hash',
      });

      // Now update should be allowed
      const delta2 = { operation: 'update', triples: [] };
      const receipt2 = await gate.admit(delta2, {}, []);
      expect(receipt2.result).toBe('admit');
    });

    it('should allow non-forbidden operations', async () => {
      const addDelta = { operation: 'add', triples: [] };
      const queryDelta = { operation: 'query', query: 'SELECT *' };

      const receipt1 = await gate.admit(addDelta, {}, []);
      const receipt2 = await gate.admit(queryDelta, {}, []);

      expect(receipt1.result).toBe('admit');
      expect(receipt2.result).toBe('admit');
    });

    it('should support custom forbidden operations', async () => {
      const customGate = new AdmissionGate({
        forbiddenOperations: ['query'],
        timeSource: () => mockTime++,
      });

      const delta = { operation: 'query', query: 'SELECT *' };
      const receipt = await customGate.admit(delta, {}, []);

      expect(receipt.result).toBe('deny');
      expect(receipt.reason).toContain("Forbidden operation 'query'");
    });
  });

  describe('Chain Verification', () => {
    it('should verify valid chain', async () => {
      await gate.admit({ operation: 'add', triples: [] }, {}, []);
      await gate.admit({ operation: 'add', triples: [] }, {}, []);
      await gate.admit({ operation: 'query', query: 'SELECT *' }, {}, []);

      const chain = gate.getReceiptChain();
      const isValid = await gate.verifyChain(chain);

      expect(isValid).toBe(true);
    });

    it('should detect tampered receipt hash', async () => {
      await gate.admit({ operation: 'add', triples: [] }, {}, []);
      await gate.admit({ operation: 'add', triples: [] }, {}, []);

      const chain = gate.getReceiptChain();

      // Tamper with hash
      chain[1].hash = 'tampered_hash_0000000000000000000000000000000000000000000000000000000000';

      const isValid = await gate.verifyChain(chain);

      expect(isValid).toBe(false);
    });

    it('should detect broken chain linkage', async () => {
      await gate.admit({ operation: 'add', triples: [] }, {}, []);
      await gate.admit({ operation: 'add', triples: [] }, {}, []);
      await gate.admit({ operation: 'add', triples: [] }, {}, []);

      const chain = gate.getReceiptChain();

      // Break linkage
      chain[2].parent_receipt_id = 'wrong_parent_id';

      const isValid = await gate.verifyChain(chain);

      expect(isValid).toBe(false);
    });

    it('should detect non-monotonic timestamps', async () => {
      await gate.admit({ operation: 'add', triples: [] }, {}, []);
      await gate.admit({ operation: 'add', triples: [] }, {}, []);

      const chain = gate.getReceiptChain();

      // Make timestamp go backwards
      chain[1].timestamp_ns = chain[0].timestamp_ns - 1n;

      const isValid = await gate.verifyChain(chain);

      expect(isValid).toBe(false);
    });

    it('should reject empty chain', async () => {
      const isValid = await gate.verifyChain([]);

      expect(isValid).toBe(false);
    });

    it('should verify single receipt chain', async () => {
      await gate.admit({ operation: 'add', triples: [] }, {}, []);

      const chain = gate.getReceiptChain();
      const isValid = await gate.verifyChain(chain);

      expect(isValid).toBe(true);
    });
  });

  describe('Merkle Root Computation', () => {
    it('should compute Merkle root for single receipt', async () => {
      const delta = { operation: 'add', triples: [] };
      const receipt = await gate.admit(delta, {}, []);

      const root = await gate.computeMerkleRoot([receipt]);

      expect(root).toBe(receipt.hash);
    });

    it('should compute Merkle root for multiple receipts', async () => {
      await gate.admit({ operation: 'add', triples: [] }, {}, []);
      await gate.admit({ operation: 'add', triples: [] }, {}, []);
      await gate.admit({ operation: 'add', triples: [] }, {}, []);
      await gate.admit({ operation: 'add', triples: [] }, {}, []);

      const chain = gate.getReceiptChain();
      const root = await gate.computeMerkleRoot(chain);

      expect(root).toBeTruthy();
      expect(root).toMatch(/^[a-f0-9]{64}$/); // BLAKE3 hash
    });

    it('should compute deterministic Merkle root', async () => {
      await gate.admit({ operation: 'add', triples: [] }, {}, []);
      await gate.admit({ operation: 'add', triples: [] }, {}, []);

      const chain = gate.getReceiptChain();
      const root1 = await gate.computeMerkleRoot(chain);
      const root2 = await gate.computeMerkleRoot(chain);

      expect(root1).toBe(root2);
    });

    it('should handle odd number of receipts', async () => {
      await gate.admit({ operation: 'add', triples: [] }, {}, []);
      await gate.admit({ operation: 'add', triples: [] }, {}, []);
      await gate.admit({ operation: 'add', triples: [] }, {}, []);

      const chain = gate.getReceiptChain();
      const root = await gate.computeMerkleRoot(chain);

      expect(root).toBeTruthy();
      expect(root).toMatch(/^[a-f0-9]{64}$/);
    });

    it('should handle empty receipt array', async () => {
      const root = await gate.computeMerkleRoot([]);

      expect(root).toBeTruthy();
      expect(root).toMatch(/^[a-f0-9]{64}$/);
    });
  });

  describe('Batch Anchoring', () => {
    it('should batch anchor multiple operations', async () => {
      const deltas = [
        { operation: 'add', triples: [] },
        { operation: 'add', triples: [] },
        { operation: 'query', query: 'SELECT *' },
      ];

      const batchReceipt = await gate.batchAnchor(deltas, { maxTriples: 100 });

      expect(batchReceipt.type).toBe('batch');
      expect(batchReceipt.operation_count).toBe(3);
      expect(batchReceipt.merkle_root).toBeTruthy();
      expect(batchReceipt.receipts).toHaveLength(3);
      expect(batchReceipt.all_admitted).toBe(true);
    });

    it('should sort operations deterministically', async () => {
      // Create two fresh gates with same time source for deterministic comparison
      let time1 = 3000000n;
      const gate1 = new AdmissionGate({
        timeSource: () => time1++,
      });

      let time2 = 3000000n;
      const gate2 = new AdmissionGate({
        timeSource: () => time2++,
      });

      const deltas1 = [
        { operation: 'query', query: 'SELECT *' },
        { operation: 'add', triples: [] },
        { operation: 'add', triples: [] },
      ];

      const batch1 = await gate1.batchAnchor(deltas1, {});

      const deltas2 = [
        { operation: 'add', triples: [] },
        { operation: 'query', query: 'SELECT *' },
        { operation: 'add', triples: [] },
      ];

      const batch2 = await gate2.batchAnchor(deltas2, {});

      // Merkle roots should be the same (deterministic sorting)
      expect(batch1.merkle_root).toBe(batch2.merkle_root);
    });

    it('should detect failures in batch', async () => {
      const deltas = [
        { operation: 'add', triples: [] },
        { operation: 'update', triples: [] }, // Forbidden - will be denied
        { operation: 'add', triples: [] },
      ];

      const batchReceipt = await gate.batchAnchor(deltas, {});

      expect(batchReceipt.all_admitted).toBe(false);
      // After sorting, operations are: add, add, update (alphabetical)
      // So update is at index 2
      expect(batchReceipt.receipts[2].result).toBe('deny');
    });

    it('should throw on empty batch', async () => {
      await expect(gate.batchAnchor([], {})).rejects.toThrow('non-empty array');
    });

    it('should include bounds in batch receipts', async () => {
      const deltas = [
        { operation: 'add', triples: [] },
        { operation: 'add', triples: [] },
      ];
      const bounds = { maxTriples: 50, maxTimeMs: 1000 };

      const batchReceipt = await gate.batchAnchor(deltas, bounds);

      expect(batchReceipt.receipts[0].bounds_used).toEqual(bounds);
      expect(batchReceipt.receipts[1].bounds_used).toEqual(bounds);
    });
  });

  describe('Preserve(Q) Invariant', () => {
    it('should preserve query results on first execution', async () => {
      const delta = { operation: 'query', query: 'SELECT * WHERE { ?s ?p ?o }' };

      const receipt = await gate.admit(delta, {}, []);

      expect(receipt.result).toBe('admit');
      expect(gate.preservedQueries.has('SELECT * WHERE { ?s ?p ?o }')).toBe(true);
    });

    it('should track query execution count', async () => {
      const query = 'SELECT * WHERE { ?s ?p ?o }';

      await gate.admit({ operation: 'query', query }, {}, []);
      await gate.admit({ operation: 'query', query }, {}, []);
      await gate.admit({ operation: 'query', query }, {}, []);

      const preserved = gate.preservedQueries.get(query);
      expect(preserved.count).toBe(3);
    });

    it('should preserve different queries independently', async () => {
      await gate.admit({ operation: 'query', query: 'SELECT * WHERE { ?s ?p ?o }' }, {}, []);
      await gate.admit({ operation: 'query', query: 'SELECT ?s WHERE { ?s a ?type }' }, {}, []);

      expect(gate.preservedQueries.size).toBe(2);
    });
  });

  describe('Edge Cases', () => {
    it('should handle metadata in delta', async () => {
      const delta = {
        operation: 'add',
        triples: [],
        metadata: { user: 'test', timestamp: '2025-01-01' },
      };

      const receipt = await gate.admit(delta, {}, []);

      expect(receipt.result).toBe('admit');
    });

    it('should validate delta schema', async () => {
      const invalidDelta = {
        operation: 'invalid_operation',
        triples: [],
      };

      await expect(gate.admit(invalidDelta, {}, [])).rejects.toThrow();
    });

    it('should validate bounds schema', async () => {
      const delta = { operation: 'add', triples: [] };
      const invalidBounds = { maxTriples: -5 }; // Negative not allowed

      await expect(gate.admit(delta, invalidBounds, [])).rejects.toThrow();
    });

    it('should clear receipt chain', async () => {
      await gate.admit({ operation: 'add', triples: [] }, {}, []);
      await gate.admit({ operation: 'add', triples: [] }, {}, []);

      expect(gate.getReceiptChain()).toHaveLength(2);

      gate.clearReceiptChain();

      expect(gate.getReceiptChain()).toHaveLength(0);
      expect(gate.preservedQueries.size).toBe(0);
    });

    it('should handle operation without triples', async () => {
      const delta = { operation: 'query', query: 'SELECT *' };

      const receipt = await gate.admit(delta, {}, []);

      expect(receipt.result).toBe('admit');
    });

    it('should generate unique receipt IDs', async () => {
      await gate.admit({ operation: 'add', triples: [] }, {}, []);
      await gate.admit({ operation: 'add', triples: [] }, {}, []);
      await gate.admit({ operation: 'add', triples: [] }, {}, []);

      const chain = gate.getReceiptChain();
      const ids = chain.map((r) => r.id);

      // All IDs should be unique
      const uniqueIds = new Set(ids);
      expect(uniqueIds.size).toBe(ids.length);
    });
  });
});
