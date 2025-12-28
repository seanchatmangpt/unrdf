/**
 * KGC Multiverse - Worker Task Tests
 * Comprehensive tests for all worker task types
 */

import { describe, it, expect } from 'vitest';
import workerTask, {
  TaskType,
  generateQStarIDWorker,
  createUniverseTask,
  applyMorphismTask,
  generateReceiptTask,
  freezeUniverseTask,
  computeHashTask,
} from '../src/worker-task.mjs';

describe('Worker Task Module', () => {
  describe('generateQStarIDWorker', () => {
    it('generates valid Q* ID with required fields', async () => {
      const qid = await generateQStarIDWorker();

      expect(qid.Q_ID).toMatch(/^Q\*_[a-f0-9]{16}$/);
      expect(qid.Q_RDF).toMatch(/^http:\/\/kgc\.io\/multiverse\/[a-f0-9]{16}$/);
      expect(qid.Q_PROV).toBeDefined();
      expect(typeof qid.Q_PROV.createdAt).toBe('bigint');
    });

    it('includes createdBy when provided', async () => {
      const qid = await generateQStarIDWorker({ createdBy: 'test-agent' });

      expect(qid.Q_PROV.createdBy).toBe('test-agent');
    });

    it('includes parentID when provided', async () => {
      const qid = await generateQStarIDWorker({ parentID: 'Q*_parent123' });

      expect(qid.Q_PROV.parentID).toBe('Q*_parent123');
    });

    it('includes batchIndex when provided', async () => {
      const qid = await generateQStarIDWorker({ batchIndex: 42 });

      expect(qid.Q_PROV.batchIndex).toBe(42);
    });

    it('generates unique IDs on successive calls', async () => {
      const qid1 = await generateQStarIDWorker();
      const qid2 = await generateQStarIDWorker();

      expect(qid1.Q_ID).not.toBe(qid2.Q_ID);
      expect(qid1.Q_RDF).not.toBe(qid2.Q_RDF);
    });

    it('uses BigInt timestamp', async () => {
      const qid = await generateQStarIDWorker();

      expect(qid.Q_PROV.createdAt).toBeGreaterThan(0n);
      expect(qid.Q_PROV.createdAt).toBeLessThan(BigInt(Date.now() + 1000) * 1_000_000n);
    });
  });

  describe('createUniverseTask', () => {
    it('creates universe with GENESIS state', async () => {
      const result = await createUniverseTask({ batchIndex: 0 });

      expect(result.state).toBe('GENESIS');
      expect(result.eventCount).toBe(0);
      expect(result.batchIndex).toBe(0);
      expect(result.id).toBeDefined();
      expect(result.id.Q_ID).toMatch(/^Q\*_[a-f0-9]{16}$/);
      expect(result.metadata).toEqual({});
    });

    it('includes createdBy in Q* provenance', async () => {
      const result = await createUniverseTask({
        batchIndex: 1,
        createdBy: 'test-creator',
      });

      expect(result.id.Q_PROV.createdBy).toBe('test-creator');
    });

    it('includes batchIndex in Q* provenance', async () => {
      const result = await createUniverseTask({ batchIndex: 5 });

      expect(result.id.Q_PROV.batchIndex).toBe(5);
      expect(result.batchIndex).toBe(5);
    });

    it('creates unique universes for each call', async () => {
      const u1 = await createUniverseTask({ batchIndex: 0 });
      const u2 = await createUniverseTask({ batchIndex: 1 });

      expect(u1.id.Q_ID).not.toBe(u2.id.Q_ID);
    });
  });

  describe('applyMorphismTask', () => {
    it('applies SCHEMA morphism and generates deltas', async () => {
      const universe = {
        id: {
          Q_ID: 'Q*_test123456789abc',
          Q_RDF: 'http://kgc.io/multiverse/test123456789abc',
          Q_PROV: { createdAt: 1234567890n },
        },
        state: 'ACTIVE',
        eventCount: 10,
      };

      const morphismConfig = {
        id: 'Φ_morphism123',
        type: 'SCHEMA',
      };

      const result = await applyMorphismTask({ universe, morphismConfig });

      expect(result.universeID).toBe('Q*_test123456789abc');
      expect(result.morphismID).toBe('Φ_morphism123');
      expect(result.deltas).toBeDefined();
      expect(Array.isArray(result.deltas)).toBe(true);
      expect(result.deltaCount).toBe(result.deltas.length);
      expect(result.resultHash).toMatch(/^[a-f0-9]{64}$/);
      expect(typeof result.timestamp).toBe('bigint');
    });

    it('generates correct delta structure for SCHEMA type', async () => {
      const universe = {
        id: {
          Q_ID: 'Q*_test123456789abc',
          Q_RDF: 'http://kgc.io/multiverse/test123456789abc',
          Q_PROV: { createdAt: 1234567890n },
        },
        state: 'ACTIVE',
      };

      const morphismConfig = {
        id: 'Φ_schema',
        type: 'SCHEMA',
      };

      const result = await applyMorphismTask({ universe, morphismConfig });

      expect(result.deltas.length).toBeGreaterThan(0);
      const delta = result.deltas[0];
      expect(delta.type).toBe('add');
      expect(delta.subject).toContain('http://kgc.io/entity/');
      expect(delta.predicate).toBe('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
      expect(delta.object.type).toBe('NamedNode');
      expect(delta.object.value).toBe('http://kgc.io/types/TransformedEntity');
    });

    it('handles non-SCHEMA morphisms with empty deltas', async () => {
      const universe = {
        id: {
          Q_ID: 'Q*_test123',
          Q_RDF: 'http://kgc.io/multiverse/test123',
          Q_PROV: { createdAt: 1234567890n },
        },
      };

      const morphismConfig = {
        id: 'Φ_other',
        type: 'STATE',
      };

      const result = await applyMorphismTask({ universe, morphismConfig });

      expect(result.deltas).toEqual([]);
      expect(result.deltaCount).toBe(0);
    });

    it('handles BigInt serialization correctly', async () => {
      const universe = {
        id: {
          Q_ID: 'Q*_test',
          Q_RDF: 'http://kgc.io/multiverse/test',
          Q_PROV: { createdAt: 999999999999999999n },
        },
        bigValue: 123456789012345678901234567890n,
      };

      const morphismConfig = { id: 'Φ_test', type: 'OTHER' };

      const result = await applyMorphismTask({ universe, morphismConfig });

      // Should not throw on BigInt serialization
      expect(result.resultHash).toBeDefined();
      expect(typeof result.timestamp).toBe('bigint');
    });
  });

  describe('generateReceiptTask', () => {
    it('generates valid receipt with Q* ID', async () => {
      const result = await generateReceiptTask({
        universeID: 'Q*_universe123',
        operations: [{ op: 'test' }],
        operationType: 'CREATE',
        batchIndex: 0,
      });

      expect(result.Q_ID).toMatch(/^Q\*_[a-f0-9]{16}$/);
      expect(result.Q_RDF).toMatch(/^http:\/\/kgc\.io\/receipts\/[a-f0-9]{16}$/);
      expect(result.Q_PROV).toBeDefined();
    });

    it('includes correct provenance data', async () => {
      const operations = [{ op: 'create' }, { op: 'update' }];
      const result = await generateReceiptTask({
        universeID: 'Q*_uni123',
        operations,
        operationType: 'BATCH_CREATE',
        batchIndex: 5,
      });

      expect(typeof result.Q_PROV.timestamp).toBe('bigint');
      expect(result.Q_PROV.batchSize).toBe(2);
      expect(result.Q_PROV.operationType).toBe('BATCH_CREATE');
      expect(result.Q_PROV.universeID).toBe('Q*_uni123');
      expect(result.Q_PROV.contentHash).toMatch(/^[a-f0-9]{64}$/);
      expect(result.Q_PROV.batchIndex).toBe(5);
    });

    it('handles empty operations array', async () => {
      const result = await generateReceiptTask({
        universeID: 'Q*_test',
        operations: [],
        operationType: 'NOOP',
        batchIndex: 0,
      });

      expect(result.Q_PROV.batchSize).toBe(0);
      expect(result.Q_PROV.contentHash).toBeDefined();
    });

    it('generates unique receipt IDs for same universe', async () => {
      const params = {
        universeID: 'Q*_same',
        operations: [{ op: 'test' }],
        operationType: 'TEST',
        batchIndex: 0,
      };

      const r1 = await generateReceiptTask(params);
      // Small delay to ensure different timestamp
      await new Promise((resolve) => setTimeout(resolve, 2));
      const r2 = await generateReceiptTask(params);

      expect(r1.Q_ID).not.toBe(r2.Q_ID);
    });

    it('handles BigInt in operations', async () => {
      const operations = [
        { timestamp: 123456789012345678n, value: 'test' },
      ];

      const result = await generateReceiptTask({
        universeID: 'Q*_test',
        operations,
        operationType: 'BIGINT_TEST',
        batchIndex: 0,
      });

      expect(result.Q_PROV.contentHash).toBeDefined();
    });
  });

  describe('freezeUniverseTask', () => {
    it('freezes universe and generates hash', async () => {
      const universe = {
        id: {
          Q_ID: 'Q*_test123',
          Q_RDF: 'http://kgc.io/multiverse/test123',
          Q_PROV: { createdAt: 1234567890n },
        },
        state: 'ACTIVE',
        eventCount: 42,
      };

      const result = await freezeUniverseTask({ universe });

      expect(result.state).toBe('FROZEN');
      expect(result.universeHash).toMatch(/^[a-f0-9]{64}$/);
      expect(typeof result.frozenAt).toBe('bigint');
      expect(result.id).toEqual(universe.id);
      expect(result.eventCount).toBe(42);
    });

    it('includes quad count in hash computation', async () => {
      const universe = {
        id: { Q_ID: 'Q*_test', Q_RDF: 'http://test', Q_PROV: { createdAt: 1n } },
        state: 'ACTIVE',
        eventCount: 10,
      };

      const quads = [
        { s: 'ex:s1', p: 'ex:p1', o: 'ex:o1' },
        { s: 'ex:s2', p: 'ex:p2', o: 'ex:o2' },
      ];

      const result = await freezeUniverseTask({ universe, quads });

      expect(result.universeHash).toBeDefined();
    });

    it('handles universe with no quads', async () => {
      const universe = {
        id: { Q_ID: 'Q*_empty', Q_RDF: 'http://empty', Q_PROV: { createdAt: 1n } },
        state: 'ACTIVE',
        eventCount: 0,
      };

      const result = await freezeUniverseTask({ universe, quads: [] });

      expect(result.state).toBe('FROZEN');
      expect(result.universeHash).toBeDefined();
    });

    it('produces different hashes for different universes', async () => {
      const u1 = {
        id: { Q_ID: 'Q*_test1', Q_RDF: 'http://test1', Q_PROV: { createdAt: 1n } },
        eventCount: 10,
      };

      const u2 = {
        id: { Q_ID: 'Q*_test2', Q_RDF: 'http://test2', Q_PROV: { createdAt: 1n } },
        eventCount: 20,
      };

      const r1 = await freezeUniverseTask({ universe: u1 });
      const r2 = await freezeUniverseTask({ universe: u2 });

      expect(r1.universeHash).not.toBe(r2.universeHash);
    });

    it('preserves all universe properties', async () => {
      const universe = {
        id: { Q_ID: 'Q*_test', Q_RDF: 'http://test', Q_PROV: { createdAt: 1n } },
        state: 'ACTIVE',
        eventCount: 5,
        metadata: { custom: 'data' },
        extraField: 'preserved',
      };

      const result = await freezeUniverseTask({ universe });

      expect(result.metadata).toEqual({ custom: 'data' });
      expect(result.extraField).toBe('preserved');
    });
  });

  describe('computeHashTask', () => {
    it('computes hash for string data', async () => {
      const result = await computeHashTask({
        data: 'test string',
        index: 0,
      });

      expect(result.hash).toMatch(/^[a-f0-9]{64}$/);
      expect(result.index).toBe(0);
      expect(result.dataSize).toBe('test string'.length);
    });

    it('computes hash for object data', async () => {
      const data = { key: 'value', nested: { field: 123 } };
      const result = await computeHashTask({ data, index: 1 });

      expect(result.hash).toBeDefined();
      expect(result.index).toBe(1);
      expect(result.dataSize).toBeGreaterThan(0);
    });

    it('handles BigInt in data', async () => {
      const data = { timestamp: 123456789012345678n };
      const result = await computeHashTask({ data, index: 2 });

      expect(result.hash).toBeDefined();
      expect(result.dataSize).toBeGreaterThan(0);
    });

    it('produces deterministic hashes', async () => {
      const data = { test: 'data' };
      const r1 = await computeHashTask({ data, index: 0 });
      const r2 = await computeHashTask({ data, index: 0 });

      expect(r1.hash).toBe(r2.hash);
    });

    it('produces different hashes for different data', async () => {
      const r1 = await computeHashTask({ data: 'data1', index: 0 });
      const r2 = await computeHashTask({ data: 'data2', index: 0 });

      expect(r1.hash).not.toBe(r2.hash);
    });

    it('handles empty string', async () => {
      const result = await computeHashTask({ data: '', index: 0 });

      expect(result.hash).toBeDefined();
      expect(result.dataSize).toBe(0);
    });

    it('handles complex nested objects', async () => {
      const data = {
        level1: {
          level2: {
            level3: {
              array: [1, 2, 3],
              string: 'deep',
            },
          },
        },
      };

      const result = await computeHashTask({ data, index: 5 });

      expect(result.hash).toBeDefined();
      expect(result.index).toBe(5);
    });
  });

  describe('workerTask - Main Dispatcher', () => {
    it('dispatches CREATE_UNIVERSE task', async () => {
      const task = {
        type: TaskType.CREATE_UNIVERSE,
        params: { batchIndex: 0 },
      };

      const result = await workerTask(task);

      expect(result.state).toBe('GENESIS');
      expect(result.id).toBeDefined();
    });

    it('dispatches APPLY_MORPHISM task', async () => {
      const task = {
        type: TaskType.APPLY_MORPHISM,
        params: {
          universe: {
            id: { Q_ID: 'Q*_test', Q_RDF: 'http://test', Q_PROV: { createdAt: 1n } },
          },
          morphismConfig: { id: 'Φ_test', type: 'SCHEMA' },
        },
      };

      const result = await workerTask(task);

      expect(result.deltas).toBeDefined();
      expect(result.universeID).toBe('Q*_test');
    });

    it('dispatches GENERATE_RECEIPT task', async () => {
      const task = {
        type: TaskType.GENERATE_RECEIPT,
        params: {
          universeID: 'Q*_test',
          operations: [],
          operationType: 'TEST',
          batchIndex: 0,
        },
      };

      const result = await workerTask(task);

      expect(result.Q_ID).toMatch(/^Q\*_/);
      expect(result.Q_PROV).toBeDefined();
    });

    it('dispatches FREEZE_UNIVERSE task', async () => {
      const task = {
        type: TaskType.FREEZE_UNIVERSE,
        params: {
          universe: {
            id: { Q_ID: 'Q*_test', Q_RDF: 'http://test', Q_PROV: { createdAt: 1n } },
            eventCount: 0,
          },
        },
      };

      const result = await workerTask(task);

      expect(result.state).toBe('FROZEN');
      expect(result.universeHash).toBeDefined();
    });

    it('dispatches COMPUTE_HASH task', async () => {
      const task = {
        type: TaskType.COMPUTE_HASH,
        params: { data: 'test', index: 0 },
      };

      const result = await workerTask(task);

      expect(result.hash).toBeDefined();
      expect(result.dataSize).toBeDefined();
    });

    it('throws error for unknown task type', async () => {
      const task = {
        type: 'UNKNOWN_TYPE',
        params: {},
      };

      await expect(workerTask(task)).rejects.toThrow(/Unknown task type: UNKNOWN_TYPE/);
    });

    it('handles all valid TaskType enum values', async () => {
      const tasks = [
        { type: TaskType.CREATE_UNIVERSE, params: { batchIndex: 0 } },
        {
          type: TaskType.APPLY_MORPHISM,
          params: {
            universe: { id: { Q_ID: 'Q*_t', Q_RDF: 'http://t', Q_PROV: { createdAt: 1n } } },
            morphismConfig: { id: 'Φ_t', type: 'STATE' },
          },
        },
        {
          type: TaskType.GENERATE_RECEIPT,
          params: { universeID: 'Q*_t', operations: [], operationType: 'T', batchIndex: 0 },
        },
        {
          type: TaskType.FREEZE_UNIVERSE,
          params: {
            universe: { id: { Q_ID: 'Q*_t', Q_RDF: 'http://t', Q_PROV: { createdAt: 1n } }, eventCount: 0 },
          },
        },
        { type: TaskType.COMPUTE_HASH, params: { data: 'test', index: 0 } },
      ];

      for (const task of tasks) {
        const result = await workerTask(task);
        expect(result).toBeDefined();
      }
    });
  });
});
