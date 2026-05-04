/**
 * KnowledgeStore Comprehensive Tests
 *
 * Tests:
 * 1. Determinism - repeated builds produce identical hashes
 * 2. Immutability - append-only property enforced
 * 3. Snapshot - round-trip serialization with hash verification
 * 4. Query - pattern matching with wildcards
 * 5. State commitment - lightweight verification
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { KnowledgeStore } from '../src/KnowledgeStore.mjs';
import { dataFactory } from '@unrdf/oxigraph';
import { rmSync } from 'fs';

describe('KnowledgeStore', () => {
  let store;
  const testGitDir = '.test-kgc-substrate-git';

  beforeEach(() => {
    store = new KnowledgeStore({ nodeId: 'test-node', gitDir: testGitDir });
  });

  afterEach(() => {
    // Clean up git directory
    try {
      rmSync(testGitDir, { recursive: true, force: true });
    } catch {
      // Ignore cleanup errors
    }
  });

  describe('Construction', () => {
    it('should create instance with default options', () => {
      const defaultStore = new KnowledgeStore();
      expect(defaultStore).toBeDefined();
      expect(defaultStore.getNodeId()).toMatch(/^ks-/);
      expect(defaultStore.getLogIndex()).toBe(0n);
      expect(defaultStore.getEpoch()).toBe(0);
    });

    it('should create instance with custom node ID', () => {
      expect(store.getNodeId()).toBe('test-node');
    });

    it('should throw on invalid options', () => {
      expect(() => new KnowledgeStore('invalid')).toThrow(TypeError);
    });
  });

  describe('Append-Only Log', () => {
    it('should append triple with sequential index', async () => {
      const s = dataFactory.namedNode('http://example.org/subject');
      const p = dataFactory.namedNode('http://example.org/predicate');
      const o = dataFactory.literal('value');

      const result1 = await store.appendTriple('add', s, p, o);
      expect(result1.index).toBe(0n);
      expect(typeof result1.timestamp_ns).toBe('bigint');

      const result2 = await store.appendTriple('add', s, p, dataFactory.literal('value2'));
      expect(result2.index).toBe(1n);
      expect(result2.index > result1.index).toBe(true);
    });

    it('should enforce immutability (log index only increases)', async () => {
      const s = dataFactory.namedNode('http://example.org/s');
      const p = dataFactory.namedNode('http://example.org/p');
      const o = dataFactory.literal('v');

      await store.appendTriple('add', s, p, o);
      const indexBefore = store.getLogIndex();

      await store.appendTriple('add', s, p, dataFactory.literal('v2'));
      const indexAfter = store.getLogIndex();

      expect(indexAfter).toBeGreaterThan(indexBefore);
      expect(indexAfter - indexBefore).toBe(1n);
    });

    it('should support delete operations', async () => {
      const s = dataFactory.namedNode('http://example.org/s');
      const p = dataFactory.namedNode('http://example.org/p');
      const o = dataFactory.literal('v');

      const addResult = await store.appendTriple('add', s, p, o);
      expect(addResult.index).toBe(0n);

      const deleteResult = await store.appendTriple('delete', s, p, o);
      expect(deleteResult.index).toBe(1n);
    });

    it('should validate operation type', async () => {
      const s = dataFactory.namedNode('http://example.org/s');
      const p = dataFactory.namedNode('http://example.org/p');
      const o = dataFactory.literal('v');

      await expect(
        store.appendTriple('invalid', s, p, o)
      ).rejects.toThrow(TypeError);
    });

    it('should validate RDF terms', async () => {
      const s = dataFactory.namedNode('http://example.org/s');
      const p = dataFactory.namedNode('http://example.org/p');

      await expect(
        store.appendTriple('add', null, p, dataFactory.literal('v'))
      ).rejects.toThrow(TypeError);

      await expect(
        store.appendTriple('add', s, null, dataFactory.literal('v'))
      ).rejects.toThrow(TypeError);

      await expect(
        store.appendTriple('add', s, p, null)
      ).rejects.toThrow(TypeError);
    });
  });

  describe('Query Interface', () => {
    beforeEach(async () => {
      const s1 = dataFactory.namedNode('http://example.org/s1');
      const s2 = dataFactory.namedNode('http://example.org/s2');
      const p = dataFactory.namedNode('http://example.org/p');
      const o = dataFactory.literal('value');

      await store.appendTriple('add', s1, p, o);
      await store.appendTriple('add', s2, p, o);
    });

    it('should query with subject wildcard', () => {
      const p = dataFactory.namedNode('http://example.org/p');
      const results = store.selectTriples({ subject: null, predicate: p, object: null });

      expect(results.size).toBeGreaterThanOrEqual(2);
    });

    it('should query with specific subject', () => {
      const s1 = dataFactory.namedNode('http://example.org/s1');
      const results = store.selectTriples({ subject: s1, predicate: null, object: null });

      expect(results.size).toBeGreaterThanOrEqual(1);
      for (const quad of results) {
        expect(quad.subject.value).toBe('http://example.org/s1');
      }
    });

    it('should query with all wildcards', () => {
      const results = store.selectTriples({ subject: null, predicate: null, object: null });

      expect(results.size).toBeGreaterThanOrEqual(2);
    });

    it('should validate query pattern', () => {
      expect(() => store.selectTriples('invalid')).toThrow();
    });
  });

  describe('Snapshot Generation (Determinism)', () => {
    it('should generate snapshot with deterministic hash', async () => {
      const s = dataFactory.namedNode('http://example.org/s');
      const p = dataFactory.namedNode('http://example.org/p');
      const o = dataFactory.literal('value');

      await store.appendTriple('add', s, p, o);

      const snapshot1 = await store.generateSnapshot();

      expect(snapshot1.epoch).toBe(0);
      expect(snapshot1.quads_hash).toBeDefined();
      expect(typeof snapshot1.quads_hash).toBe('string');
      expect(snapshot1.quads_hash.length).toBeGreaterThan(0);
      expect(snapshot1.commit_hash).toBeDefined();
      expect(snapshot1.snapshot_id).toBeDefined();
      expect(typeof snapshot1.timestamp_ns).toBe('bigint');
      expect(snapshot1.quad_count).toBeGreaterThanOrEqual(1);
    });

    it('should produce identical hashes for same state', async () => {
      const s = dataFactory.namedNode('http://example.org/s');
      const p = dataFactory.namedNode('http://example.org/p');
      const o = dataFactory.literal('value');

      await store.appendTriple('add', s, p, o);

      const snapshot1 = await store.generateSnapshot();

      // Create second store with same state
      const store2 = new KnowledgeStore({ nodeId: 'test-node-2', gitDir: '.test-kgc-substrate-git-2' });
      await store2.appendTriple('add', s, p, o);

      const snapshot2 = await store2.generateSnapshot();

      expect(snapshot1.quads_hash).toBe(snapshot2.quads_hash);

      // Cleanup
      try {
        rmSync('.test-kgc-substrate-git-2', { recursive: true, force: true });
      } catch {}
    });

    it('should increment epoch on each snapshot', async () => {
      const s = dataFactory.namedNode('http://example.org/s');
      const p = dataFactory.namedNode('http://example.org/p');
      const o = dataFactory.literal('value');

      await store.appendTriple('add', s, p, o);
      const snapshot1 = await store.generateSnapshot();
      expect(snapshot1.epoch).toBe(0);

      await store.appendTriple('add', s, p, dataFactory.literal('value2'));
      const snapshot2 = await store.generateSnapshot();
      expect(snapshot2.epoch).toBe(1);
    });

    it('should handle empty store snapshot', async () => {
      const snapshot = await store.generateSnapshot();

      expect(snapshot.epoch).toBe(0);
      expect(snapshot.quads_hash).toBeDefined();
      expect(snapshot.quad_count).toBe(0);
    });
  });

  describe('State Commitment', () => {
    it('should generate state commitment', async () => {
      const s = dataFactory.namedNode('http://example.org/s');
      const p = dataFactory.namedNode('http://example.org/p');
      const o = dataFactory.literal('value');

      await store.appendTriple('add', s, p, o);

      const commitment = await store.getStateCommitment();

      expect(commitment.state_hash).toBeDefined();
      expect(typeof commitment.state_hash).toBe('string');
      expect(commitment.state_hash.length).toBeGreaterThan(0);
      expect(commitment.log_index).toBe(1n);
      expect(typeof commitment.timestamp_ns).toBe('bigint');
      expect(commitment.quad_count).toBeGreaterThanOrEqual(1);
    });

    it('should produce same hash for same state', async () => {
      const s = dataFactory.namedNode('http://example.org/s');
      const p = dataFactory.namedNode('http://example.org/p');
      const o = dataFactory.literal('value');

      await store.appendTriple('add', s, p, o);

      const commitment1 = await store.getStateCommitment();
      const commitment2 = await store.getStateCommitment();

      expect(commitment1.state_hash).toBe(commitment2.state_hash);
    });

    it('should change hash when state changes', async () => {
      const s = dataFactory.namedNode('http://example.org/s');
      const p = dataFactory.namedNode('http://example.org/p');
      const o = dataFactory.literal('value');

      await store.appendTriple('add', s, p, o);
      const commitment1 = await store.getStateCommitment();

      await store.appendTriple('add', s, p, dataFactory.literal('value2'));
      const commitment2 = await store.getStateCommitment();

      expect(commitment1.state_hash).not.toBe(commitment2.state_hash);
    });
  });

  describe('Round-trip Verification', () => {
    it('should verify snapshot hash matches commitment hash (when no changes)', async () => {
      const s = dataFactory.namedNode('http://example.org/s');
      const p = dataFactory.namedNode('http://example.org/p');
      const o = dataFactory.literal('value');

      await store.appendTriple('add', s, p, o);

      const snapshot = await store.generateSnapshot();
      const commitment = await store.getStateCommitment();

      // Hashes should match since state hasn't changed
      expect(snapshot.quads_hash).toBe(commitment.state_hash);
    });
  });

  describe('Edge Cases', () => {
    it('should handle unicode literals', async () => {
      const s = dataFactory.namedNode('http://example.org/s');
      const p = dataFactory.namedNode('http://example.org/p');
      const o = dataFactory.literal('Hello 世界 🌍');

      const result = await store.appendTriple('add', s, p, o);
      expect(result.index).toBe(0n);

      const snapshot = await store.generateSnapshot();
      expect(snapshot.quads_hash).toBeDefined();
    });

    it('should handle blank nodes', async () => {
      const s = dataFactory.blankNode('b1');
      const p = dataFactory.namedNode('http://example.org/p');
      const o = dataFactory.literal('value');

      const result = await store.appendTriple('add', s, p, o);
      expect(result.index).toBe(0n);
    });

    it('should handle large number of triples', async () => {
      const s = dataFactory.namedNode('http://example.org/s');
      const p = dataFactory.namedNode('http://example.org/p');

      for (let i = 0; i < 100; i++) {
        await store.appendTriple('add', s, p, dataFactory.literal(`value${i}`));
      }

      expect(store.getLogIndex()).toBe(100n);

      const snapshot = await store.generateSnapshot();
      expect(snapshot.quad_count).toBeGreaterThanOrEqual(100);
    });
  });

  describe('Quad Count', () => {
    it('should return correct quad count', async () => {
      expect(await store.getQuadCount()).toBe(0);

      const s = dataFactory.namedNode('http://example.org/s');
      const p = dataFactory.namedNode('http://example.org/p');
      const o = dataFactory.literal('value');

      await store.appendTriple('add', s, p, o);
      expect(await store.getQuadCount()).toBeGreaterThanOrEqual(1);
    });
  });
});
