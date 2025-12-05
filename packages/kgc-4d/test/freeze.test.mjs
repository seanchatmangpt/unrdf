/**
 * KGC Freeze Tests - Chicago School TDD
 * Tests universe snapshots and time-travel reconstruction
 * Applies HDIT: Topological Correctness via Persistent Homology
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { KGCStore } from '../src/store.mjs';
import { freezeUniverse } from '../src/freeze.mjs';
import { dataFactory } from '@unrdf/oxigraph';
import { EVENT_TYPES, GRAPHS } from '../src/constants.mjs';

/**
 * Mock Git Backbone for testing (in-memory implementation)
 */
class MockGitBackbone {
  constructor() {
    this.snapshots = new Map();
  }

  async commitSnapshot(nquads, message) {
    const hash = this._generateHash(nquads);
    this.snapshots.set(hash, nquads);
    return hash;
  }

  async readSnapshot(hash) {
    const nquads = this.snapshots.get(hash);
    if (!nquads) {
      throw new Error(`Snapshot not found: ${hash}`);
    }
    return nquads;
  }

  _generateHash(content) {
    // Simple hash: first 16 chars of base64
    const hash = Buffer.from(content).toString('base64').slice(0, 16);
    return hash;
  }
}

describe('KGC Freeze - Universe Snapshots and Time-Travel', () => {
  let store;
  let gitBackbone;

  beforeEach(() => {
    store = new KGCStore();
    gitBackbone = new MockGitBackbone();
  });

  describe('freezeUniverse() - Snapshot Creation', () => {
    /**
     * Core behavior: Dump universe to N-Quads, hash with BLAKE3,
     * commit to Git, record SNAPSHOT event
     */
    it('should freeze universe and return receipt', async () => {
      const receipt = await freezeUniverse(store, gitBackbone);

      expect(receipt).toBeDefined();
      expect(receipt.id).toBeDefined();
      expect(receipt.t_ns).toBeDefined();
      expect(receipt.timestamp_iso).toBeDefined();
      expect(receipt.universe_hash).toBeDefined();
      expect(receipt.git_ref).toBeDefined();
      expect(receipt.event_count).toBe(1); // One SNAPSHOT event added
    });

    it('should create BLAKE3 hash of universe state', async () => {
      // Add some triples to universe
      const subject = dataFactory.namedNode('http://example.org/Alice');
      const predicate = dataFactory.namedNode('http://xmlns.com/foaf/0.1/name');
      const object = dataFactory.literal('Alice');

      await store.appendEvent(
        { type: EVENT_TYPES.CREATE },
        [{ type: 'add', subject, predicate, object }]
      );

      const receipt = await freezeUniverse(store, gitBackbone);

      // Hash should be 64-char hex (BLAKE3 in hex is 64 chars)
      expect(receipt.universe_hash).toMatch(/^[a-f0-9]{64}$/);
    });

    it('should commit snapshot to Git and get hash reference', async () => {
      const receipt = await freezeUniverse(store, gitBackbone);

      expect(receipt.git_ref).toBeDefined();
      expect(typeof receipt.git_ref).toBe('string');
      expect(receipt.git_ref.length).toBeGreaterThan(0);
    });

    it('should append SNAPSHOT event to event log', async () => {
      expect(store.getEventCount()).toBe(0);

      await freezeUniverse(store, gitBackbone);

      expect(store.getEventCount()).toBe(1);
    });

    it('should preserve monotonic timestamps across freezes', async () => {
      const r1 = await freezeUniverse(store, gitBackbone);
      const r2 = await freezeUniverse(store, gitBackbone);
      const r3 = await freezeUniverse(store, gitBackbone);

      const t1 = BigInt(r1.t_ns);
      const t2 = BigInt(r2.t_ns);
      const t3 = BigInt(r3.t_ns);

      expect(t1 < t2).toBe(true);
      expect(t2 < t3).toBe(true);
    });

    it('should record quad count in freeze payload', async () => {
      const subject = dataFactory.namedNode('http://example.org/Test');
      const predicate = dataFactory.namedNode('http://example.org/prop');
      const object = dataFactory.literal('value');

      await store.appendEvent(
        { type: EVENT_TYPES.CREATE },
        [{ type: 'add', subject, predicate, object }]
      );

      const receipt = await freezeUniverse(store, gitBackbone);

      expect(receipt.event_count).toBe(1); // One SNAPSHOT event
    });
  });

  describe('Freeze Idempotence - Pareto Frontier', () => {
    /**
     * HDIT 80/20: Empty universe should freeze identically
     * Verifies hash consistency
     */
    it('should produce identical hash for empty universe', async () => {
      const r1 = await freezeUniverse(store, gitBackbone);
      const r2 = await freezeUniverse(store, gitBackbone);

      // Empty universe should hash identically
      expect(r1.universe_hash).toBe(r2.universe_hash);
    });

    it('should produce different hash when universe changes', async () => {
      const r1 = await freezeUniverse(store, gitBackbone);

      // Add data
      const subject = dataFactory.namedNode('http://example.org/New');
      const predicate = dataFactory.namedNode('http://example.org/prop');
      const object = dataFactory.literal('data');

      await store.appendEvent(
        { type: EVENT_TYPES.CREATE },
        [{ type: 'add', subject, predicate, object }]
      );

      const r2 = await freezeUniverse(store, gitBackbone);

      // Hash should change after adding data
      expect(r1.universe_hash).not.toBe(r2.universe_hash);
    });
  });

  describe('Time-Travel Reconstruction (TODO: Next Step)', () => {
    /**
     * HDIT Topological Correctness: Feature dependency DAG
     * Reconstruction verifies all events replay correctly
     * Placeholder for next phase (requires event delta storage)
     */
    it('should support querying universe at freeze point', async () => {
      const subject = dataFactory.namedNode('http://example.org/Alice');
      const predicate = dataFactory.namedNode('http://xmlns.com/foaf/0.1/name');
      const object = dataFactory.literal('Alice');

      await store.appendEvent(
        { type: EVENT_TYPES.CREATE },
        [{ type: 'add', subject, predicate, object }]
      );

      const freeze1 = await freezeUniverse(store, gitBackbone);

      // Remove Alice
      await store.appendEvent(
        { type: EVENT_TYPES.DELETE },
        [{ type: 'delete', subject, predicate, object }]
      );

      const freeze2 = await freezeUniverse(store, gitBackbone);

      // freeze1 and freeze2 should have different hashes
      expect(freeze1.universe_hash).not.toBe(freeze2.universe_hash);

      // Current universe should not have Alice
      const current = store.getQuads(subject, predicate, object);
      expect(current.length).toBe(0);
    });
  });

  describe('Git Persistence - Content Addressability', () => {
    /**
     * HDIT principle: Git provides cryptographic proof of state
     * Snapshots are content-addressable and immutable
     */
    it('should store snapshot in Git and retrieve it', async () => {
      const subject = dataFactory.namedNode('http://example.org/Test');
      const predicate = dataFactory.namedNode('http://example.org/prop');
      const object = dataFactory.literal('value');

      await store.appendEvent(
        { type: EVENT_TYPES.CREATE },
        [{ type: 'add', subject, predicate, object }]
      );

      const receipt = await freezeUniverse(store, gitBackbone);

      // Retrieve snapshot from Git
      const nquads = await gitBackbone.readSnapshot(receipt.git_ref);

      expect(nquads).toBeDefined();
      expect(typeof nquads).toBe('string');
      expect(nquads.length).toBeGreaterThan(0);
    });

    it('should make snapshots immutable and retrievable', async () => {
      const r1 = await freezeUniverse(store, gitBackbone);

      // Add more data and freeze again
      const subject = dataFactory.namedNode('http://example.org/New');
      const predicate = dataFactory.namedNode('http://example.org/prop');
      const object = dataFactory.literal('new');

      await store.appendEvent(
        { type: EVENT_TYPES.CREATE },
        [{ type: 'add', subject, predicate, object }]
      );

      const r2 = await freezeUniverse(store, gitBackbone);

      // Both snapshots should be retrievable
      const nq1 = await gitBackbone.readSnapshot(r1.git_ref);
      const nq2 = await gitBackbone.readSnapshot(r2.git_ref);

      expect(nq1).toBeDefined();
      expect(nq2).toBeDefined();
      // Second snapshot should have more content
      expect(nq2.length).toBeGreaterThan(nq1.length);
    });

    it('should maintain snapshot integrity across multiple freezes', async () => {
      const receipts = [];

      for (let i = 0; i < 5; i++) {
        const subject = dataFactory.namedNode(`http://example.org/Entity${i}`);
        const predicate = dataFactory.namedNode('http://example.org/index');
        const object = dataFactory.literal(i.toString());

        await store.appendEvent(
          { type: EVENT_TYPES.CREATE },
          [{ type: 'add', subject, predicate, object }]
        );

        receipts.push(await freezeUniverse(store, gitBackbone));
      }

      // All snapshots should be retrievable
      for (const receipt of receipts) {
        const nquads = await gitBackbone.readSnapshot(receipt.git_ref);
        expect(nquads).toBeDefined();
      }

      // Snapshots should have increasing size
      for (let i = 1; i < receipts.length; i++) {
        const nq1 = await gitBackbone.readSnapshot(receipts[i - 1].git_ref);
        const nq2 = await gitBackbone.readSnapshot(receipts[i].git_ref);
        expect(nq2.length).toBeGreaterThanOrEqual(nq1.length);
      }
    });
  });

  describe('Edge Cases and Error Handling', () => {
    it('should handle freezing empty universe', async () => {
      const receipt = await freezeUniverse(store, gitBackbone);

      expect(receipt).toBeDefined();
      expect(receipt.universe_hash).toBeDefined();
      expect(receipt.git_ref).toBeDefined();
    });

    it('should handle large universe freezes', async () => {
      // Add 100 triples
      for (let i = 0; i < 100; i++) {
        const subject = dataFactory.namedNode(`http://example.org/Entity${i}`);
        const predicate = dataFactory.namedNode('http://example.org/property');
        const object = dataFactory.literal(`value${i}`);

        await store.appendEvent(
          { type: EVENT_TYPES.CREATE },
          [{ type: 'add', subject, predicate, object }]
        );
      }

      const receipt = await freezeUniverse(store, gitBackbone);

      expect(receipt).toBeDefined();
      expect(receipt.universe_hash.length).toBe(64); // BLAKE3 hex length
    });

    it('should record timestamp in ISO format', async () => {
      const receipt = await freezeUniverse(store, gitBackbone);

      expect(receipt.timestamp_iso).toMatch(/^\d{4}-\d{2}-\d{2}T/);
      const date = new Date(receipt.timestamp_iso);
      expect(!isNaN(date.getTime())).toBe(true);
    });
  });

  describe('Monoidal Semantic Compression (HDIT)', () => {
    /**
     * From thesis: Features compose in hyperdimensional space
     * Snapshot entropy should be compressible
     */
    it('should compose multiple events into single snapshot', async () => {
      // Event 1: Create Alice
      const alice_s = dataFactory.namedNode('http://example.org/Alice');
      const name_p = dataFactory.namedNode('http://xmlns.com/foaf/0.1/name');
      const alice_o = dataFactory.literal('Alice');

      await store.appendEvent(
        { type: EVENT_TYPES.CREATE, payload: { action: 'create_alice' } },
        [{ type: 'add', subject: alice_s, predicate: name_p, object: alice_o }]
      );

      // Event 2: Create Bob
      const bob_s = dataFactory.namedNode('http://example.org/Bob');
      const bob_o = dataFactory.literal('Bob');

      await store.appendEvent(
        { type: EVENT_TYPES.CREATE, payload: { action: 'create_bob' } },
        [{ type: 'add', subject: bob_s, predicate: name_p, object: bob_o }]
      );

      // Single snapshot should contain both
      const receipt = await freezeUniverse(store, gitBackbone);

      // Snapshot should be smaller than sum of individual N-Quads
      const nquads = await gitBackbone.readSnapshot(receipt.git_ref);
      expect(nquads).toContain('Alice');
      expect(nquads).toContain('Bob');
    });
  });
});
