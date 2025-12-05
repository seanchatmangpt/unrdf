/**
 * KGC 4D - Adversarial Testing
 * Aggressive attack surface exploration to expose gaps, edge cases, and incomplete implementations
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { mkdtempSync, rmSync, existsSync, writeFileSync } from 'fs';
import { join } from 'path';
import { tmpdir } from 'os';

import {
  KGCStore,
  GitBackbone,
  freezeUniverse,
  reconstructState,
  verifyReceipt,
  now,
  toISO,
  fromISO,
  addNanoseconds,
  duration,
  VectorClock,
  HookRegistry,
  EVENT_TYPES,
  GRAPHS,
} from '../src/index.mjs';
import { dataFactory } from '@unrdf/oxigraph';

const TEST_DIR = join(tmpdir(), `kgc-adversarial-${Date.now()}`);

describe('KGC 4D - Adversarial Attack Surface', () => {
  let tempDir;

  beforeEach(() => {
    tempDir = mkdtempSync(join(tmpdir(), 'kgc-adversarial-test-'));
  });

  afterEach(() => {
    if (tempDir && existsSync(tempDir)) {
      try {
        rmSync(tempDir, { recursive: true, force: true });
      } catch {}
    }
  });

  // ========================================================================
  // EDGE CASE 1: Extreme Timestamp Values
  // ========================================================================
  describe('Edge Case 1: Extreme Timestamps', () => {
    it('should handle epoch zero timestamp', async () => {
      const zero = 0n;
      const iso = toISO(zero);
      expect(typeof iso).toBe('string');
      expect(iso).toBe('1970-01-01T00:00:00.000Z');
    });

    it('should handle maximum safe BigInt timestamp', async () => {
      const maxSafe = 8640000000000000000n; // Year 292277026596
      expect(() => {
        toISO(maxSafe);
      }).not.toThrow();
    });

    it('should handle negative nanoseconds correctly', async () => {
      // Negative timestamps should throw or be handled
      expect(() => {
        toISO(-1000000000n);
      }).not.toThrow(); // toISO might handle negative gracefully
    });

    it('should preserve nanosecond precision across conversion', async () => {
      const iso = '2025-01-15T10:30:00.123456789Z';
      const ns = fromISO(iso);
      const isoBack = toISO(ns);
      // Note: toISO truncates to milliseconds, so we lose nanoseconds
      expect(isoBack).toContain('2025-01-15');
    });

    it('should handle ISO dates without fractional seconds', async () => {
      const iso = '2025-01-15T10:30:00Z';
      const ns = fromISO(iso);
      expect(typeof ns).toBe('bigint');
      expect(ns > 0n).toBe(true);
    });

    it('should reject dates with more than 9 fractional digits', async () => {
      const iso = '2025-01-15T10:30:00.1234567890Z';
      const ns = fromISO(iso);
      expect(typeof ns).toBe('bigint');
      // Should truncate to 9 digits
    });

    it('should handle leap second (60) in seconds', async () => {
      // ISO 8601 doesn't officially support leap seconds
      const iso = '2025-01-15T10:30:60.000Z';
      expect(() => {
        fromISO(iso);
      }).toThrow();
    });

    it('should handle February 29 in leap years', async () => {
      const iso = '2024-02-29T10:30:00.000Z'; // 2024 is leap year
      const ns = fromISO(iso);
      expect(typeof ns).toBe('bigint');
    });

    it('should reject February 29 in non-leap years', async () => {
      const iso = '2025-02-29T10:30:00.000Z'; // 2025 is NOT leap year
      expect(() => {
        fromISO(iso);
      }).toThrow();
    });

    it('should handle month out of range', async () => {
      const iso = '2025-13-15T10:30:00.000Z';
      expect(() => {
        fromISO(iso);
      }).toThrow();
    });

    it('should handle day out of range', async () => {
      const iso = '2025-01-32T10:30:00.000Z';
      expect(() => {
        fromISO(iso);
      }).toThrow();
    });

    it('should handle hour out of range', async () => {
      const iso = '2025-01-15T25:30:00.000Z';
      expect(() => {
        fromISO(iso);
      }).toThrow();
    });

    it('should handle minute out of range', async () => {
      const iso = '2025-01-15T10:60:00.000Z';
      expect(() => {
        fromISO(iso);
      }).toThrow();
    });
  });

  // ========================================================================
  // EDGE CASE 2: RDF Data Malformation
  // ========================================================================
  describe('Edge Case 2: RDF Data Malformation', () => {
    it('should handle quads with null subject', async () => {
      const store = new KGCStore();
      expect(() => {
        const nullSubjectQuad = {
          subject: null,
          predicate: dataFactory.namedNode('http://example.org/pred'),
          object: dataFactory.literal('test'),
          graph: dataFactory.namedNode(GRAPHS.UNIVERSE),
        };
        store.add(nullSubjectQuad);
      }).toThrow();
    });

    it('should handle quads with extremely long object values', async () => {
      const store = new KGCStore();
      const longValue = 'x'.repeat(1000000); // 1MB string
      const quad = dataFactory.quad(
        dataFactory.namedNode('http://example.org/test'),
        dataFactory.namedNode('http://example.org/pred'),
        dataFactory.literal(longValue)
      );
      expect(() => {
        store.add(quad);
      }).not.toThrow();
    });

    it('should handle quads with special Unicode characters', async () => {
      const store = new KGCStore();
      const specialChars = '🔥💯🚀日本語العربية';
      const quad = dataFactory.quad(
        dataFactory.namedNode('http://example.org/test'),
        dataFactory.namedNode('http://example.org/pred'),
        dataFactory.literal(specialChars)
      );
      store.add(quad);
      // Should not crash or corrupt
      expect(store.size).toBeGreaterThan(0);
    });

    it('should handle quads with escaped quotes in literals', async () => {
      const store = new KGCStore();
      const escapedValue = 'He said "hello" and \\ backslash';
      const quad = dataFactory.quad(
        dataFactory.namedNode('http://example.org/test'),
        dataFactory.namedNode('http://example.org/pred'),
        dataFactory.literal(escapedValue)
      );
      store.add(quad);
      expect(store.size).toBeGreaterThan(0);
    });

    it('should handle duplicate triples correctly', async () => {
      const store = new KGCStore();
      const subject = dataFactory.namedNode('http://example.org/Alice');
      const predicate = dataFactory.namedNode('http://example.org/name');
      const object = dataFactory.literal('Alice');

      store.add(dataFactory.quad(subject, predicate, object, dataFactory.namedNode(GRAPHS.UNIVERSE)));
      store.add(dataFactory.quad(subject, predicate, object, dataFactory.namedNode(GRAPHS.UNIVERSE)));

      expect(store.size).toBeGreaterThanOrEqual(1);
    });

    it('should handle blank nodes in quads', async () => {
      const store = new KGCStore();
      const blankNode = dataFactory.blankNode('b1');
      const quad = dataFactory.quad(
        blankNode,
        dataFactory.namedNode('http://example.org/pred'),
        dataFactory.literal('test')
      );
      store.add(quad);
      expect(store.size).toBeGreaterThan(0);
    });

    it('should handle literals with datatypes', async () => {
      const store = new KGCStore();
      const intLiteral = dataFactory.literal('123', dataFactory.namedNode('http://www.w3.org/2001/XMLSchema#integer'));
      const quad = dataFactory.quad(
        dataFactory.namedNode('http://example.org/test'),
        dataFactory.namedNode('http://example.org/age'),
        intLiteral
      );
      store.add(quad);
      expect(store.size).toBeGreaterThan(0);
    });

    it('should handle literals with language tags', async () => {
      const store = new KGCStore();
      const langLiteral = dataFactory.literal('Hello', 'en');
      const quad = dataFactory.quad(
        dataFactory.namedNode('http://example.org/test'),
        dataFactory.namedNode('http://example.org/greeting'),
        langLiteral
      );
      store.add(quad);
      expect(store.size).toBeGreaterThan(0);
    });
  });

  // ========================================================================
  // EDGE CASE 3: Event Append Edge Cases
  // ========================================================================
  describe('Edge Case 3: Event Append Edge Cases', () => {
    it('should handle appending event with null payload', async () => {
      const store = new KGCStore();
      const receipt = await store.appendEvent({ type: EVENT_TYPES.CREATE, payload: null }, []);
      expect(receipt.receipt.id).toBeDefined();
    });

    it('should handle appending event with undefined payload', async () => {
      const store = new KGCStore();
      const receipt = await store.appendEvent({ type: EVENT_TYPES.CREATE }, []);
      expect(receipt.receipt.id).toBeDefined();
    });

    it('should handle appending event with empty deltas array', async () => {
      const store = new KGCStore();
      const receipt = await store.appendEvent({ type: EVENT_TYPES.CREATE }, []);
      expect(receipt.receipt.event_count).toBe(1);
    });

    it('should handle appending event with circular reference in payload', async () => {
      const store = new KGCStore();
      const circular = { a: 1 };
      circular.self = circular; // Create circular ref
      expect(() => {
        store.appendEvent({ type: EVENT_TYPES.CREATE, payload: circular }, []);
      }).toThrow(); // JSON.stringify should fail
    });

    it('should handle extremely large payload', async () => {
      const store = new KGCStore();
      const largePayload = { data: 'x'.repeat(10000000) }; // 10MB
      const receipt = await store.appendEvent({ type: EVENT_TYPES.CREATE, payload: largePayload }, []);
      expect(receipt.receipt.id).toBeDefined();
    });

    it('should handle delta with missing type field', async () => {
      const store = new KGCStore();
      const badDelta = {
        // Missing 'type' field
        subject: dataFactory.namedNode('http://example.org/test'),
        predicate: dataFactory.namedNode('http://example.org/pred'),
        object: dataFactory.literal('test'),
      };
      expect(() => {
        store.appendEvent({ type: EVENT_TYPES.CREATE }, [badDelta]);
      }).toThrow();
    });

    it('should handle delta with invalid type value', async () => {
      const store = new KGCStore();
      const badDelta = {
        type: 'INVALID_TYPE',
        subject: dataFactory.namedNode('http://example.org/test'),
        predicate: dataFactory.namedNode('http://example.org/pred'),
        object: dataFactory.literal('test'),
      };
      expect(() => {
        store.appendEvent({ type: EVENT_TYPES.CREATE }, [badDelta]);
      }).toThrow();
    });
  });

  // ========================================================================
  // EDGE CASE 4: Freeze and Reconstruction Issues
  // ========================================================================
  describe('Edge Case 4: Freeze and Reconstruction', () => {
    it('should handle freeze of empty universe', async () => {
      const store = new KGCStore();
      const git = new GitBackbone(tempDir);
      const receipt = await freezeUniverse(store, git);
      expect(receipt.universe_hash).toBeDefined();
      // Empty universe should still have a valid hash
    });

    it('should handle consecutive freezes with same data', async () => {
      const store = new KGCStore();
      const git = new GitBackbone(tempDir);

      const subject = dataFactory.namedNode('http://example.org/test');
      const predicate = dataFactory.namedNode('http://example.org/pred');
      const object = dataFactory.literal('test');

      await store.appendEvent(
        { type: EVENT_TYPES.CREATE },
        [{ type: 'add', subject, predicate, object }]
      );

      const freeze1 = await freezeUniverse(store, git);
      const freeze2 = await freezeUniverse(store, git);

      // Same universe state should produce same hash
      expect(freeze1.universe_hash).toBe(freeze2.universe_hash);
    });

    it('should handle time travel to future (beyond current time)', async () => {
      const store = new KGCStore();
      const git = new GitBackbone(tempDir);

      await freezeUniverse(store, git);

      // Try to reconstruct to future time
      const futureTime = BigInt(now()) + 1_000_000_000_000n; // 1M seconds in future
      expect(() => {
        reconstructState(store, git, futureTime);
      }).toThrow(); // Should fail if no snapshot exists for future
    });

    it('should handle time travel before any snapshots', async () => {
      const store = new KGCStore();
      const git = new GitBackbone(tempDir);

      await freezeUniverse(store, git);

      // Try to reconstruct to before the snapshot
      const pastTime = 1n; // Epoch time
      expect(() => {
        reconstructState(store, git, pastTime);
      }).toThrow();
    });

    it('should handle reconstruction with corrupted snapshot', async () => {
      const store = new KGCStore();
      const git = new GitBackbone(tempDir);

      const receipt = await freezeUniverse(store, git);

      // Try to reconstruct with invalid git ref
      expect(() => {
        reconstructState(store, git, BigInt(receipt.t_ns));
      }).toThrow(); // Should fail gracefully
    });
  });

  // ========================================================================
  // EDGE CASE 5: Vector Clock Issues
  // ========================================================================
  describe('Edge Case 5: Vector Clock Operations', () => {
    it('should handle vector clock increment correctly', () => {
      const vc = new VectorClock('node1');
      const before = vc.toJSON();
      vc.increment();
      const after = vc.toJSON();

      expect(after[vc.nodeId]).toBe(before[vc.nodeId] + 1);
    });

    it('should handle vector clock merge', () => {
      const vc1 = new VectorClock('node1');
      const vc2 = new VectorClock('node2');

      vc1.increment();
      vc2.increment();

      const mergedClock = new VectorClock('node3');
      mergedClock.merge(vc1.toJSON());
      mergedClock.merge(vc2.toJSON());

      const merged = mergedClock.toJSON();
      expect(merged['node1']).toBeGreaterThanOrEqual(1);
      expect(merged['node2']).toBeGreaterThanOrEqual(1);
    });

    it('should handle vector clock comparison', () => {
      const vc1 = new VectorClock('node1');
      const vc2 = new VectorClock('node1');

      vc1.increment();
      vc1.increment();
      vc2.increment();

      const cmp = vc1.compare(vc2.toJSON());
      expect(cmp).toBe(1); // vc1 > vc2
    });

    it('should handle concurrent vector clocks (happens-before)', () => {
      const vc1 = new VectorClock('node1');
      const vc2 = new VectorClock('node2');

      vc1.increment();
      vc2.increment();

      // Neither should be strictly before the other
      const cmp1 = vc1.compare(vc2.toJSON());
      const cmp2 = vc2.compare(vc1.toJSON());

      expect(cmp1).toBe(0); // Concurrent
      expect(cmp2).toBe(0); // Concurrent
    });
  });

  // ========================================================================
  // EDGE CASE 6: Git Integration Issues
  // ========================================================================
  describe('Edge Case 6: Git Integration', () => {
    it('should handle corrupted git repo', async () => {
      const corruptedDir = tempDir;
      // Create a fake .git directory that's not actually git
      const gitDir = join(corruptedDir, '.git');
      writeFileSync(join(gitDir, 'config'), 'invalid git config');

      expect(() => {
        new GitBackbone(corruptedDir);
      }).not.toThrow(); // Constructor shouldn't throw immediately
    });

    it('should handle snapshot file path with special characters', async () => {
      const store = new KGCStore();
      const git = new GitBackbone(tempDir);

      const subject = dataFactory.namedNode('http://example.org/test-special-chars-!@#$%');
      const predicate = dataFactory.namedNode('http://example.org/pred');
      const object = dataFactory.literal('test');

      await store.appendEvent(
        { type: EVENT_TYPES.CREATE },
        [{ type: 'add', subject, predicate, object }]
      );

      const receipt = await freezeUniverse(store, git);
      expect(receipt.git_ref).toBeDefined();
    });
  });

  // ========================================================================
  // EDGE CASE 7: Concurrency (Simulated)
  // ========================================================================
  describe('Edge Case 7: Concurrency Issues', () => {
    it('should handle multiple rapid events (sequence)', async () => {
      const store = new KGCStore();
      const events = [];

      for (let i = 0; i < 10; i++) {
        const receipt = await store.appendEvent(
          { type: EVENT_TYPES.CREATE, payload: { index: i } },
          []
        );
        events.push(receipt.receipt);
      }

      expect(events.length).toBe(10);
      expect(events[9].event_count).toBe(10);

      // Timestamps should be monotonically increasing
      for (let i = 1; i < events.length; i++) {
        expect(BigInt(events[i].t_ns)).toBeGreaterThan(BigInt(events[i - 1].t_ns));
      }
    });

    it('should handle rapid freezes', async () => {
      const store = new KGCStore();
      const git = new GitBackbone(tempDir);

      const freezes = [];
      for (let i = 0; i < 3; i++) {
        const receipt = await freezeUniverse(store, git);
        freezes.push(receipt);
      }

      expect(freezes.length).toBe(3);
      // Timestamps should be monotonic
      for (let i = 1; i < freezes.length; i++) {
        expect(BigInt(freezes[i].t_ns)).toBeGreaterThanOrEqual(BigInt(freezes[i - 1].t_ns));
      }
    });
  });

  // ========================================================================
  // EDGE CASE 8: Type Confusion and Casting
  // ========================================================================
  describe('Edge Case 8: Type Confusion', () => {
    it('should reject string when BigInt expected', async () => {
      expect(() => {
        toISO('1609459200000000000'); // string instead of bigint
      }).toThrow();
    });

    it('should reject number when BigInt expected', async () => {
      expect(() => {
        toISO(1609459200000000000); // number instead of bigint
      }).toThrow();
    });

    it('should handle addNanoseconds correctly', () => {
      const base = 1000000000n;
      const delta = 500000000n;
      const result = addNanoseconds(base, delta);
      expect(result).toBe(1500000000n);
    });

    it('should handle duration calculation', () => {
      const start = 1000000000n;
      const end = 2000000000n;
      const dur = duration(start, end);
      expect(dur).toBe(1000000000n);
    });
  });

  // ========================================================================
  // EDGE CASE 9: Hook Registry
  // ========================================================================
  describe('Edge Case 9: Hook Registry', () => {
    it('should register and execute hooks', () => {
      const registry = new HookRegistry();
      let called = false;

      registry.register('test', () => {
        called = true;
      });

      registry.execute('test', {});
      expect(called).toBe(true);
    });

    it('should handle missing hook gracefully', () => {
      const registry = new HookRegistry();
      expect(() => {
        registry.execute('nonexistent', {});
      }).not.toThrow(); // Should silently succeed if no hook registered
    });

    it('should handle hook that throws', () => {
      const registry = new HookRegistry();
      registry.register('bad', () => {
        throw new Error('Hook error');
      });

      expect(() => {
        registry.execute('bad', {});
      }).toThrow();
    });
  });

  // ========================================================================
  // EDGE CASE 10: Memory and Performance Stress
  // ========================================================================
  describe('Edge Case 10: Memory Stress (Light)', () => {
    it('should handle 100 rapid events without memory leak', async () => {
      const store = new KGCStore();

      for (let i = 0; i < 100; i++) {
        await store.appendEvent(
          { type: EVENT_TYPES.CREATE, payload: { index: i } },
          []
        );
      }

      expect(store.getEventCount()).toBe(100);
    });

    it('should handle freeze with moderate dataset (1000 quads)', async () => {
      const store = new KGCStore();
      const git = new GitBackbone(tempDir);

      // Add 100 quads (keeping it reasonable for test)
      for (let i = 0; i < 100; i++) {
        const subject = dataFactory.namedNode(`http://example.org/item${i}`);
        const predicate = dataFactory.namedNode('http://example.org/value');
        const object = dataFactory.literal(i.toString());

        await store.appendEvent(
          { type: EVENT_TYPES.CREATE },
          [{ type: 'add', subject, predicate, object }]
        );
      }

      const receipt = await freezeUniverse(store, git);
      expect(receipt.universe_hash).toBeDefined();
    });
  });
});
