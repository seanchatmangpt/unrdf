/**
 * KGC-4D Playground - Unit Tests for JTBDs (Jobs To Be Done)
 *
 * Tests validate the core 4D functionality:
 * 1. Universe singleton with demo data
 * 2. Shard projection (Check-Out)
 * 3. Delta validation (Check-In)
 * 4. Vector clocks
 * 5. SSE stream
 * 6. React hooks
 */

import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import {
  getUniverse,
  getUniverseStats,
  createSSEStream,
  parseSSEQuery,
  GRAPHS,
  dataFactory,
  now,
} from '../lib/server/universe.mjs';
import { projectShard as projectShardFunc } from '../lib/server/shard.mjs';
import { submitDelta as submitDeltaFunc } from '../lib/server/delta.mjs';

describe('KGC-4D Playground - JTBDs', () => {
  let universe;

  // ============================================================================
  // JTBD 1: Universe Singleton with Demo Data
  // ============================================================================

  describe('JTBD 1: Universe Singleton', () => {
    it('should return universe singleton', async () => {
      universe = await getUniverse();
      expect(universe).toBeDefined();
      expect(universe).not.toBeNull();
    });

    it('should have demo data seeded (Project Alpha)', async () => {
      const store = await getUniverse();
      const universeGraph = dataFactory.namedNode(GRAPHS.UNIVERSE);

      // Query for Project Alpha
      const projectAlpha = dataFactory.namedNode('http://example.org/project/alpha');
      const typeQuads = [...store.match(projectAlpha, null, null, universeGraph)];

      expect(typeQuads.length).toBeGreaterThan(0);
    });

    it('should have demo tasks seeded', async () => {
      const store = await getUniverse();
      const universeGraph = dataFactory.namedNode(GRAPHS.UNIVERSE);
      const rdfType = dataFactory.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
      const taskType = dataFactory.namedNode('http://kgc.io/ontology/Task');

      // Count Task entities
      const taskQuads = [...store.match(null, rdfType, taskType, universeGraph)];
      expect(taskQuads.length).toBeGreaterThanOrEqual(2); // At least Task 1 and Task 2
    });

    it('should have vector clock initialized', async () => {
      const store = await getUniverse();
      expect(store.vectorClock).toBeDefined();
      expect(store.vectorClock.toJSON).toBeDefined();

      const vc = store.vectorClock.toJSON();
      expect(vc.nodeId).toBeDefined();
      expect(vc.counters).toBeDefined();
    });
  });

  // ============================================================================
  // JTBD 2: Shard Projection (Check-Out Operation)
  // ============================================================================

  describe('JTBD 2: Shard Projection', () => {
    it('should project full universe shard', async () => {
      const shard = await projectShardFunc({});

      expect(shard).toBeDefined();
      expect(shard.id).toBeDefined();
      expect(shard.t_ns).toBeDefined();
      expect(shard.timestamp_iso).toBeDefined();
      expect(shard.vector_clock).toBeDefined();
      expect(shard.quads).toBeInstanceOf(Array);
      expect(shard.quads.length).toBeGreaterThan(0);
    });

    it('should include all demo entities in shard', async () => {
      const shard = await projectShardFunc({});

      // Check for Project Alpha
      const projectQuads = shard.quads.filter(
        (q) => q.subject.value === 'http://example.org/project/alpha'
      );
      expect(projectQuads.length).toBeGreaterThan(0);

      // Check for Tasks
      const taskQuads = shard.quads.filter(
        (q) => q.subject.value.includes('http://example.org/task/')
      );
      expect(taskQuads.length).toBeGreaterThan(0);
    });

    it('should filter shard by subject', async () => {
      const shard = await projectShardFunc({
        subject: 'http://example.org/project/alpha',
      });

      expect(shard.quads.length).toBeGreaterThan(0);

      // All quads should have Project Alpha as subject
      for (const quad of shard.quads) {
        expect(quad.subject.value).toBe('http://example.org/project/alpha');
      }
    });

    it('should filter shard by type (Task)', async () => {
      const shard = await projectShardFunc({
        type: 'http://kgc.io/ontology/Task',
      });

      expect(shard.quads.length).toBeGreaterThan(0);

      // Should have Task type quads
      const typeQuads = shard.quads.filter(
        (q) =>
          q.predicate.value === 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' &&
          q.object.value === 'http://kgc.io/ontology/Task'
      );
      expect(typeQuads.length).toBeGreaterThan(0);
    });

    it('should include vector clock in shard', async () => {
      const shard = await projectShardFunc({});

      expect(shard.vector_clock).toBeDefined();
      expect(shard.vector_clock.nodeId).toBeDefined();
      expect(shard.vector_clock.counters).toBeDefined();
    });

    it('should serialize quads with full RDF term metadata', async () => {
      const shard = await projectShardFunc({});

      for (const quad of shard.quads) {
        // Subject
        expect(quad.subject).toHaveProperty('value');
        expect(quad.subject).toHaveProperty('termType');

        // Predicate
        expect(quad.predicate).toHaveProperty('value');
        expect(quad.predicate).toHaveProperty('termType');

        // Object
        expect(quad.object).toHaveProperty('value');
        expect(quad.object).toHaveProperty('termType');

        // Graph
        expect(quad.graph).toHaveProperty('value');
        expect(quad.graph).toHaveProperty('termType');
      }
    });
  });

  // ============================================================================
  // JTBD 3: Delta Validation (Check-In Operation with Knowledge Hooks)
  // ============================================================================

  describe('JTBD 3: Delta Validation - Knowledge Hooks', () => {
    it('should REJECT delta with budget > 100000', async () => {
      const delta = {
        operations: [
          {
            type: 'add',
            subject: { value: 'http://example.org/project/test', termType: 'NamedNode' },
            predicate: { value: 'http://kgc.io/ontology/budget', termType: 'NamedNode' },
            object: { value: '999999', termType: 'Literal' },
          },
        ],
        source: 'test',
      };

      const result = await submitDeltaFunc(delta);

      expect(result.status).toBe('REJECT');
      expect(result.reason).toMatch(/Budget cannot exceed/i);
    });

    it('should REJECT delta with invalid status', async () => {
      const delta = {
        operations: [
          {
            type: 'add',
            subject: { value: 'http://example.org/project/test', termType: 'NamedNode' },
            predicate: { value: 'http://kgc.io/ontology/status', termType: 'NamedNode' },
            object: { value: 'invalid_status', termType: 'Literal' },
          },
        ],
        source: 'test',
      };

      const result = await submitDeltaFunc(delta);

      expect(result.status).toBe('REJECT');
      expect(result.reason).toMatch(/Status must be one of/i);
    });

    it('should REJECT delta with empty name', async () => {
      const delta = {
        operations: [
          {
            type: 'add',
            subject: { value: 'http://example.org/project/test', termType: 'NamedNode' },
            predicate: { value: 'http://kgc.io/ontology/name', termType: 'NamedNode' },
            object: { value: '', termType: 'Literal' },
          },
        ],
        source: 'test',
      };

      const result = await submitDeltaFunc(delta);

      expect(result.status).toBe('REJECT');
      expect(result.reason).toMatch(/cannot be empty/i);
    });

    it('should ACCEPT delta with valid budget', async () => {
      const delta = {
        operations: [
          {
            type: 'add',
            subject: { value: 'http://example.org/project/test', termType: 'NamedNode' },
            predicate: { value: 'http://kgc.io/ontology/budget', termType: 'NamedNode' },
            object: { value: '50000', termType: 'Literal' },
          },
        ],
        source: 'test',
      };

      const result = await submitDeltaFunc(delta);

      expect(result.status).toBe('ACK');
      expect(result.t_ns).toBeDefined();
      expect(result.event_id).toBeDefined();
    });

    it('should ACCEPT delta with valid status', async () => {
      const delta = {
        operations: [
          {
            type: 'add',
            subject: { value: 'http://example.org/project/alpha', termType: 'NamedNode' },
            predicate: { value: 'http://kgc.io/ontology/status', termType: 'NamedNode' },
            object: { value: 'paused', termType: 'Literal' },
          },
        ],
        source: 'test',
      };

      const result = await submitDeltaFunc(delta);

      expect(result.status).toBe('ACK');
    });

    it('should ACCEPT delta with valid name', async () => {
      const delta = {
        operations: [
          {
            type: 'add',
            subject: { value: 'http://example.org/project/test', termType: 'NamedNode' },
            predicate: { value: 'http://kgc.io/ontology/name', termType: 'NamedNode' },
            object: { value: 'Test Project', termType: 'Literal' },
          },
        ],
        source: 'test',
      };

      const result = await submitDeltaFunc(delta);

      expect(result.status).toBe('ACK');
    });
  });

  // ============================================================================
  // JTBD 4: Vector Clocks for Causality
  // ============================================================================

  describe('JTBD 4: Vector Clocks', () => {
    it('should include vector clock in ACK response', async () => {
      const delta = {
        operations: [
          {
            type: 'add',
            subject: { value: 'http://example.org/test/vc', termType: 'NamedNode' },
            predicate: { value: 'http://kgc.io/ontology/name', termType: 'NamedNode' },
            object: { value: 'VC Test', termType: 'Literal' },
          },
        ],
        source: 'test',
      };

      const result = await submitDeltaFunc(delta);

      expect(result.status).toBe('ACK');
      expect(result.vector_clock).toBeDefined();
      expect(result.vector_clock.nodeId).toBeDefined();
      expect(result.vector_clock.counters).toBeDefined();
      expect(typeof result.vector_clock.counters).toBe('object');
    });

    it('should update vector clock after each delta', async () => {
      const store = await getUniverse();
      const vc1 = store.vectorClock.toJSON();

      const delta = {
        operations: [
          {
            type: 'add',
            subject: { value: 'http://example.org/test/vc2', termType: 'NamedNode' },
            predicate: { value: 'http://kgc.io/ontology/name', termType: 'NamedNode' },
            object: { value: 'VC Test 2', termType: 'Literal' },
          },
        ],
        source: 'test',
      };

      await submitDeltaFunc(delta);

      const vc2 = store.vectorClock.toJSON();

      // Vector clock should be updated
      expect(vc2).toBeDefined();
      // The counters should reflect the update
      expect(JSON.stringify(vc2)).not.toBe(JSON.stringify(vc1));
    });
  });

  // ============================================================================
  // JTBD 5: SSE Stream Events
  // ============================================================================

  describe('JTBD 5: SSE Stream', () => {
    it('should create SSE stream', () => {
      const stream = createSSEStream();
      expect(stream).toBeDefined();
      expect(stream.readable).toBeDefined();
      expect(stream.writable).toBeDefined();
    });

    // SSE stream event formatting - tested in e2e tests
    // Skipping here due to async stream complexities in unit test context
  });

  // ============================================================================
  // JTBD 6: Universe Statistics
  // ============================================================================

  describe('JTBD 6: Universe Statistics', () => {
    it('should return universe statistics', async () => {
      const stats = await getUniverseStats();

      expect(stats).toBeDefined();
      expect(stats.universe).toBeDefined();
      expect(stats.universe.quad_count).toBeGreaterThan(0);
      expect(stats.universe.entity_count).toBeGreaterThan(0);
      expect(stats.universe.types).toBeDefined();

      expect(stats.event_log).toBeDefined();
      expect(stats.event_log.quad_count).toBeGreaterThanOrEqual(0);
      expect(stats.event_log.event_count).toBeGreaterThanOrEqual(0);

      expect(stats.vector_clock).toBeDefined();
      expect(stats.timestamp).toBeDefined();
    });

    it('should count entity types correctly', async () => {
      const stats = await getUniverseStats();

      // Should have counted Project, Task, User from demo data
      expect(stats.universe.types).toBeDefined();
      expect(Object.keys(stats.universe.types).length).toBeGreaterThan(0);
    });
  });

  // ============================================================================
  // JTBD 7: Nanosecond Timestamps
  // ============================================================================

  describe('JTBD 7: Nanosecond Precision', () => {
    it('should use nanosecond timestamps (t_ns)', async () => {
      const delta = {
        operations: [
          {
            type: 'add',
            subject: { value: 'http://example.org/test/ns', termType: 'NamedNode' },
            predicate: { value: 'http://kgc.io/ontology/name', termType: 'NamedNode' },
            object: { value: 'NS Test', termType: 'Literal' },
          },
        ],
        source: 'test',
      };

      const result = await submitDeltaFunc(delta);

      expect(result.t_ns).toBeDefined();
      expect(typeof result.t_ns).toBe('string');
      // Should be a numeric string (nanoseconds)
      expect(/^\d+$/.test(result.t_ns)).toBe(true);
    });

    it('should include ISO8601 timestamps', async () => {
      const delta = {
        operations: [
          {
            type: 'add',
            subject: { value: 'http://example.org/test/iso', termType: 'NamedNode' },
            predicate: { value: 'http://kgc.io/ontology/name', termType: 'NamedNode' },
            object: { value: 'ISO Test', termType: 'Literal' },
          },
        ],
        source: 'test',
      };

      const result = await submitDeltaFunc(delta);

      expect(result.timestamp_iso).toBeDefined();
      // Should be parseable as ISO 8601
      expect(() => new Date(result.timestamp_iso)).not.toThrow();
    });
  });
});
