/**
 * @file Temporal SPARQL Tests - Production Quality
 * @description Comprehensive tests for temporal SPARQL with time-travel queries
 *
 * Tests cover:
 * - Query parsing (AT TIMESTAMP, BETWEEN)
 * - Point-in-time queries
 * - Time-range queries
 * - Cache performance (>75% hit rate)
 * - Nanosecond precision
 * - Performance targets (<250ms cold, <50ms cached)
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { mkdtempSync, rmSync, existsSync } from 'fs';
import { join } from 'path';
import { tmpdir } from 'os';
import { KGCStore } from '../src/store.mjs';
import { GitBackbone } from '../src/git.mjs';
import { freezeUniverse } from '../src/freeze.mjs';
import { TemporalSPARQL } from '../src/temporal-sparql.mjs';
import { parseTemporalQuery, extractBaseSparql, hasTemporalClauses, validateTemporalQuery } from '../src/temporal-query-parser.mjs';
import { TemporalCache } from '../src/temporal-cache.mjs';
import { HistoryReconstructor } from '../src/history-reconstructor.mjs';
import { dataFactory } from '@unrdf/oxigraph';
import { EVENT_TYPES, GRAPHS } from '../src/constants.mjs';
import { now, toISO, fromISO } from '../src/time.mjs';

const { namedNode, literal } = dataFactory;

describe('Temporal SPARQL - Query Parser', () => {
  describe('parseTemporalQuery()', () => {
    it('should parse AT TIMESTAMP query', () => {
      const query = `
        SELECT ?s ?p ?o WHERE { ?s ?p ?o }
        AT TIMESTAMP '2026-01-01T00:00:00Z'
      `;

      const result = parseTemporalQuery(query);

      expect(result.mode).toBe('point-in-time');
      expect(result.timestamp).toBe('2026-01-01T00:00:00Z');
      expect(result.timestampNs).toBeDefined();
      expect(typeof result.timestampNs).toBe('bigint');
      expect(result.baseSparql).toContain('SELECT ?s ?p ?o');
    });

    it('should parse BETWEEN query', () => {
      const query = `
        SELECT ?s ?p ?o WHERE { ?s ?p ?o }
        BETWEEN '2026-01-01T00:00:00Z' AND '2026-01-02T00:00:00Z'
      `;

      const result = parseTemporalQuery(query);

      expect(result.mode).toBe('time-range');
      expect(result.startTimestamp).toBe('2026-01-01T00:00:00Z');
      expect(result.endTimestamp).toBe('2026-01-02T00:00:00Z');
      expect(result.startTimestampNs).toBeDefined();
      expect(result.endTimestampNs).toBeDefined();
      expect(result.baseSparql).toContain('SELECT ?s ?p ?o');
    });

    it('should parse current time query (no temporal clause)', () => {
      const query = 'SELECT ?s ?p ?o WHERE { ?s ?p ?o }';

      const result = parseTemporalQuery(query);

      expect(result.mode).toBe('current');
      expect(result.baseSparql).toBe(query);
    });

    it('should throw on invalid timestamp format', () => {
      const query = `
        SELECT ?s ?p ?o WHERE { ?s ?p ?o }
        AT TIMESTAMP 'invalid-timestamp'
      `;

      expect(() => parseTemporalQuery(query)).toThrow(/Invalid timestamp format/);
    });

    it('should throw on invalid BETWEEN range', () => {
      const query = `
        SELECT ?s ?p ?o WHERE { ?s ?p ?o }
        BETWEEN '2026-01-02T00:00:00Z' AND '2026-01-01T00:00:00Z'
      `;

      expect(() => parseTemporalQuery(query)).toThrow(/Start timestamp must be before end timestamp/);
    });

    it('should throw on empty query', () => {
      expect(() => parseTemporalQuery('')).toThrow(/must be a non-empty string/);
    });
  });

  describe('extractBaseSparql()', () => {
    it('should extract base SPARQL from temporal query', () => {
      const query = `
        SELECT ?s ?p ?o WHERE { ?s ?p ?o }
        AT TIMESTAMP '2026-01-01T00:00:00Z'
      `;

      const base = extractBaseSparql(query);

      expect(base).toContain('SELECT ?s ?p ?o');
      expect(base).not.toContain('AT TIMESTAMP');
    });
  });

  describe('hasTemporalClauses()', () => {
    it('should detect AT TIMESTAMP clause', () => {
      const query = `SELECT ?s ?p ?o AT TIMESTAMP '2026-01-01T00:00:00Z'`;
      expect(hasTemporalClauses(query)).toBe(true);
    });

    it('should detect BETWEEN clause', () => {
      const query = `SELECT ?s ?p ?o BETWEEN '2026-01-01T00:00:00Z' AND '2026-01-02T00:00:00Z'`;
      expect(hasTemporalClauses(query)).toBe(true);
    });

    it('should return false for current time query', () => {
      const query = 'SELECT ?s ?p ?o WHERE { ?s ?p ?o }';
      expect(hasTemporalClauses(query)).toBe(false);
    });
  });

  describe('validateTemporalQuery()', () => {
    it('should validate correct query', () => {
      const query = `SELECT ?s ?p ?o AT TIMESTAMP '2026-01-01T00:00:00Z'`;
      const result = validateTemporalQuery(query);

      expect(result.valid).toBe(true);
      expect(result.error).toBeUndefined();
    });

    it('should return error for invalid query', () => {
      const query = `SELECT ?s ?p ?o AT TIMESTAMP 'invalid'`;
      const result = validateTemporalQuery(query);

      expect(result.valid).toBe(false);
      expect(result.error).toBeDefined();
    });
  });
});

describe('Temporal SPARQL - Cache', () => {
  let cache;

  beforeEach(() => {
    cache = new TemporalCache({ maxSize: 10, ttl: 60000 });
  });

  describe('TemporalCache', () => {
    it('should cache and retrieve values', async () => {
      const key = await cache.generateKey('SELECT * WHERE { ?s ?p ?o }', 123456789n);
      const value = { results: [], metadata: {} };

      cache.set(key, value);
      const retrieved = cache.get(key);

      expect(retrieved).toEqual(value);
    });

    it('should return null for cache miss', () => {
      const result = cache.get('nonexistent-key');
      expect(result).toBeNull();
    });

    it('should evict LRU entries when full', async () => {
      // Fill cache to max
      for (let i = 0; i < 10; i++) {
        const key = await cache.generateKey(`query${i}`, BigInt(i));
        cache.set(key, { data: i });
      }

      expect(cache.size()).toBe(10);

      // Add one more - should evict first
      const newKey = await cache.generateKey('query10', 10n);
      cache.set(newKey, { data: 10 });

      expect(cache.size()).toBe(10);
      expect(cache.getStats().evictions).toBe(1);
    });

    it('should update access order on get', async () => {
      const key1 = await cache.generateKey('query1', 1n);
      const key2 = await cache.generateKey('query2', 2n);

      cache.set(key1, { data: 1 });
      cache.set(key2, { data: 2 });

      // Access key1 to make it most recent
      cache.get(key1);

      // Fill cache - key2 should be evicted first
      for (let i = 3; i < 12; i++) {
        const key = await cache.generateKey(`query${i}`, BigInt(i));
        cache.set(key, { data: i });
      }

      // key1 should still exist
      expect(cache.has(key1)).toBe(true);
      // key2 should be evicted
      expect(cache.has(key2)).toBe(false);
    });

    it('should track cache statistics', async () => {
      const key = await cache.generateKey('query', 123n);

      cache.set(key, { data: 'test' });

      cache.get(key); // Hit
      cache.get('nonexistent'); // Miss

      const stats = cache.getStats();

      expect(stats.hits).toBe(1);
      expect(stats.misses).toBe(1);
      expect(parseFloat(stats.hitRate)).toBe(50.0);
    });

    it('should expire entries after TTL', async () => {
      cache = new TemporalCache({ maxSize: 10, ttl: 100 });

      const key = await cache.generateKey('query', 123n);
      cache.set(key, { data: 'test' });

      expect(cache.get(key)).toBeDefined();

      // Wait for TTL expiration
      await new Promise(resolve => setTimeout(resolve, 150));

      expect(cache.get(key)).toBeNull();
    });

    it('should clear all entries', async () => {
      for (let i = 0; i < 5; i++) {
        const key = await cache.generateKey(`query${i}`, BigInt(i));
        cache.set(key, { data: i });
      }

      expect(cache.size()).toBe(5);

      cache.clear();

      expect(cache.size()).toBe(0);
    });
  });
});

describe('Temporal SPARQL - History Reconstructor', () => {
  let store;
  let git;
  let tempDir;
  let reconstructor;

  beforeEach(() => {
    store = new KGCStore({ nodeId: 'test-reconstructor' });
    tempDir = mkdtempSync(join(tmpdir(), 'temporal-reconstructor-test-'));
    git = new GitBackbone(tempDir);
    reconstructor = new HistoryReconstructor(store, git);
  });

  afterEach(() => {
    if (tempDir && existsSync(tempDir)) {
      rmSync(tempDir, { recursive: true, force: true });
    }
  });

  describe('HistoryReconstructor', () => {
    it('should reconstruct state at specific time', async () => {
      const ex = (name) => namedNode(`http://example.org/${name}`);
      const foaf = (name) => namedNode(`http://xmlns.com/foaf/0.1/${name}`);

      // Create initial state
      await store.appendEvent(
        { type: EVENT_TYPES.CREATE },
        [{ type: 'add', subject: ex('Alice'), predicate: foaf('name'), object: literal('Alice') }]
      );
      const freezeReceipt = await freezeUniverse(store, git);
      const time1 = BigInt(freezeReceipt.t_ns);

      // Reconstruct at time1
      const pastStore = await reconstructor.reconstructAtTime(time1);
      const results = await pastStore.query(`
        SELECT ?name WHERE {
          GRAPH <${GRAPHS.UNIVERSE}> {
            <http://example.org/Alice> <http://xmlns.com/foaf/0.1/name> ?name .
          }
        }
      `);

      expect(results.length).toBe(1);
      expect(results[0].name.value).toBe('Alice');
    });

    it('should track reconstruction statistics', async () => {
      const freezeReceipt = await freezeUniverse(store, git);
      const time = BigInt(freezeReceipt.t_ns);

      await reconstructor.reconstructAtTime(time);

      const stats = reconstructor.getStats();

      expect(stats.reconstructions).toBe(1);
      expect(stats.cacheMisses).toBe(1);
    });

    it('should use snapshot cache on repeated reconstructions', async () => {
      const freezeReceipt = await freezeUniverse(store, git);
      const time = BigInt(freezeReceipt.t_ns);

      // First reconstruction - cache miss
      await reconstructor.reconstructAtTime(time);
      expect(reconstructor.getStats().cacheMisses).toBe(1);

      // Second reconstruction - cache hit
      await reconstructor.reconstructAtTime(time);
      expect(reconstructor.getStats().cacheHits).toBe(1);
    });

    it('should reconstruct at multiple time points', async () => {
      const times = [];

      for (let i = 0; i < 3; i++) {
        await store.appendEvent(
          { type: EVENT_TYPES.CREATE },
          [{ type: 'add', subject: namedNode(`http://ex.org/e${i}`), predicate: namedNode('http://ex.org/p'), object: literal(`v${i}`) }]
        );
        const freezeReceipt = await freezeUniverse(store, git);
        times.push(BigInt(freezeReceipt.t_ns));
        await new Promise(r => setTimeout(r, 10));
      }

      const stores = await reconstructor.reconstructAtTimes(times);

      expect(stores.length).toBe(3);
    });
  });
});

describe('Temporal SPARQL - Integration Tests', () => {
  let store;
  let git;
  let tempDir;
  let temporal;

  beforeEach(() => {
    store = new KGCStore({ nodeId: 'test-temporal' });
    tempDir = mkdtempSync(join(tmpdir(), 'temporal-sparql-test-'));
    git = new GitBackbone(tempDir);
    temporal = new TemporalSPARQL(store, git, {
      cache: { maxSize: 100, ttl: 300000 }
    });
  });

  afterEach(() => {
    if (tempDir && existsSync(tempDir)) {
      rmSync(tempDir, { recursive: true, force: true });
    }
  });

  describe('TemporalSPARQL - Point-in-Time Queries', () => {
    it('should query at specific point in time', async () => {
      const ex = (name) => namedNode(`http://example.org/${name}`);
      const foaf = (name) => namedNode(`http://xmlns.com/foaf/0.1/${name}`);

      // Event 1: Alice created
      await store.appendEvent(
        { type: EVENT_TYPES.CREATE },
        [
          { type: 'add', subject: ex('Alice'), predicate: foaf('name'), object: literal('Alice') },
          { type: 'add', subject: ex('Alice'), predicate: foaf('age'), object: literal('30') }
        ]
      );
      const freeze1 = await freezeUniverse(store, git);
      const time1 = BigInt(freeze1.t_ns);

      await new Promise(r => setTimeout(r, 10));

      // Event 2: Add Bob
      await store.appendEvent(
        { type: EVENT_TYPES.CREATE },
        [
          { type: 'add', subject: ex('Bob'), predicate: foaf('name'), object: literal('Bob') },
          { type: 'add', subject: ex('Bob'), predicate: foaf('age'), object: literal('25') }
        ]
      );
      const freeze2 = await freezeUniverse(store, git);
      const time2 = BigInt(freeze2.t_ns);

      // Query at time1 - should only see Alice
      const result1 = await temporal.query(`
        SELECT ?name WHERE {
          GRAPH <${GRAPHS.UNIVERSE}> {
            ?person <http://xmlns.com/foaf/0.1/name> ?name .
          }
        }
        AT TIMESTAMP '${toISO(time1)}'
      `);

      expect(result1.results.length).toBe(1);
      expect(result1.results[0].name.value).toBe('Alice');

      // Query at time2 - should see both
      const result2 = await temporal.query(`
        SELECT ?name WHERE {
          GRAPH <${GRAPHS.UNIVERSE}> {
            ?person <http://xmlns.com/foaf/0.1/name> ?name .
          }
        }
        AT TIMESTAMP '${toISO(time2)}'
      `);

      expect(result2.results.length).toBe(2);
    });

    it('should use cache for repeated queries', async () => {
      const freezeReceipt = await freezeUniverse(store, git);
      const time = BigInt(freezeReceipt.t_ns);

      const query = `
        SELECT ?s ?p ?o WHERE {
          GRAPH <${GRAPHS.UNIVERSE}> {
            ?s ?p ?o .
          }
        }
        AT TIMESTAMP '${toISO(time)}'
      `;

      // First query - cache miss
      const result1 = await temporal.query(query);
      expect(result1.metadata.cached).toBe(false);

      // Second query - cache hit
      const result2 = await temporal.query(query);
      expect(result2.metadata.cached).toBe(true);

      const stats = temporal.getStats();
      expect(parseFloat(stats.cache.hitRate)).toBeGreaterThan(0);
    });

    it('should meet performance targets for cached queries', async () => {
      const freezeReceipt = await freezeUniverse(store, git);
      const time = BigInt(freezeReceipt.t_ns);

      const query = `
        SELECT ?s ?p ?o WHERE {
          GRAPH <${GRAPHS.UNIVERSE}> {
            ?s ?p ?o .
          }
        }
        AT TIMESTAMP '${toISO(time)}'
      `;

      // Warm cache
      await temporal.query(query);

      // Measure cached query
      const start = Date.now();
      await temporal.query(query);
      const duration = Date.now() - start;

      // Cached queries should be <50ms
      expect(duration).toBeLessThan(50);
    });
  });

  describe('TemporalSPARQL - Time-Range Queries', () => {
    it('should query across time range', async () => {
      const ex = (name) => namedNode(`http://example.org/${name}`);
      const foaf = (name) => namedNode(`http://xmlns.com/foaf/0.1/${name}`);

      const times = [];

      // Create 3 events
      for (let i = 0; i < 3; i++) {
        await store.appendEvent(
          { type: EVENT_TYPES.CREATE },
          [{ type: 'add', subject: ex(`Person${i}`), predicate: foaf('name'), object: literal(`Name${i}`) }]
        );
        const freezeReceipt = await freezeUniverse(store, git);
        times.push(BigInt(freezeReceipt.t_ns));
        await new Promise(r => setTimeout(r, 10));
      }

      const startTime = times[0];
      const endTime = times[times.length - 1];

      const result = await temporal.query(`
        SELECT ?s ?p ?o WHERE {
          GRAPH <${GRAPHS.UNIVERSE}> {
            ?s ?p ?o .
          }
        }
        BETWEEN '${toISO(startTime)}' AND '${toISO(endTime)}'
      `);

      expect(result.results.length).toBeGreaterThan(0);
      expect(result.metadata.startTime).toBeDefined();
      expect(result.metadata.endTime).toBeDefined();
    });
  });

  describe('TemporalSPARQL - Convenience Methods', () => {
    it('should support queryAtTime convenience method', async () => {
      const freezeReceipt = await freezeUniverse(store, git);
      const time = BigInt(freezeReceipt.t_ns);

      const result = await temporal.queryAtTime(
        `SELECT ?s ?p ?o WHERE { GRAPH <${GRAPHS.UNIVERSE}> { ?s ?p ?o } }`,
        toISO(time)
      );

      expect(result.results).toBeDefined();
      expect(result.metadata).toBeDefined();
    });

    it('should support queryBetween convenience method', async () => {
      const freeze1 = await freezeUniverse(store, git);
      const startTime = BigInt(freeze1.t_ns);
      await new Promise(r => setTimeout(r, 10));
      const freeze2 = await freezeUniverse(store, git);
      const endTime = BigInt(freeze2.t_ns);

      const result = await temporal.queryBetween(
        `SELECT ?s ?p ?o WHERE { GRAPH <${GRAPHS.UNIVERSE}> { ?s ?p ?o } }`,
        toISO(startTime),
        toISO(endTime)
      );

      expect(result.results).toBeDefined();
      expect(result.metadata).toBeDefined();
    });
  });

  describe('TemporalSPARQL - Statistics', () => {
    it('should track query statistics', async () => {
      const freezeReceipt = await freezeUniverse(store, git);
      const time = BigInt(freezeReceipt.t_ns);

      await temporal.query(`SELECT ?s ?p ?o WHERE { ?s ?p ?o } AT TIMESTAMP '${toISO(time)}'`);
      await temporal.query(`SELECT ?s ?p ?o WHERE { ?s ?p ?o } AT TIMESTAMP '${toISO(time)}'`);

      const stats = temporal.getStats();

      expect(stats.queries).toBe(2);
      expect(stats.totalQueryTime_ms).toBeGreaterThan(0);
      expect(parseFloat(stats.avgQueryTime_ms)).toBeGreaterThan(0);
    });

    it('should reset statistics', async () => {
      const freezeReceipt = await freezeUniverse(store, git);
      const time = BigInt(freezeReceipt.t_ns);

      await temporal.query(`SELECT ?s ?p ?o WHERE { ?s ?p ?o } AT TIMESTAMP '${toISO(time)}'`);

      temporal.resetStats();

      const stats = temporal.getStats();
      expect(stats.queries).toBe(0);
      expect(stats.cache.hits).toBe(0);
      expect(stats.cache.misses).toBe(0);
    });

    it('should clear cache', async () => {
      const freezeReceipt = await freezeUniverse(store, git);
      const time = BigInt(freezeReceipt.t_ns);

      await temporal.query(`SELECT ?s ?p ?o WHERE { ?s ?p ?o } AT TIMESTAMP '${toISO(time)}'`);

      temporal.clearCache();

      const stats = temporal.getStats();
      expect(stats.cache.size).toBe(0);
    });
  });

  describe('TemporalSPARQL - Cache Performance', () => {
    it('should achieve >75% cache hit rate for repeated queries', async () => {
      const times = [];
      for (let i = 0; i < 5; i++) {
        times.push(now());
        await freezeUniverse(store, git);
        await new Promise(r => setTimeout(r, 5));
      }

      // Run 100 queries with repetition
      for (let i = 0; i < 100; i++) {
        const randomTime = times[Math.floor(Math.random() * times.length)];
        await temporal.query(`SELECT ?s ?p ?o WHERE { ?s ?p ?o } AT TIMESTAMP '${toISO(randomTime)}'`);
      }

      const stats = temporal.getStats();
      const hitRate = parseFloat(stats.cache.hitRate);

      expect(hitRate).toBeGreaterThan(75);
    });
  });
});
