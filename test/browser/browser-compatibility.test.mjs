/**
 * @fileoverview Browser compatibility tests
 *
 * Tests complete browser feature matrix:
 * - IndexedDB quad store (10K+ quads, <200ms query latency)
 * - SPARQL query execution (SELECT, ASK, CONSTRUCT)
 * - Lockchain audit trail
 * - File system adapter
 * - Browser examples
 *
 * @module test/browser/browser-compatibility.test
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { IndexedDBQuadStore } from '../../src/browser/indexeddb-store.mjs';
import { BrowserQueryExecutor } from '../../src/browser/comunica-browser-adapter.mjs';
import { BrowserLockchainWriter } from '../../src/browser/browser-lockchain-writer.mjs';
import { createFsAdapter } from '../../src/browser/fs-adapter.mjs';
import { DataFactory } from '@rdfjs/data-model';

const { namedNode, literal, quad } = DataFactory;

// Mock IndexedDB for testing
if (typeof indexedDB === 'undefined') {
  const { IDBFactory } = await import('fake-indexeddb');
  global.indexedDB = new IDBFactory();
}

// Mock Web Crypto API
if (typeof globalThis.crypto === 'undefined') {
  globalThis.crypto = {
    subtle: {
      digest: async (algo, data) => {
        // Simple hash simulation
        const hash = new Uint8Array(32);
        for (let i = 0; i < 32; i++) {
          hash[i] = (data[i % data.length] || 0) ^ i;
        }
        return hash.buffer;
      },
    },
    getRandomValues: (arr) => {
      for (let i = 0; i < arr.length; i++) {
        arr[i] = Math.floor(Math.random() * 256);
      }
      return arr;
    },
  };
}

describe('Browser Compatibility', () => {
  describe('IndexedDB Quad Store', () => {
    let store;

    beforeEach(async () => {
      store = new IndexedDBQuadStore();
      await store.init();
    });

    afterEach(async () => {
      if (store) {
        await store.clear();
        store.close();
      }
    });

    it('should store and retrieve quads', async () => {
      const testQuad = quad(
        namedNode('http://example.org/alice'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Alice')
      );

      await store.addQuad(testQuad);

      const quads = await store.getAllQuads();
      expect(quads.length).toBe(1);
      expect(quads[0].subject.value).toBe('http://example.org/alice');
    });

    it('should match quads by pattern', async () => {
      const quads = [
        quad(
          namedNode('http://example.org/alice'),
          namedNode('http://xmlns.com/foaf/0.1/name'),
          literal('Alice')
        ),
        quad(
          namedNode('http://example.org/bob'),
          namedNode('http://xmlns.com/foaf/0.1/name'),
          literal('Bob')
        ),
        quad(
          namedNode('http://example.org/alice'),
          namedNode('http://xmlns.com/foaf/0.1/age'),
          literal('30', namedNode('http://www.w3.org/2001/XMLSchema#integer'))
        ),
      ];

      await store.addQuads(quads);

      // Match by subject
      const aliceQuads = await store.match({
        subject: namedNode('http://example.org/alice'),
      });
      expect(aliceQuads.length).toBe(2);

      // Match by predicate
      const nameQuads = await store.match({
        predicate: namedNode('http://xmlns.com/foaf/0.1/name'),
      });
      expect(nameQuads.length).toBe(2);
    });

    it('should handle 10K+ quads efficiently', async () => {
      const startTime = Date.now();

      // Generate 10K quads
      const quads = [];
      for (let i = 0; i < 10000; i++) {
        quads.push(
          quad(
            namedNode(`http://example.org/person${i}`),
            namedNode('http://xmlns.com/foaf/0.1/name'),
            literal(`Person ${i}`)
          )
        );
      }

      await store.addQuads(quads);
      const loadTime = Date.now() - startTime;

      expect(await store.size()).toBe(10000);
      console.log(`   Loaded 10K quads in ${loadTime}ms`);

      // Test query performance
      const queryStart = Date.now();
      const results = await store.match({
        predicate: namedNode('http://xmlns.com/foaf/0.1/name'),
      });
      const queryTime = Date.now() - queryStart;

      expect(results.length).toBe(10000);
      expect(queryTime).toBeLessThan(200); // Target: <200ms query latency

      console.log(`   Queried 10K quads in ${queryTime}ms`);
    });

    it('should count quads correctly', async () => {
      const quads = [
        quad(namedNode('s1'), namedNode('p1'), literal('o1')),
        quad(namedNode('s2'), namedNode('p1'), literal('o2')),
        quad(namedNode('s3'), namedNode('p2'), literal('o3')),
      ];

      await store.addQuads(quads);

      const total = await store.count();
      expect(total).toBe(3);

      const p1Count = await store.count({
        predicate: namedNode('p1'),
      });
      expect(p1Count).toBe(2);
    });

    it('should remove quads', async () => {
      const testQuad = quad(
        namedNode('s'),
        namedNode('p'),
        literal('o')
      );

      await store.addQuad(testQuad);
      expect(await store.size()).toBe(1);

      await store.removeQuad(testQuad);
      expect(await store.size()).toBe(0);
    });
  });

  describe('Browser Query Executor', () => {
    let store;
    let executor;

    beforeEach(async () => {
      store = new IndexedDBQuadStore();
      executor = new BrowserQueryExecutor(store);
      await executor.init();

      // Add sample data
      const quads = [
        quad(
          namedNode('http://example.org/alice'),
          namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
          namedNode('http://xmlns.com/foaf/0.1/Person')
        ),
        quad(
          namedNode('http://example.org/alice'),
          namedNode('http://xmlns.com/foaf/0.1/name'),
          literal('Alice')
        ),
        quad(
          namedNode('http://example.org/alice'),
          namedNode('http://xmlns.com/foaf/0.1/age'),
          literal('30', namedNode('http://www.w3.org/2001/XMLSchema#integer'))
        ),
      ];

      await store.addQuads(quads);
    });

    afterEach(async () => {
      if (executor) await executor.close();
      if (store) {
        await store.clear();
        store.close();
      }
    });

    it('should execute SELECT queries', async () => {
      const query = `
        SELECT ?name WHERE {
          <http://example.org/alice> <http://xmlns.com/foaf/0.1/name> ?name .
        }
      `;

      const result = await executor.select(query);

      expect(result.length).toBe(1);
      expect(result[0].name).toBe('Alice');
    });

    it('should execute ASK queries', async () => {
      const query = `
        ASK {
          <http://example.org/alice> <http://xmlns.com/foaf/0.1/name> "Alice" .
        }
      `;

      const result = await executor.ask(query);

      expect(result).toBe(true);
    });

    it('should execute CONSTRUCT queries', async () => {
      const query = `
        CONSTRUCT {
          ?person <http://example.org/hasName> ?name .
        } WHERE {
          ?person <http://xmlns.com/foaf/0.1/name> ?name .
        }
      `;

      const result = await executor.construct(query);

      expect(result.length).toBe(1);
      expect(result[0].predicate.value).toBe('http://example.org/hasName');
    });
  });

  describe('Browser Lockchain Writer', () => {
    let lockchain;

    beforeEach(async () => {
      lockchain = new BrowserLockchainWriter();
      await lockchain.init();
    });

    afterEach(async () => {
      if (lockchain) {
        await lockchain.clear();
        lockchain.close();
      }
    });

    it('should record changes', async () => {
      const hash = await lockchain.recordChange({
        type: 'add',
        data: 'test data',
        author: 'test-user',
        message: 'Test commit',
      });

      expect(typeof hash).toBe('string');
      expect(hash.length).toBe(64); // SHA-256 hash
    });

    it('should retrieve commit history', async () => {
      await lockchain.recordChange({
        type: 'add',
        data: 'data 1',
        author: 'user1',
        message: 'First commit',
      });

      await lockchain.recordChange({
        type: 'modify',
        data: 'data 2',
        author: 'user2',
        message: 'Second commit',
      });

      const history = await lockchain.getHistory();

      expect(history.length).toBe(2);
      expect(history[0].message).toBe('Second commit'); // Most recent first
      expect(history[1].message).toBe('First commit');
    });

    it('should verify chain integrity', async () => {
      await lockchain.recordChange({
        type: 'add',
        data: 'data 1',
        author: 'user',
        message: 'Commit 1',
      });

      await lockchain.recordChange({
        type: 'add',
        data: 'data 2',
        author: 'user',
        message: 'Commit 2',
      });

      const verification = await lockchain.verifyChain();

      expect(verification.valid).toBe(true);
      expect(verification.commits).toBe(2);
    });

    it('should export and import lockchain', async () => {
      await lockchain.recordChange({
        type: 'add',
        data: 'test data',
        author: 'user',
        message: 'Test',
      });

      const exported = await lockchain.export();

      expect(exported.version).toBe('1.0');
      expect(exported.commits.length).toBe(1);

      // Clear and re-import
      await lockchain.clear();
      expect((await lockchain.getHistory()).length).toBe(0);

      await lockchain.import(exported);
      expect((await lockchain.getHistory()).length).toBe(1);
    });
  });

  describe('File System Adapter', () => {
    it('should create fs adapter', async () => {
      const fs = await createFsAdapter();

      expect(fs).toBeDefined();
      expect(typeof fs.readFile).toBe('function');
      expect(typeof fs.writeFile).toBe('function');
      expect(typeof fs.mkdir).toBe('function');
    });
  });

  describe('Feature Matrix', () => {
    it('should support all core browser features', () => {
      const features = {
        'IndexedDB Quad Store': IndexedDBQuadStore,
        'SPARQL Query Executor': BrowserQueryExecutor,
        'Lockchain Writer': BrowserLockchainWriter,
        'File System Adapter': createFsAdapter,
      };

      for (const [name, feature] of Object.entries(features)) {
        expect(feature).toBeDefined();
        console.log(`   ✅ ${name}`);
      }
    });
  });
});
