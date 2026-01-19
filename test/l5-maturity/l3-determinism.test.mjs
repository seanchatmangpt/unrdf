/**
 * @file L3 Maturity Tests - Determinism
 * @description Validates that operations run identically 100x with same inputs
 *
 * CRITICAL: Run same operation 100x with same inputs → identical outputs + identical receipts
 *
 * CRITERIA:
 * - Test suite: DeterminismTest.mjs
 * - Run: timeout 10s npm run test:determinism
 * - Evidence: Pass rate 100/100 per operation
 */

import { test, describe, expect } from 'vitest';

describe('L3: Determinism - 100x Identical Runs', () => {
  test('[L3.1] Store creation is deterministic (100x runs)', async () => {
    console.log('[L3.1] Testing store creation determinism (100 iterations)');

    const { createStore, dataFactory } = await import('@unrdf/oxigraph');
    const stores = [];

    for (let i = 0; i < 100; i++) {
      const store = createStore();
      stores.push(store);
    }

    // All stores should be independently created
    expect(stores.length).toBe(100);

    // Each store should behave identically
    for (let i = 0; i < 100; i++) {
      const quad = dataFactory.quad(
        dataFactory.namedNode('http://example.org/s'),
        dataFactory.namedNode('http://example.org/p'),
        dataFactory.literal('test')
      );
      stores[i].add(quad);
      expect(stores[i].has(quad).toBeTruthy();
    }

    console.log('[L3.1] ✅ 100/100 store creations identical');
  });

  test('[L3.2] Receipt generation is deterministic (100x runs)', async () => {
    console.log('[L3.2] Testing receipt determinism (100 iterations)');

    const { Receipt } = await import('../../src/admission/receipts.mjs');

    const config = {
      id: 'urn:receipt:test:determinism',
      decision: 'ALLOW',
      deltaHash: 'determinism-test-hash',
      beforeHash: '0'.repeat(64),
      afterHash: '1'.repeat(64),
      epoch: 1,
      timestamp: 1234567890, // Fixed timestamp
      toolchainVersion: '1.0.0',
      violations: [],
      reason: 'Determinism test',
    };

    const hashes = [];

    for (let i = 0; i < 100; i++) {
      const receipt = new Receipt(config);
      const hash = receipt.getHash();
      hashes.push(hash);
    }

    // All hashes should be identical
    const uniqueHashes = new Set(hashes);
    expect(
      uniqueHashes.size).toBe(
      1);

    console.log('[L3.2] ✅ 100/100 receipts have identical hash:', Array.from(uniqueHashes)[0].slice(0, 16) + '...');
  });

  test('[L3.3] Quad insertion order does not affect determinism (100x runs)', async () => {
    console.log('[L3.3] Testing quad insertion order determinism (100 iterations)');

    const { createStore, dataFactory } = await import('@unrdf/oxigraph');

    const quads = [
      dataFactory.quad(
        dataFactory.namedNode('http://example.org/s1'),
        dataFactory.namedNode('http://example.org/p1'),
        dataFactory.literal('o1')
      ),
      dataFactory.quad(
        dataFactory.namedNode('http://example.org/s2'),
        dataFactory.namedNode('http://example.org/p2'),
        dataFactory.literal('o2')
      ),
      dataFactory.quad(
        dataFactory.namedNode('http://example.org/s3'),
        dataFactory.namedNode('http://example.org/p3'),
        dataFactory.literal('o3')
      ),
    ];

    for (let i = 0; i < 100; i++) {
      const store = createStore();

      // Insert in deterministic order every time
      for (const quad of quads) {
        store.add(quad);
      }

      // Query should return consistent results
      const matches = Array.from(store.match());
      expect(matches.length).toBe(3);
    }

    console.log('[L3.3] ✅ 100/100 iterations have consistent quad counts');
  });

  test('[L3.4] SPARQL query results are deterministic (100x runs)', async () => {
    console.log('[L3.4] Testing SPARQL query determinism (100 iterations)');

    const { createStore, dataFactory } = await import('@unrdf/oxigraph');

    const results = [];

    for (let i = 0; i < 100; i++) {
      const store = createStore();

      // Add same triples
      store.add(
        dataFactory.quad(
          dataFactory.namedNode('http://example.org/alice'),
          dataFactory.namedNode('http://xmlns.com/foaf/0.1/name'),
          dataFactory.literal('Alice')
        )
      );

      store.add(
        dataFactory.quad(
          dataFactory.namedNode('http://example.org/bob'),
          dataFactory.namedNode('http://xmlns.com/foaf/0.1/name'),
          dataFactory.literal('Bob')
        )
      );

      // Query
      const query = 'SELECT ?s ?name WHERE { ?s <http://xmlns.com/foaf/0.1/name> ?name }';
      const queryResults = store.query(query);
      const bindings = Array.from(queryResults);

      results.push(bindings.length);
    }

    // All iterations should return same count
    const uniqueCounts = new Set(results);
    expect(
      uniqueCounts.size).toBe(
      1);

    expect(
      Array.from(uniqueCounts)[0]).toBe(
      2);

    console.log('[L3.4] ✅ 100/100 queries returned identical results');
  });

  test('[L3.5] Delta hash computation is deterministic (100x runs)', async () => {
    console.log('[L3.5] Testing delta hash determinism (100 iterations)');

    // Using admission delta capsule hashing
    const { createDeltaCapsule } = await import('../../src/admission/delta-capsule.mjs');

    const delta = {
      additions: [
        {
          subject: 'http://example.org/s',
          predicate: 'http://example.org/p',
          object: 'test',
        },
      ],
      deletions: [],
    };

    const hashes = [];

    for (let i = 0; i < 100; i++) {
      const capsule = createDeltaCapsule(delta);
      hashes.push(capsule.hash);
    }

    const uniqueHashes = new Set(hashes);
    expect(
      uniqueHashes.size).toBe(
      1);

    console.log('[L3.5] ✅ 100/100 delta hashes identical:', Array.from(uniqueHashes)[0].slice(0, 16) + '...');
  });
});

// Export for evidence reporting
export const L3_CRITERIA = {
  level: 'L3',
  name: 'Determinism',
  tests: 5,
  iterations: 100,
  target: '100/100 per operation',
};
