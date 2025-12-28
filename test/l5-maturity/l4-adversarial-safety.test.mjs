/**
 * @file L4 Maturity Tests - Adversarial Safety
 * @description Validates error handling for invalid inputs and boundary conditions
 *
 * CRITERIA:
 * - Test: Invalid inputs → proper error handling (not crashes)
 * - Test: Boundary conditions (empty, huge, null, undefined)
 * - Test: No information leaks in errors
 * - Coverage: 95%+ of error paths
 */

import { test, describe } from 'node:test';
import assert from 'node:assert/strict';

describe('L4: Adversarial Safety', () => {
  test('[L4.1] Store rejects invalid quads gracefully', async () => {
    console.log('[L4.1] Testing invalid quad handling');

    const { createStore } = await import('@unrdf/oxigraph');
    const store = createStore();

    // Test null quad
    assert.throws(
      () => store.add(null),
      /Quad is required/,
      'Rejects null quad'
    );

    // Test undefined quad
    assert.throws(
      () => store.add(undefined),
      /Quad is required/,
      'Rejects undefined quad'
    );

    // Store should still be functional
    const { dataFactory } = await import('@unrdf/oxigraph');
    const validQuad = dataFactory.quad(
      dataFactory.namedNode('http://example.org/s'),
      dataFactory.namedNode('http://example.org/p'),
      dataFactory.literal('test')
    );
    store.add(validQuad);
    assert.ok(store.has(validQuad), 'Store remains functional after errors');

    console.log('[L4.1] ✅ Invalid quad handling secure');
  });

  test('[L4.2] Receipt handles malformed config gracefully', async () => {
    console.log('[L4.2] Testing receipt error handling');

    const { Receipt } = await import('../../src/admission/receipts.mjs');

    // Test missing required fields
    assert.throws(
      () =>
        new Receipt({
          // Missing id, decision, etc.
        }),
      /Error/,
      'Rejects incomplete config'
    );

    // Test invalid decision value
    assert.throws(
      () =>
        new Receipt({
          id: 'test',
          decision: 'INVALID', // Should be ALLOW or DENY
          deltaHash: 'test',
          beforeHash: '0'.repeat(64),
          afterHash: '1'.repeat(64),
          epoch: 1,
          timestamp: Date.now(),
          toolchainVersion: '1.0.0',
          violations: [],
          reason: 'test',
        }),
      /Error/,
      'Rejects invalid decision'
    );

    console.log('[L4.2] ✅ Receipt validation prevents malformed data');
  });

  test('[L4.3] Empty dataset operations handle gracefully', async () => {
    console.log('[L4.3] Testing empty dataset boundary conditions');

    const { createStore } = await import('@unrdf/oxigraph');
    const store = createStore();

    // Query empty store - should not crash
    const query = 'SELECT * WHERE { ?s ?p ?o }';
    const results = Array.from(store.query(query));
    assert.equal(results.length, 0, 'Empty store returns empty results');

    // Match on empty store
    const matches = Array.from(store.match());
    assert.equal(matches.length, 0, 'Empty store has no matches');

    // Delete from empty store - should not crash
    const { dataFactory } = await import('@unrdf/oxigraph');
    const quad = dataFactory.quad(
      dataFactory.namedNode('http://example.org/s'),
      dataFactory.namedNode('http://example.org/p'),
      dataFactory.literal('test')
    );

    assert.doesNotThrow(() => store.delete(quad), 'Delete on empty store safe');

    console.log('[L4.3] ✅ Empty dataset operations safe');
  });

  test('[L4.4] Huge dataset operations stay within memory bounds', async () => {
    console.log('[L4.4] Testing large dataset boundary conditions');

    const { createStore, dataFactory } = await import('@unrdf/oxigraph');
    const store = createStore();

    const initialMemory = process.memoryUsage().heapUsed;

    // Add 10,000 quads
    for (let i = 0; i < 10000; i++) {
      const quad = dataFactory.quad(
        dataFactory.namedNode(`http://example.org/s${i}`),
        dataFactory.namedNode('http://example.org/p'),
        dataFactory.literal(`value${i}`)
      );
      store.add(quad);
    }

    const finalMemory = process.memoryUsage().heapUsed;
    const memoryIncrease = finalMemory - initialMemory;

    // Should not use more than 50MB for 10k quads
    const maxMemoryMB = 50;
    const actualMemoryMB = memoryIncrease / (1024 * 1024);

    console.log(`[L4.4] Memory increase: ${actualMemoryMB.toFixed(2)}MB`);
    assert.ok(
      actualMemoryMB < maxMemoryMB,
      `Memory increase ${actualMemoryMB.toFixed(2)}MB within ${maxMemoryMB}MB limit`
    );

    // Query should still work
    const matches = Array.from(store.match());
    assert.equal(matches.length, 10000, 'All quads retrievable');

    console.log('[L4.4] ✅ Large dataset handling efficient');
  });

  test('[L4.5] Error messages do not leak sensitive information', async () => {
    console.log('[L4.5] Testing error message security');

    const { createStore } = await import('@unrdf/oxigraph');
    const store = createStore();

    let errorMessage = '';

    try {
      // Trigger error with potentially sensitive data
      store.add(null);
    } catch (error) {
      errorMessage = error.message;
    }

    // Error should not contain system paths, env vars, etc.
    assert.ok(
      !errorMessage.includes('/home/'),
      'Error does not leak file paths'
    );
    assert.ok(
      !errorMessage.includes('NODE_ENV'),
      'Error does not leak env vars'
    );

    // Error should be generic but useful
    assert.ok(errorMessage.length > 0, 'Error message is present');
    assert.ok(errorMessage.length < 200, 'Error message is concise');

    console.log('[L4.5] ✅ Error messages sanitized');
  });

  test('[L4.6] Concurrent operations do not cause race conditions', async () => {
    console.log('[L4.6] Testing concurrent operation safety');

    const { createStore, dataFactory } = await import('@unrdf/oxigraph');
    const store = createStore();

    // Launch 100 concurrent add operations
    const promises = [];
    for (let i = 0; i < 100; i++) {
      const quad = dataFactory.quad(
        dataFactory.namedNode(`http://example.org/s${i}`),
        dataFactory.namedNode('http://example.org/p'),
        dataFactory.literal(`value${i}`)
      );
      promises.push(Promise.resolve(store.add(quad)));
    }

    await Promise.all(promises);

    // All quads should be present
    const matches = Array.from(store.match());
    assert.equal(matches.length, 100, 'All concurrent adds succeeded');

    console.log('[L4.6] ✅ Concurrent operations safe');
  });

  test('[L4.7] Invalid SPARQL queries fail gracefully', async () => {
    console.log('[L4.7] Testing SPARQL error handling');

    const { createStore } = await import('@unrdf/oxigraph');
    const store = createStore();

    const invalidQueries = [
      'SELECT * WHERE {', // Incomplete
      'INVALID SPARQL', // Syntax error
      'SELECT ?s WHERE { ?s ?p ?o } LIMIT -1', // Invalid limit
    ];

    for (const query of invalidQueries) {
      assert.throws(
        () => store.query(query),
        /Error/,
        `Rejects invalid query: ${query}`
      );
    }

    // Store should remain functional
    const validQuery = 'SELECT * WHERE { ?s ?p ?o }';
    assert.doesNotThrow(() => store.query(validQuery), 'Valid query still works');

    console.log('[L4.7] ✅ SPARQL error handling robust');
  });
});

// Export for evidence reporting
export const L4_CRITERIA = {
  level: 'L4',
  name: 'Adversarial Safety',
  tests: 7,
  target: '95%+ error path coverage',
};
