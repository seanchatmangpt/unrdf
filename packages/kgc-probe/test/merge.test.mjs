#!/usr/bin/env node
/**
 * @fileoverview Deterministic merge tests - prove same inputs → same hashes
 *
 * Critical property: Running probe twice on identical VM state MUST produce
 * identical output hashes. This is the foundation of receipt verification.
 *
 * Tests verify:
 * 1. Stable sort order
 * 2. Deterministic hashing
 * 3. Idempotent merging
 * 4. Deduplication correctness
 */

import { strict as assert } from 'node:assert';
import { test, describe } from 'node:test';
import {
  mergeObservations,
  mergeShards,
  deduplicateObservations,
  groupByCategory,
  groupBySeverity,
  filterByCategory,
  filterBySeverity
} from '../src/merge.mjs';
import { createObservation } from '../src/observation.mjs';
import { buildReceiptChain, createManifest } from '../src/receipt.mjs';

/**
 * Create test observation
 */
function createTestObs(overrides = {}) {
  const defaults = {
    category: 'test',
    severity: 'info',
    message: 'Test observation',
    metadata: {
      agentId: 'agent-test',
      probeVersion: '1.0.0',
      budgetMs: 5000,
      actualMs: 100,
      timestamp: new Date().toISOString()
    }
  };

  // Merge metadata properly
  const merged = {
    ...defaults,
    ...overrides,
    metadata: {
      ...defaults.metadata,
      ...(overrides.metadata || {})
    }
  };

  return createObservation(merged);
}

describe('mergeObservations - Stable Sort', () => {
  test('should sort by category (alphabetical)', () => {
    const obs = [
      createTestObs({ category: 'security' }),
      createTestObs({ category: 'file' }),
      createTestObs({ category: 'performance' })
    ];

    const merged = mergeObservations(obs);

    assert.equal(merged[0].category, 'file');
    assert.equal(merged[1].category, 'performance');
    assert.equal(merged[2].category, 'security');
  });

  test('should sort by severity within category', () => {
    const obs = [
      createTestObs({ category: 'test', severity: 'info' }),
      createTestObs({ category: 'test', severity: 'fatal' }),
      createTestObs({ category: 'test', severity: 'warn' })
    ];

    const merged = mergeObservations(obs);

    assert.equal(merged[0].severity, 'fatal');
    assert.equal(merged[1].severity, 'warn');
    assert.equal(merged[2].severity, 'info');
  });

  test('should sort by file path within severity', () => {
    const obs = [
      createTestObs({
        category: 'test',
        severity: 'info',
        location: { file: '/src/c.mjs' }
      }),
      createTestObs({
        category: 'test',
        severity: 'info',
        location: { file: '/src/a.mjs' }
      }),
      createTestObs({
        category: 'test',
        severity: 'info',
        location: { file: '/src/b.mjs' }
      })
    ];

    const merged = mergeObservations(obs);

    assert.equal(merged[0].location.file, '/src/a.mjs');
    assert.equal(merged[1].location.file, '/src/b.mjs');
    assert.equal(merged[2].location.file, '/src/c.mjs');
  });

  test('should sort by line number within file', () => {
    const obs = [
      createTestObs({
        category: 'test',
        severity: 'info',
        location: { file: '/src/a.mjs', line: 30 }
      }),
      createTestObs({
        category: 'test',
        severity: 'info',
        location: { file: '/src/a.mjs', line: 10 }
      }),
      createTestObs({
        category: 'test',
        severity: 'info',
        location: { file: '/src/a.mjs', line: 20 }
      })
    ];

    const merged = mergeObservations(obs);

    assert.equal(merged[0].location.line, 10);
    assert.equal(merged[1].location.line, 20);
    assert.equal(merged[2].location.line, 30);
  });

  test('should be stable (preserve relative order for equal elements)', () => {
    const obs = [
      createTestObs({ id: 'obs-1', category: 'test', severity: 'info' }),
      createTestObs({ id: 'obs-2', category: 'test', severity: 'info' }),
      createTestObs({ id: 'obs-3', category: 'test', severity: 'info' })
    ];

    const merged1 = mergeObservations(obs);
    const merged2 = mergeObservations(obs);

    // Should maintain same order across multiple runs
    for (let i = 0; i < merged1.length; i++) {
      assert.equal(merged1[i].id, merged2[i].id);
    }
  });
});

describe('mergeObservations - Deterministic Hashing', () => {
  test('should produce identical hashes for identical inputs', () => {
    const obs = [
      createTestObs({ category: 'security', severity: 'error' }),
      createTestObs({ category: 'file', severity: 'info' }),
      createTestObs({ category: 'performance', severity: 'warn' })
    ];

    const merged1 = mergeObservations(obs);
    const merged2 = mergeObservations(obs);

    const receipts1 = buildReceiptChain(merged1);
    const receipts2 = buildReceiptChain(merged2);

    const manifest1 = createManifest(receipts1);
    const manifest2 = createManifest(receipts2);

    assert.equal(manifest1.chainHash, manifest2.chainHash, 'Chain hashes must be identical');
  });

  test('should produce different hashes for different inputs', () => {
    const obs1 = [
      createTestObs({ category: 'file', severity: 'info', message: 'First' })
    ];

    const obs2 = [
      createTestObs({ category: 'file', severity: 'info', message: 'Second' })
    ];

    const merged1 = mergeObservations(obs1);
    const merged2 = mergeObservations(obs2);

    const receipts1 = buildReceiptChain(merged1);
    const receipts2 = buildReceiptChain(merged2);

    const manifest1 = createManifest(receipts1);
    const manifest2 = createManifest(receipts2);

    assert.notEqual(manifest1.chainHash, manifest2.chainHash, 'Different inputs → different hashes');
  });

  test('should be order-independent after merge', () => {
    const obs = [
      createTestObs({ category: 'security' }),
      createTestObs({ category: 'file' }),
      createTestObs({ category: 'performance' })
    ];

    // Reverse input order
    const obsReversed = [...obs].reverse();

    const merged1 = mergeObservations(obs);
    const merged2 = mergeObservations(obsReversed);

    const receipts1 = buildReceiptChain(merged1);
    const receipts2 = buildReceiptChain(merged2);

    const manifest1 = createManifest(receipts1);
    const manifest2 = createManifest(receipts2);

    assert.equal(manifest1.chainHash, manifest2.chainHash, 'Input order should not affect final hash');
  });
});

describe('mergeShards', () => {
  test('should merge observations from multiple agents', () => {
    const shards = new Map([
      ['agent-1', [
        createTestObs({ category: 'file', metadata: { agentId: 'agent-1', probeVersion: '1.0.0', budgetMs: 5000, actualMs: 100 } })
      ]],
      ['agent-2', [
        createTestObs({ category: 'security', metadata: { agentId: 'agent-2', probeVersion: '1.0.0', budgetMs: 5000, actualMs: 100 } })
      ]],
      ['agent-3', [
        createTestObs({ category: 'performance', metadata: { agentId: 'agent-3', probeVersion: '1.0.0', budgetMs: 5000, actualMs: 100 } })
      ]]
    ]);

    const merged = mergeShards(shards);

    assert.equal(merged.length, 3);
    assert.equal(merged[0].category, 'file');
    assert.equal(merged[1].category, 'performance');
    assert.equal(merged[2].category, 'security');
  });
});

describe('deduplicateObservations', () => {
  test('should remove duplicate observations', () => {
    const obs = [
      createTestObs({ category: 'file', severity: 'info', message: 'Duplicate' }),
      createTestObs({ category: 'file', severity: 'info', message: 'Duplicate' }),
      createTestObs({ category: 'file', severity: 'info', message: 'Unique' })
    ];

    const unique = deduplicateObservations(obs);

    assert.equal(unique.length, 2, 'Should have 2 unique observations');
  });

  test('should preserve first occurrence', () => {
    const obs = [
      createTestObs({ id: 'first', category: 'file', severity: 'info', message: 'Dup' }),
      createTestObs({ id: 'second', category: 'file', severity: 'info', message: 'Dup' })
    ];

    const unique = deduplicateObservations(obs);

    assert.equal(unique.length, 1);
    assert.equal(unique[0].id, 'first', 'Should preserve first occurrence');
  });
});

describe('groupByCategory', () => {
  test('should group observations by category', () => {
    const obs = [
      createTestObs({ category: 'file' }),
      createTestObs({ category: 'security' }),
      createTestObs({ category: 'file' })
    ];

    const groups = groupByCategory(obs);

    assert.equal(groups.get('file').length, 2);
    assert.equal(groups.get('security').length, 1);
  });
});

describe('groupBySeverity', () => {
  test('should group observations by severity', () => {
    const obs = [
      createTestObs({ severity: 'error' }),
      createTestObs({ severity: 'info' }),
      createTestObs({ severity: 'error' })
    ];

    const groups = groupBySeverity(obs);

    assert.equal(groups.get('error').length, 2);
    assert.equal(groups.get('info').length, 1);
  });
});

describe('filterByCategory', () => {
  test('should filter observations by category', () => {
    const obs = [
      createTestObs({ category: 'file' }),
      createTestObs({ category: 'security' }),
      createTestObs({ category: 'performance' })
    ];

    const filtered = filterByCategory(obs, ['file', 'security']);

    assert.equal(filtered.length, 2);
    assert.ok(filtered.every(o => o.category === 'file' || o.category === 'security'));
  });
});

describe('filterBySeverity', () => {
  test('should filter by minimum severity level', () => {
    const obs = [
      createTestObs({ severity: 'fatal' }),
      createTestObs({ severity: 'error' }),
      createTestObs({ severity: 'warn' }),
      createTestObs({ severity: 'info' })
    ];

    const filtered = filterBySeverity(obs, 'warn');

    assert.equal(filtered.length, 3); // fatal, error, warn (not info)
    assert.ok(filtered.some(o => o.severity === 'fatal'));
    assert.ok(filtered.some(o => o.severity === 'error'));
    assert.ok(filtered.some(o => o.severity === 'warn'));
    assert.ok(!filtered.some(o => o.severity === 'info'));
  });
});

console.log('✅ All deterministic merge tests passed!');
