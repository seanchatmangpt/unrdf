/**
 * Shadow Modes - Comprehensive Test Suite
 * All 7 test scenarios from PLAN.md
 */

import { describe, test, expect } from 'vitest';
import {
  shadowWrite,
  shadowRead,
  partialServe,
  canonicalDiff,
  createMismatchReport,
} from '../src/index.mjs';

describe('Shadow Modes - Agent 9', () => {
  // Test 1: Shadow Write - Matching Outputs
  test('shadow write with matching outputs', async () => {
    const legacyAdd = async (x) => x + 1;
    const facadeAdd = async (x) => x + 1;

    const result = await shadowWrite(legacyAdd, facadeAdd, 5);

    expect(result.mismatch).toBe(false);
    expect(result.result).toBe(6);
    expect(result.reportHash).toBeUndefined();
  });

  // Test 2: Shadow Write - Detected Mismatch
  test('shadow write detects and reports mismatch', async () => {
    const legacyAdd = async (x) => x + 1;
    const facadeAdd = async (x) => x + 2; // Bug!

    const reports = [];
    const result = await shadowWrite(legacyAdd, facadeAdd, 5, {
      onMismatch: (report) => reports.push(report),
    });

    expect(result.mismatch).toBe(true);
    expect(result.result).toBe(6); // Uses legacy by default
    expect(result.legacyResult).toBe(6);
    expect(result.facadeResult).toBe(7);
    expect(result.reportHash).toBeTruthy();
    expect(reports.length).toBe(1);
    expect(reports[0].timestamp).toBeTruthy();
    expect(reports[0].type).toBe('write');
  });

  // Test 3: Mismatch Report Determinism
  test('same mismatch produces same hash', async () => {
    const legacy = async (x) => ({ value: x + 1 });
    const facade = async (x) => ({ value: x + 2 });

    const result1 = await shadowWrite(legacy, facade, 5);
    const result2 = await shadowWrite(legacy, facade, 5);

    // Same hash despite different timestamps
    expect(result1.reportHash).toBe(result2.reportHash);
    expect(result1.report.timestamp).not.toBe(result2.report.timestamp);
  });

  // Test 4: Canonical Diff - Deep Object Comparison
  test('canonical diff detects nested differences', () => {
    const legacy = { user: { id: 1, name: 'Alice' }, count: 5 };
    const facade = { user: { id: 1, name: 'Bob' }, count: 5 };

    const diff = canonicalDiff(legacy, facade);

    expect(diff.hasDifference).toBe(true);
    expect(diff.paths).toContain('root.user.name: "Alice" vs "Bob"');
    expect(diff.paths.length).toBe(1);
  });

  // Test 5: Partial Serve - Random Routing
  test('partial serve routes based on percentage', async () => {
    const legacy = async () => 'legacy';
    const facade = async () => 'facade';

    const router = { facadePercent: 50, strategy: 'random' };

    const results = await Promise.all(
      Array(100)
        .fill(0)
        .map(() => partialServe(router, legacy, facade, {}))
    );

    const facadeCount = results.filter((r) => r.routing === 'facade').length;

    // Should be ~50% (allow 30-70% range for randomness)
    expect(facadeCount).toBeGreaterThan(30);
    expect(facadeCount).toBeLessThan(70);
  });

  // Test 6: Partial Serve - Shadow Mode
  test('partial serve shadow mode runs both handlers', async () => {
    const legacy = async (x) => x + 1;
    const facade = async (x) => x + 2;

    const router = { strategy: 'shadow' };

    const result = await partialServe(router, legacy, facade, 5);

    expect(result.routing).toBe('shadow');
    expect(result.response).toBe(6); // Legacy result
    expect(result.mismatches.length).toBe(1);
    expect(result.mismatches[0].hash).toBeTruthy();
  });

  // Test 7: Mismatch Deduplication
  test('duplicate mismatches share same hash', async () => {
    const legacy = async (x) => x * 2;
    const facade = async (x) => x * 3;

    const reports = [];
    const options = {
      onMismatch: (report) => reports.push(report),
    };

    // Generate same mismatch 5 times
    for (let i = 0; i < 5; i++) {
      await shadowWrite(legacy, facade, 10, options);
    }

    // All reports should have same hash
    const hashes = new Set(reports.map((r) => r.hash));
    expect(hashes.size).toBe(1);

    // But different timestamps
    const timestamps = new Set(reports.map((r) => r.timestamp.toString()));
    expect(timestamps.size).toBe(5);
  });

  // Additional test: Shadow Read
  test('shadow read detects query mismatches', async () => {
    const legacyQuery = async (q) => [{ id: 1, name: 'Alice' }];
    const facadeQuery = async (q) => [{ id: 1, name: 'Bob' }];

    const result = await shadowRead(legacyQuery, facadeQuery, 'SELECT * FROM users');

    expect(result.mismatch).toBe(true);
    expect(result.reportHash).toBeTruthy();
    expect(result.report.type).toBe('read');
  });

  // Additional test: Canonical diff with arrays
  test('canonical diff handles array differences', () => {
    const legacy = { items: [1, 2, 3] };
    const facade = { items: [1, 2, 4] };

    const diff = canonicalDiff(legacy, facade);

    expect(diff.hasDifference).toBe(true);
    expect(diff.paths).toContain('root.items[2]: 3 vs 4');
  });

  // Additional test: Canonical diff with missing keys
  test('canonical diff detects missing keys', () => {
    const legacy = { a: 1, b: 2 };
    const facade = { a: 1, c: 3 };

    const diff = canonicalDiff(legacy, facade);

    expect(diff.hasDifference).toBe(true);
    expect(diff.paths).toContain('root.b: missing in facade');
    expect(diff.paths).toContain('root.c: missing in legacy');
  });

  // Additional test: Canonical diff with type mismatches
  test('canonical diff detects type mismatches', () => {
    const legacy = { value: 42 };
    const facade = { value: '42' };

    const diff = canonicalDiff(legacy, facade);

    expect(diff.hasDifference).toBe(true);
    expect(diff.paths).toContain('root.value: 42 vs "42"');
  });

  // Additional test: Error handling - both handlers fail
  test('shadow write throws when both handlers fail', async () => {
    const legacyFail = async () => {
      throw new Error('Legacy error');
    };
    const facadeFail = async () => {
      throw new Error('Facade error');
    };

    await expect(shadowWrite(legacyFail, facadeFail, 5)).rejects.toThrow(
      'Both legacy and facade handlers failed'
    );
  });

  // Additional test: Error handling - one handler fails
  test('shadow write reports mismatch when one handler fails', async () => {
    const legacySuccess = async (x) => x + 1;
    const facadeFail = async () => {
      throw new Error('Facade error');
    };

    const result = await shadowWrite(legacySuccess, facadeFail, 5);

    expect(result.mismatch).toBe(true);
    expect(result.result).toBe(6);
    expect(result.legacyResult).toBe(6);
    expect(result.facadeResult).toBeUndefined();
    expect(result.reportHash).toBeTruthy();
  });

  // Additional test: Partial serve with hash-based routing
  test('partial serve hash-based routing is deterministic', async () => {
    const legacy = async () => 'legacy';
    const facade = async () => 'facade';

    const router = { facadePercent: 50, strategy: 'hash-based' };

    // Same request should always route the same way
    const request1 = { id: 'test-123' };
    const result1 = await partialServe(router, legacy, facade, request1);
    const result2 = await partialServe(router, legacy, facade, request1);

    expect(result1.routing).toBe(result2.routing);
  });

  // Additional test: Partial serve with 0% facade
  test('partial serve with 0% routes to legacy', async () => {
    const legacy = async () => 'legacy';
    const facade = async () => 'facade';

    const router = { facadePercent: 0 };

    const result = await partialServe(router, legacy, facade, {});

    expect(result.routing).toBe('legacy');
    expect(result.response).toBe('legacy');
  });

  // Additional test: Partial serve with 100% facade
  test('partial serve with 100% routes to facade', async () => {
    const legacy = async () => 'legacy';
    const facade = async () => 'facade';

    const router = { facadePercent: 100 };

    const result = await partialServe(router, legacy, facade, {});

    expect(result.routing).toBe('facade');
    expect(result.response).toBe('facade');
  });

  // Additional test: Content-addressable report creation
  test('mismatch report is content-addressable', async () => {
    const diff = {
      hasDifference: true,
      paths: ['root.value: 1 vs 2'],
    };

    const report1 = await createMismatchReport({
      type: 'write',
      input: { x: 1 },
      legacyOutput: 1,
      facadeOutput: 2,
      difference: diff,
    });

    // Wait to ensure different timestamp
    await new Promise((resolve) => setTimeout(resolve, 1));

    const report2 = await createMismatchReport({
      type: 'write',
      input: { x: 1 },
      legacyOutput: 1,
      facadeOutput: 2,
      difference: diff,
    });

    // Same hash despite different timestamps
    expect(report1.hash).toBe(report2.hash);
    expect(report1.timestamp).not.toBe(report2.timestamp);
  });
});
