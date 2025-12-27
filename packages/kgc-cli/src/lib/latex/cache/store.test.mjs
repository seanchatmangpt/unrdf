/**
 * @fileoverview Tests for cache store operations.
 */

import { describe, it } from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp, rm, readFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { createHash } from 'node:crypto';
import {
  getCached,
  setCached,
  listCached,
  getCacheStats,
  clearCache,
  verifyCached
} from './store.mjs';

/**
 * Hash content for testing.
 */
function hash(content) {
  return createHash('sha256').update(content).digest('hex');
}

describe('Cache Store', () => {
  it('should store and retrieve cached file', async () => {
    const tmpDir = await mkdtemp(join(tmpdir(), 'cache-test-'));

    try {
      const content = 'test package content';
      const contentHash = hash(content);

      // Store
      const relativePath = await setCached(
        tmpDir,
        'test.sty',
        contentHash,
        content
      );

      assert.ok(relativePath.includes('test.sty'));

      // Retrieve
      const retrieved = await getCached(tmpDir, 'test.sty');
      assert.ok(retrieved);
      assert.equal(retrieved.toString(), content);
    } finally {
      await rm(tmpDir, { recursive: true, force: true });
    }
  });

  it('should verify hash when retrieving', async () => {
    const tmpDir = await mkdtemp(join(tmpdir(), 'cache-test-'));

    try {
      const content = 'test content';
      const contentHash = hash(content);

      await setCached(tmpDir, 'test.sty', contentHash, content);

      // Retrieve with correct hash
      const retrieved1 = await getCached(tmpDir, 'test.sty', contentHash);
      assert.ok(retrieved1);

      // Retrieve with wrong hash
      const wrongHash = hash('different content');
      const retrieved2 = await getCached(tmpDir, 'test.sty', wrongHash);
      assert.equal(retrieved2, null);
    } finally {
      await rm(tmpDir, { recursive: true, force: true });
    }
  });

  it('should reject hash mismatch when storing', async () => {
    const tmpDir = await mkdtemp(join(tmpdir(), 'cache-test-'));

    try {
      const content = 'test content';
      const wrongHash = hash('different content');

      await assert.rejects(
        async () => {
          await setCached(tmpDir, 'test.sty', wrongHash, content);
        },
        /Hash mismatch/
      );
    } finally {
      await rm(tmpDir, { recursive: true, force: true });
    }
  });

  it('should store files with package names', async () => {
    const tmpDir = await mkdtemp(join(tmpdir(), 'cache-test-'));

    try {
      const content = 'hyperref content';
      const contentHash = hash(content);

      const relativePath = await setCached(
        tmpDir,
        'hyperref.sty',
        contentHash,
        content,
        { packageName: 'hyperref' }
      );

      assert.ok(relativePath.includes('hyperref/hyperref.sty'));

      // Verify file exists at expected location
      const fullPath = join(tmpDir, relativePath);
      const fileContent = await readFile(fullPath, 'utf-8');
      assert.equal(fileContent, content);
    } finally {
      await rm(tmpDir, { recursive: true, force: true });
    }
  });

  it('should list all cached files', async () => {
    const tmpDir = await mkdtemp(join(tmpdir(), 'cache-test-'));

    try {
      // Store multiple files
      await setCached(tmpDir, 'a.sty', hash('a'), 'a');
      await setCached(tmpDir, 'b.sty', hash('b'), 'b');
      await setCached(tmpDir, 'c.cls', hash('c'), 'c');

      const entries = await listCached(tmpDir);

      assert.equal(entries.length, 3);
      assert.ok(entries.some(e => e.name === 'a.sty'));
      assert.ok(entries.some(e => e.name === 'b.sty'));
      assert.ok(entries.some(e => e.name === 'c.cls'));
    } finally {
      await rm(tmpDir, { recursive: true, force: true });
    }
  });

  it('should get cache statistics', async () => {
    const tmpDir = await mkdtemp(join(tmpdir(), 'cache-test-'));

    try {
      const content1 = 'content 1';
      const content2 = 'longer content 2';

      await setCached(tmpDir, 'a.sty', hash(content1), content1);
      await setCached(tmpDir, 'b.sty', hash(content2), content2);

      const stats = await getCacheStats(tmpDir);

      assert.equal(stats.fileCount, 2);
      assert.equal(stats.totalSize, content1.length + content2.length);
    } finally {
      await rm(tmpDir, { recursive: true, force: true });
    }
  });

  it('should clear cache', async () => {
    const tmpDir = await mkdtemp(join(tmpdir(), 'cache-test-'));

    try {
      // Store files
      await setCached(tmpDir, 'a.sty', hash('a'), 'a');
      await setCached(tmpDir, 'b.sty', hash('b'), 'b');

      const stats1 = await getCacheStats(tmpDir);
      assert.equal(stats1.fileCount, 2);

      // Clear
      const removed = await clearCache(tmpDir);
      assert.equal(removed, 2);

      // Verify empty
      const stats2 = await getCacheStats(tmpDir);
      assert.equal(stats2.fileCount, 0);
    } finally {
      await rm(tmpDir, { recursive: true, force: true });
    }
  });

  it('should verify cached file hash', async () => {
    const tmpDir = await mkdtemp(join(tmpdir(), 'cache-test-'));

    try {
      const content = 'test content';
      const contentHash = hash(content);

      const relativePath = await setCached(tmpDir, 'test.sty', contentHash, content);

      // Verify correct hash
      const valid = await verifyCached(tmpDir, relativePath, contentHash);
      assert.ok(valid);

      // Verify wrong hash
      const invalid = await verifyCached(tmpDir, relativePath, hash('wrong'));
      assert.ok(!invalid);
    } finally {
      await rm(tmpDir, { recursive: true, force: true });
    }
  });

  it('should return null for missing cached file', async () => {
    const tmpDir = await mkdtemp(join(tmpdir(), 'cache-test-'));

    try {
      const retrieved = await getCached(tmpDir, 'missing.sty');
      assert.equal(retrieved, null);
    } finally {
      await rm(tmpDir, { recursive: true, force: true });
    }
  });

  it('should handle empty cache directory', async () => {
    const tmpDir = await mkdtemp(join(tmpdir(), 'cache-test-'));

    try {
      const entries = await listCached(tmpDir);
      assert.equal(entries.length, 0);

      const stats = await getCacheStats(tmpDir);
      assert.equal(stats.fileCount, 0);
      assert.equal(stats.totalSize, 0);
    } finally {
      await rm(tmpDir, { recursive: true, force: true });
    }
  });
});
