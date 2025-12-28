/**
 * @fileoverview Tests for lockfile management.
 *
 * Verifies lockfile creation, validation, verification, and persistence.
 */

import { describe, it } from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp, rm, writeFile, readFile, mkdir } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join, dirname } from 'node:path';
import { createHash } from 'node:crypto';
import {
  createLockfile,
  loadLockfile,
  saveLockfile,
  addEntry,
  verifyEntry,
  verifyAllEntries,
  getEntry,
  removeEntry,
  listEntries,
  mergeLockfiles,
  pruneLockfile,
  createLockEntry
} from './lockfile.mjs';

/**
 * Hash content for testing.
 * @param {string|Buffer|Uint8Array} content - Content to hash
 * @returns {string} SHA-256 hex hash
 */
function hash(content) {
  return createHash('sha256').update(content).digest('hex');
}

describe('Lockfile Management', () => {
  it('should create empty lockfile with correct structure', () => {
    const lockfile = createLockfile('xetex');

    assert.equal(lockfile.version, '1.0.0');
    assert.equal(lockfile.engine, 'xetex');
    assert.deepEqual(lockfile.entries, {});
    assert.ok(lockfile.createdAt > 0);
    assert.ok(lockfile.updatedAt > 0);
  });

  it('should create lock entry with correct hash', () => {
    const content = 'test content';
    const entry = createLockEntry({
      name: 'test.sty',
      content,
      cachedPath: 'packages/test/test.sty',
      sourceUrl: 'https://example.com/test.sty'
    });

    assert.equal(entry.name, 'test.sty');
    assert.equal(entry.hash, hash(content));
    assert.equal(entry.cachedPath, 'packages/test/test.sty');
    assert.equal(entry.sourceUrl, 'https://example.com/test.sty');
    assert.ok(entry.fetchedAt > 0);
    assert.equal(entry.size, content.length);
  });

  it('should add entry to lockfile', () => {
    const lockfile = createLockfile('xetex');
    const entry = createLockEntry({
      name: 'hyperref.sty',
      content: 'hyperref content',
      cachedPath: 'packages/hyperref/hyperref.sty'
    });

    addEntry(lockfile, entry);

    assert.ok(lockfile.entries['hyperref.sty']);
    assert.equal(lockfile.entries['hyperref.sty'].hash, entry.hash);
  });

  it('should verify entry with correct hash', () => {
    const content = 'test content';
    const lockfile = createLockfile('xetex');
    const entry = createLockEntry({
      name: 'test.sty',
      content,
      cachedPath: 'packages/test/test.sty'
    });

    addEntry(lockfile, entry);

    assert.ok(verifyEntry(lockfile, 'test.sty', content));
    assert.ok(!verifyEntry(lockfile, 'test.sty', 'different content'));
  });

  it('should get entry by name', () => {
    const lockfile = createLockfile('xetex');
    const entry = createLockEntry({
      name: 'test.sty',
      content: 'content',
      cachedPath: 'packages/test/test.sty'
    });

    addEntry(lockfile, entry);

    const retrieved = getEntry(lockfile, 'test.sty');
    assert.ok(retrieved);
    assert.equal(retrieved.name, 'test.sty');
  });

  it('should remove entry from lockfile', () => {
    const lockfile = createLockfile('xetex');
    const entry = createLockEntry({
      name: 'test.sty',
      content: 'content',
      cachedPath: 'packages/test/test.sty'
    });

    addEntry(lockfile, entry);
    assert.ok(lockfile.entries['test.sty']);

    const removed = removeEntry(lockfile, 'test.sty');
    assert.ok(removed);
    assert.ok(!lockfile.entries['test.sty']);
  });

  it('should list all entries sorted', () => {
    const lockfile = createLockfile('xetex');

    addEntry(lockfile, createLockEntry({
      name: 'c.sty',
      content: 'c',
      cachedPath: 'packages/c/c.sty'
    }));
    addEntry(lockfile, createLockEntry({
      name: 'a.sty',
      content: 'a',
      cachedPath: 'packages/a/a.sty'
    }));
    addEntry(lockfile, createLockEntry({
      name: 'b.sty',
      content: 'b',
      cachedPath: 'packages/b/b.sty'
    }));

    const entries = listEntries(lockfile);
    assert.deepEqual(entries, ['a.sty', 'b.sty', 'c.sty']);
  });

  it('should merge two lockfiles', () => {
    const lock1 = createLockfile('xetex');
    const lock2 = createLockfile('xetex');

    addEntry(lock1, createLockEntry({
      name: 'a.sty',
      content: 'a',
      cachedPath: 'packages/a/a.sty'
    }));

    addEntry(lock2, createLockEntry({
      name: 'b.sty',
      content: 'b',
      cachedPath: 'packages/b/b.sty'
    }));

    const merged = mergeLockfiles(lock1, lock2);

    assert.ok(merged.entries['a.sty']);
    assert.ok(merged.entries['b.sty']);
    assert.equal(merged.engine, 'xetex');
  });

  it('should throw when merging lockfiles with different engines', () => {
    const lock1 = createLockfile('xetex');
    const lock2 = createLockfile('pdftex');

    assert.throws(() => {
      mergeLockfiles(lock1, lock2);
    }, /Cannot merge lockfiles with different engines/);
  });

  it('should prune lockfile to valid entries', () => {
    const lockfile = createLockfile('xetex');

    addEntry(lockfile, createLockEntry({
      name: 'a.sty',
      content: 'a',
      cachedPath: 'packages/a/a.sty'
    }));
    addEntry(lockfile, createLockEntry({
      name: 'b.sty',
      content: 'b',
      cachedPath: 'packages/b/b.sty'
    }));
    addEntry(lockfile, createLockEntry({
      name: 'c.sty',
      content: 'c',
      cachedPath: 'packages/c/c.sty'
    }));

    const pruned = pruneLockfile(lockfile, ['a.sty', 'c.sty']);

    assert.ok(pruned.entries['a.sty']);
    assert.ok(!pruned.entries['b.sty']);
    assert.ok(pruned.entries['c.sty']);
  });

  it('should save and load lockfile with stable formatting', async () => {
    const tmpDir = await mkdtemp(join(tmpdir(), 'lockfile-test-'));

    try {
      const lockfile = createLockfile('xetex');
      addEntry(lockfile, createLockEntry({
        name: 'test.sty',
        content: 'content',
        cachedPath: 'packages/test/test.sty'
      }));

      const lockfilePath = join(tmpDir, 'test.lock.json');
      await saveLockfile(lockfilePath, lockfile);

      // Verify file exists and is valid JSON
      const raw = await readFile(lockfilePath, 'utf-8');
      const parsed = JSON.parse(raw);

      assert.equal(parsed.version, '1.0.0');
      assert.equal(parsed.engine, 'xetex');
      assert.ok(parsed.entries['test.sty']);

      // Load and verify
      const loaded = await loadLockfile(lockfilePath);
      assert.ok(loaded);
      assert.equal(loaded.engine, 'xetex');
      assert.ok(loaded.entries['test.sty']);
    } finally {
      await rm(tmpDir, { recursive: true, force: true });
    }
  });

  it('should return null when loading missing lockfile', async () => {
    const tmpDir = await mkdtemp(join(tmpdir(), 'lockfile-test-'));

    try {
      const lockfilePath = join(tmpDir, 'missing.lock.json');
      const loaded = await loadLockfile(lockfilePath);

      assert.equal(loaded, null);
    } finally {
      await rm(tmpDir, { recursive: true, force: true });
    }
  });

  it('should return null when loading invalid lockfile', async () => {
    const tmpDir = await mkdtemp(join(tmpdir(), 'lockfile-test-'));

    try {
      const lockfilePath = join(tmpDir, 'invalid.lock.json');
      await writeFile(lockfilePath, '{"invalid": true}', 'utf-8');

      const loaded = await loadLockfile(lockfilePath);

      assert.equal(loaded, null);
    } finally {
      await rm(tmpDir, { recursive: true, force: true });
    }
  });

  it('should verify all entries in cache', async () => {
    const tmpDir = await mkdtemp(join(tmpdir(), 'lockfile-test-'));

    try {
      const cacheDir = join(tmpDir, 'cache');
      const lockfile = createLockfile('xetex');

      // Create test files
      const content1 = 'content 1';
      const content2 = 'content 2';

      const entry1 = createLockEntry({
        name: 'test1.sty',
        content: content1,
        cachedPath: 'packages/test1/test1.sty'
      });

      const entry2 = createLockEntry({
        name: 'test2.sty',
        content: content2,
        cachedPath: 'packages/test2/test2.sty'
      });

      addEntry(lockfile, entry1);
      addEntry(lockfile, entry2);

      // Write files to cache
      const file1Path = join(cacheDir, entry1.cachedPath);
      const file2Path = join(cacheDir, entry2.cachedPath);

      await mkdir(dirname(file1Path), { recursive: true });
      await mkdir(dirname(file2Path), { recursive: true });

      await writeFile(file1Path, content1);
      await writeFile(file2Path, content2);

      // Verify
      const result = await verifyAllEntries(lockfile, cacheDir);

      assert.equal(result.valid.length, 2);
      assert.equal(result.invalid.length, 0);
      assert.equal(result.missing.length, 0);
      assert.ok(result.valid.includes('test1.sty'));
      assert.ok(result.valid.includes('test2.sty'));
    } finally {
      await rm(tmpDir, { recursive: true, force: true });
    }
  });

  it('should detect hash mismatches', async () => {
    const tmpDir = await mkdtemp(join(tmpdir(), 'lockfile-test-'));

    try {
      const cacheDir = join(tmpDir, 'cache');
      const lockfile = createLockfile('xetex');

      const entry = createLockEntry({
        name: 'test.sty',
        content: 'original content',
        cachedPath: 'packages/test/test.sty'
      });

      addEntry(lockfile, entry);

      // Write different content to cache
      const filePath = join(cacheDir, entry.cachedPath);
      await mkdir(dirname(filePath), { recursive: true });
      await writeFile(filePath, 'different content');

      // Verify
      const result = await verifyAllEntries(lockfile, cacheDir);

      assert.equal(result.valid.length, 0);
      assert.equal(result.invalid.length, 1);
      assert.equal(result.missing.length, 0);
      assert.ok(result.invalid.includes('test.sty'));
    } finally {
      await rm(tmpDir, { recursive: true, force: true });
    }
  });

  it('should detect missing files', async () => {
    const tmpDir = await mkdtemp(join(tmpdir(), 'lockfile-test-'));

    try {
      const cacheDir = join(tmpDir, 'cache');
      const lockfile = createLockfile('xetex');

      const entry = createLockEntry({
        name: 'missing.sty',
        content: 'content',
        cachedPath: 'packages/missing/missing.sty'
      });

      addEntry(lockfile, entry);

      // Don't create the file

      // Verify
      const result = await verifyAllEntries(lockfile, cacheDir);

      assert.equal(result.valid.length, 0);
      assert.equal(result.invalid.length, 0);
      assert.equal(result.missing.length, 1);
      assert.ok(result.missing.includes('missing.sty'));
    } finally {
      await rm(tmpDir, { recursive: true, force: true });
    }
  });
});
