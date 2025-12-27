/**
 * @fileoverview Tests for bundle export/import.
 */

import { describe, it } from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp, rm, readFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { createHash } from 'node:crypto';
import { createLockfile, addEntry, createLockEntry } from './lockfile.mjs';
import { setCached } from './store.mjs';
import { exportBundle, importBundle, verifyBundle, listBundle } from './bundle.mjs';

function hash(content) {
  return createHash('sha256').update(content).digest('hex');
}

describe('Bundle Operations', () => {
  it('should export bundle with manifest', async () => {
    const tmpDir = await mkdtemp(join(tmpdir(), 'bundle-test-'));

    try {
      const cacheDir = join(tmpDir, 'cache');
      const bundleDir = join(tmpDir, 'bundle');

      // Create lockfile
      const lockfile = createLockfile('xetex');

      // Add entries and cache files
      const content1 = 'package 1 content';
      const content2 = 'package 2 content';

      const entry1 = createLockEntry({
        name: 'pkg1.sty',
        content: content1,
        cachedPath: await setCached(cacheDir, 'pkg1.sty', hash(content1), content1)
      });

      const entry2 = createLockEntry({
        name: 'pkg2.sty',
        content: content2,
        cachedPath: await setCached(cacheDir, 'pkg2.sty', hash(content2), content2)
      });

      addEntry(lockfile, entry1);
      addEntry(lockfile, entry2);

      // Export bundle
      const manifest = await exportBundle(lockfile, cacheDir, bundleDir);

      // Verify manifest
      assert.equal(manifest.version, '1.0.0');
      assert.equal(manifest.engine, 'xetex');
      assert.equal(manifest.fileCount, 2);
      assert.ok(manifest.files.length === 2);

      // Verify manifest file exists
      const manifestPath = join(bundleDir, 'bundle.manifest.json');
      const manifestContent = await readFile(manifestPath, 'utf-8');
      assert.ok(manifestContent.includes('pkg1.sty'));
      assert.ok(manifestContent.includes('pkg2.sty'));
    } finally {
      await rm(tmpDir, { recursive: true, force: true });
    }
  });

  it('should import bundle into cache', async () => {
    const tmpDir = await mkdtemp(join(tmpdir(), 'bundle-test-'));

    try {
      const cacheDir1 = join(tmpDir, 'cache1');
      const cacheDir2 = join(tmpDir, 'cache2');
      const bundleDir = join(tmpDir, 'bundle');

      // Create and export bundle
      const lockfile = createLockfile('xetex');
      const content = 'test package';

      const entry = createLockEntry({
        name: 'test.sty',
        content,
        cachedPath: await setCached(cacheDir1, 'test.sty', hash(content), content)
      });

      addEntry(lockfile, entry);
      await exportBundle(lockfile, cacheDir1, bundleDir);

      // Import to new cache
      const result = await importBundle(bundleDir, cacheDir2);

      assert.equal(result.imported, 1);
      assert.equal(result.failed, 0);

      // Verify file exists in new cache
      const importedPath = join(cacheDir2, entry.cachedPath);
      const importedContent = await readFile(importedPath, 'utf-8');
      assert.equal(importedContent, content);
    } finally {
      await rm(tmpDir, { recursive: true, force: true });
    }
  });

  it('should verify bundle integrity', async () => {
    const tmpDir = await mkdtemp(join(tmpdir(), 'bundle-test-'));

    try {
      const cacheDir = join(tmpDir, 'cache');
      const bundleDir = join(tmpDir, 'bundle');

      // Create bundle
      const lockfile = createLockfile('xetex');
      const content = 'test content';

      const entry = createLockEntry({
        name: 'test.sty',
        content,
        cachedPath: await setCached(cacheDir, 'test.sty', hash(content), content)
      });

      addEntry(lockfile, entry);
      await exportBundle(lockfile, cacheDir, bundleDir);

      // Verify bundle
      const result = await verifyBundle(bundleDir);

      assert.equal(result.valid, 1);
      assert.equal(result.invalid, 0);
      assert.equal(result.missing, 0);
    } finally {
      await rm(tmpDir, { recursive: true, force: true });
    }
  });

  it('should list bundle contents', async () => {
    const tmpDir = await mkdtemp(join(tmpdir(), 'bundle-test-'));

    try {
      const cacheDir = join(tmpDir, 'cache');
      const bundleDir = join(tmpDir, 'bundle');

      // Create bundle
      const lockfile = createLockfile('xetex');
      const content1 = 'content 1';
      const content2 = 'content 2';

      const entry1 = createLockEntry({
        name: 'a.sty',
        content: content1,
        cachedPath: await setCached(cacheDir, 'a.sty', hash(content1), content1)
      });

      const entry2 = createLockEntry({
        name: 'b.sty',
        content: content2,
        cachedPath: await setCached(cacheDir, 'b.sty', hash(content2), content2)
      });

      addEntry(lockfile, entry1);
      addEntry(lockfile, entry2);
      await exportBundle(lockfile, cacheDir, bundleDir);

      // List bundle
      const manifest = await listBundle(bundleDir);

      assert.equal(manifest.fileCount, 2);
      assert.ok(manifest.files.some(f => f.name === 'a.sty'));
      assert.ok(manifest.files.some(f => f.name === 'b.sty'));
    } finally {
      await rm(tmpDir, { recursive: true, force: true });
    }
  });

  it('should throw on invalid lockfile', async () => {
    const tmpDir = await mkdtemp(join(tmpdir(), 'bundle-test-'));

    try {
      const cacheDir = join(tmpDir, 'cache');
      const bundleDir = join(tmpDir, 'bundle');

      const invalidLockfile = {
        version: '999.0.0',  // Invalid version
        engine: 'invalid',
        entries: {}
      };

      await assert.rejects(
        async () => {
          await exportBundle(invalidLockfile, cacheDir, bundleDir);
        },
        /Invalid lockfile/
      );
    } finally {
      await rm(tmpDir, { recursive: true, force: true });
    }
  });
});
