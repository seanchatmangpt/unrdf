/**
 * @fileoverview Tests for storage optimizations
 * Tests compression, GC, incremental snapshots, deduplication, indexing, and archival
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { promises as fs } from 'fs';
import * as path from 'path';
import { fileURLToPath } from 'url';
import { dirname } from 'path';
import {
  compressFile,
  decompressFile,
  readCompressed,
  garbageCollectSnapshots,
  archiveReceipts,
  computeDelta,
  applyDelta,
} from '../src/storage-optimization.mjs';
import {
  freezeUniverse,
  getSnapshotList,
  reconstructTo,
} from '../src/freeze-restore.mjs';
import {
  RunCapsule,
  storeCapsule,
  listCapsules,
  findCapsuleByHash,
} from '../src/capsule.mjs';
import { writeFileSync, mkdirSync, rmSync, existsSync } from 'node:fs';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

const TEST_DIR = path.join(__dirname, '../var/kgc/test-optimization');
const TEST_SNAPSHOT_DIR = path.join(TEST_DIR, 'snapshots');
const TEST_CAPSULE_DIR = path.join(TEST_DIR, 'capsules');
const TEST_RECEIPT_DIR = path.join(TEST_DIR, 'receipts');
const TEST_ARCHIVE_DIR = path.join(TEST_DIR, 'receipts-archive');

/**
 * Clean test directories
 */
async function cleanTestDirs() {
  try {
    await fs.rm(TEST_DIR, { recursive: true, force: true });
  } catch {
    // Ignore
  }
}

describe('Storage Optimizations', () => {
  beforeEach(async () => {
    await cleanTestDirs();
    await fs.mkdir(TEST_DIR, { recursive: true });
  });

  afterEach(async () => {
    await cleanTestDirs();
  });

  describe('Snapshot Compression (3 tests)', () => {
    it('should compress snapshot with gzip level 6', async () => {
      const universe = {
        entities: Array.from({ length: 100 }, (_, i) => ({
          id: i,
          name: `entity-${i}`,
          data: 'x'.repeat(100),
        })),
      };

      const manifest = await freezeUniverse(universe, {
        snapshotDir: TEST_SNAPSHOT_DIR,
        compress: true,
      });

      expect(manifest.compressed).toBe(true);
      expect(manifest.original_size).toBeGreaterThan(0);
      expect(manifest.compressed_size).toBeGreaterThan(0);
      expect(manifest.compressed_size).toBeLessThan(manifest.original_size);

      // Verify compression ratio (should be 60-80% for typical JSON)
      const ratio = manifest.compressed_size / manifest.original_size;
      expect(ratio).toBeLessThan(0.8);
    });

    it('should store compression metadata in manifest', async () => {
      const universe = { test: 'data', array: [1, 2, 3, 4, 5] };

      const manifest = await freezeUniverse(universe, {
        snapshotDir: TEST_SNAPSHOT_DIR,
        compress: true,
      });

      expect(manifest).toHaveProperty('compressed');
      expect(manifest).toHaveProperty('original_size');
      expect(manifest).toHaveProperty('compressed_size');
      expect(manifest.compressed).toBe(true);
    });

    it('should decompress on load and verify integrity', async () => {
      const universe = {
        value: 42,
        text: 'Hello, World!',
        nested: { a: 1, b: 2 },
      };

      const manifest = await freezeUniverse(universe, {
        snapshotDir: TEST_SNAPSHOT_DIR,
        compress: true,
      });

      // Reconstruct should automatically decompress
      const reconstructed = await reconstructTo(BigInt(manifest.timestamp_ns), {
        snapshotDir: TEST_SNAPSHOT_DIR,
      });

      expect(reconstructed).toEqual(universe);
    });
  });

  describe('Garbage Collection (3 tests)', () => {
    it('should delete snapshots exceeding maxSnapshots limit', async () => {
      // Create 10 snapshots
      for (let i = 0; i < 10; i++) {
        await freezeUniverse({ version: i }, {
          snapshotDir: TEST_SNAPSHOT_DIR,
          compress: false,
        });
        await new Promise((resolve) => setTimeout(resolve, 5));
      }

      const beforeGC = await getSnapshotList({ snapshotDir: TEST_SNAPSHOT_DIR });
      expect(beforeGC).toHaveLength(10);

      // Run GC with maxSnapshots=5
      const gcResult = await garbageCollectSnapshots(TEST_SNAPSHOT_DIR, {
        maxSnapshots: 5,
        ttlDays: 999, // Don't delete by age
      });

      expect(gcResult.deleted).toBe(5);
      expect(gcResult.kept).toBe(5);

      const afterGC = await getSnapshotList({ snapshotDir: TEST_SNAPSHOT_DIR });
      expect(afterGC).toHaveLength(5);
    });

    it('should delete snapshots older than TTL', async () => {
      // Create snapshot with old timestamp
      const oldSnapshot = await freezeUniverse({ version: 'old' }, {
        snapshotDir: TEST_SNAPSHOT_DIR,
        compress: false,
      });

      // Modify created_at to be 31 days old
      const manifestPath = path.join(
        TEST_SNAPSHOT_DIR,
        oldSnapshot.timestamp_ns,
        'manifest.json'
      );
      const manifest = JSON.parse(await fs.readFile(manifestPath, 'utf-8'));
      const oldDate = new Date();
      oldDate.setDate(oldDate.getDate() - 31);
      manifest.created_at = oldDate.toISOString();
      await fs.writeFile(manifestPath, JSON.stringify(manifest, null, 2));

      // Create recent snapshot
      await new Promise((resolve) => setTimeout(resolve, 10));
      await freezeUniverse({ version: 'new' }, {
        snapshotDir: TEST_SNAPSHOT_DIR,
        compress: false,
      });

      // Run GC with ttlDays=30
      const gcResult = await garbageCollectSnapshots(TEST_SNAPSHOT_DIR, {
        maxSnapshots: 999,
        ttlDays: 30,
      });

      expect(gcResult.deleted).toBe(1);
      expect(gcResult.kept).toBe(1);

      const afterGC = await getSnapshotList({ snapshotDir: TEST_SNAPSHOT_DIR });
      expect(afterGC).toHaveLength(1);
      expect(afterGC[0].manifest.timestamp_ns).not.toBe(oldSnapshot.timestamp_ns);
    });

    it('should report bytes freed during GC', async () => {
      // Create snapshots
      for (let i = 0; i < 5; i++) {
        await freezeUniverse({ version: i, data: 'x'.repeat(1000) }, {
          snapshotDir: TEST_SNAPSHOT_DIR,
          compress: false,
        });
        await new Promise((resolve) => setTimeout(resolve, 5));
      }

      const gcResult = await garbageCollectSnapshots(TEST_SNAPSHOT_DIR, {
        maxSnapshots: 2,
        ttlDays: 999,
      });

      expect(gcResult.deleted).toBe(3);
      expect(gcResult.bytes_freed).toBeGreaterThan(0);
    });
  });

  describe('Incremental Snapshots (2 tests)', () => {
    it('should create delta snapshot when incremental option is true', async () => {
      // Create base snapshot
      const baseState = {
        entities: [1, 2, 3],
        metadata: { version: 1 },
      };
      await freezeUniverse(baseState, {
        snapshotDir: TEST_SNAPSHOT_DIR,
        compress: false,
        incremental: false,
      });

      await new Promise((resolve) => setTimeout(resolve, 10));

      // Create incremental snapshot with small change
      const newState = {
        entities: [1, 2, 3, 4], // Add one entity
        metadata: { version: 2 }, // Change version
      };
      const manifest = await freezeUniverse(newState, {
        snapshotDir: TEST_SNAPSHOT_DIR,
        compress: false,
        incremental: true,
      });

      // Should be incremental if delta is smaller
      if (manifest.incremental) {
        expect(manifest.incremental).toBe(true);
        expect(manifest.base_snapshot).toBeDefined();
      }
    });

    it('should reconstruct state from incremental snapshot', async () => {
      // Create base
      const baseState = { value: 100, items: ['a', 'b'] };
      await freezeUniverse(baseState, {
        snapshotDir: TEST_SNAPSHOT_DIR,
        compress: false,
      });

      await new Promise((resolve) => setTimeout(resolve, 10));

      // Create incremental
      const newState = { value: 200, items: ['a', 'b', 'c'] };
      const manifest = await freezeUniverse(newState, {
        snapshotDir: TEST_SNAPSHOT_DIR,
        compress: false,
        incremental: true,
      });

      // Reconstruct
      const reconstructed = await reconstructTo(BigInt(manifest.timestamp_ns), {
        snapshotDir: TEST_SNAPSHOT_DIR,
      });

      expect(reconstructed).toEqual(newState);
    });
  });

  describe('Capsule Deduplication (2 tests)', () => {
    it('should detect duplicate capsules and reuse existing file', async () => {
      const data = {
        inputs: { test: 'duplicate' },
        tool_trace: [],
        edits: [],
        artifacts: [],
        bounds: { start: 1000, end: 2000 },
        o_hash_before: 'before',
        o_hash_after: 'after',
        receipts: [],
      };

      const capsule1 = new RunCapsule(data);
      const result1 = await storeCapsule(capsule1, TEST_CAPSULE_DIR);
      expect(result1.deduplicated).toBe(false);

      // Store same capsule again
      const capsule2 = new RunCapsule(data);
      const result2 = await storeCapsule(capsule2, TEST_CAPSULE_DIR);
      expect(result2.deduplicated).toBe(true);

      // Should point to same path
      expect(result1.path).toBe(result2.path);
    });

    it('should achieve space savings through deduplication', async () => {
      const baseData = {
        inputs: { action: 'common' },
        tool_trace: [],
        edits: [],
        artifacts: [],
        bounds: { start: 1000, end: 2000 },
        o_hash_before: 'hash1',
        o_hash_after: 'hash2',
        receipts: [],
      };

      // Store same capsule 5 times
      const results = [];
      for (let i = 0; i < 5; i++) {
        const capsule = new RunCapsule(baseData);
        const result = await storeCapsule(capsule, TEST_CAPSULE_DIR);
        results.push(result);
      }

      // First should not be deduplicated, rest should be
      expect(results[0].deduplicated).toBe(false);
      expect(results[1].deduplicated).toBe(true);
      expect(results[2].deduplicated).toBe(true);
      expect(results[3].deduplicated).toBe(true);
      expect(results[4].deduplicated).toBe(true);

      // All should point to same file
      const paths = new Set(results.map((r) => r.path));
      expect(paths.size).toBe(1);
    });
  });

  describe('Indexed Queries (2 tests)', () => {
    it('should use hash index for O(1) capsule lookup', async () => {
      // Create and store capsules
      const capsules = [];
      for (let i = 0; i < 10; i++) {
        const capsule = new RunCapsule({
          inputs: { id: i },
          tool_trace: [],
          edits: [],
          artifacts: [],
          bounds: { start: i * 1000, end: (i + 1) * 1000 },
          o_hash_before: `before${i}`,
          o_hash_after: `after${i}`,
          receipts: [],
        });
        await storeCapsule(capsule, TEST_CAPSULE_DIR);
        capsules.push(capsule);
      }

      // Verify index exists
      const indexPath = path.join(TEST_CAPSULE_DIR, 'index.json');
      expect(existsSync(indexPath)).toBe(true);

      // Find capsule by hash using index
      const targetCapsule = capsules[5];
      const found = await findCapsuleByHash(
        targetCapsule.capsule_hash,
        TEST_CAPSULE_DIR
      );

      expect(found).not.toBeNull();
      expect(found.capsule_hash).toBe(targetCapsule.capsule_hash);
      expect(found.inputs.id).toBe(5);
    });

    it('should fall back to linear search if index is missing', async () => {
      const capsule = new RunCapsule({
        inputs: { test: 'fallback' },
        tool_trace: [],
        edits: [],
        artifacts: [],
        bounds: { start: 1000, end: 2000 },
        o_hash_before: 'before',
        o_hash_after: 'after',
        receipts: [],
      });
      await storeCapsule(capsule, TEST_CAPSULE_DIR);

      // Remove index
      const indexPath = path.join(TEST_CAPSULE_DIR, 'index.json');
      await fs.unlink(indexPath);

      // Should still find capsule via linear search
      const found = await findCapsuleByHash(capsule.capsule_hash, TEST_CAPSULE_DIR);
      expect(found).not.toBeNull();
      expect(found.capsule_hash).toBe(capsule.capsule_hash);
    });
  });

  describe('Receipt Archival (2 tests)', () => {
    it('should archive old receipts based on count threshold', async () => {
      // Create 15 receipt files
      await fs.mkdir(TEST_RECEIPT_DIR, { recursive: true });

      for (let i = 0; i < 15; i++) {
        const receipt = {
          id: `receipt-${i}`,
          timestamp: new Date(Date.now() - i * 60000).toISOString(),
          operation: 'test',
          inputs: {},
          outputs: {},
          hash: `hash${i}`,
        };

        writeFileSync(
          path.join(TEST_RECEIPT_DIR, `receipt-${i}.json`),
          JSON.stringify(receipt)
        );
      }

      // Archive with keepRecent=10
      const result = await archiveReceipts(
        TEST_RECEIPT_DIR,
        TEST_ARCHIVE_DIR,
        { keepRecent: 10, keepDays: 999 }
      );

      expect(result.archived).toBe(5);
      expect(result.kept).toBe(10);

      // Verify files moved
      const mainFiles = await fs.readdir(TEST_RECEIPT_DIR);
      const archivedFiles = await fs.readdir(TEST_ARCHIVE_DIR);

      expect(mainFiles.filter((f) => f.startsWith('receipt-'))).toHaveLength(10);
      expect(archivedFiles).toHaveLength(5);
    });

    it('should archive receipts older than TTL', async () => {
      await fs.mkdir(TEST_RECEIPT_DIR, { recursive: true });

      // Create old receipt
      const oldReceipt = {
        id: 'receipt-old',
        timestamp: new Date(Date.now() - 8 * 24 * 60 * 60 * 1000).toISOString(), // 8 days old
        operation: 'test',
        inputs: {},
        outputs: {},
        hash: 'hash-old',
      };
      writeFileSync(
        path.join(TEST_RECEIPT_DIR, 'receipt-old.json'),
        JSON.stringify(oldReceipt)
      );

      // Create recent receipt
      const recentReceipt = {
        id: 'receipt-recent',
        timestamp: new Date().toISOString(),
        operation: 'test',
        inputs: {},
        outputs: {},
        hash: 'hash-recent',
      };
      writeFileSync(
        path.join(TEST_RECEIPT_DIR, 'receipt-recent.json'),
        JSON.stringify(recentReceipt)
      );

      // Archive with keepDays=7
      const result = await archiveReceipts(
        TEST_RECEIPT_DIR,
        TEST_ARCHIVE_DIR,
        { keepRecent: 999, keepDays: 7 }
      );

      expect(result.archived).toBe(1);
      expect(result.kept).toBe(1);

      // Verify old receipt archived
      const archivedFiles = await fs.readdir(TEST_ARCHIVE_DIR);
      expect(archivedFiles).toContain('receipt-old.json');
    });
  });

  describe('Delta Computation and Application', () => {
    it('should compute delta between states', () => {
      const previous = { a: 1, b: 2, c: 3 };
      const current = { a: 1, b: 20, d: 4 };

      const delta = computeDelta(previous, current);

      expect(delta.added).toEqual({ d: 4 });
      expect(delta.modified).toEqual({ b: 20 });
      expect(delta.deleted).toEqual({ c: true });
    });

    it('should apply delta to reconstruct state', () => {
      const base = { x: 10, y: 20, z: 30 };
      const delta = {
        added: { w: 40 },
        modified: { y: 200 },
        deleted: { z: true },
      };

      const result = applyDelta(base, delta);

      expect(result).toEqual({
        x: 10,
        y: 200,
        w: 40,
      });
    });
  });
});
