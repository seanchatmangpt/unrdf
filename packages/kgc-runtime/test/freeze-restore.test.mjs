/**
 * Tests for KGC Runtime Freeze-Restore functionality
 *
 * Validates:
 * - Freeze and verify operations
 * - Reconstruct from snapshot
 * - Hash consistency
 * - Multi-snapshot handling
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { promises as fs } from 'fs';
import * as path from 'path';
import { fileURLToPath } from 'url';
import { dirname } from 'path';
import {
  freezeUniverse,
  verifyFreeze,
  reconstructTo,
  getSnapshotList,
} from '../src/freeze-restore.mjs';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// Test snapshot directory
const TEST_SNAPSHOT_DIR = path.join(__dirname, '../var/kgc/snapshots-test');

/**
 * Clean test snapshot directory
 */
async function cleanTestSnapshots() {
  try {
    await fs.rm(TEST_SNAPSHOT_DIR, { recursive: true, force: true });
  } catch (error) {
    // Ignore if doesn't exist
  }
}

describe('KGC Runtime Freeze-Restore', () => {
  beforeEach(async () => {
    await cleanTestSnapshots();
  });

  afterEach(async () => {
    await cleanTestSnapshots();
  });

  describe('freezeUniverse', () => {
    it('should freeze simple universe state', async () => {
      const universe = {
        entities: ['entity1', 'entity2'],
        count: 2,
      };

      const manifest = await freezeUniverse(universe, { snapshotDir: TEST_SNAPSHOT_DIR });

      expect(manifest).toBeDefined();
      expect(manifest.timestamp_ns).toBeDefined();
      expect(manifest.o_hash).toBeDefined();
      expect(manifest.o_hash).toHaveLength(64); // BLAKE3 produces 32-byte (64-char hex) hash
      expect(manifest.file_count).toBe(1);
      expect(manifest.total_bytes).toBeGreaterThan(0);
      expect(manifest.created_at).toBeDefined();
    });

    it('should freeze empty universe', async () => {
      const universe = {};

      const manifest = await freezeUniverse(universe, { snapshotDir: TEST_SNAPSHOT_DIR });

      expect(manifest).toBeDefined();
      expect(manifest.o_hash).toBeDefined();
      expect(manifest.file_count).toBe(1);
    });

    it('should freeze universe with BigInt timestamp', async () => {
      const universe = {
        data: 'test',
        timestamp: 1234567890123456789n,
      };

      const manifest = await freezeUniverse(universe, { snapshotDir: TEST_SNAPSHOT_DIR });

      expect(manifest).toBeDefined();
      expect(manifest.timestamp_ns).toBeDefined();
    });

    it('should produce deterministic hash for same state', async () => {
      const universe = {
        entities: ['a', 'b', 'c'],
        count: 3,
      };

      const manifest1 = await freezeUniverse(universe, { snapshotDir: TEST_SNAPSHOT_DIR });

      // Wait a bit to ensure different timestamp
      await new Promise(resolve => setTimeout(resolve, 10));

      const manifest2 = await freezeUniverse(universe, { snapshotDir: TEST_SNAPSHOT_DIR });

      // Different timestamps but same hash for same content
      expect(manifest1.timestamp_ns).not.toBe(manifest2.timestamp_ns);
      expect(manifest1.o_hash).toBe(manifest2.o_hash);
    });

    it('should produce different hash for different state', async () => {
      const universe1 = { data: 'version1' };
      const universe2 = { data: 'version2' };

      const manifest1 = await freezeUniverse(universe1, { snapshotDir: TEST_SNAPSHOT_DIR });
      const manifest2 = await freezeUniverse(universe2, { snapshotDir: TEST_SNAPSHOT_DIR });

      expect(manifest1.o_hash).not.toBe(manifest2.o_hash);
    });

    it('should handle nested objects', async () => {
      const universe = {
        level1: {
          level2: {
            level3: {
              data: 'deep',
            },
          },
        },
      };

      const manifest = await freezeUniverse(universe, { snapshotDir: TEST_SNAPSHOT_DIR });

      expect(manifest).toBeDefined();
      expect(manifest.o_hash).toBeDefined();
    });

    it('should throw TypeError for non-object input', async () => {
      await expect(freezeUniverse(null, { snapshotDir: TEST_SNAPSHOT_DIR }))
        .rejects.toThrow(TypeError);

      await expect(freezeUniverse('string', { snapshotDir: TEST_SNAPSHOT_DIR }))
        .rejects.toThrow(TypeError);

      await expect(freezeUniverse(123, { snapshotDir: TEST_SNAPSHOT_DIR }))
        .rejects.toThrow(TypeError);
    });
  });

  describe('verifyFreeze', () => {
    it('should verify valid snapshot', async () => {
      const universe = { data: 'test' };
      const manifest = await freezeUniverse(universe, { snapshotDir: TEST_SNAPSHOT_DIR });

      const isValid = await verifyFreeze(manifest, { snapshotDir: TEST_SNAPSHOT_DIR });

      expect(isValid).toBe(true);
    });

    it('should verify snapshot by timestamp string', async () => {
      const universe = { data: 'test' };
      const manifest = await freezeUniverse(universe, { snapshotDir: TEST_SNAPSHOT_DIR });

      const isValid = await verifyFreeze(manifest.timestamp_ns, { snapshotDir: TEST_SNAPSHOT_DIR });

      expect(isValid).toBe(true);
    });

    it('should detect corrupted snapshot', async () => {
      const universe = { data: 'original' };
      const manifest = await freezeUniverse(universe, { snapshotDir: TEST_SNAPSHOT_DIR });

      // Corrupt the state file
      const statePath = path.join(TEST_SNAPSHOT_DIR, manifest.timestamp_ns, 'state.json');
      await fs.writeFile(statePath, JSON.stringify({ data: 'corrupted' }), 'utf-8');

      const isValid = await verifyFreeze(manifest, { snapshotDir: TEST_SNAPSHOT_DIR });

      expect(isValid).toBe(false);
    });

    it('should throw for non-existent snapshot', async () => {
      await expect(verifyFreeze('999999999999999999', { snapshotDir: TEST_SNAPSHOT_DIR }))
        .rejects.toThrow('Snapshot not found');
    });

    it('should throw TypeError for invalid input', async () => {
      await expect(verifyFreeze(null, { snapshotDir: TEST_SNAPSHOT_DIR }))
        .rejects.toThrow(TypeError);

      await expect(verifyFreeze(123, { snapshotDir: TEST_SNAPSHOT_DIR }))
        .rejects.toThrow(TypeError);

      await expect(verifyFreeze({}, { snapshotDir: TEST_SNAPSHOT_DIR }))
        .rejects.toThrow(TypeError);
    });
  });

  describe('reconstructTo', () => {
    it('should reconstruct from snapshot', async () => {
      const universe = {
        entities: ['e1', 'e2', 'e3'],
        count: 3,
        metadata: { version: 1 },
      };

      const manifest = await freezeUniverse(universe, { snapshotDir: TEST_SNAPSHOT_DIR });
      const targetTime = BigInt(manifest.timestamp_ns);

      const reconstructed = await reconstructTo(targetTime, { snapshotDir: TEST_SNAPSHOT_DIR });

      expect(reconstructed).toEqual(universe);
    });

    it('should reconstruct from string timestamp', async () => {
      const universe = { data: 'test' };
      const manifest = await freezeUniverse(universe, { snapshotDir: TEST_SNAPSHOT_DIR });

      const reconstructed = await reconstructTo(manifest.timestamp_ns, { snapshotDir: TEST_SNAPSHOT_DIR });

      expect(reconstructed).toEqual(universe);
    });

    it('should find closest snapshot before target time', async () => {
      const universe1 = { version: 1 };
      const manifest1 = await freezeUniverse(universe1, { snapshotDir: TEST_SNAPSHOT_DIR });

      await new Promise(resolve => setTimeout(resolve, 10));

      const universe2 = { version: 2 };
      const manifest2 = await freezeUniverse(universe2, { snapshotDir: TEST_SNAPSHOT_DIR });

      // Target time far in the future should get latest snapshot
      const futureTime = BigInt(manifest2.timestamp_ns) + 1000000n;
      const reconstructed = await reconstructTo(futureTime, { snapshotDir: TEST_SNAPSHOT_DIR });

      expect(reconstructed).toEqual(universe2);
    });

    it('should reconstruct earliest snapshot when target is far past', async () => {
      const universe = { data: 'earliest' };
      const manifest = await freezeUniverse(universe, { snapshotDir: TEST_SNAPSHOT_DIR });

      // Target time slightly after snapshot
      const targetTime = BigInt(manifest.timestamp_ns) + 1n;
      const reconstructed = await reconstructTo(targetTime, { snapshotDir: TEST_SNAPSHOT_DIR });

      expect(reconstructed).toEqual(universe);
    });

    it('should handle BigInt values in reconstructed state', async () => {
      const universe = {
        timestamp: 9876543210123456789n,
        value: 42n,
      };

      const manifest = await freezeUniverse(universe, { snapshotDir: TEST_SNAPSHOT_DIR });
      const reconstructed = await reconstructTo(BigInt(manifest.timestamp_ns), { snapshotDir: TEST_SNAPSHOT_DIR });

      expect(reconstructed.timestamp).toBe(9876543210123456789n);
      expect(reconstructed.value).toBe(42n);
    });

    it('should throw when no snapshots exist', async () => {
      await expect(reconstructTo(123456789n, { snapshotDir: TEST_SNAPSHOT_DIR }))
        .rejects.toThrow('No snapshots available');
    });

    it('should throw when target time is before all snapshots', async () => {
      const universe = { data: 'test' };
      const manifest = await freezeUniverse(universe, { snapshotDir: TEST_SNAPSHOT_DIR });

      const earlyTime = BigInt(manifest.timestamp_ns) - 1000000n;

      await expect(reconstructTo(earlyTime, { snapshotDir: TEST_SNAPSHOT_DIR }))
        .rejects.toThrow('No snapshot found');
    });

    it('should require exact match when exact option is true', async () => {
      const universe = { data: 'test' };
      const manifest = await freezeUniverse(universe, { snapshotDir: TEST_SNAPSHOT_DIR });

      const exactTime = BigInt(manifest.timestamp_ns);
      const reconstructed = await reconstructTo(exactTime, {
        snapshotDir: TEST_SNAPSHOT_DIR,
        exact: true,
      });

      expect(reconstructed).toEqual(universe);

      // Different time should fail with exact option
      const differentTime = exactTime + 1n;
      await expect(reconstructTo(differentTime, {
        snapshotDir: TEST_SNAPSHOT_DIR,
        exact: true,
      })).rejects.toThrow('No snapshot found');
    });

    it('should throw TypeError for invalid time', async () => {
      await expect(reconstructTo('not-a-number', { snapshotDir: TEST_SNAPSHOT_DIR }))
        .rejects.toThrow();

      await expect(reconstructTo(null, { snapshotDir: TEST_SNAPSHOT_DIR }))
        .rejects.toThrow(TypeError);

      await expect(reconstructTo({}, { snapshotDir: TEST_SNAPSHOT_DIR }))
        .rejects.toThrow(TypeError);
    });

    it('should throw RangeError for negative time', async () => {
      await expect(reconstructTo(-1n, { snapshotDir: TEST_SNAPSHOT_DIR }))
        .rejects.toThrow(RangeError);
    });
  });

  describe('getSnapshotList', () => {
    it('should return empty array when no snapshots exist', async () => {
      const snapshots = await getSnapshotList({ snapshotDir: TEST_SNAPSHOT_DIR });

      expect(snapshots).toEqual([]);
    });

    it('should list all snapshots sorted by time (newest first)', async () => {
      const universe1 = { version: 1 };
      await freezeUniverse(universe1, { snapshotDir: TEST_SNAPSHOT_DIR });

      await new Promise(resolve => setTimeout(resolve, 10));

      const universe2 = { version: 2 };
      await freezeUniverse(universe2, { snapshotDir: TEST_SNAPSHOT_DIR });

      await new Promise(resolve => setTimeout(resolve, 10));

      const universe3 = { version: 3 };
      await freezeUniverse(universe3, { snapshotDir: TEST_SNAPSHOT_DIR });

      const snapshots = await getSnapshotList({ snapshotDir: TEST_SNAPSHOT_DIR });

      expect(snapshots).toHaveLength(3);

      // Verify sorted newest first
      const time1 = BigInt(snapshots[0].manifest.timestamp_ns);
      const time2 = BigInt(snapshots[1].manifest.timestamp_ns);
      const time3 = BigInt(snapshots[2].manifest.timestamp_ns);

      expect(time1 > time2).toBe(true);
      expect(time2 > time3).toBe(true);
    });

    it('should list snapshots in ascending order when requested', async () => {
      const universe1 = { version: 1 };
      await freezeUniverse(universe1, { snapshotDir: TEST_SNAPSHOT_DIR });

      await new Promise(resolve => setTimeout(resolve, 10));

      const universe2 = { version: 2 };
      await freezeUniverse(universe2, { snapshotDir: TEST_SNAPSHOT_DIR });

      const snapshots = await getSnapshotList({
        snapshotDir: TEST_SNAPSHOT_DIR,
        ascending: true,
      });

      expect(snapshots).toHaveLength(2);

      // Verify sorted oldest first
      const time1 = BigInt(snapshots[0].manifest.timestamp_ns);
      const time2 = BigInt(snapshots[1].manifest.timestamp_ns);

      expect(time1 < time2).toBe(true);
    });

    it('should include manifest and path for each snapshot', async () => {
      const universe = { data: 'test' };
      await freezeUniverse(universe, { snapshotDir: TEST_SNAPSHOT_DIR });

      const snapshots = await getSnapshotList({ snapshotDir: TEST_SNAPSHOT_DIR });

      expect(snapshots).toHaveLength(1);
      expect(snapshots[0].manifest).toBeDefined();
      expect(snapshots[0].manifest.timestamp_ns).toBeDefined();
      expect(snapshots[0].manifest.o_hash).toBeDefined();
      expect(snapshots[0].path).toBeDefined();
      expect(snapshots[0].path).toContain(TEST_SNAPSHOT_DIR);
    });

    it('should skip invalid snapshots', async () => {
      const universe = { data: 'test' };
      await freezeUniverse(universe, { snapshotDir: TEST_SNAPSHOT_DIR });

      // Create invalid snapshot directory (no manifest)
      const invalidPath = path.join(TEST_SNAPSHOT_DIR, 'invalid-snapshot');
      await fs.mkdir(invalidPath, { recursive: true });

      const snapshots = await getSnapshotList({ snapshotDir: TEST_SNAPSHOT_DIR });

      // Should only return the valid snapshot
      expect(snapshots).toHaveLength(1);
    });
  });

  describe('Hash Consistency', () => {
    it('should produce same hash regardless of property order', async () => {
      const universe1 = { a: 1, b: 2, c: 3 };
      const universe2 = { c: 3, a: 1, b: 2 };

      const manifest1 = await freezeUniverse(universe1, { snapshotDir: TEST_SNAPSHOT_DIR });

      await new Promise(resolve => setTimeout(resolve, 10));

      const manifest2 = await freezeUniverse(universe2, { snapshotDir: TEST_SNAPSHOT_DIR });

      expect(manifest1.o_hash).toBe(manifest2.o_hash);
    });

    it('should produce same hash for nested objects with different property order', async () => {
      const universe1 = {
        outer: { a: 1, b: { x: 10, y: 20 } },
      };
      const universe2 = {
        outer: { b: { y: 20, x: 10 }, a: 1 },
      };

      const manifest1 = await freezeUniverse(universe1, { snapshotDir: TEST_SNAPSHOT_DIR });

      await new Promise(resolve => setTimeout(resolve, 10));

      const manifest2 = await freezeUniverse(universe2, { snapshotDir: TEST_SNAPSHOT_DIR });

      expect(manifest1.o_hash).toBe(manifest2.o_hash);
    });
  });

  describe('Multi-Snapshot Handling', () => {
    it('should manage multiple snapshots independently', async () => {
      const snapshots = [];

      for (let i = 0; i < 5; i++) {
        const universe = { version: i, data: `snapshot-${i}` };
        const manifest = await freezeUniverse(universe, { snapshotDir: TEST_SNAPSHOT_DIR });
        snapshots.push(manifest);

        await new Promise(resolve => setTimeout(resolve, 10));
      }

      const list = await getSnapshotList({ snapshotDir: TEST_SNAPSHOT_DIR });
      expect(list).toHaveLength(5);

      // Verify each snapshot independently
      for (const manifest of snapshots) {
        const isValid = await verifyFreeze(manifest, { snapshotDir: TEST_SNAPSHOT_DIR });
        expect(isValid).toBe(true);
      }
    });

    it('should reconstruct correct state from multiple snapshots', async () => {
      const states = [];

      for (let i = 0; i < 3; i++) {
        const universe = { version: i };
        const manifest = await freezeUniverse(universe, { snapshotDir: TEST_SNAPSHOT_DIR });
        states.push({ manifest, universe });

        await new Promise(resolve => setTimeout(resolve, 10));
      }

      // Reconstruct each state
      for (const { manifest, universe } of states) {
        const reconstructed = await reconstructTo(BigInt(manifest.timestamp_ns), {
          snapshotDir: TEST_SNAPSHOT_DIR
        });
        expect(reconstructed).toEqual(universe);
      }
    });
  });
});
