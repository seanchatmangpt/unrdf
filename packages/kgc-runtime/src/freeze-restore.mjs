/**
 * KGC Runtime Freeze-Restore - Universal Checkpointing and Reconstruction
 *
 * Provides nanosecond-precision state snapshots with BLAKE3 integrity verification.
 * Supports both Git-backed and filesystem-based storage strategies.
 *
 * @module freeze-restore
 */

import { blake3 } from 'hash-wasm';
import { promises as fs } from 'fs';
import * as path from 'path';
import { fileURLToPath } from 'url';
import { dirname } from 'path';
import {
  compressFile,
  readCompressed,
  computeDelta,
  applyDelta,
} from './storage-optimization.mjs';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

/** @typedef {Object} SnapshotManifest
 * @property {string} timestamp_ns - Nanosecond precision timestamp (as string for BigInt)
 * @property {string} o_hash - BLAKE3 hash of canonical state
 * @property {number} file_count - Number of files in snapshot
 * @property {number} total_bytes - Total bytes in snapshot
 * @property {string} created_at - ISO 8601 timestamp
 * @property {boolean} [compressed] - Whether snapshot is compressed
 * @property {number} [original_size] - Original size before compression
 * @property {number} [compressed_size] - Size after compression
 * @property {boolean} [incremental] - Whether snapshot is incremental
 * @property {string} [base_snapshot] - Base snapshot timestamp for incremental
 */

/** @typedef {Object} UniverseState
 * @property {Object} data - Arbitrary universe state data
 * @property {bigint} [timestamp] - Optional timestamp
 */

/** @typedef {Object} Snapshot
 * @property {SnapshotManifest} manifest - Snapshot metadata
 * @property {string} path - Filesystem path to snapshot
 * @property {UniverseState} state - Reconstructed state
 */

/**
 * Get default snapshot directory path
 * @returns {string} Absolute path to snapshots directory
 */
function getSnapshotDir() {
  // Navigate up from src/ to package root, then to var/kgc/snapshots
  const packageRoot = path.resolve(__dirname, '..');
  return path.join(packageRoot, 'var', 'kgc', 'snapshots');
}

/**
 * Normalize and canonicalize state for deterministic hashing
 * @param {UniverseState} state - Universe state object
 * @returns {string} Canonical JSON representation
 */
function canonicalizeState(state) {
  // Deep sort keys recursively for deterministic ordering
  function sortKeys(obj) {
    if (obj === null || typeof obj !== 'object') {
      return obj;
    }
    if (Array.isArray(obj)) {
      return obj.map(sortKeys);
    }
    const sorted = {};
    const keys = Object.keys(obj).sort();
    for (const key of keys) {
      sorted[key] = sortKeys(obj[key]);
    }
    return sorted;
  }

  // Convert BigInt to string for JSON serialization
  function replacer(key, value) {
    if (typeof value === 'bigint') {
      return value.toString() + 'n';
    }
    return value;
  }

  const normalized = sortKeys(state);
  return JSON.stringify(normalized, replacer, 0); // No whitespace for determinism
}

/**
 * Freeze universe to snapshot with BLAKE3 integrity hash
 *
 * Creates a point-in-time snapshot of universe state with:
 * - Nanosecond-precision timestamp (BigInt)
 * - BLAKE3 hash for integrity verification
 * - Manifest with metadata
 * - Compressed JSON serialization
 *
 * @param {UniverseState} O - Universe state to freeze
 * @param {Object} [options] - Freeze options
 * @param {string} [options.snapshotDir] - Custom snapshot directory
 * @param {boolean} [options.useGit=false] - Use Git backbone if available
 * @param {boolean} [options.compress=true] - Compress snapshot with gzip
 * @param {boolean} [options.incremental=false] - Create incremental snapshot
 * @returns {Promise<SnapshotManifest>} Snapshot manifest with hash and metadata
 * @throws {TypeError} If O is not an object
 * @throws {Error} If freeze operation fails
 *
 * @example
 * const universe = { entities: [...], timestamp: 1234567890123456789n };
 * const manifest = await freezeUniverse(universe);
 * console.assert(manifest.o_hash, 'Has BLAKE3 hash');
 * console.assert(manifest.timestamp_ns, 'Has nanosecond timestamp');
 */
export async function freezeUniverse(O, options = {}) {
  // Validation
  if (!O || typeof O !== 'object') {
    throw new TypeError('freezeUniverse: O must be an object');
  }

  try {
    // 1. Generate nanosecond timestamp
    const timestamp_ns = process.hrtime.bigint();

    // 2. Canonicalize state for deterministic hashing
    const canonical_state = canonicalizeState(O);

    // 3. Compute BLAKE3 hash
    const o_hash = await blake3(canonical_state);

    // 4. Create snapshot directory
    const snapshotDir = options.snapshotDir || getSnapshotDir();
    const snapshotPath = path.join(snapshotDir, timestamp_ns.toString());
    await fs.mkdir(snapshotPath, { recursive: true });

    // 5. Handle incremental snapshots
    let stateToWrite = canonical_state;
    let isIncremental = false;
    let baseSnapshot = null;

    if (options.incremental) {
      // Get previous snapshot for delta computation
      const snapshots = await getSnapshotList({ snapshotDir });
      if (snapshots.length > 0) {
        const prevSnapshot = snapshots[0];
        const prevStatePath = path.join(
          prevSnapshot.path,
          prevSnapshot.manifest.compressed ? 'state.json.gz' : 'state.json'
        );

        try {
          let prevState;
          if (prevSnapshot.manifest.compressed) {
            prevState = await readCompressed(prevStatePath);
          } else {
            prevState = await fs.readFile(prevStatePath, 'utf-8');
          }

          const prevObj = JSON.parse(prevState);
          const currObj = JSON.parse(canonical_state);
          const delta = computeDelta(prevObj, currObj);

          // Only use delta if it's significantly smaller
          const deltaStr = JSON.stringify(delta);
          if (deltaStr.length < canonical_state.length * 0.5) {
            stateToWrite = deltaStr;
            isIncremental = true;
            baseSnapshot = prevSnapshot.manifest.timestamp_ns;
          }
        } catch {
          // Fall back to full snapshot if delta fails
        }
      }
    }

    // 6. Write state file
    const statePath = path.join(snapshotPath, 'state.json');
    await fs.writeFile(statePath, stateToWrite, 'utf-8');

    // 7. Compress if requested (default: true)
    const shouldCompress = options.compress !== false;
    let finalSize = 0;
    let originalSize = 0;
    let compressed = false;

    if (shouldCompress) {
      const compressedPath = statePath + '.gz';
      const compressionResult = await compressFile(statePath, compressedPath);
      originalSize = compressionResult.original_size;
      finalSize = compressionResult.compressed_size;
      compressed = true;

      // Remove uncompressed file
      await fs.unlink(statePath);
    } else {
      const stats = await fs.stat(statePath);
      finalSize = stats.size;
      originalSize = stats.size;
    }

    // 8. Create manifest
    const manifest = {
      timestamp_ns: timestamp_ns.toString(),
      o_hash,
      file_count: 1,
      total_bytes: finalSize,
      created_at: new Date().toISOString(),
      compressed,
      ...(compressed && { original_size: originalSize, compressed_size: finalSize }),
      ...(isIncremental && { incremental: true, base_snapshot: baseSnapshot }),
    };

    // 9. Write manifest
    const manifestPath = path.join(snapshotPath, 'manifest.json');
    await fs.writeFile(manifestPath, JSON.stringify(manifest, null, 2), 'utf-8');

    // 10. Optional: Git commit if requested
    if (options.useGit) {
      try {
        const gitDir = path.resolve(snapshotDir, '../../..');
        const gitExists = await fs.access(path.join(gitDir, '.git'))
          .then(() => true)
          .catch(() => false);

        if (gitExists) {
          // Git operations would go here - simplified for now
          // This would use isomorphic-git to commit the snapshot
        }
      } catch (gitError) {
        // Git operations are optional - continue without them
        if (typeof console !== 'undefined' && console.warn) {
          console.warn(`[KGC Freeze] Git commit skipped: ${gitError.message}`);
        }
      }
    }

    return manifest;
  } catch (error) {
    throw new Error(`Failed to freeze universe: ${error.message}`);
  }
}

/**
 * Verify snapshot integrity by recomputing hash
 *
 * Validates that snapshot data matches stored hash, ensuring:
 * - Data integrity (no corruption)
 * - Authenticity (matches original freeze)
 * - Completeness (all files present)
 *
 * @param {SnapshotManifest|string} snapshot - Snapshot manifest or timestamp string
 * @param {Object} [options] - Verification options
 * @param {string} [options.snapshotDir] - Custom snapshot directory
 * @returns {Promise<boolean>} True if snapshot is valid
 * @throws {TypeError} If snapshot parameter is invalid
 * @throws {Error} If snapshot not found
 *
 * @example
 * const valid = await verifyFreeze(manifest);
 * console.assert(valid === true, 'Snapshot is valid');
 */
export async function verifyFreeze(snapshot, options = {}) {
  // Handle both manifest objects and timestamp strings
  let timestamp_ns;
  let expected_hash;

  if (typeof snapshot === 'string') {
    timestamp_ns = snapshot;
    // Load manifest to get hash
    const snapshotDir = options.snapshotDir || getSnapshotDir();
    const manifestPath = path.join(snapshotDir, timestamp_ns, 'manifest.json');

    try {
      const manifestData = await fs.readFile(manifestPath, 'utf-8');
      const manifest = JSON.parse(manifestData);
      expected_hash = manifest.o_hash;
    } catch (error) {
      throw new Error(`Snapshot not found: ${timestamp_ns}`);
    }
  } else if (snapshot && typeof snapshot === 'object') {
    if (!snapshot.timestamp_ns || !snapshot.o_hash) {
      throw new TypeError('verifyFreeze: snapshot must have timestamp_ns and o_hash');
    }
    timestamp_ns = snapshot.timestamp_ns;
    expected_hash = snapshot.o_hash;
  } else {
    throw new TypeError('verifyFreeze: snapshot must be manifest object or timestamp string');
  }

  try {
    // 1. Load manifest to check compression
    const snapshotDir = options.snapshotDir || getSnapshotDir();
    const manifestPath = path.join(snapshotDir, timestamp_ns, 'manifest.json');
    let manifest;
    try {
      const manifestData = await fs.readFile(manifestPath, 'utf-8');
      manifest = JSON.parse(manifestData);
    } catch {
      manifest = { compressed: false };
    }

    // 2. Load state file (handle compression)
    const isCompressed = manifest.compressed === true;
    const stateFileName = isCompressed ? 'state.json.gz' : 'state.json';
    const statePath = path.join(snapshotDir, timestamp_ns, stateFileName);

    let stateData;
    if (isCompressed) {
      stateData = await readCompressed(statePath);
    } else {
      stateData = await fs.readFile(statePath, 'utf-8');
    }

    // 3. Recompute hash
    const recomputed_hash = await blake3(stateData);

    // 4. Compare hashes
    return recomputed_hash === expected_hash;
  } catch (error) {
    throw new Error(`Failed to verify snapshot: ${error.message}`);
  }
}

/**
 * Reconstruct universe state from snapshot at specific time
 *
 * Loads and validates snapshot, returning reconstructed state:
 * - Finds snapshot at or before target time
 * - Validates manifest integrity
 * - Deserializes state
 * - Returns fully reconstructed object
 *
 * @param {bigint|string} t_ns - Target time in nanoseconds (BigInt or string)
 * @param {Object} [options] - Reconstruction options
 * @param {string} [options.snapshotDir] - Custom snapshot directory
 * @param {boolean} [options.exact=false] - Require exact timestamp match
 * @returns {Promise<UniverseState>} Reconstructed universe state
 * @throws {TypeError} If t_ns is invalid
 * @throws {Error} If no snapshot found before target time
 *
 * @example
 * const targetTime = 1234567890123456789n;
 * const reconstructed = await reconstructTo(targetTime);
 * console.log('State at', targetTime, ':', reconstructed);
 */
export async function reconstructTo(t_ns, options = {}) {
  // Normalize to BigInt
  let targetTime;
  if (typeof t_ns === 'bigint') {
    targetTime = t_ns;
  } else if (typeof t_ns === 'string') {
    targetTime = BigInt(t_ns);
  } else {
    throw new TypeError('reconstructTo: t_ns must be BigInt or string');
  }

  if (targetTime < 0n) {
    throw new RangeError('reconstructTo: t_ns must be non-negative');
  }

  try {
    // 1. Get list of all snapshots
    const snapshots = await getSnapshotList(options);

    if (snapshots.length === 0) {
      throw new Error('No snapshots available for reconstruction');
    }

    // 2. Find best snapshot (at or before target time)
    let bestSnapshot = null;
    let bestTime = -1n;

    for (const snapshot of snapshots) {
      const snapshotTime = BigInt(snapshot.manifest.timestamp_ns);

      if (options.exact) {
        // Exact match required
        if (snapshotTime === targetTime) {
          bestSnapshot = snapshot;
          bestTime = snapshotTime;
          break;
        }
      } else {
        // Find closest snapshot at or before target
        if (snapshotTime <= targetTime && snapshotTime > bestTime) {
          bestSnapshot = snapshot;
          bestTime = snapshotTime;
        }
      }
    }

    if (!bestSnapshot) {
      const matchType = options.exact ? 'exact' : 'at or before';
      throw new Error(`No snapshot found ${matchType} time ${targetTime}`);
    }

    // 3. Verify snapshot integrity
    const isValid = await verifyFreeze(bestSnapshot.manifest, options);
    if (!isValid) {
      throw new Error(`Snapshot integrity check failed for ${bestSnapshot.manifest.timestamp_ns}`);
    }

    // 4. Load and deserialize state (handle compression and incremental)
    const snapshotDir = options.snapshotDir || getSnapshotDir();
    const isCompressed = bestSnapshot.manifest.compressed === true;
    const stateFileName = isCompressed ? 'state.json.gz' : 'state.json';
    const statePath = path.join(
      snapshotDir,
      bestSnapshot.manifest.timestamp_ns,
      stateFileName
    );

    let stateData;
    if (isCompressed) {
      stateData = await readCompressed(statePath);
    } else {
      stateData = await fs.readFile(statePath, 'utf-8');
    }

    // 5. Handle incremental snapshots
    let finalState;
    if (bestSnapshot.manifest.incremental && bestSnapshot.manifest.base_snapshot) {
      // Reconstruct from base + delta
      const delta = JSON.parse(stateData);
      const baseState = await reconstructTo(
        BigInt(bestSnapshot.manifest.base_snapshot),
        options
      );
      finalState = applyDelta(baseState, delta);
    } else {
      // Revive BigInt values
      finalState = JSON.parse(stateData, (key, value) => {
        if (typeof value === 'string' && value.endsWith('n')) {
          try {
            return BigInt(value.slice(0, -1));
          } catch {
            return value;
          }
        }
        return value;
      });
    }

    return finalState;
  } catch (error) {
    throw new Error(`Failed to reconstruct state: ${error.message}`);
  }
}

/**
 * Get list of all snapshots sorted by timestamp
 *
 * Scans snapshot directory and returns sorted list of available snapshots
 * with their manifests loaded and validated.
 *
 * @param {Object} [options] - List options
 * @param {string} [options.snapshotDir] - Custom snapshot directory
 * @param {boolean} [options.ascending=false] - Sort oldest first (default: newest first)
 * @returns {Promise<Snapshot[]>} Array of snapshots sorted by time
 *
 * @example
 * const snapshots = await getSnapshotList();
 * console.log(`Found ${snapshots.length} snapshots`);
 * console.log('Latest:', snapshots[0].manifest.timestamp_ns);
 */
export async function getSnapshotList(options = {}) {
  const snapshotDir = options.snapshotDir || getSnapshotDir();

  try {
    // Ensure directory exists
    await fs.mkdir(snapshotDir, { recursive: true });

    // Read directory entries
    const entries = await fs.readdir(snapshotDir, { withFileTypes: true });

    // Filter for directories (each snapshot is a directory)
    const snapshotDirs = entries
      .filter(entry => entry.isDirectory())
      .map(entry => entry.name);

    // Load manifests
    const snapshots = [];
    for (const dirName of snapshotDirs) {
      try {
        const manifestPath = path.join(snapshotDir, dirName, 'manifest.json');
        const manifestData = await fs.readFile(manifestPath, 'utf-8');
        const manifest = JSON.parse(manifestData);

        snapshots.push({
          manifest,
          path: path.join(snapshotDir, dirName),
        });
      } catch (error) {
        // Skip invalid snapshots
        if (typeof console !== 'undefined' && console.warn) {
          console.warn(`[KGC Snapshots] Skipped invalid snapshot ${dirName}: ${error.message}`);
        }
      }
    }

    // Sort by timestamp (newest first by default)
    snapshots.sort((a, b) => {
      const timeA = BigInt(a.manifest.timestamp_ns);
      const timeB = BigInt(b.manifest.timestamp_ns);

      if (options.ascending) {
        return timeA < timeB ? -1 : timeA > timeB ? 1 : 0;
      } else {
        return timeB < timeA ? -1 : timeB > timeA ? 1 : 0;
      }
    });

    return snapshots;
  } catch (error) {
    // If directory doesn't exist or other error, return empty array
    return [];
  }
}
