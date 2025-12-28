/**
 * @fileoverview Storage optimization utilities for KGC runtime
 * Provides compression, garbage collection, and archival capabilities
 */

import { promises as fs } from 'fs';
import { createGzip, createGunzip } from 'zlib';
import { pipeline } from 'stream/promises';
import { createReadStream, createWriteStream } from 'fs';
import * as path from 'path';

/**
 * Compress file using gzip (level 6)
 * @param {string} inputPath - Path to input file
 * @param {string} outputPath - Path to output file (will be .gz)
 * @returns {Promise<{original_size: number, compressed_size: number}>} Size metadata
 */
export async function compressFile(inputPath, outputPath) {
  const stats = await fs.stat(inputPath);
  const originalSize = stats.size;

  const gzip = createGzip({ level: 6 });
  const source = createReadStream(inputPath);
  const destination = createWriteStream(outputPath);

  await pipeline(source, gzip, destination);

  const compressedStats = await fs.stat(outputPath);
  const compressedSize = compressedStats.size;

  return {
    original_size: originalSize,
    compressed_size: compressedSize,
  };
}

/**
 * Decompress gzip file
 * @param {string} inputPath - Path to .gz file
 * @param {string} outputPath - Path to output file
 * @returns {Promise<void>}
 */
export async function decompressFile(inputPath, outputPath) {
  const gunzip = createGunzip();
  const source = createReadStream(inputPath);
  const destination = createWriteStream(outputPath);

  await pipeline(source, gunzip, destination);
}

/**
 * Read and decompress gzip file to string
 * @param {string} filePath - Path to .gz file
 * @returns {Promise<string>} Decompressed content
 */
export async function readCompressed(filePath) {
  const chunks = [];
  const gunzip = createGunzip();
  const source = createReadStream(filePath);

  await pipeline(
    source,
    gunzip,
    async function* (source) {
      for await (const chunk of source) {
        chunks.push(chunk);
      }
    }
  );

  return Buffer.concat(chunks).toString('utf-8');
}

/**
 * Garbage collection configuration
 * @typedef {Object} GCConfig
 * @property {number} maxSnapshots - Maximum number of snapshots to keep
 * @property {number} ttlDays - Time-to-live in days
 */

/**
 * Apply garbage collection to snapshots
 * @param {string} snapshotDir - Snapshot directory path
 * @param {GCConfig} config - GC configuration
 * @returns {Promise<{deleted: number, kept: number, bytes_freed: number}>} GC results
 */
export async function garbageCollectSnapshots(
  snapshotDir,
  config = { maxSnapshots: 100, ttlDays: 30 }
) {
  try {
    await fs.access(snapshotDir);
  } catch {
    return { deleted: 0, kept: 0, bytes_freed: 0 };
  }

  const entries = await fs.readdir(snapshotDir, { withFileTypes: true });
  const snapshotDirs = entries
    .filter((entry) => entry.isDirectory())
    .map((entry) => entry.name);

  const snapshots = [];
  for (const dirName of snapshotDirs) {
    try {
      const manifestPath = path.join(snapshotDir, dirName, 'manifest.json');
      const manifestData = await fs.readFile(manifestPath, 'utf-8');
      const manifest = JSON.parse(manifestData);

      snapshots.push({
        dirName,
        path: path.join(snapshotDir, dirName),
        manifest,
        timestamp_ns: BigInt(manifest.timestamp_ns),
        created_at: new Date(manifest.created_at),
      });
    } catch {
      // Skip invalid snapshots
    }
  }

  // Sort by timestamp (newest first)
  snapshots.sort((a, b) => (a.timestamp_ns > b.timestamp_ns ? -1 : 1));

  const now = new Date();
  const ttlMs = config.ttlDays * 24 * 60 * 60 * 1000;
  const cutoffDate = new Date(now.getTime() - ttlMs);

  let deleted = 0;
  let bytesFreed = 0;

  for (let i = 0; i < snapshots.length; i++) {
    const snapshot = snapshots[i];
    const shouldDelete =
      i >= config.maxSnapshots || snapshot.created_at < cutoffDate;

    if (shouldDelete) {
      try {
        // Calculate size before deletion
        const size = await getDirectorySize(snapshot.path);
        await fs.rm(snapshot.path, { recursive: true, force: true });
        deleted++;
        bytesFreed += size;
      } catch (error) {
        // Continue if deletion fails
        if (typeof console !== 'undefined' && console.warn) {
          console.warn(
            `[GC] Failed to delete snapshot ${snapshot.dirName}: ${error.message}`
          );
        }
      }
    }
  }

  return {
    deleted,
    kept: snapshots.length - deleted,
    bytes_freed: bytesFreed,
  };
}

/**
 * Get total size of directory
 * @param {string} dirPath - Directory path
 * @returns {Promise<number>} Total bytes
 * @private
 */
async function getDirectorySize(dirPath) {
  let totalSize = 0;

  async function traverse(currentPath) {
    const entries = await fs.readdir(currentPath, { withFileTypes: true });

    for (const entry of entries) {
      const fullPath = path.join(currentPath, entry.name);

      if (entry.isDirectory()) {
        await traverse(fullPath);
      } else {
        const stats = await fs.stat(fullPath);
        totalSize += stats.size;
      }
    }
  }

  await traverse(dirPath);
  return totalSize;
}

/**
 * Receipt archival configuration
 * @typedef {Object} ArchivalConfig
 * @property {number} keepRecent - Number of recent receipts to keep
 * @property {number} keepDays - Days to keep receipts in main store
 */

/**
 * Archive old receipts
 * @param {string} receiptDir - Receipt directory path
 * @param {string} archiveDir - Archive directory path
 * @param {ArchivalConfig} config - Archival configuration
 * @returns {Promise<{archived: number, kept: number}>} Archival results
 */
export async function archiveReceipts(
  receiptDir,
  archiveDir,
  config = { keepRecent: 1000, keepDays: 7 }
) {
  try {
    await fs.access(receiptDir);
  } catch {
    return { archived: 0, kept: 0 };
  }

  await fs.mkdir(archiveDir, { recursive: true });

  const files = await fs.readdir(receiptDir);
  const receiptFiles = files.filter((f) => f.startsWith('receipt-') && f.endsWith('.json'));

  const receipts = [];
  for (const file of receiptFiles) {
    try {
      const filePath = path.join(receiptDir, file);
      const stats = await fs.stat(filePath);
      const data = await fs.readFile(filePath, 'utf-8');
      const receipt = JSON.parse(data);

      receipts.push({
        file,
        path: filePath,
        receipt,
        timestamp: new Date(receipt.timestamp),
        modified: stats.mtime,
      });
    } catch {
      // Skip invalid files
    }
  }

  // Sort by timestamp (newest first)
  receipts.sort((a, b) => b.timestamp - a.timestamp);

  const now = new Date();
  const keepMs = config.keepDays * 24 * 60 * 60 * 1000;
  const cutoffDate = new Date(now.getTime() - keepMs);

  let archived = 0;

  for (let i = 0; i < receipts.length; i++) {
    const receipt = receipts[i];
    const shouldArchive = i >= config.keepRecent || receipt.timestamp < cutoffDate;

    if (shouldArchive) {
      try {
        const archivePath = path.join(archiveDir, receipt.file);
        await fs.rename(receipt.path, archivePath);
        archived++;
      } catch (error) {
        // Continue if archival fails
        if (typeof console !== 'undefined' && console.warn) {
          console.warn(
            `[Archive] Failed to archive receipt ${receipt.file}: ${error.message}`
          );
        }
      }
    }
  }

  return {
    archived,
    kept: receipts.length - archived,
  };
}

/**
 * Compute delta between two objects (for incremental snapshots)
 * @param {Object} previous - Previous state
 * @param {Object} current - Current state
 * @returns {Object} Delta object with changes
 */
export function computeDelta(previous, current) {
  const delta = {
    added: {},
    modified: {},
    deleted: {},
  };

  const prevStr = JSON.stringify(previous);
  const currStr = JSON.stringify(current);

  // If identical, return empty delta
  if (prevStr === currStr) {
    return delta;
  }

  // Simple delta: store full current state if different
  // Phase 1: basic implementation (can be optimized later)
  const prevKeys = new Set(Object.keys(previous || {}));
  const currKeys = new Set(Object.keys(current || {}));

  // Added keys
  for (const key of currKeys) {
    if (!prevKeys.has(key)) {
      delta.added[key] = current[key];
    }
  }

  // Modified keys
  for (const key of currKeys) {
    if (prevKeys.has(key)) {
      const prevValue = JSON.stringify(previous[key]);
      const currValue = JSON.stringify(current[key]);
      if (prevValue !== currValue) {
        delta.modified[key] = current[key];
      }
    }
  }

  // Deleted keys
  for (const key of prevKeys) {
    if (!currKeys.has(key)) {
      delta.deleted[key] = true;
    }
  }

  return delta;
}

/**
 * Apply delta to base state
 * @param {Object} base - Base state
 * @param {Object} delta - Delta to apply
 * @returns {Object} Reconstructed state
 */
export function applyDelta(base, delta) {
  const result = { ...base };

  // Apply additions
  if (delta.added) {
    for (const [key, value] of Object.entries(delta.added)) {
      result[key] = value;
    }
  }

  // Apply modifications
  if (delta.modified) {
    for (const [key, value] of Object.entries(delta.modified)) {
      result[key] = value;
    }
  }

  // Apply deletions
  if (delta.deleted) {
    for (const key of Object.keys(delta.deleted)) {
      delete result[key];
    }
  }

  return result;
}
