/**
 * @fileoverview Store restore implementation with OTEL instrumentation
 *
 * @description
 * Restores RDF stores from compressed backups with validation and
 * rollback support. Fully instrumented with OpenTelemetry.
 *
 * @module cli/store-restore
 * @version 2.1.1
 * @license MIT
 */

import { writeFile, mkdir, access, rm } from 'node:fs/promises';
import { createReadStream, createWriteStream } from 'node:fs';
import { createGunzip } from 'node:zlib';
import { join } from 'node:path';
import { z } from 'zod';
import { defaultObservabilityManager } from '../knowledge-engine/observability.mjs';
import { fileExists, ensureDir } from '../utils/io-utils.mjs';
import { promises as fs } from 'node:fs';

/**
 * Restore options schema
 */
const RestoreOptionsSchema = z.object({
  target: z.string(),
  overwrite: z.boolean().default(false),
  validate: z.boolean().default(true),
  createBackup: z.boolean().default(true)
});

/**
 * Restore result schema
 */
const RestoreResultSchema = z.object({
  storePath: z.string(),
  quadCount: z.number(),
  graphCount: z.number(),
  filesRestored: z.number(),
  duration: z.number(),
  validated: z.boolean()
});

/**
 * Restore RDF store from backup
 *
 * @param {string} backupPath - Path to backup file
 * @param {Object} options - Restore options
 * @param {string} options.target - Target store directory path
 * @param {boolean} [options.overwrite=false] - Overwrite existing store
 * @param {boolean} [options.validate=true] - Validate backup before restore
 * @param {boolean} [options.createBackup=true] - Backup existing store before overwrite
 * @returns {Promise<Object>} Restore result with statistics
 *
 * @throws {Error} If backup doesn't exist, validation fails, or restore fails
 *
 * @example
 * const result = await restoreStore('backup-2024.tar.gz', {
 *   target: './restored-store',
 *   overwrite: false,
 *   validate: true
 * });
 * console.log(`Restored ${result.quadCount} quads`);
 */
export async function restoreStore(backupPath, options) {
  // Initialize observability
  await defaultObservabilityManager.initialize();

  const transactionId = `restore-${Date.now()}`;
  const spanContext = defaultObservabilityManager.startTransactionSpan(transactionId, {
    'operation': 'store.restore',
    'backup.path': backupPath
  });

  const startTime = Date.now();

  try {
    // Validate options
    const opts = RestoreOptionsSchema.parse(options);

    // Check if backup exists
    const backupExists = await fileExists(backupPath);
    if (!backupExists) {
      throw new Error(`Backup file does not exist: ${backupPath}`);
    }

    // Check if target exists
    const targetExists = await fileExists(opts.target);
    if (targetExists && !opts.overwrite) {
      throw new Error(`Target directory already exists: ${opts.target}. Use --overwrite to replace.`);
    }

    // Backup existing store if requested
    if (targetExists && opts.createBackup) {
      console.log('📦 Creating safety backup of existing store...');
      const safetyBackupPath = `${opts.target}-backup-${Date.now()}.tar.gz`;
      // We would call backupStore here, but to avoid circular dependency,
      // we'll just rename the directory
      await fs.rename(opts.target, `${opts.target}-pre-restore-${Date.now()}`);
    } else if (targetExists) {
      // Remove existing store
      await rm(opts.target, { recursive: true, force: true });
    }

    console.log(`📂 Restoring backup to ${opts.target}...`);

    // Read and decompress backup
    const backupData = await readBackupFile(backupPath);

    // Validate backup if requested
    if (opts.validate) {
      console.log('🔍 Validating backup integrity...');
      validateBackupData(backupData);
    }

    // Create target directory
    await ensureDir(opts.target);

    // Restore files
    const stats = await restoreFiles(opts.target, backupData);

    const duration = Date.now() - startTime;

    const result = RestoreResultSchema.parse({
      storePath: opts.target,
      quadCount: stats.quadCount,
      graphCount: stats.graphCount,
      filesRestored: stats.filesRestored,
      duration,
      validated: opts.validate
    });

    // End span successfully
    defaultObservabilityManager.endTransactionSpan(transactionId, {
      'restore.quads': stats.quadCount,
      'restore.graphs': stats.graphCount,
      'restore.files': stats.filesRestored,
      'restore.duration_ms': duration
    });

    return result;
  } catch (error) {
    // Record error
    defaultObservabilityManager.recordError(error, {
      'operation': 'store.restore',
      'backup.path': backupPath
    });

    // End span with error
    defaultObservabilityManager.endTransactionSpan(transactionId, {}, error);

    throw error;
  }
}

/**
 * Read and decompress backup file
 *
 * @param {string} backupPath - Backup file path
 * @returns {Promise<Object>} Backup data object
 * @private
 */
async function readBackupFile(backupPath) {
  const isCompressed = backupPath.endsWith('.gz');

  if (isCompressed) {
    // Read compressed backup
    const chunks = [];
    const readStream = createReadStream(backupPath);
    const gunzip = createGunzip();

    readStream.pipe(gunzip);

    for await (const chunk of gunzip) {
      chunks.push(chunk);
    }

    const json = Buffer.concat(chunks).toString('utf-8');
    return JSON.parse(json);
  } else {
    // Read uncompressed backup
    const content = await fs.readFile(backupPath, 'utf-8');
    return JSON.parse(content);
  }
}

/**
 * Validate backup data structure
 *
 * @param {Object} backupData - Backup data to validate
 * @throws {Error} If backup is invalid
 * @private
 */
function validateBackupData(backupData) {
  if (!backupData.manifest) {
    throw new Error('Invalid backup: missing manifest');
  }

  if (!backupData.files) {
    throw new Error('Invalid backup: missing files');
  }

  if (!backupData.manifest.version) {
    throw new Error('Invalid backup: missing version');
  }

  // Validate file count matches
  const manifestFileCount = backupData.manifest.files.length;
  const actualFileCount = Object.keys(backupData.files).length;

  if (manifestFileCount !== actualFileCount) {
    throw new Error(
      `Backup integrity check failed: manifest lists ${manifestFileCount} files but backup contains ${actualFileCount}`
    );
  }
}

/**
 * Restore files from backup data
 *
 * @param {string} targetPath - Target directory path
 * @param {Object} backupData - Backup data object
 * @returns {Promise<Object>} Restore statistics
 * @private
 */
async function restoreFiles(targetPath, backupData) {
  let quadCount = 0;
  const graphs = new Set();
  let filesRestored = 0;

  // Restore each file
  for (const [filename, content] of Object.entries(backupData.files)) {
    const filePath = join(targetPath, filename);

    // Ensure subdirectory exists
    const fileDir = join(targetPath, ...filename.split('/').slice(0, -1));
    if (fileDir !== targetPath) {
      await ensureDir(fileDir);
    }

    // Write file
    await writeFile(filePath, content, 'utf-8');
    filesRestored++;

    // Count quads
    if (filename.endsWith('.nq') || filename.endsWith('.nt')) {
      const lines = content.split('\n').filter(line => line.trim() && !line.startsWith('#'));
      quadCount += lines.length;

      // Extract graphs
      for (const line of lines) {
        const graphMatch = line.match(/<([^>]+)>\s*\.?\s*$/);
        if (graphMatch) {
          graphs.add(graphMatch[1]);
        }
      }
    }

    // Progress reporting
    if (filesRestored % 10 === 0) {
      console.log(`  Restored ${filesRestored} files...`);
    }
  }

  return {
    quadCount,
    graphCount: graphs.size,
    filesRestored
  };
}
