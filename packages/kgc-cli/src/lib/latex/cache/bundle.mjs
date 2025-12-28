/**
 * @fileoverview Bundle export/import for offline LaTeX compilation.
 *
 * Supports creating self-contained bundles of all cached packages
 * for deployment to air-gapped environments or CI/CD.
 *
 * Bundle structure:
 *   bundle/
 *     bundle.manifest.json    # Metadata + file listing
 *     packages/               # All cached packages
 *     fonts/                  # All cached fonts
 *
 * @module lib/latex/cache/bundle
 */

import { readFile, writeFile, mkdir, copyFile, readdir as _readdir } from 'node:fs/promises';
import { join, relative as _relative, dirname } from 'node:path';
import { createHash } from 'node:crypto';
import { z } from 'zod';
import { listCached } from './store.mjs';
import { LockfileSchema } from './lockfile.mjs';

/**
 * Bundle manifest schema.
 * @type {z.ZodSchema}
 */
const BundleManifestSchema = z.object({
  version: z.literal('1.0.0').describe('Bundle format version'),
  createdAt: z.number().int().positive().describe('Unix timestamp (ms) when created'),
  engine: z.enum(['xetex', 'pdftex', 'luatex']).describe('LaTeX engine'),
  fileCount: z.number().int().nonnegative().describe('Total number of files in bundle'),
  totalSize: z.number().int().nonnegative().describe('Total size in bytes'),
  files: z.array(z.object({
    name: z.string(),
    relativePath: z.string(),
    hash: z.string(),
    size: z.number().int().nonnegative()
  })).describe('List of files in bundle')
});

/**
 * @typedef {z.infer<typeof BundleManifestSchema>} BundleManifest
 */

/**
 * Export all cached packages to a bundle directory.
 *
 * Creates a self-contained bundle that can be imported on another machine.
 *
 * @param {Object} lockfile - Lockfile object (from lockfile.mjs)
 * @param {string} cacheDir - Cache directory (absolute path)
 * @param {string} outputDir - Output directory for bundle (absolute path)
 * @returns {Promise<BundleManifest>} Bundle manifest
 */
export async function exportBundle(lockfile, cacheDir, outputDir) {
  // Validate lockfile
  const lockResult = LockfileSchema.safeParse(lockfile);
  if (!lockResult.success) {
    throw new Error(`Invalid lockfile: ${lockResult.error.message}`);
  }

  // Create output directory
  await mkdir(outputDir, { recursive: true });

  // Get all cached files
  const cachedFiles = await listCached(cacheDir);

  // Filter to only files in lockfile
  const lockfileNames = new Set(Object.keys(lockfile.entries));
  const bundleFiles = cachedFiles.filter(entry => lockfileNames.has(entry.name));

  // Copy files to bundle
  const copiedFiles = [];
  for (const entry of bundleFiles) {
    const destPath = join(outputDir, entry.relativePath);
    await mkdir(dirname(destPath), { recursive: true });
    await copyFile(entry.path, destPath);

    copiedFiles.push({
      name: entry.name,
      relativePath: entry.relativePath,
      hash: entry.hash,
      size: entry.size
    });
  }

  // Create manifest
  const manifest = {
    version: '1.0.0',
    createdAt: Date.now(),
    engine: lockfile.engine,
    fileCount: copiedFiles.length,
    totalSize: copiedFiles.reduce((sum, f) => sum + f.size, 0),
    files: copiedFiles.sort((a, b) => a.relativePath.localeCompare(b.relativePath))
  };

  // Validate manifest
  const manifestResult = BundleManifestSchema.safeParse(manifest);
  if (!manifestResult.success) {
    throw new Error(`Invalid manifest: ${manifestResult.error.message}`);
  }

  // Write manifest
  const manifestPath = join(outputDir, 'bundle.manifest.json');
  await writeFile(
    manifestPath,
    JSON.stringify(manifestResult.data, null, 2) + '\n',
    'utf-8'
  );

  return manifestResult.data;
}

/**
 * Import bundle into local cache.
 *
 * Copies all files from bundle directory to cache.
 * Verifies hashes during import.
 *
 * @param {string} bundleDir - Bundle directory (absolute path)
 * @param {string} cacheDir - Cache directory (absolute path)
 * @returns {Promise<{imported: number, skipped: number, failed: number}>}
 */
export async function importBundle(bundleDir, cacheDir) {
  // Load manifest
  const manifestPath = join(bundleDir, 'bundle.manifest.json');
  const manifestRaw = await readFile(manifestPath, 'utf-8');
  const manifestParsed = JSON.parse(manifestRaw);

  // Validate manifest
  const result = BundleManifestSchema.safeParse(manifestParsed);
  if (!result.success) {
    throw new Error(`Invalid bundle manifest: ${result.error.message}`);
  }

  const manifest = result.data;

  // Import files
  let imported = 0;
  let skipped = 0;
  let failed = 0;

  for (const file of manifest.files) {
    try {
      const sourcePath = join(bundleDir, file.relativePath);
      const destPath = join(cacheDir, file.relativePath);

      // Create parent directory
      await mkdir(dirname(destPath), { recursive: true });

      // Copy file
      await copyFile(sourcePath, destPath);

      // Verify hash
      const content = await readFile(destPath);
      const actualHash = hashContent(content);

      if (actualHash === file.hash) {
        imported++;
      } else {
        console.warn(`Hash mismatch for ${file.name}: expected ${file.hash}, got ${actualHash}`);
        failed++;
      }
    } catch (err) {
      console.error(`Failed to import ${file.name}:`, err.message);
      failed++;
    }
  }

  return { imported, skipped, failed };
}

/**
 * Verify bundle integrity.
 *
 * Checks that all files in manifest exist and have correct hashes.
 *
 * @param {string} bundleDir - Bundle directory (absolute path)
 * @returns {Promise<{valid: number, invalid: number, missing: number}>}
 */
export async function verifyBundle(bundleDir) {
  // Load manifest
  const manifestPath = join(bundleDir, 'bundle.manifest.json');
  const manifestRaw = await readFile(manifestPath, 'utf-8');
  const manifestParsed = JSON.parse(manifestRaw);

  // Validate manifest
  const result = BundleManifestSchema.safeParse(manifestParsed);
  if (!result.success) {
    throw new Error(`Invalid bundle manifest: ${result.error.message}`);
  }

  const manifest = result.data;

  let valid = 0;
  let invalid = 0;
  let missing = 0;

  for (const file of manifest.files) {
    try {
      const filePath = join(bundleDir, file.relativePath);
      const content = await readFile(filePath);
      const actualHash = hashContent(content);

      if (actualHash === file.hash) {
        valid++;
      } else {
        invalid++;
      }
    } catch (err) {
      if (err.code === 'ENOENT') {
        missing++;
      } else {
        throw err;
      }
    }
  }

  return { valid, invalid, missing };
}

/**
 * List files in bundle.
 *
 * @param {string} bundleDir - Bundle directory (absolute path)
 * @returns {Promise<BundleManifest>} Bundle manifest
 */
export async function listBundle(bundleDir) {
  const manifestPath = join(bundleDir, 'bundle.manifest.json');
  const manifestRaw = await readFile(manifestPath, 'utf-8');
  const manifestParsed = JSON.parse(manifestRaw);

  const result = BundleManifestSchema.safeParse(manifestParsed);
  if (!result.success) {
    throw new Error(`Invalid bundle manifest: ${result.error.message}`);
  }

  return result.data;
}

/**
 * Compute SHA-256 hash of content.
 *
 * @private
 * @param {Uint8Array|Buffer|string} content - Content to hash
 * @returns {string} Hex-encoded SHA-256 hash
 */
function hashContent(content) {
  const hash = createHash('sha256');
  hash.update(content);
  return hash.digest('hex');
}
