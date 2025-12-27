/**
 * @fileoverview LaTeX lockfile for deterministic builds across machines.
 *
 * Core responsibilities:
 * - Load/save lockfile with resolved input metadata
 * - Record resolved inputs (hash, source URL, cached path)
 * - Enable cache validation and reuse
 * - Prevent unnecessary network calls
 *
 * Lockfile location: `${cacheDir}/latex.lock.json`
 *
 * Invariants:
 * - Lockfile format is stable (sorted keys) for git diffs
 * - Hash mismatches MUST invalidate cache entry
 * - Timestamps update on any modification
 * - Multiple builds yield identical lockfile (except timestamps)
 */

import { readFile, writeFile, mkdir } from 'node:fs/promises';
import { dirname } from 'node:path';
import { z } from 'zod';

/**
 * Schema for a single resolved input entry.
 * @type {z.ZodSchema}
 */
export const ResolvedInputSchema = z.object({
  hash: z.string().describe('SHA-256 hash of file content'),
  sourceUrl: z.string().optional().describe('Original source URL (if fetched)'),
  cachedPath: z.string().describe('Absolute path to cached file'),
  resolvedAt: z.string().describe('ISO timestamp when resolved')
});

/**
 * Schema for the complete lockfile.
 * @type {z.ZodSchema}
 */
export const LatexLockSchema = z.object({
  version: z.literal('1.0.0').describe('Lockfile schema version'),
  engine: z.enum(['xetex', 'pdftex', 'luatex']).describe('LaTeX engine'),
  resolvedInputs: z.record(
    z.string().describe('Input name (e.g., dissertation.tex, logo.png)'),
    ResolvedInputSchema
  ).describe('Map of input name to resolved metadata'),
  createdAt: z.string().describe('ISO timestamp when lockfile created'),
  updatedAt: z.string().describe('ISO timestamp of last update')
});

/**
 * Load existing lockfile from disk.
 *
 * Returns null if lockfile doesn't exist or is invalid.
 * Invalid lockfiles are treated as missing (fail-safe).
 *
 * @param {string} lockPath - Absolute path to latex.lock.json
 * @returns {Promise<Object|null>} Parsed lockfile or null
 */
export async function loadLatexLock(lockPath) {
  try {
    const raw = await readFile(lockPath, 'utf-8');
    const parsed = JSON.parse(raw);

    // Validate against schema
    const validation = LatexLockSchema.safeParse(parsed);
    if (!validation.success) {
      console.warn(`Invalid lockfile at ${lockPath}: ${validation.error.message}`);
      return null;
    }

    return validation.data;
  } catch (err) {
    if (err.code === 'ENOENT') {
      // File doesn't exist - normal for first build
      return null;
    }

    // Other errors (parse failures, permissions) - treat as missing
    console.warn(`Failed to load lockfile at ${lockPath}: ${err.message}`);
    return null;
  }
}

/**
 * Save lockfile to disk atomically.
 *
 * Updates `updatedAt` timestamp automatically.
 * Creates parent directory if needed.
 * Formats JSON with sorted keys for stable diffs.
 *
 * @param {string} lockPath - Absolute path to latex.lock.json
 * @param {Object} lockObj - Lockfile object (will be validated)
 * @returns {Promise<void>}
 * @throws {Error} If lockObj invalid or write fails
 */
export async function saveLatexLock(lockPath, lockObj) {
  // Update timestamp
  const updated = {
    ...lockObj,
    updatedAt: new Date().toISOString()
  };

  // Validate before writing
  const validation = LatexLockSchema.safeParse(updated);
  if (!validation.success) {
    throw new Error(`Invalid lockfile object: ${validation.error.message}`);
  }

  // Ensure parent directory exists
  const dir = dirname(lockPath);
  await mkdir(dir, { recursive: true });

  // Write with stable formatting (sorted keys, 2-space indent)
  const json = JSON.stringify(validation.data, sortedReplacer, 2);
  await writeFile(lockPath, json + '\n', 'utf-8');
}

/**
 * Record a newly resolved input in the lockfile.
 *
 * Mutates lockObj in-place (functional-style would be overkill here).
 * Does NOT save to disk - caller must call saveLatexLock().
 *
 * @param {Object} lockObj - Lockfile object (must have resolvedInputs)
 * @param {Object} entry - Resolved input metadata
 * @param {string} entry.inputName - Name of input file
 * @param {string} entry.hash - SHA-256 hash of content
 * @param {string} [entry.sourceUrl] - Original URL if fetched
 * @param {string} entry.cachedPath - Absolute path to cached file
 * @returns {void}
 */
export function recordResolvedInput(lockObj, { inputName, hash, sourceUrl, cachedPath }) {
  if (!lockObj.resolvedInputs) {
    lockObj.resolvedInputs = {};
  }

  lockObj.resolvedInputs[inputName] = {
    hash,
    ...(sourceUrl && { sourceUrl }),
    cachedPath,
    resolvedAt: new Date().toISOString()
  };
}

/**
 * Create a new empty lockfile object.
 *
 * @param {string} engine - LaTeX engine (xetex, pdftex, luatex)
 * @returns {Object} New lockfile ready to populate
 */
export function createLatexLock(engine = 'xetex') {
  const now = new Date().toISOString();
  return {
    version: '1.0.0',
    engine,
    resolvedInputs: {},
    createdAt: now,
    updatedAt: now
  };
}

/**
 * Validate that a cached file matches the lockfile hash.
 *
 * Used by resolver to ensure cache integrity before reusing files.
 *
 * @param {Object} lockEntry - Entry from lockfile.resolvedInputs
 * @param {string} actualHash - Hash of file currently on disk
 * @returns {boolean} True if hashes match
 */
export function validateCachedFile(lockEntry, actualHash) {
  return lockEntry.hash === actualHash;
}

/**
 * Get resolved input from lockfile if available.
 *
 * @param {Object|null} lockObj - Lockfile object
 * @param {string} inputName - Name of input to look up
 * @returns {Object|null} Resolved input entry or null
 */
export function getResolvedInput(lockObj, inputName) {
  if (!lockObj || !lockObj.resolvedInputs) {
    return null;
  }

  return lockObj.resolvedInputs[inputName] || null;
}

/**
 * Check if lockfile is valid for a specific engine.
 *
 * @param {Object|null} lockObj - Lockfile object
 * @param {string} engine - Expected LaTeX engine
 * @returns {boolean} True if lockfile exists and matches engine
 */
export function isLockValid(lockObj, engine) {
  if (!lockObj) return false;
  return lockObj.engine === engine;
}

/**
 * JSON replacer that sorts object keys for stable output.
 * Arrays and primitives pass through unchanged.
 *
 * @private
 * @param {string} key - Object key
 * @param {any} value - Value to serialize
 * @returns {any} Sorted object or original value
 */
function sortedReplacer(key, value) {
  if (value && typeof value === 'object' && !Array.isArray(value)) {
    // Sort keys for deterministic output
    return Object.keys(value)
      .sort()
      .reduce((sorted, k) => {
        sorted[k] = value[k];
        return sorted;
      }, {});
  }
  return value;
}

/**
 * Merge two lockfiles (for multi-document builds).
 *
 * Takes union of resolvedInputs, preferring newer entries on conflict.
 * Engine must match across both lockfiles.
 *
 * @param {Object} lock1 - First lockfile
 * @param {Object} lock2 - Second lockfile
 * @returns {Object} Merged lockfile
 * @throws {Error} If engines don't match
 */
export function mergeLocks(lock1, lock2) {
  if (lock1.engine !== lock2.engine) {
    throw new Error(
      `Cannot merge lockfiles with different engines: ${lock1.engine} vs ${lock2.engine}`
    );
  }

  const merged = createLatexLock(lock1.engine);
  merged.createdAt = lock1.createdAt; // Preserve original creation time

  // Merge resolvedInputs, preferring newer
  for (const [name, entry] of Object.entries(lock1.resolvedInputs)) {
    merged.resolvedInputs[name] = entry;
  }

  for (const [name, entry] of Object.entries(lock2.resolvedInputs)) {
    const existing = merged.resolvedInputs[name];
    if (!existing || entry.resolvedAt > existing.resolvedAt) {
      merged.resolvedInputs[name] = entry;
    }
  }

  return merged;
}

/**
 * Prune lockfile entries that no longer exist in cache.
 *
 * Useful for cleanup after cache invalidation.
 *
 * @param {Object} lockObj - Lockfile to prune
 * @param {Set<string>} validInputs - Set of input names still in use
 * @returns {Object} New lockfile with only valid entries
 */
export function pruneLock(lockObj, validInputs) {
  const pruned = {
    ...lockObj,
    resolvedInputs: {}
  };

  for (const [name, entry] of Object.entries(lockObj.resolvedInputs)) {
    if (validInputs.has(name)) {
      pruned.resolvedInputs[name] = entry;
    }
  }

  return pruned;
}
