/**
 * @fileoverview Enhanced lockfile management with verification and bundle support.
 *
 * Core responsibilities:
 * - Load/save lockfile with package metadata
 * - Verify cache integrity (hash validation)
 * - Support bundle export/import for offline mode
 * - Coordinate with cache store
 *
 * Lockfile schema: See LOCKFILE-SCHEMA.md
 *
 * @module lib/latex/cache/lockfile
 */

import { readFile, writeFile, mkdir, access } from 'node:fs/promises';
import { createHash } from 'node:crypto';
import { dirname, join } from 'node:path';
import { z } from 'zod';

/**
 * Schema for a single lock entry (package/file).
 * @type {z.ZodSchema}
 */
export const LockEntrySchema = z.object({
  name: z.string().describe('Package or file name (e.g., hyperref.sty)'),
  hash: z.string().regex(/^[a-f0-9]{64}$/).describe('SHA-256 hash of content'),
  sourceUrl: z.string().url().optional().describe('Original source URL (CTAN, etc.)'),
  cachedPath: z.string().describe('Relative path in cache directory'),
  fetchedAt: z.number().int().positive().describe('Unix timestamp (ms) when fetched'),
  size: z.number().int().nonnegative().optional().describe('File size in bytes')
});

/**
 * Schema for complete lockfile.
 * @type {z.ZodSchema}
 */
export const LockfileSchema = z.object({
  version: z.literal('1.0.0').describe('Lockfile format version'),
  engine: z.enum(['xetex', 'pdftex', 'luatex']).describe('LaTeX engine'),
  entries: z.record(
    z.string().describe('Entry key (usually same as name)'),
    LockEntrySchema
  ).describe('Map of package name to lock entry'),
  createdAt: z.number().int().positive().describe('Unix timestamp (ms) when created'),
  updatedAt: z.number().int().positive().describe('Unix timestamp (ms) last updated')
});

/**
 * @typedef {z.infer<typeof LockEntrySchema>} LockEntry
 * @typedef {z.infer<typeof LockfileSchema>} Lockfile
 */

/**
 * Create a new empty lockfile.
 *
 * @param {string} engine - LaTeX engine (xetex, pdftex, luatex)
 * @returns {Lockfile} New lockfile object
 */
export function createLockfile(engine = 'xetex') {
  const now = Date.now();
  return {
    version: '1.0.0',
    engine,
    entries: {},
    createdAt: now,
    updatedAt: now
  };
}

/**
 * Load lockfile from disk.
 *
 * Returns null if file doesn't exist or is invalid.
 * Invalid lockfiles are treated as missing (fail-safe).
 *
 * @param {string} lockfilePath - Absolute path to lockfile
 * @returns {Promise<Lockfile|null>} Parsed and validated lockfile, or null
 */
export async function loadLockfile(lockfilePath) {
  try {
    const raw = await readFile(lockfilePath, 'utf-8');
    const parsed = JSON.parse(raw);

    // Validate schema
    const result = LockfileSchema.safeParse(parsed);
    if (!result.success) {
      console.warn(`Invalid lockfile at ${lockfilePath}:`, result.error.format());
      return null;
    }

    return result.data;
  } catch (err) {
    if (err.code === 'ENOENT') {
      return null; // Normal - file doesn't exist yet
    }

    console.warn(`Failed to load lockfile: ${err.message}`);
    return null;
  }
}

/**
 * Save lockfile to disk with stable formatting.
 *
 * Updates `updatedAt` timestamp automatically.
 * Creates parent directory if needed.
 * Sorts keys for deterministic diffs.
 *
 * @param {string} lockfilePath - Absolute path to lockfile
 * @param {Lockfile} lockfile - Lockfile object to save
 * @returns {Promise<void>}
 * @throws {Error} If validation fails or write fails
 */
export async function saveLockfile(lockfilePath, lockfile) {
  // Update timestamp
  const updated = {
    ...lockfile,
    updatedAt: Date.now()
  };

  // Validate before writing
  const result = LockfileSchema.safeParse(updated);
  if (!result.success) {
    throw new Error(`Invalid lockfile: ${result.error.message}`);
  }

  // Ensure parent directory exists
  await mkdir(dirname(lockfilePath), { recursive: true });

  // Write with sorted keys for stable diffs
  const json = JSON.stringify(result.data, sortedReplacer, 2);
  await writeFile(lockfilePath, json + '\n', 'utf-8');
}

/**
 * Add or update an entry in the lockfile.
 *
 * Mutates lockfile in-place.
 * Does NOT save to disk - caller must call saveLockfile().
 *
 * @param {Lockfile} lockfile - Lockfile object
 * @param {LockEntry} entry - Entry to add/update
 * @returns {void}
 */
export function addEntry(lockfile, entry) {
  // Validate entry
  const result = LockEntrySchema.safeParse(entry);
  if (!result.success) {
    throw new Error(`Invalid lock entry: ${result.error.message}`);
  }

  lockfile.entries[entry.name] = result.data;
}

/**
 * Verify that cached content matches lockfile hash.
 *
 * @param {Lockfile} lockfile - Lockfile object
 * @param {string} name - Entry name to verify
 * @param {Uint8Array|Buffer|string} content - Actual content to verify
 * @returns {boolean} True if hash matches, false otherwise
 */
export function verifyEntry(lockfile, name, content) {
  const entry = lockfile.entries[name];
  if (!entry) {
    return false; // Entry doesn't exist in lockfile
  }

  const actualHash = hashContent(content);
  return actualHash === entry.hash;
}

/**
 * Verify all entries in lockfile against cached files.
 *
 * Checks that:
 * 1. Cached file exists at expected path
 * 2. File hash matches lockfile entry
 *
 * @param {Lockfile} lockfile - Lockfile object
 * @param {string} cacheDir - Cache directory (absolute path)
 * @returns {Promise<{valid: string[], invalid: string[], missing: string[]}>}
 */
export async function verifyAllEntries(lockfile, cacheDir) {
  const valid = [];
  const invalid = [];
  const missing = [];

  for (const [name, entry] of Object.entries(lockfile.entries)) {
    const cachedPath = join(cacheDir, entry.cachedPath);

    try {
      // Check if file exists
      await access(cachedPath);

      // Read and verify hash
      const content = await readFile(cachedPath);
      const actualHash = hashContent(content);

      if (actualHash === entry.hash) {
        valid.push(name);
      } else {
        invalid.push(name);
      }
    } catch (err) {
      if (err.code === 'ENOENT') {
        missing.push(name);
      } else {
        throw err;
      }
    }
  }

  return { valid, invalid, missing };
}

/**
 * Get entry from lockfile by name.
 *
 * @param {Lockfile} lockfile - Lockfile object
 * @param {string} name - Entry name
 * @returns {LockEntry|null} Entry or null if not found
 */
export function getEntry(lockfile, name) {
  return lockfile.entries[name] || null;
}

/**
 * Remove entry from lockfile.
 *
 * Mutates lockfile in-place.
 *
 * @param {Lockfile} lockfile - Lockfile object
 * @param {string} name - Entry name to remove
 * @returns {boolean} True if entry was removed, false if it didn't exist
 */
export function removeEntry(lockfile, name) {
  if (lockfile.entries[name]) {
    delete lockfile.entries[name];
    return true;
  }
  return false;
}

/**
 * List all entry names in lockfile.
 *
 * @param {Lockfile} lockfile - Lockfile object
 * @returns {string[]} Array of entry names (sorted)
 */
export function listEntries(lockfile) {
  return Object.keys(lockfile.entries).sort();
}

/**
 * Compute SHA-256 hash of content.
 *
 * Accepts Uint8Array, Buffer, or string.
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

/**
 * JSON replacer that sorts object keys for deterministic output.
 *
 * @private
 * @param {string} key - Object key
 * @param {any} value - Value to serialize
 * @returns {any} Sorted object or original value
 */
function sortedReplacer(key, value) {
  if (value && typeof value === 'object' && !Array.isArray(value)) {
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
 * Merge two lockfiles.
 *
 * Prefers entries from lockfile2 on conflict.
 * Engines must match.
 *
 * @param {Lockfile} lockfile1 - First lockfile
 * @param {Lockfile} lockfile2 - Second lockfile
 * @returns {Lockfile} Merged lockfile
 * @throws {Error} If engines don't match
 */
export function mergeLockfiles(lockfile1, lockfile2) {
  if (lockfile1.engine !== lockfile2.engine) {
    throw new Error(
      `Cannot merge lockfiles with different engines: ${lockfile1.engine} vs ${lockfile2.engine}`
    );
  }

  const merged = createLockfile(lockfile1.engine);
  merged.createdAt = Math.min(lockfile1.createdAt, lockfile2.createdAt);

  // Merge entries (lockfile2 wins on conflict)
  merged.entries = {
    ...lockfile1.entries,
    ...lockfile2.entries
  };

  return merged;
}

/**
 * Prune lockfile to only include specified entries.
 *
 * @param {Lockfile} lockfile - Lockfile to prune
 * @param {Set<string>|string[]} validNames - Entry names to keep
 * @returns {Lockfile} New lockfile with only valid entries
 */
export function pruneLockfile(lockfile, validNames) {
  const validSet = validNames instanceof Set ? validNames : new Set(validNames);

  const pruned = {
    ...lockfile,
    entries: {}
  };

  for (const [name, entry] of Object.entries(lockfile.entries)) {
    if (validSet.has(name)) {
      pruned.entries[name] = entry;
    }
  }

  return pruned;
}

/**
 * Generate lock entry from content.
 *
 * Helper for creating lock entries when caching new files.
 *
 * @param {Object} params - Entry parameters
 * @param {string} params.name - Package/file name
 * @param {Uint8Array|Buffer|string} params.content - File content
 * @param {string} params.cachedPath - Relative path in cache
 * @param {string} [params.sourceUrl] - Source URL (optional)
 * @returns {LockEntry} Lock entry
 */
export function createLockEntry({ name, content, cachedPath, sourceUrl }) {
  const hash = hashContent(content);
  const size = content.length;
  const fetchedAt = Date.now();

  return {
    name,
    hash,
    cachedPath,
    fetchedAt,
    size,
    ...(sourceUrl && { sourceUrl })
  };
}
