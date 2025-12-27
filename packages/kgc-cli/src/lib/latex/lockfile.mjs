/**
 * Lockfile Manager (Agent 5)
 * Tracks resolved dependencies and compilation state
 *
 * @module lib/latex/lockfile
 */

import { promises as fs } from 'node:fs';
import { join } from 'node:path';

/**
 * Lockfile structure
 * @typedef {object} Lockfile
 * @property {string} version - Lockfile format version
 * @property {object} dependencies - Resolved dependencies (name -> { version, hash, path })
 * @property {object} [lastCompilation] - Last successful compilation metadata
 */

const LOCKFILE_NAME = '.latex-lock.json';

/**
 * Load lockfile from project directory
 * @param {string} projectDir - Project directory path
 * @returns {Promise<Lockfile | null>} Lockfile object or null if not found
 */
export async function loadLockfile(projectDir) {
  const lockfilePath = join(projectDir, LOCKFILE_NAME);

  try {
    const content = await fs.readFile(lockfilePath, 'utf-8');
    return JSON.parse(content);
  } catch (error) {
    if (error.code === 'ENOENT') {
      return null; // Lockfile doesn't exist
    }
    throw error;
  }
}

/**
 * Create new empty lockfile
 * @returns {Lockfile} Empty lockfile
 */
export function createLockfile() {
  return {
    version: '1.0.0',
    dependencies: {},
    lastCompilation: null,
  };
}

/**
 * Update lockfile with newly resolved inputs
 * @param {Lockfile} lockfile - Lockfile object
 * @param {Map<string, Uint8Array>} resolvedInputs - Resolved files
 */
export function updateLockfileWithResolved(lockfile, resolvedInputs) {
  // TODO (Agent 5): Implement detailed dependency tracking
  // For now, just count resolved inputs
  for (const [path, content] of resolvedInputs) {
    const name = path.split('/').pop(); // Extract filename
    lockfile.dependencies[name] = {
      path,
      size: content.length,
      timestamp: new Date().toISOString(),
    };
  }
}

/**
 * Update lockfile with successful compilation info
 * @param {Lockfile} lockfile - Lockfile object
 * @param {object} compileInfo - Compilation metadata
 */
export function updateLockfileWithSuccess(lockfile, compileInfo) {
  lockfile.lastCompilation = {
    ...compileInfo,
    success: true,
  };
}

/**
 * Save lockfile to project directory
 * @param {Lockfile} lockfile - Lockfile object
 * @param {string} projectDir - Project directory path
 * @returns {Promise<void>}
 */
export async function saveLockfile(lockfile, projectDir) {
  const lockfilePath = join(projectDir, LOCKFILE_NAME);

  // Deterministic JSON serialization (sorted keys)
  const json = JSON.stringify(lockfile, Object.keys(lockfile).sort(), 2);

  await fs.writeFile(lockfilePath, json, 'utf-8');
}
