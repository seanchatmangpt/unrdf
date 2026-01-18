/**
 * VFS Collection (Agent 2)
 * Collects project files into virtual file system
 *
 * @module lib/latex/vfs
 */

import { promises as fs } from 'node:fs';
import { join, relative as _relative } from 'node:path';

/**
 * Collect all project files into VFS
 * @param {string} projectDir - Absolute path to project directory
 * @returns {Promise<Map<string, Uint8Array>>} VFS map (path -> content)
 */
export async function collectProjectFiles(projectDir) {
  // Stub implementation - recursive file collection pending
  const vfs = new Map();

  // Stub implementation: only collect .tex files in root
  try {
    const entries = await fs.readdir(projectDir, { withFileTypes: true });
    for (const entry of entries) {
      if (entry.isFile() && entry.name.endsWith('.tex')) {
        const fullPath = join(projectDir, entry.name);
        const content = await fs.readFile(fullPath);
        vfs.set(entry.name, new Uint8Array(content));
      }
    }
  } catch (error) {
    console.warn(`VFS collection warning: ${error.message}`);
  }

  return vfs;
}
