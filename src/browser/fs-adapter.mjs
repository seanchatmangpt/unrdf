/**
 * @fileoverview Unified file system adapter for Node.js and browser
 *
 * Provides a consistent fs API that works in both Node.js and browser environments.
 * Automatically selects the appropriate backend:
 * - Node.js: native fs module
 * - Browser: IndexedDB-based file system
 *
 * @module browser/fs-adapter
 */

import { isBrowser } from './browser-shim.mjs';

let fsBackend = null;
let pathBackend = null;

/**
 * Initialize file system backend
 * @private
 */
async function initBackend() {
  if (fsBackend) return;

  if (isBrowser()) {
    const { fs, path } = await import('./browser-shim.mjs');
    fsBackend = fs;
    pathBackend = path;
  } else {
    const fs = await import('fs');
    const path = await import('path');
    fsBackend = fs;
    pathBackend = path;
  }
}

/**
 * Unified file system adapter
 *
 * @example
 * import { createFsAdapter } from './browser/fs-adapter.mjs';
 *
 * const fs = await createFsAdapter();
 * await fs.writeFile('/data/graph.ttl', turtleData);
 * const data = await fs.readFile('/data/graph.ttl', 'utf8');
 */
export class FileSystemAdapter {
  constructor() {
    this.initialized = false;
  }

  /**
   * Ensure backend is initialized
   * @private
   */
  async ensureInit() {
    if (!this.initialized) {
      await initBackend();
      this.initialized = true;
    }
  }

  /**
   * Read file
   * @param {string} filePath - File path
   * @param {string|Object} [options] - Encoding or options
   * @returns {Promise<string|Buffer>} File contents
   */
  async readFile(filePath, options) {
    await this.ensureInit();
    return fsBackend.promises.readFile(filePath, options);
  }

  /**
   * Write file
   * @param {string} filePath - File path
   * @param {string|Buffer|Uint8Array} data - File contents
   * @param {string|Object} [options] - Encoding or options
   * @returns {Promise<void>}
   */
  async writeFile(filePath, data, options) {
    await this.ensureInit();
    return fsBackend.promises.writeFile(filePath, data, options);
  }

  /**
   * Create directory
   * @param {string} dirPath - Directory path
   * @param {Object} [options] - Options
   * @returns {Promise<void>}
   */
  async mkdir(dirPath, options) {
    await this.ensureInit();
    return fsBackend.promises.mkdir(dirPath, options);
  }

  /**
   * Read directory
   * @param {string} dirPath - Directory path
   * @param {Object} [options] - Options
   * @returns {Promise<string[]>} Directory entries
   */
  async readdir(dirPath, options) {
    await this.ensureInit();
    return fsBackend.promises.readdir(dirPath, options);
  }

  /**
   * Get file stats
   * @param {string} filePath - File path
   * @returns {Promise<Object>} File stats
   */
  async stat(filePath) {
    await this.ensureInit();
    return fsBackend.promises.stat(filePath);
  }

  /**
   * Check if file exists
   * @param {string} filePath - File path
   * @returns {Promise<boolean>} True if exists
   */
  async exists(filePath) {
    await this.ensureInit();

    if (fsBackend.promises.exists) {
      return fsBackend.promises.exists(filePath);
    }

    // Fallback using stat
    try {
      await this.stat(filePath);
      return true;
    } catch (err) {
      return false;
    }
  }

  /**
   * Delete file
   * @param {string} filePath - File path
   * @returns {Promise<void>}
   */
  async unlink(filePath) {
    await this.ensureInit();
    return fsBackend.promises.unlink(filePath);
  }

  /**
   * Remove directory
   * @param {string} dirPath - Directory path
   * @param {Object} [options] - Options
   * @returns {Promise<void>}
   */
  async rmdir(dirPath, options) {
    await this.ensureInit();
    return fsBackend.promises.rmdir(dirPath, options);
  }

  /**
   * Rename/move file or directory
   * @param {string} oldPath - Old path
   * @param {string} newPath - New path
   * @returns {Promise<void>}
   */
  async rename(oldPath, newPath) {
    await this.ensureInit();
    return fsBackend.promises.rename(oldPath, newPath);
  }

  /**
   * Get path utilities
   * @returns {Object} Path utilities
   */
  getPath() {
    return pathBackend;
  }
}

/**
 * Create unified file system adapter
 * @returns {Promise<FileSystemAdapter>} File system adapter
 */
export async function createFsAdapter() {
  const adapter = new FileSystemAdapter();
  await adapter.ensureInit();
  return adapter;
}

/**
 * Get path utilities (works in both Node.js and browser)
 * @returns {Promise<Object>} Path utilities
 */
export async function getPath() {
  await initBackend();
  return pathBackend;
}
