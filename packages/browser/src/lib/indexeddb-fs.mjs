/**
 * @fileoverview IndexedDB-based file system for browser
 *
 * Implements a virtual file system using IndexedDB to store files and directories.
 * Supports POSIX-like operations: readFile, writeFile, mkdir, readdir, stat, etc.
 *
 * @module browser/indexeddb-fs
 */

/* global indexedDB */

const DB_NAME = 'unrdf-fs';
const DB_VERSION = 1;
const FILES_STORE = 'files';
const METADATA_STORE = 'metadata';

/**
 * IndexedDB-based file system implementation
 */
export class IndexedDBFileSystem {
  /**
   *
   */
  constructor() {
    this.db = null;
    this.initPromise = this.init();
  }

  /**
   * Initialize IndexedDB database
   * @private
   * @returns {Promise<void>}
   */
  async init() {
    if (this.db) return;

    return new Promise((resolve, reject) => {
      const request = indexedDB.open(DB_NAME, DB_VERSION);

      request.onerror = () => reject(request.error);
      request.onsuccess = () => {
        this.db = request.result;
        resolve();
      };

      request.onupgradeneeded = event => {
        const db = event.target.result;

        // Create files object store
        if (!db.objectStoreNames.contains(FILES_STORE)) {
          db.createObjectStore(FILES_STORE, { keyPath: 'path' });
        }

        // Create metadata object store
        if (!db.objectStoreNames.contains(METADATA_STORE)) {
          const metaStore = db.createObjectStore(METADATA_STORE, {
            keyPath: 'path',
          });
          metaStore.createIndex('parentPath', 'parentPath', { unique: false });
        }
      };
    });
  }

  /**
   * Ensure database is initialized
   * @private
   */
  async ensureInit() {
    await this.initPromise;
  }

  /**
   * Normalize path (remove trailing slashes, resolve ..)
   * @private
   * @param {string} filePath - Path to normalize
   * @returns {string} Normalized path
   */
  normalizePath(filePath) {
    // Remove trailing slashes
    let normalized = filePath.replace(/\/+$/, '');

    // Handle empty path
    if (!normalized || normalized === '/') {
      return '/';
    }

    // Ensure leading slash
    if (!normalized.startsWith('/')) {
      normalized = '/' + normalized;
    }

    // Resolve .. and .
    const parts = normalized.split('/').filter(Boolean);
    const resolved = [];

    for (const part of parts) {
      if (part === '..') {
        resolved.pop();
      } else if (part !== '.') {
        resolved.push(part);
      }
    }

    return '/' + resolved.join('/');
  }

  /**
   * Get parent directory path
   * @private
   * @param {string} filePath - File path
   * @returns {string} Parent directory path
   */
  getParentPath(filePath) {
    const normalized = this.normalizePath(filePath);
    if (normalized === '/') return null;

    const parts = normalized.split('/').filter(Boolean);
    parts.pop();

    return parts.length === 0 ? '/' : '/' + parts.join('/');
  }

  /**
   * Read file from IndexedDB
   * @param {string} filePath - File path
   * @param {string} [encoding='utf8'] - File encoding
   * @returns {Promise<string|Uint8Array>} File contents
   */
  async readFile(filePath, encoding = 'utf8') {
    await this.ensureInit();
    const normalized = this.normalizePath(filePath);

    return new Promise((resolve, reject) => {
      const transaction = this.db.transaction([FILES_STORE], 'readonly');
      const store = transaction.objectStore(FILES_STORE);
      const request = store.get(normalized);

      request.onsuccess = () => {
        const record = request.result;
        if (!record) {
          reject(new Error(`ENOENT: no such file or directory, open '${filePath}'`));
          return;
        }

        if (encoding === 'utf8' || encoding === 'utf-8') {
          resolve(record.data);
        } else if (!encoding) {
          // Return as Uint8Array
          const encoder = new TextEncoder();
          resolve(encoder.encode(record.data));
        } else {
          resolve(record.data);
        }
      };

      request.onerror = () => reject(request.error);
    });
  }

  /**
   * Write file to IndexedDB
   * @param {string} filePath - File path
   * @param {string|Uint8Array} data - File contents
   * @param {string} [encoding='utf8'] - File encoding
   * @returns {Promise<void>}
   */
  async writeFile(filePath, data, _encoding = 'utf8') {
    await this.ensureInit();
    const normalized = this.normalizePath(filePath);
    const parentPath = this.getParentPath(normalized);

    // Ensure parent directory exists
    if (parentPath && parentPath !== '/') {
      await this.mkdir(parentPath, { recursive: true });
    }

    // Convert Uint8Array to string if needed
    let fileData = data;
    if (data instanceof Uint8Array) {
      const decoder = new TextDecoder();
      fileData = decoder.decode(data);
    }

    return new Promise((resolve, reject) => {
      const transaction = this.db.transaction([FILES_STORE, METADATA_STORE], 'readwrite');

      // Store file content
      const filesStore = transaction.objectStore(FILES_STORE);
      filesStore.put({
        path: normalized,
        data: fileData,
        timestamp: Date.now(),
      });

      // Store metadata
      const metaStore = transaction.objectStore(METADATA_STORE);
      metaStore.put({
        path: normalized,
        parentPath: parentPath || '/',
        type: 'file',
        size: fileData.length,
        mtime: Date.now(),
        ctime: Date.now(),
      });

      transaction.oncomplete = () => resolve();
      transaction.onerror = () => reject(transaction.error);
    });
  }

  /**
   * Create directory
   * @param {string} dirPath - Directory path
   * @param {Object} [options] - Options
   * @param {boolean} [options.recursive=false] - Create parent directories
   * @returns {Promise<void>}
   */
  async mkdir(dirPath, options = {}) {
    await this.ensureInit();
    const normalized = this.normalizePath(dirPath);

    // Check if already exists
    const exists = await this.exists(normalized);
    if (exists) {
      const stats = await this.stat(normalized);
      if (stats.isDirectory()) {
        return; // Already exists, no-op
      } else {
        throw new Error(`EEXIST: file already exists, mkdir '${dirPath}'`);
      }
    }

    const parentPath = this.getParentPath(normalized);

    // Handle recursive option
    if (options.recursive && parentPath && parentPath !== '/') {
      await this.mkdir(parentPath, options);
    } else if (parentPath && parentPath !== '/') {
      // Check parent exists
      const parentExists = await this.exists(parentPath);
      if (!parentExists) {
        throw new Error(`ENOENT: no such file or directory, mkdir '${dirPath}'`);
      }
    }

    return new Promise((resolve, reject) => {
      const transaction = this.db.transaction([METADATA_STORE], 'readwrite');
      const store = transaction.objectStore(METADATA_STORE);

      store.put({
        path: normalized,
        parentPath: parentPath || '/',
        type: 'directory',
        size: 0,
        mtime: Date.now(),
        ctime: Date.now(),
      });

      transaction.oncomplete = () => resolve();
      transaction.onerror = () => reject(transaction.error);
    });
  }

  /**
   * Read directory
   * @param {string} dirPath - Directory path
   * @returns {Promise<string[]>} Directory entries
   */
  async readdir(dirPath) {
    await this.ensureInit();
    const normalized = this.normalizePath(dirPath);

    // Check if directory exists
    const exists = await this.exists(normalized);
    if (!exists) {
      throw new Error(`ENOENT: no such file or directory, scandir '${dirPath}'`);
    }

    const stats = await this.stat(normalized);
    if (!stats.isDirectory()) {
      throw new Error(`ENOTDIR: not a directory, scandir '${dirPath}'`);
    }

    return new Promise((resolve, reject) => {
      const transaction = this.db.transaction([METADATA_STORE], 'readonly');
      const store = transaction.objectStore(METADATA_STORE);
      const index = store.index('parentPath');
      const request = index.getAll(normalized);

      request.onsuccess = () => {
        const entries = request.result.map(entry => {
          const parts = entry.path.split('/').filter(Boolean);
          return parts[parts.length - 1];
        });
        resolve(entries);
      };

      request.onerror = () => reject(request.error);
    });
  }

  /**
   * Get file/directory stats
   * @param {string} filePath - Path to stat
   * @returns {Promise<Object>} File stats
   */
  async stat(filePath) {
    await this.ensureInit();
    const normalized = this.normalizePath(filePath);

    // Root directory special case
    if (normalized === '/') {
      return {
        isFile: () => false,
        isDirectory: () => true,
        size: 0,
        mtime: new Date(),
        ctime: new Date(),
      };
    }

    return new Promise((resolve, reject) => {
      const transaction = this.db.transaction([METADATA_STORE], 'readonly');
      const store = transaction.objectStore(METADATA_STORE);
      const request = store.get(normalized);

      request.onsuccess = () => {
        const metadata = request.result;
        if (!metadata) {
          reject(new Error(`ENOENT: no such file or directory, stat '${filePath}'`));
          return;
        }

        resolve({
          isFile: () => metadata.type === 'file',
          isDirectory: () => metadata.type === 'directory',
          size: metadata.size || 0,
          mtime: new Date(metadata.mtime),
          ctime: new Date(metadata.ctime),
        });
      };

      request.onerror = () => reject(request.error);
    });
  }

  /**
   * Check if file/directory exists
   * @param {string} filePath - Path to check
   * @returns {Promise<boolean>} True if exists
   */
  async exists(filePath) {
    try {
      await this.stat(filePath);
      return true;
    } catch (err) {
      if (err.message.includes('ENOENT')) {
        return false;
      }
      throw err;
    }
  }

  /**
   * Delete file
   * @param {string} filePath - File path
   * @returns {Promise<void>}
   */
  async unlink(filePath) {
    await this.ensureInit();
    const normalized = this.normalizePath(filePath);

    const stats = await this.stat(normalized);
    if (stats.isDirectory()) {
      throw new Error(`EISDIR: illegal operation on a directory, unlink '${filePath}'`);
    }

    return new Promise((resolve, reject) => {
      const transaction = this.db.transaction([FILES_STORE, METADATA_STORE], 'readwrite');

      transaction.objectStore(FILES_STORE).delete(normalized);
      transaction.objectStore(METADATA_STORE).delete(normalized);

      transaction.oncomplete = () => resolve();
      transaction.onerror = () => reject(transaction.error);
    });
  }

  /**
   * Remove directory
   * @param {string} dirPath - Directory path
   * @param {Object} [options] - Options
   * @param {boolean} [options.recursive=false] - Remove recursively
   * @returns {Promise<void>}
   */
  async rmdir(dirPath, options = {}) {
    await this.ensureInit();
    const normalized = this.normalizePath(dirPath);

    const stats = await this.stat(normalized);
    if (!stats.isDirectory()) {
      throw new Error(`ENOTDIR: not a directory, rmdir '${dirPath}'`);
    }

    const entries = await this.readdir(normalized);

    if (entries.length > 0 && !options.recursive) {
      throw new Error(`ENOTEMPTY: directory not empty, rmdir '${dirPath}'`);
    }

    // Remove children recursively if needed
    if (options.recursive && entries.length > 0) {
      for (const entry of entries) {
        const childPath = normalized + '/' + entry;
        const childStats = await this.stat(childPath);

        if (childStats.isDirectory()) {
          await this.rmdir(childPath, options);
        } else {
          await this.unlink(childPath);
        }
      }
    }

    return new Promise((resolve, reject) => {
      const transaction = this.db.transaction([METADATA_STORE], 'readwrite');
      transaction.objectStore(METADATA_STORE).delete(normalized);

      transaction.oncomplete = () => resolve();
      transaction.onerror = () => reject(transaction.error);
    });
  }

  /**
   * Clear all data from IndexedDB
   * @returns {Promise<void>}
   */
  async clear() {
    await this.ensureInit();

    return new Promise((resolve, reject) => {
      const transaction = this.db.transaction([FILES_STORE, METADATA_STORE], 'readwrite');

      transaction.objectStore(FILES_STORE).clear();
      transaction.objectStore(METADATA_STORE).clear();

      transaction.oncomplete = () => resolve();
      transaction.onerror = () => reject(transaction.error);
    });
  }
}
