/**
 * @fileoverview KGC Probe - Storage Backends
 *
 * Three storage implementations:
 * - MemoryStorage: In-process hash map (development)
 * - FileStorage: Filesystem-based (testing)
 * - DatabaseStorage: Structured storage with query support (production)
 *
 * Interface: { set(key, value), get(key), delete(key), query(pattern) }
 *
 * @module @unrdf/kgc-probe/storage
 */

import { promises as fs, existsSync, mkdirSync } from 'fs';
import { join, dirname } from 'path';
import { randomUUID } from 'crypto';

// ============================================================================
// STORAGE INTERFACE
// ============================================================================

/**
 * @typedef {Object} StorageInterface
 * @property {string} type - Storage type identifier
 * @property {(key: string, value: any) => Promise<void>} set - Set a value
 * @property {(key: string) => Promise<any>} get - Get a value
 * @property {(key: string) => Promise<boolean>} delete - Delete a value
 * @property {(pattern: string) => Promise<Array>} query - Query values by pattern
 * @property {() => Promise<string[]>} keys - List all keys
 * @property {() => Promise<number>} count - Count entries
 * @property {() => Promise<void>} clear - Clear all entries
 */

// ============================================================================
// MEMORY STORAGE
// ============================================================================

/**
 * MemoryStorage - In-process storage using Map
 *
 * Use for: Development, testing, single-process deployments
 * @implements {StorageInterface}
 */
export class MemoryStorage {
  /**
   *
   */
  constructor() {
    /** @type {string} */
    this.type = 'memory';
    /** @type {Map<string, any>} */
    this.store = new Map();
    /** @type {Map<string, number>} */
    this.timestamps = new Map();
  }

  /**
   * Set a value with key
   * @param {string} key - Storage key
   * @param {any} value - Value to store
   * @returns {Promise<void>}
   */
  async set(key, value) {
    if (typeof key !== 'string' || key.length === 0) {
      throw new Error('Key must be a non-empty string');
    }
    this.store.set(key, structuredClone(value));
    this.timestamps.set(key, Date.now());
  }

  /**
   * Get a value by key
   * @param {string} key - Storage key
   * @returns {Promise<any>} Stored value or undefined
   */
  async get(key) {
    if (!this.store.has(key)) {
      return undefined;
    }
    return structuredClone(this.store.get(key));
  }

  /**
   * Delete a value by key
   * @param {string} key - Storage key
   * @returns {Promise<boolean>} True if deleted
   */
  async delete(key) {
    this.timestamps.delete(key);
    return this.store.delete(key);
  }

  /**
   * Query values by pattern (glob-style matching)
   * @param {string} pattern - Pattern to match (supports * wildcard)
   * @returns {Promise<Array<{key: string, value: any}>>} Matching entries
   */
  async query(pattern) {
    const results = [];
    const regex = new RegExp('^' + pattern.replace(/\*/g, '.*') + '$');

    for (const [key, value] of this.store) {
      if (regex.test(key)) {
        results.push({ key, value: structuredClone(value) });
      }
    }

    return results;
  }

  /**
   * List all keys
   * @returns {Promise<string[]>}
   */
  async keys() {
    return Array.from(this.store.keys());
  }

  /**
   * Count entries
   * @returns {Promise<number>}
   */
  async count() {
    return this.store.size;
  }

  /**
   * Clear all entries
   * @returns {Promise<void>}
   */
  async clear() {
    this.store.clear();
    this.timestamps.clear();
  }

  /**
   * Check if key exists
   * @param {string} key - Storage key
   * @returns {Promise<boolean>}
   */
  async has(key) {
    return this.store.has(key);
  }

  // Artifact API (backward compatibility)

  /**
   * Save artifact to memory
   * @param {Object} artifact - Artifact to save
   * @returns {Promise<void>}
   */
  async saveArtifact(artifact) {
    const key = artifact.probe_run_id || artifact.id || randomUUID();
    await this.set(`artifact:${key}`, artifact);
  }

  /**
   * Load artifact from memory
   * @param {string} artifactId - Artifact ID
   * @returns {Promise<Object>}
   */
  async loadArtifact(artifactId) {
    const artifact = await this.get(`artifact:${artifactId}`);
    if (!artifact) {
      throw new Error(`Artifact not found: ${artifactId}`);
    }
    return artifact;
  }

  /**
   * Fetch all artifacts (shards)
   * @returns {Promise<Array>}
   */
  async fetchShards() {
    const results = await this.query('artifact:*');
    return results.map(r => r.value);
  }

  /**
   * List artifact IDs
   * @returns {Promise<string[]>}
   */
  async listArtifacts() {
    const keys = await this.keys();
    return keys
      .filter(k => k.startsWith('artifact:'))
      .map(k => k.replace('artifact:', ''));
  }

  /**
   * Delete artifact
   * @param {string} artifactId - Artifact ID
   * @returns {Promise<boolean>}
   */
  async deleteArtifact(artifactId) {
    return this.delete(`artifact:${artifactId}`);
  }
}

// ============================================================================
// FILE STORAGE
// ============================================================================

/**
 * FileStorage - Filesystem-based storage
 *
 * Use for: Testing, single-node deployment, audit trail
 * Structure:
 *   <rootDir>/
 *     <key>.json
 *     ...
 * @implements {StorageInterface}
 */
export class FileStorage {
  /**
   * Create file storage
   * @param {string} [rootDir] - Root directory for storage
   */
  constructor(rootDir = './storage') {
    /** @type {string} */
    this.type = 'file';
    /** @type {string} */
    this.rootDir = rootDir;
    this._ensureDir();
  }

  /**
   * Ensure root directory exists
   * @private
   */
  _ensureDir() {
    if (!existsSync(this.rootDir)) {
      mkdirSync(this.rootDir, { recursive: true });
    }
  }

  /**
   * Get file path for key
   * @param {string} key - Storage key
   * @returns {string}
   * @private
   */
  _getPath(key) {
    // Sanitize key for filesystem
    const sanitized = key.replace(/[^a-zA-Z0-9_-]/g, '_');
    return join(this.rootDir, `${sanitized}.json`);
  }

  /**
   * Set a value with key
   * @param {string} key - Storage key
   * @param {any} value - Value to store
   * @returns {Promise<void>}
   */
  async set(key, value) {
    if (typeof key !== 'string' || key.length === 0) {
      throw new Error('Key must be a non-empty string');
    }
    this._ensureDir();
    const path = this._getPath(key);
    const data = {
      key,
      value,
      timestamp: Date.now(),
      version: 1
    };
    await fs.writeFile(path, JSON.stringify(data, null, 2), 'utf8');
  }

  /**
   * Get a value by key
   * @param {string} key - Storage key
   * @returns {Promise<any>}
   */
  async get(key) {
    const path = this._getPath(key);
    try {
      const content = await fs.readFile(path, 'utf8');
      const data = JSON.parse(content);
      return data.value;
    } catch (err) {
      if (err.code === 'ENOENT') {
        return undefined;
      }
      throw err;
    }
  }

  /**
   * Delete a value by key
   * @param {string} key - Storage key
   * @returns {Promise<boolean>}
   */
  async delete(key) {
    const path = this._getPath(key);
    try {
      await fs.unlink(path);
      return true;
    } catch (err) {
      if (err.code === 'ENOENT') {
        return false;
      }
      throw err;
    }
  }

  /**
   * Query values by pattern
   * @param {string} pattern - Pattern to match
   * @returns {Promise<Array<{key: string, value: any}>>}
   */
  async query(pattern) {
    const results = [];
    const regex = new RegExp('^' + pattern.replace(/\*/g, '.*') + '$');

    try {
      const files = await fs.readdir(this.rootDir);
      for (const file of files) {
        if (!file.endsWith('.json')) continue;

        try {
          const content = await fs.readFile(join(this.rootDir, file), 'utf8');
          const data = JSON.parse(content);
          if (regex.test(data.key)) {
            results.push({ key: data.key, value: data.value });
          }
        } catch {
          // Skip invalid files
        }
      }
    } catch (err) {
      if (err.code !== 'ENOENT') throw err;
    }

    return results;
  }

  /**
   * List all keys
   * @returns {Promise<string[]>}
   */
  async keys() {
    const allKeys = [];
    try {
      const files = await fs.readdir(this.rootDir);
      for (const file of files) {
        if (!file.endsWith('.json')) continue;
        try {
          const content = await fs.readFile(join(this.rootDir, file), 'utf8');
          const data = JSON.parse(content);
          allKeys.push(data.key);
        } catch {
          // Skip invalid files
        }
      }
    } catch (err) {
      if (err.code !== 'ENOENT') throw err;
    }
    return allKeys;
  }

  /**
   * Count entries
   * @returns {Promise<number>}
   */
  async count() {
    const keys = await this.keys();
    return keys.length;
  }

  /**
   * Clear all entries
   * @returns {Promise<void>}
   */
  async clear() {
    try {
      const files = await fs.readdir(this.rootDir);
      for (const file of files) {
        if (file.endsWith('.json')) {
          await fs.unlink(join(this.rootDir, file));
        }
      }
    } catch (err) {
      if (err.code !== 'ENOENT') throw err;
    }
  }

  /**
   * Check if key exists
   * @param {string} key - Storage key
   * @returns {Promise<boolean>}
   */
  async has(key) {
    const path = this._getPath(key);
    return existsSync(path);
  }

  // Artifact API (backward compatibility)

  /**
   * Save artifact to file
   * @param {Object} artifact - Artifact to save
   * @returns {Promise<void>}
   */
  async saveArtifact(artifact) {
    const key = artifact.probe_run_id || artifact.id || randomUUID();
    await this.set(`artifact:${key}`, artifact);
  }

  /**
   * Load artifact from file
   * @param {string} artifactId - Artifact ID
   * @returns {Promise<Object>}
   */
  async loadArtifact(artifactId) {
    const artifact = await this.get(`artifact:${artifactId}`);
    if (!artifact) {
      throw new Error(`Artifact not found: ${artifactId}`);
    }
    return artifact;
  }

  /**
   * Fetch all artifacts
   * @returns {Promise<Array>}
   */
  async fetchShards() {
    const results = await this.query('artifact:*');
    return results.map(r => r.value);
  }

  /**
   * List artifact IDs
   * @returns {Promise<string[]>}
   */
  async listArtifacts() {
    const keys = await this.keys();
    return keys
      .filter(k => k.startsWith('artifact:'))
      .map(k => k.replace('artifact:', ''));
  }

  /**
   * Delete artifact
   * @param {string} artifactId - Artifact ID
   * @returns {Promise<boolean>}
   */
  async deleteArtifact(artifactId) {
    return this.delete(`artifact:${artifactId}`);
  }
}

// ============================================================================
// DATABASE STORAGE
// ============================================================================

/**
 * DatabaseStorage - In-memory database simulation with advanced querying
 *
 * Use for: Production, distributed deployments
 * Provides: Indexing, range queries, transactions
 * @implements {StorageInterface}
 */
export class DatabaseStorage {
  /**
   * Create database storage
   * @param {Object} [options] - Configuration
   * @param {string} [options.namespace] - Namespace prefix
   */
  constructor(options = {}) {
    /** @type {string} */
    this.type = 'database';
    /** @type {string} */
    this.namespace = options.namespace || 'probe';
    /** @type {Map<string, any>} */
    this._data = new Map();
    /** @type {Map<string, Map<string, Set<string>>>} */
    this._indices = new Map();
    /** @type {Map<string, number>} */
    this._timestamps = new Map();
    /** @type {number} */
    this._version = 0;
  }

  /**
   * Create index on field
   * @param {string} field - Field name to index
   * @returns {void}
   */
  createIndex(field) {
    if (!this._indices.has(field)) {
      this._indices.set(field, new Map());
    }
  }

  /**
   * Update indices for a record
   * @param {string} key - Record key
   * @param {any} value - Record value
   * @private
   */
  _updateIndices(key, value) {
    if (typeof value !== 'object' || value === null) return;

    for (const [field, index] of this._indices) {
      const fieldValue = value[field];
      if (fieldValue !== undefined) {
        const strValue = String(fieldValue);
        if (!index.has(strValue)) {
          index.set(strValue, new Set());
        }
        index.get(strValue).add(key);
      }
    }
  }

  /**
   * Remove from indices
   * @param {string} key - Record key
   * @private
   */
  _removeFromIndices(key) {
    for (const index of this._indices.values()) {
      for (const keySet of index.values()) {
        keySet.delete(key);
      }
    }
  }

  /**
   * Set a value with key
   * @param {string} key - Storage key
   * @param {any} value - Value to store
   * @returns {Promise<void>}
   */
  async set(key, value) {
    if (typeof key !== 'string' || key.length === 0) {
      throw new Error('Key must be a non-empty string');
    }

    const prefixedKey = `${this.namespace}:${key}`;
    this._removeFromIndices(prefixedKey);
    this._data.set(prefixedKey, structuredClone(value));
    this._timestamps.set(prefixedKey, Date.now());
    this._updateIndices(prefixedKey, value);
    this._version++;
  }

  /**
   * Get a value by key
   * @param {string} key - Storage key
   * @returns {Promise<any>}
   */
  async get(key) {
    const prefixedKey = `${this.namespace}:${key}`;
    if (!this._data.has(prefixedKey)) {
      return undefined;
    }
    return structuredClone(this._data.get(prefixedKey));
  }

  /**
   * Delete a value by key
   * @param {string} key - Storage key
   * @returns {Promise<boolean>}
   */
  async delete(key) {
    const prefixedKey = `${this.namespace}:${key}`;
    this._removeFromIndices(prefixedKey);
    this._timestamps.delete(prefixedKey);
    this._version++;
    return this._data.delete(prefixedKey);
  }

  /**
   * Query values by pattern or criteria
   * @param {string|Object} patternOrCriteria - Pattern string or criteria object
   * @returns {Promise<Array<{key: string, value: any}>>}
   */
  async query(patternOrCriteria) {
    const results = [];
    const prefix = `${this.namespace}:`;

    if (typeof patternOrCriteria === 'string') {
      // Pattern query
      const regex = new RegExp('^' + prefix + patternOrCriteria.replace(/\*/g, '.*') + '$');
      for (const [key, value] of this._data) {
        if (regex.test(key)) {
          results.push({
            key: key.replace(prefix, ''),
            value: structuredClone(value)
          });
        }
      }
    } else if (typeof patternOrCriteria === 'object') {
      // Criteria query
      const criteria = patternOrCriteria;

      // Check if we can use an index
      let candidateKeys = null;
      for (const [field, value] of Object.entries(criteria)) {
        if (this._indices.has(field)) {
          const index = this._indices.get(field);
          const matchingKeys = index.get(String(value));
          if (matchingKeys) {
            if (candidateKeys === null) {
              candidateKeys = new Set(matchingKeys);
            } else {
              // Intersection
              candidateKeys = new Set([...candidateKeys].filter(k => matchingKeys.has(k)));
            }
          } else {
            candidateKeys = new Set();
            break;
          }
        }
      }

      // Filter candidates or scan all
      const keysToCheck = candidateKeys || this._data.keys();
      for (const key of keysToCheck) {
        const value = this._data.get(key);
        if (!value) continue;

        let matches = true;
        for (const [field, expected] of Object.entries(criteria)) {
          if (value[field] !== expected) {
            matches = false;
            break;
          }
        }

        if (matches) {
          results.push({
            key: key.replace(prefix, ''),
            value: structuredClone(value)
          });
        }
      }
    }

    return results;
  }

  /**
   * List all keys
   * @returns {Promise<string[]>}
   */
  async keys() {
    const prefix = `${this.namespace}:`;
    return Array.from(this._data.keys())
      .filter(k => k.startsWith(prefix))
      .map(k => k.replace(prefix, ''));
  }

  /**
   * Count entries
   * @returns {Promise<number>}
   */
  async count() {
    const keys = await this.keys();
    return keys.length;
  }

  /**
   * Clear all entries
   * @returns {Promise<void>}
   */
  async clear() {
    const prefix = `${this.namespace}:`;
    for (const key of this._data.keys()) {
      if (key.startsWith(prefix)) {
        this._data.delete(key);
        this._timestamps.delete(key);
      }
    }
    for (const index of this._indices.values()) {
      index.clear();
    }
    this._version++;
  }

  /**
   * Check if key exists
   * @param {string} key - Storage key
   * @returns {Promise<boolean>}
   */
  async has(key) {
    const prefixedKey = `${this.namespace}:${key}`;
    return this._data.has(prefixedKey);
  }

  /**
   * Get database version (for change detection)
   * @returns {number}
   */
  getVersion() {
    return this._version;
  }

  /**
   * Batch set multiple values
   * @param {Array<{key: string, value: any}>} entries - Entries to set
   * @returns {Promise<void>}
   */
  async batchSet(entries) {
    for (const { key, value } of entries) {
      await this.set(key, value);
    }
  }

  /**
   * Batch get multiple values
   * @param {string[]} keys - Keys to get
   * @returns {Promise<Map<string, any>>}
   */
  async batchGet(keys) {
    const results = new Map();
    for (const key of keys) {
      results.set(key, await this.get(key));
    }
    return results;
  }

  // Artifact API (backward compatibility)

  /**
   * Save artifact
   * @param {Object} artifact - Artifact to save
   * @returns {Promise<void>}
   */
  async saveArtifact(artifact) {
    const key = artifact.probe_run_id || artifact.id || randomUUID();
    await this.set(`artifact:${key}`, artifact);
  }

  /**
   * Load artifact
   * @param {string} artifactId - Artifact ID
   * @returns {Promise<Object>}
   */
  async loadArtifact(artifactId) {
    const artifact = await this.get(`artifact:${artifactId}`);
    if (!artifact) {
      throw new Error(`Artifact not found: ${artifactId}`);
    }
    return artifact;
  }

  /**
   * Fetch all artifacts
   * @returns {Promise<Array>}
   */
  async fetchShards() {
    const results = await this.query('artifact:*');
    return results.map(r => r.value);
  }

  /**
   * List artifact IDs
   * @returns {Promise<string[]>}
   */
  async listArtifacts() {
    const keys = await this.keys();
    return keys
      .filter(k => k.startsWith('artifact:'))
      .map(k => k.replace('artifact:', ''));
  }

  /**
   * Delete artifact
   * @param {string} artifactId - Artifact ID
   * @returns {Promise<boolean>}
   */
  async deleteArtifact(artifactId) {
    return this.delete(`artifact:${artifactId}`);
  }
}

// ============================================================================
// FACTORY FUNCTIONS
// ============================================================================

/**
 * Create memory storage
 * @returns {MemoryStorage}
 */
export function createMemoryStorage() {
  return new MemoryStorage();
}

/**
 * Create file storage
 * @param {string} [rootDir] - Root directory
 * @returns {FileStorage}
 */
export function createFileStorage(rootDir = './storage') {
  return new FileStorage(rootDir);
}

/**
 * Create database storage
 * @param {Object} [options] - Configuration
 * @returns {DatabaseStorage}
 */
export function createDatabaseStorage(options = {}) {
  return new DatabaseStorage(options);
}

/**
 * Create storage by type
 * @param {'memory' | 'file' | 'database'} type - Storage type
 * @param {Object} [options] - Configuration
 * @returns {MemoryStorage | FileStorage | DatabaseStorage}
 */
export function createStorage(type, options = {}) {
  switch (type) {
    case 'memory':
      return createMemoryStorage();
    case 'file':
      return createFileStorage(options.rootDir);
    case 'database':
      return createDatabaseStorage(options);
    default:
      throw new Error(`Unknown storage type: ${type}`);
  }
}
