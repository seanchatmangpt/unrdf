/**
 * @fileoverview KGC Probe - Storage Backends
 *
 * Storage implementations:
 * - Memory: In-process hash map (development)
 * - File: Filesystem-based (testing)
 * - Database: KGC Substrate backed (production)
 *
 * @module @unrdf/kgc-probe/storage
 */

import { promises as fs } from 'fs';
import { join, dirname } from 'path';

/**
 * Storage interface definition
 * @typedef {Object} Storage
 * @property {string} type - Backend type
 * @property {Function} saveArtifact - Save artifact
 * @property {Function} loadArtifact - Load artifact
 * @property {Function} fetchShards - Fetch distributed shards
 * @property {Function} listArtifacts - List all artifacts
 * @property {Function} deleteArtifact - Delete artifact
 */

// ============================================================================
// MEMORY STORAGE
// ============================================================================

/**
 * MemoryStorage - In-process storage using Map
 *
 * Use for: Development, testing, single-process deployments
 */
export class MemoryStorage {
  constructor() {
    this.type = 'memory';
    this.store = new Map();
  }

  /**
   * Save artifact to memory
   * @param {Object} artifact - Artifact to save
   * @returns {Promise<void>}
   */
  async saveArtifact(artifact) {
    if (!artifact.probe_run_id) {
      throw new Error('Artifact must have probe_run_id');
    }
    this.store.set(artifact.probe_run_id, artifact);
  }

  /**
   * Load artifact from memory
   * @param {string} artifactId - Artifact ID
   * @returns {Promise<Object>} Artifact
   */
  async loadArtifact(artifactId) {
    const artifact = this.store.get(artifactId);
    if (!artifact) {
      throw new Error(`Artifact not found: ${artifactId}`);
    }
    return artifact;
  }

  /**
   * Fetch all shards (for merging)
   * @returns {Promise<Array>} All artifacts in store
   */
  async fetchShards() {
    return Array.from(this.store.values());
  }

  /**
   * List all artifact IDs
   * @returns {Promise<Array>} Array of artifact IDs
   */
  async listArtifacts() {
    return Array.from(this.store.keys());
  }

  /**
   * Get artifact count
   * @returns {Promise<number>}
   */
  async count() {
    return this.store.size;
  }

  /**
   * Delete artifact
   * @param {string} artifactId - Artifact ID
   * @returns {Promise<boolean>} True if deleted
   */
  async deleteArtifact(artifactId) {
    return this.store.delete(artifactId);
  }

  /**
   * Clear all artifacts
   * @returns {Promise<void>}
   */
  async clear() {
    this.store.clear();
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
 *     <probe_run_id>.json
 *     <probe_run_id>.json
 *     ...
 */
export class FileStorage {
  /**
   * Create file storage
   * @param {string} rootDir - Root directory for artifacts
   */
  constructor(rootDir = './artifacts') {
    this.type = 'file';
    this.rootDir = rootDir;
  }

  /**
   * Ensure root directory exists
   * @private
   */
  async ensureDir() {
    try {
      await fs.mkdir(this.rootDir, { recursive: true });
    } catch (err) {
      if (err.code !== 'EEXIST') throw err;
    }
  }

  /**
   * Get path for artifact
   * @param {string} artifactId - Artifact ID
   * @returns {string} File path
   * @private
   */
  getPath(artifactId) {
    return join(this.rootDir, `${artifactId}.json`);
  }

  /**
   * Save artifact to file
   * @param {Object} artifact - Artifact to save
   * @returns {Promise<void>}
   */
  async saveArtifact(artifact) {
    await this.ensureDir();

    if (!artifact.probe_run_id) {
      throw new Error('Artifact must have probe_run_id');
    }

    const path = this.getPath(artifact.probe_run_id);
    const json = JSON.stringify(artifact, null, 2);

    await fs.writeFile(path, json, 'utf8');
  }

  /**
   * Load artifact from file
   * @param {string} artifactId - Artifact ID
   * @returns {Promise<Object>} Artifact
   */
  async loadArtifact(artifactId) {
    const path = this.getPath(artifactId);

    try {
      const json = await fs.readFile(path, 'utf8');
      return JSON.parse(json);
    } catch (err) {
      if (err.code === 'ENOENT') {
        throw new Error(`Artifact not found: ${artifactId}`);
      }
      throw err;
    }
  }

  /**
   * Fetch all shards from files
   * @returns {Promise<Array>} All artifacts
   */
  async fetchShards() {
    await this.ensureDir();

    try {
      const files = await fs.readdir(this.rootDir);
      const jsonFiles = files.filter(f => f.endsWith('.json'));

      const artifacts = [];
      for (const file of jsonFiles) {
        try {
          const path = join(this.rootDir, file);
          const json = await fs.readFile(path, 'utf8');
          artifacts.push(JSON.parse(json));
        } catch (err) {
          console.error(`Failed to load artifact ${file}:`, err);
        }
      }

      return artifacts;
    } catch (err) {
      if (err.code === 'ENOENT') {
        return [];
      }
      throw err;
    }
  }

  /**
   * List all artifact IDs
   * @returns {Promise<Array>}
   */
  async listArtifacts() {
    try {
      const files = await fs.readdir(this.rootDir);
      return files
        .filter(f => f.endsWith('.json'))
        .map(f => f.replace('.json', ''));
    } catch (err) {
      if (err.code === 'ENOENT') {
        return [];
      }
      throw err;
    }
  }

  /**
   * Delete artifact
   * @param {string} artifactId - Artifact ID
   * @returns {Promise<boolean>}
   */
  async deleteArtifact(artifactId) {
    const path = this.getPath(artifactId);

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
}

// ============================================================================
// DATABASE STORAGE
// ============================================================================

/**
 * DatabaseStorage - KGC Substrate backed storage
 *
 * Use for: Production, distributed deployments
 * Stores artifacts as RDF quads in the knowledge store
 */
export class DatabaseStorage {
  /**
   * Create database storage
   * @param {Object} options - Configuration
   * @param {Object} [options.store] - KGC Substrate store
   * @param {string} [options.namespace] - RDF namespace
   */
  constructor(options = {}) {
    this.type = 'database';
    this.store = options.store;
    this.namespace = options.namespace || 'https://probe.unrdf.org/';

    if (!this.store) {
      throw new Error('DatabaseStorage requires store option');
    }
  }

  /**
   * Save artifact as RDF quads
   * @param {Object} artifact - Artifact to save
   * @returns {Promise<void>}
   */
  async saveArtifact(artifact) {
    if (!artifact.probe_run_id) {
      throw new Error('Artifact must have probe_run_id');
    }

    // Would convert artifact to RDF quads and add to store
    // This is a placeholder for the actual implementation
    // using @unrdf/oxigraph dataFactory

    // Example structure:
    // <artifact:id> rdf:type probe:Artifact
    // <artifact:id> probe:generated_at "timestamp"
    // <artifact:id> probe:observation <obs:1>
    // <obs:1> probe:agent "agent_id"
    // ... etc
  }

  /**
   * Load artifact from database
   * @param {string} artifactId - Artifact ID
   * @returns {Promise<Object>}
   */
  async loadArtifact(artifactId) {
    // Query store for artifact quads
    // Reconstruct artifact object from RDF
    throw new Error('DatabaseStorage.loadArtifact not implemented');
  }

  /**
   * Fetch all shards
   * @returns {Promise<Array>}
   */
  async fetchShards() {
    // Query all artifact quads from store
    throw new Error('DatabaseStorage.fetchShards not implemented');
  }

  /**
   * List artifact IDs
   * @returns {Promise<Array>}
   */
  async listArtifacts() {
    // Query for all probe:Artifact subjects
    throw new Error('DatabaseStorage.listArtifacts not implemented');
  }

  /**
   * Delete artifact
   * @param {string} artifactId - Artifact ID
   * @returns {Promise<boolean>}
   */
  async deleteArtifact(artifactId) {
    // Remove all quads with artifact:id as subject
    throw new Error('DatabaseStorage.deleteArtifact not implemented');
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
export function createFileStorage(rootDir = './artifacts') {
  return new FileStorage(rootDir);
}

/**
 * Create database storage
 * @param {Object} options - Configuration
 * @returns {DatabaseStorage}
 */
export function createDatabaseStorage(options) {
  return new DatabaseStorage(options);
}
