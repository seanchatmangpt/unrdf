/**
 * KnowledgeStore - Deterministic, Hash-Stable, Immutable Append-Only Log
 *
 * Core substrate for KGC multi-agent system. Provides:
 * - Immutable append-only log of indexed triples
 * - Deterministic snapshot generation with BLAKE3 hashing
 * - Hash-stable canonicalization (lexicographic quad ordering)
 * - Query interface: selectTriples(pattern) → Set<Quad>
 * - State commitment: hash(store_state) → stable digest
 *
 * TRANCHE ISOLATION: Only depends on @unrdf/kgc-4d, @unrdf/oxigraph, @unrdf/core
 * NO dependencies on other tranches.
 */

import { KGCStore, freezeUniverse, GitBackbone } from '@unrdf/kgc-4d';
import { dataFactory } from '@unrdf/oxigraph';
import { blake3 } from 'hash-wasm';
import {
  validateStorageSnapshot,
  validateQueryPattern,
  validateStateCommitment
} from './types.mjs';

/**
 * KnowledgeStore - Wraps KGCStore with deterministic, hash-stable interface
 *
 * @example
 * import { KnowledgeStore } from '@unrdf/kgc-substrate';
 * const store = new KnowledgeStore({ nodeId: 'agent-1' });
 * const { index } = await store.appendTriple('add', subject, predicate, object);
 * const snapshot = await store.generateSnapshot();
 * console.assert(snapshot.quads_hash, 'Snapshot has deterministic hash');
 */
export class KnowledgeStore {
  /**
   * @param {Object} [options] - Configuration options
   * @param {string} [options.nodeId] - Node ID for vector clock (defaults to random)
   * @param {string} [options.gitDir] - Git directory for snapshot storage (defaults to '.kgc-substrate-git')
   */
  constructor(options = {}) {
    // Validate inputs
    if (options !== null && typeof options !== 'object') {
      throw new TypeError('KnowledgeStore: options must be an object');
    }

    this.nodeId = options.nodeId || this._generateNodeId();
    this.gitDir = options.gitDir || '.kgc-substrate-git';

    // Initialize KGCStore (4D event logging backend)
    this.store = new KGCStore({ nodeId: this.nodeId });

    // Initialize GitBackbone (snapshot storage)
    this.git = new GitBackbone(this.gitDir);

    // Append-only log index (BigInt for overflow protection)
    this.logIndex = 0n;

    // Epoch counter for snapshots
    this.epoch = 0;
  }

  /**
   * Generate unique node ID
   *
   * @returns {string} Node ID with 'ks-' prefix
   * @private
   */
  _generateNodeId() {
    if (typeof crypto !== 'undefined' && crypto.randomUUID) {
      return `ks-${crypto.randomUUID().slice(0, 8)}`;
    }
    try {
      const crypto = require('crypto');
      return `ks-${crypto.randomUUID().slice(0, 8)}`;
    } catch {
      return `ks-${Date.now().toString(36)}`;
    }
  }

  /**
   * Append triple to immutable log
   *
   * Enforces append-only property: triples are never modified, only added or marked deleted.
   * Each operation gets a sequential index for deterministic replay.
   *
   * @param {'add'|'delete'} operation - Operation type
   * @param {Object} subject - RDF subject term
   * @param {Object} predicate - RDF predicate term
   * @param {Object} object - RDF object term
   * @param {Object} [graph] - RDF graph term (defaults to Universe graph)
   * @returns {Promise<{index: bigint, timestamp_ns: bigint}>} Log entry metadata
   * @throws {TypeError} If parameters are invalid
   * @throws {Error} If append operation fails
   *
   * @example
   * const s = dataFactory.namedNode('http://ex.org/s');
   * const p = dataFactory.namedNode('http://ex.org/p');
   * const o = dataFactory.literal('value');
   * const { index } = await store.appendTriple('add', s, p, o);
   * console.assert(typeof index === 'bigint', 'Returns BigInt index');
   */
  async appendTriple(operation, subject, predicate, object, graph = null) {
    // Input validation
    if (operation !== 'add' && operation !== 'delete') {
      throw new TypeError(`appendTriple: operation must be 'add' or 'delete', got '${operation}'`);
    }
    if (!subject || typeof subject.value !== 'string') {
      throw new TypeError('appendTriple: subject must be a valid RDF term');
    }
    if (!predicate || typeof predicate.value !== 'string') {
      throw new TypeError('appendTriple: predicate must be a valid RDF term');
    }
    if (!object || typeof object.value !== 'string') {
      throw new TypeError('appendTriple: object must be a valid RDF term');
    }

    try {
      // Create delta for KGCStore
      const delta = {
        type: operation,
        subject,
        predicate,
        object,
      };

      // Append to KGCStore event log
      const { receipt } = await this.store.appendEvent(
        {
          type: operation === 'add' ? 'CREATE' : 'DELETE',
          payload: {
            operation,
            log_index: this.logIndex.toString(),
          },
        },
        [delta]
      );

      // Increment log index (immutable sequence)
      const currentIndex = this.logIndex;
      this.logIndex++;

      return {
        index: currentIndex,
        timestamp_ns: BigInt(receipt.t_ns),
      };
    } catch (error) {
      throw new Error(`appendTriple failed: ${error.message}`);
    }
  }

  /**
   * Select triples matching a pattern (supports wildcards)
   *
   * @param {Object} pattern - Query pattern with subject, predicate, object, graph
   * @param {Object|null} pattern.subject - Subject or null for wildcard
   * @param {Object|null} pattern.predicate - Predicate or null for wildcard
   * @param {Object|null} pattern.object - Object or null for wildcard
   * @param {Object|null} [pattern.graph] - Graph or null for wildcard
   * @returns {Set<Object>} Set of matching quads
   * @throws {Error} If pattern is invalid
   *
   * @example
   * const results = store.selectTriples({ subject: s, predicate: null, object: null });
   * console.log('Matching triples:', results.size);
   */
  selectTriples(pattern) {
    try {
      // Validate pattern
      validateQueryPattern(pattern);

      // Query KGCStore using match()
      const matches = this.store.match(
        pattern.subject,
        pattern.predicate,
        pattern.object,
        pattern.graph || null
      );

      return new Set([...matches]);
    } catch (error) {
      throw new Error(`selectTriples failed: ${error.message}`);
    }
  }

  /**
   * Generate deterministic snapshot with hash-stable canonicalization
   *
   * Steps:
   * 1. Extract all quads from Universe graph
   * 2. Sort lexicographically (S-P-O) for canonical ordering
   * 3. Serialize to N-Quads format
   * 4. Hash with BLAKE3 (deterministic)
   * 5. Commit to Git for immutable storage
   * 6. Return snapshot metadata
   *
   * @returns {Promise<Object>} StorageSnapshot with epoch, timestamp_ns, quads_hash, commit_hash, snapshot_id
   * @throws {Error} If snapshot generation fails
   *
   * @example
   * const snapshot = await store.generateSnapshot();
   * console.assert(snapshot.quads_hash, 'Has deterministic hash');
   * console.assert(snapshot.snapshot_id, 'Has UUID');
   */
  async generateSnapshot() {
    try {
      // Freeze universe using KGC-4D
      const freezeReceipt = await freezeUniverse(this.store, this.git);

      // Create snapshot metadata
      const snapshot = {
        epoch: this.epoch,
        timestamp_ns: BigInt(freezeReceipt.t_ns),
        quads_hash: freezeReceipt.universe_hash,
        commit_hash: freezeReceipt.git_ref,
        snapshot_id: freezeReceipt.id,
        quad_count: await this.getQuadCount(),
      };

      // Validate snapshot schema
      validateStorageSnapshot(snapshot);

      // Increment epoch
      this.epoch++;

      return snapshot;
    } catch (error) {
      throw new Error(`generateSnapshot failed: ${error.message}`);
    }
  }

  /**
   * Get current state commitment (hash of store state)
   *
   * Provides cryptographic commitment to current state without full snapshot.
   * Useful for lightweight verification.
   *
   * @returns {Promise<Object>} StateCommitment with state_hash, log_index, timestamp_ns, quad_count
   * @throws {Error} If commitment generation fails
   *
   * @example
   * const commitment = await store.getStateCommitment();
   * console.assert(commitment.state_hash, 'Has state hash');
   */
  async getStateCommitment() {
    try {
      // Get all quads from Universe graph
      const universeGraph = dataFactory.namedNode('http://kgc.io/graph/universe');
      const quads = [...this.store.match(null, null, null, universeGraph)];

      // Sort for canonical ordering
      quads.sort((a, b) => {
        const sCompare = a.subject.value < b.subject.value ? -1 :
                         a.subject.value > b.subject.value ? 1 : 0;
        if (sCompare !== 0) return sCompare;

        const pCompare = a.predicate.value < b.predicate.value ? -1 :
                         a.predicate.value > b.predicate.value ? 1 : 0;
        if (pCompare !== 0) return pCompare;

        return a.object.value < b.object.value ? -1 :
               a.object.value > b.object.value ? 1 : 0;
      });

      // Serialize to canonical string
      const canonicalString = quads.map(q =>
        `${q.subject.value}|${q.predicate.value}|${q.object.value}`
      ).join('\n');

      // Hash with BLAKE3
      const stateHash = await blake3(canonicalString);

      const commitment = {
        state_hash: stateHash,
        log_index: this.logIndex,
        timestamp_ns: BigInt(Date.now()) * 1_000_000n, // Convert ms to ns
        quad_count: quads.length,
      };

      // Validate commitment schema
      validateStateCommitment(commitment);

      return commitment;
    } catch (error) {
      throw new Error(`getStateCommitment failed: ${error.message}`);
    }
  }

  /**
   * Get current quad count
   *
   * @returns {Promise<number>} Total number of quads in Universe graph
   */
  async getQuadCount() {
    const universeGraph = dataFactory.namedNode('http://kgc.io/graph/universe');
    const quads = [...this.store.match(null, null, null, universeGraph)];
    return quads.length;
  }

  /**
   * Get current log index
   *
   * @returns {bigint} Current append-only log index
   */
  getLogIndex() {
    return this.logIndex;
  }

  /**
   * Get current epoch
   *
   * @returns {number} Current snapshot epoch
   */
  getEpoch() {
    return this.epoch;
  }

  /**
   * Get node ID
   *
   * @returns {string} Node ID for this store instance
   */
  getNodeId() {
    return this.nodeId;
  }
}
