/**
 * KnowledgeStore - Deterministic, Hash-Stable, Immutable Append-Only Log
 *
 * Core substrate for KGC multi-agent system. Provides:
 * - Immutable append-only log of indexed triples
 * - Deterministic snapshot generation with BLAKE3 hashing
 * - Hash-stable canonicalization (lexicographic quad ordering)
 * - Query interface: selectTriples(pattern) → Set<Quad>
 * - State commitment: hash(store_state) → stable digest
 */

import { KGCStore, freezeUniverse, GitBackbone } from '@unrdf/kgc-4d';
import { dataFactory } from '@unrdf/oxigraph';
import { blake3 } from 'hash-wasm';
import {
  validateStorageSnapshot,
  validateQueryPattern,
  validateStateCommitment,
} from './types.mjs';

const UNIVERSE_GRAPH_IRI = 'http://kgc.io/graphs/universe';

export class KnowledgeStore {
  constructor(options = {}) {
    if (options !== null && typeof options !== 'object') {
      throw new TypeError('KnowledgeStore: options must be an object');
    }

    this.nodeId = options.nodeId || this._generateNodeId();
    this.gitDir = options.gitDir || '.kgc-substrate-git';
    this.store = new KGCStore({ nodeId: this.nodeId });
    this.git = new GitBackbone(this.gitDir);
    this.logIndex = 0n;
    this.epoch = 0;
  }

  _generateNodeId() {
    if (typeof crypto !== 'undefined' && crypto.randomUUID) {
      return `ks-${crypto.randomUUID().slice(0, 8)}`;
    }
    return `ks-${Date.now().toString(36)}`;
  }

  async appendTriple(operation, subject, predicate, object, graph = null) {
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
      const delta = { type: operation, subject, predicate, object, graph };
      const { receipt } = await this.store.appendEvent(
        {
          type: operation === 'add' ? 'CREATE' : 'DELETE',
          payload: { operation, log_index: this.logIndex.toString() },
        },
        [delta]
      );

      const currentIndex = this.logIndex;
      this.logIndex++;
      return { index: currentIndex, timestamp_ns: BigInt(receipt.t_ns) };
    } catch (error) {
      throw new Error(`appendTriple failed: ${error.message}`);
    }
  }

  selectTriples(pattern) {
    try {
      validateQueryPattern(pattern);
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

  async generateSnapshot() {
    try {
      const freezeReceipt = await freezeUniverse(this.store, this.git);
      const snapshot = {
        epoch: this.epoch,
        timestamp_ns: BigInt(freezeReceipt.t_ns),
        quads_hash: freezeReceipt.universe_hash,
        commit_hash: freezeReceipt.git_ref,
        snapshot_id: freezeReceipt.id,
        quad_count: await this.getQuadCount(),
      };
      validateStorageSnapshot(snapshot);
      this.epoch++;
      return snapshot;
    } catch (error) {
      throw new Error(`generateSnapshot failed: ${error.message}`);
    }
  }

  async getStateCommitment() {
    try {
      const universeGraph = dataFactory.namedNode(UNIVERSE_GRAPH_IRI);
      const quads = [...this.store.match(null, null, null, universeGraph)];
      quads.sort((a, b) => {
        const subject = a.subject.value.localeCompare(b.subject.value);
        if (subject !== 0) return subject;
        const predicate = a.predicate.value.localeCompare(b.predicate.value);
        if (predicate !== 0) return predicate;
        return a.object.value.localeCompare(b.object.value);
      });

      const canonicalString = quads
        .map(q => `${q.subject.value}|${q.predicate.value}|${q.object.value}`)
        .join('\n');
      const commitment = {
        state_hash: await blake3(canonicalString),
        log_index: this.logIndex,
        timestamp_ns: BigInt(Date.now()) * 1_000_000n,
        quad_count: quads.length,
      };
      validateStateCommitment(commitment);
      return commitment;
    } catch (error) {
      throw new Error(`getStateCommitment failed: ${error.message}`);
    }
  }

  async getQuadCount() {
    const universeGraph = dataFactory.namedNode(UNIVERSE_GRAPH_IRI);
    return [...this.store.match(null, null, null, universeGraph)].length;
  }

  getLogIndex() {
    return this.logIndex;
  }

  getEpoch() {
    return this.epoch;
  }

  getNodeId() {
    return this.nodeId;
  }
}
