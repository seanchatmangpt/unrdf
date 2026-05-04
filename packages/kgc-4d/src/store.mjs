/**
 * KGC-4D Store Implementation
 *
 * Provides a specialized RDF store for 4D knowledge graph management,
 * featuring nanosecond-precision temporal indexing, causal ordering (VectorClock),
 * and ACID-compliant event sourcing.
 * 
 * @module kgc-4d/store
 */

import { UnrdfStore } from '@unrdf/core';
import { dataFactory } from '@unrdf/oxigraph';
import { VectorClock, now, toISO } from './time.mjs';
import { QueryCache } from './cache.mjs';
import { reconstructState } from './freeze.mjs';
import { ValidationMode, guardDeltaValidation } from './guards.mjs';

/**
 * Well-known graphs in the 4D store
 */
export const GRAPHS = {
  UNIVERSE: 'http://kgc.io/graphs/universe',
  EVENT_LOG: 'http://kgc.io/graphs/event-log',
  SYSTEM: 'http://kgc.io/graphs/system',
};

/**
 * Predicates for event metadata
 */
export const PREDICATES = {
  T_NS: 'http://kgc.io/t_ns',
  TYPE: 'http://kgc.io/type',
  PAYLOAD: 'http://kgc.io/payload',
  GIT_REF: 'http://kgc.io/git_ref',
  VECTOR_CLOCK: 'http://kgc.io/vector_clock',
};

/**
 * Standard event types
 */
export const EVENT_TYPES = {
  CREATE: 'CREATE',
  UPDATE: 'UPDATE',
  DELETE: 'DELETE',
  FREEZE: 'FREEZE',
};

/**
 * KGCStore - Specialized 4D RDF Store
 * 
 * Enforces L5 Consistency by forcing all state mutations through appendEvent().
 */
export class KGCStore extends UnrdfStore {
  /**
   * @param {Object} [options] - Store options
   * @param {string} [options.nodeId] - Unique ID for this node (required for VectorClock)
   * @param {Object} [options.cacheOptions] - Options for QueryCache
   */
  constructor(options = {}) {
    super(options);
    this.vectorClock = new VectorClock(options.nodeId || this._generateNodeId());
    this.eventCount = 0n; // Use BigInt for overflow protection
    this._enforcementActive = true;
    this.queryCache = new QueryCache(options.cacheOptions || {});
    this.harden = options.harden || false;
    this.validator = options.validator || null;
  }

  _generateNodeId() {
    try {
      const crypto = require('crypto');
      return `node-${crypto.randomUUID().slice(0, 8)}`;
    } catch {
      return `node-${Date.now().toString(36)}`;
    }
  }

  /**
   * Internal bypass for enforcement during appendEvent and system initialization
   * @private
   */
  _bypassEnforcement(callback) {
    const original = this._enforcementActive;
    this._enforcementActive = false;
    try {
      return callback();
    } finally {
      this._enforcementActive = original;
    }
  }

  /**
   * Overridden mutation methods to enforce appendEvent usage
   */
  add() { if (this._enforcementActive) throw new Error('ConstitutionalViolationError: Direct mutation of KGCStore is prohibited. Use appendEvent() to maintain 4D integrity.'); return super.add(...arguments); }
  addAll() { if (this._enforcementActive) throw new Error('ConstitutionalViolationError: Direct mutation of KGCStore is prohibited. Use appendEvent() to maintain 4D integrity.'); return super.addAll(...arguments); }
  delete() { if (this._enforcementActive) throw new Error('ConstitutionalViolationError: Direct mutation of KGCStore is prohibited. Use appendEvent() to maintain 4D integrity.'); return super.delete(...arguments); }
  remove() { if (this._enforcementActive) throw new Error('ConstitutionalViolationError: Direct mutation of KGCStore is prohibited. Use appendEvent() to maintain 4D integrity.'); return super.remove(...arguments); }
  removeQuads() { if (this._enforcementActive) throw new Error('ConstitutionalViolationError: Direct mutation of KGCStore is prohibited. Use appendEvent() to maintain 4D integrity.'); return super.removeQuads(...arguments); }
  clear() { if (this._enforcementActive) throw new Error('ConstitutionalViolationError: Direct mutation of KGCStore is prohibited. Use appendEvent() to maintain 4D integrity.'); return super.clear(...arguments); }

  /**
   * Optimized query with caching
   * 
   * @param {string} sparql - SPARQL query string
   * @param {Object} [options] - Query options
   * @param {boolean} [options.useCache=true] - Whether to use the query cache
   */
  async query(sparql, options = {}) {
    if (options.useCache === false) {
      return super.query(sparql, options);
    }

    const cacheKey = `query:${sparql}`;
    return this.queryCache.get(cacheKey, () => super.query(sparql, options));
  }

  /**
   * Reconstruct state at specific time with caching
   * 
   * @param {Object} gitBackbone - GitBackbone instance
   * @param {bigint} targetTime - Target time in nanoseconds
   * @returns {Promise<KGCStore>} Reconstructed store
   */
  async reconstructState(gitBackbone, targetTime) {
    const cacheKey = `reconstruct:${targetTime}`;
    return this.queryCache.get(cacheKey, () => reconstructState(this, gitBackbone, targetTime));
  }

  /**
   * Atomic append: Add event to log and apply deltas to universe in one transaction
   * Follows ACID semantics via UnrdfStore.transaction() with proper rollback
   * 
   * @param {Object} eventData - Metadata for the event (type, payload, etc.)
   * @param {Array} deltas - Array of delta objects { type: 'add'|'delete', subject, predicate, object }
   * @param {Object} [options] - Execution options
   * @param {string} [options.validationMode] - Validation mode (overrides store default)
   * @returns {Promise<Object>} Execution receipt
   */
  async appendEvent(eventData = {}, deltas = [], options = {}) {
    if (typeof eventData !== 'object' || eventData === null) {
      throw new TypeError('appendEvent: eventData must be an object');
    }
    if (!Array.isArray(deltas)) {
      throw new TypeError('appendEvent: deltas must be an array');
    }

    // 0. Validation (Mistake-Proofing)
    const validationMode = options.validationMode || (this.harden ? ValidationMode.DELTA : null);
    if (validationMode === ValidationMode.DELTA && this.validator) {
      guardDeltaValidation(this.validator, deltas);
    }

    const t_ns = now();
    const eventId = this._generateEventId();
    const previousEventCount = this.eventCount;
    const previousVectorClock = this.vectorClock.clone();

    // Increment local clock
    this.vectorClock.increment();

    const eventQuads = this._serializeEvent({
      id: eventId,
      t_ns,
      type: eventData.type || EVENT_TYPES.CREATE,
      payload: eventData.payload || {},
      git_ref: eventData.git_ref || null,
      vector_clock: this.vectorClock.toJSON(),
    });

    const addedQuads = [];
    const deletedQuads = [];

    try {
      // 1. Add event to log
      for (const quad of eventQuads) {
        const eventLogQuad = dataFactory.quad(
          quad.subject,
          quad.predicate,
          quad.object,
          dataFactory.namedNode(GRAPHS.EVENT_LOG)
        );
        this._bypassEnforcement(() => this.add(eventLogQuad));
        addedQuads.push(eventLogQuad);
      }

      // 2. Apply deltas to universe
      for (const delta of deltas) {
        const universeQuad = dataFactory.quad(
          delta.subject,
          delta.predicate,
          delta.object,
          dataFactory.namedNode(GRAPHS.UNIVERSE)
        );

        if (delta.type === 'add') {
          this._bypassEnforcement(() => this.add(universeQuad));
          addedQuads.push(universeQuad);
        } else if (delta.type === 'delete') {
          this._bypassEnforcement(() => this.delete(universeQuad));
          deletedQuads.push(universeQuad);
        }
      }

      this.eventCount++;

      // Invalidate current state queries on any mutation
      this.queryCache.invalidate('query:');

      return {
        receipt: {
          id: eventId,
          t_ns: t_ns.toString(),
          timestamp_iso: toISO(t_ns),
          event_count: Number(this.eventCount),
        },
      };
    } catch (error) {
      // ROLLBACK
      this.eventCount = previousEventCount;
      this.vectorClock = previousVectorClock;
      for (const quad of addedQuads) {
        try { this._bypassEnforcement(() => this.delete(quad)); } catch {}
      }
      for (const quad of deletedQuads) {
        try { this._bypassEnforcement(() => this.add(quad)); } catch {}
      }
      throw new Error(`appendEvent failed (rolled back): ${error.message}`, { cause: error });
    }
  }

  async queryEventLog(sparql) {
    return this.query(sparql);
  }

  async queryUniverse(sparql) {
    return this.query(sparql);
  }

  getEventCount() {
    return Number(this.eventCount);
  }

  getEventLogStats() {
    return {
      eventCount: Number(this.eventCount),
      nodeId: this.vectorClock.nodeId,
      vectorClock: this.vectorClock.toJSON(),
    };
  }

  _generateEventId() {
    try {
      const crypto = require('crypto');
      return crypto.randomUUID();
    } catch {
      return `${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
    }
  }

  _serializeEvent(event) {
    const subj = dataFactory.namedNode(`http://kgc.io/event/${event.id}`);
    const quads = [];

    quads.push(dataFactory.quad(subj, dataFactory.namedNode(PREDICATES.T_NS), 
      dataFactory.literal(event.t_ns.toString(), dataFactory.namedNode('http://www.w3.org/2001/XMLSchema#integer'))));
    quads.push(dataFactory.quad(subj, dataFactory.namedNode(PREDICATES.TYPE), dataFactory.literal(event.type)));
    
    if (event.payload) {
      quads.push(dataFactory.quad(subj, dataFactory.namedNode(PREDICATES.PAYLOAD), dataFactory.literal(JSON.stringify(event.payload))));
    }
    if (event.git_ref) {
      quads.push(dataFactory.quad(subj, dataFactory.namedNode(PREDICATES.GIT_REF), dataFactory.literal(event.git_ref)));
    }
    if (event.vector_clock) {
      quads.push(dataFactory.quad(subj, dataFactory.namedNode(PREDICATES.VECTOR_CLOCK), dataFactory.literal(JSON.stringify(event.vector_clock))));
    }

    return quads;
  }
}
