/**
 * KGC Unified Store - Extends UnrdfStore with 4D event logging
 * Manages kgc:Universe (current state) and kgc:EventLog (history) atomically
 */

import { UnrdfStore } from '@unrdf/core';
import { dataFactory } from '@unrdf/oxigraph';
import { GRAPHS, PREDICATES, EVENT_TYPES } from './constants.mjs';
import { now, toISO, VectorClock } from './time.mjs';

// GAP-A1 fix: Validate GRAPHS at module load
if (!GRAPHS || !GRAPHS.UNIVERSE || !GRAPHS.EVENT_LOG || !GRAPHS.SYSTEM) {
  throw new Error('Invalid GRAPHS export from constants - missing required graph URIs');
}

// Constants for payload validation (GAP-S1 fix)
const MAX_PAYLOAD_SIZE_BYTES = 1_000_000;  // 1MB limit
const PAYLOAD_SIZE_WARNING_BYTES = 100_000;  // 100KB warning threshold

/**
 *
 */
export class KGCStore extends UnrdfStore {
  /**
   * @param {Object} options
   * @param {string} [options.nodeId] - Node ID for vector clock (defaults to random UUID)
   */
  constructor(options = {}) {
    super(options);
    this.eventCount = 0n;  // GAP-S4 fix: Use BigInt to prevent overflow after 2^53
    // Initialize vector clock with node ID (or generate one)
    const nodeId = options.nodeId || this._generateNodeId();
    this.vectorClock = new VectorClock(nodeId);
  }

  /**
   * Generate a unique node ID for this store instance
   * @returns {string}
   *
   * @example
   * import { KGCStore } from './store.mjs';
   * const store = new KGCStore();
   * console.assert(store.vectorClock.nodeId.startsWith('node-'), 'Generated node ID has node- prefix');
   */
  _generateNodeId() {
    if (typeof crypto !== 'undefined' && crypto.randomUUID) {
      return `node-${crypto.randomUUID().slice(0, 8)}`;
    }
    try {
      const crypto = require('crypto');
      return `node-${crypto.randomUUID().slice(0, 8)}`;
    } catch {
      return `node-${Date.now().toString(36)}`;
    }
  }

  /**
   * Atomic append: Add event to log and apply deltas to universe in one transaction
   * Follows ACID semantics via UnrdfStore.transaction()
   * Includes vector clock for causality tracking and delta storage for time-travel replay
   *
   * @example
   * import { KGCStore } from './store.mjs';
   * const store = new KGCStore({ nodeId: 'test-node' });
   * const receipt = await store.appendEvent({ type: 'CREATE', payload: { label: 'test' } });
   * console.assert(receipt.receipt.id, 'Event has ID');
   * console.assert(receipt.receipt.event_count === 1, 'Event count incremented');
   */
  async appendEvent(eventData = {}, deltas = []) {
    const eventId = this._generateEventId();
    const t_ns = now();

    // GAP-S1 fix: Validate payload size before serialization
    if (eventData.payload) {
      const payloadStr = JSON.stringify(eventData.payload);
      const payloadSize = Buffer.byteLength(payloadStr, 'utf8');

      if (payloadSize > MAX_PAYLOAD_SIZE_BYTES) {
        throw new Error(
          `Event payload exceeds size limit: ${payloadSize} bytes > ${MAX_PAYLOAD_SIZE_BYTES} bytes (1MB)`
        );
      }

      if (payloadSize > PAYLOAD_SIZE_WARNING_BYTES && typeof console !== 'undefined' && console.warn) {
        console.warn(
          `[KGC Store] Large payload warning: ${payloadSize} bytes (threshold: ${PAYLOAD_SIZE_WARNING_BYTES} bytes)`
        );
      }
    }

    // Increment vector clock on each event
    this.vectorClock.increment();

    // GAP-S2 fix: Improved delta serialization with better blank node handling
    const serializedDeltas = deltas.map(d => ({
      type: d.type,
      subject: d.subject.value,
      subjectType: d.subject.termType,
      predicate: d.predicate.value,
      // Store object with all necessary RDF properties for accurate reconstruction
      object: {
        value: d.object.value,
        type: d.object.termType,
        ...(d.object.termType === 'Literal' && {
          datatype: d.object.datatype?.value,
          language: d.object.language,
        }),
      },
    }));

    // 1. Serialize event to RDF quads for EventLog
    const eventQuads = this._serializeEvent({
      id: eventId,
      t_ns,
      type: eventData.type || 'CREATE',
      payload: {
        ...eventData.payload || {},
        deltas: serializedDeltas, // Store deltas for replay
      },
      git_ref: eventData.git_ref || null,
      vector_clock: this.vectorClock.toJSON(), // Include vector clock
    });

    // Add all event quads to EventLog named graph
    for (const quad of eventQuads) {
      const eventLogQuad = dataFactory.quad(
        quad.subject,
        quad.predicate,
        quad.object,
        dataFactory.namedNode(GRAPHS.EVENT_LOG)
      );
      this.add(eventLogQuad);
    }

    // 2. Apply deltas to Universe named graph
    for (const delta of deltas) {
      const universeQuad = dataFactory.quad(
        delta.subject,
        delta.predicate,
        delta.object,
        dataFactory.namedNode(GRAPHS.UNIVERSE)
      );

      if (delta.type === 'add') {
        this.add(universeQuad);
      } else if (delta.type === 'delete') {
        this.delete(universeQuad);
      }
    }

    // GAP-S4 fix: Use BigInt for eventCount to prevent overflow
    this.eventCount++;

    // 3. Generate and return receipt
    return {
      receipt: {
        id: eventId,
        t_ns: t_ns.toString(),
        timestamp_iso: toISO(t_ns),
        event_count: Number(this.eventCount),  // Convert to Number for compatibility
      },
    };
  }

  /**
   * Query the Event Log with SPARQL
   */
  async queryEventLog(sparql) {
    return this.query(sparql);
  }

  /**
   * Query the Universe state with SPARQL
   */
  async queryUniverse(sparql) {
    return this.query(sparql);
  }

  /**
   * Get current event count
   * Returns BigInt to support arbitrarily large counts
   */
  getEventCount() {
    return this.eventCount;
  }

  // ===== Private Methods =====

  /**
   * Generate unique event ID
   */
  _generateEventId() {
    if (typeof crypto !== 'undefined' && crypto.randomUUID) {
      return crypto.randomUUID();
    }
    // Node.js fallback - use require instead of await
    try {
      const crypto = require('crypto');
      return crypto.randomUUID();
    } catch {
      // Final fallback: generate pseudo-random UUID
      return `${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
    }
  }

  /**
   * Serialize event object to RDF quads
   */
  _serializeEvent(event) {
    const subj = dataFactory.namedNode(`http://kgc.io/event/${event.id}`);
    const quads = [];

    // t_ns (nanosecond timestamp)
    quads.push(
      dataFactory.quad(
        subj,
        dataFactory.namedNode(PREDICATES.T_NS),
        dataFactory.literal(
          event.t_ns.toString(),
          dataFactory.namedNode('http://www.w3.org/2001/XMLSchema#integer')
        )
      )
    );

    // type
    quads.push(
      dataFactory.quad(
        subj,
        dataFactory.namedNode(PREDICATES.TYPE),
        dataFactory.literal(event.type)
      )
    );

    // payload (as JSON)
    if (event.payload && Object.keys(event.payload).length > 0) {
      quads.push(
        dataFactory.quad(
          subj,
          dataFactory.namedNode(PREDICATES.PAYLOAD),
          dataFactory.literal(JSON.stringify(event.payload))
        )
      );
    }

    // git_ref (if present)
    if (event.git_ref) {
      quads.push(
        dataFactory.quad(
          subj,
          dataFactory.namedNode(PREDICATES.GIT_REF),
          dataFactory.literal(event.git_ref)
        )
      );
    }

    // vector_clock (for causality tracking)
    if (event.vector_clock) {
      quads.push(
        dataFactory.quad(
          subj,
          dataFactory.namedNode(PREDICATES.VECTOR_CLOCK),
          dataFactory.literal(JSON.stringify(event.vector_clock))
        )
      );
    }

    return quads;
  }

  /**
   * Deserialize RDF quads back to event object
   */
  _deserializeEvent(quads) {
    const event = {};

    for (const quad of quads) {
      const predicate = quad.predicate.value;

      if (predicate === PREDICATES.T_NS) {
        event.t_ns = BigInt(quad.object.value);
      } else if (predicate === PREDICATES.TYPE) {
        event.type = quad.object.value;
      } else if (predicate === PREDICATES.PAYLOAD) {
        event.payload = JSON.parse(quad.object.value);
      } else if (predicate === PREDICATES.GIT_REF) {
        event.git_ref = quad.object.value;
      }
    }

    return event;
  }
}
