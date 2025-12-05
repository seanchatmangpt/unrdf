/**
 * KGC Unified Store - Extends UnrdfStore with 4D event logging
 * Manages kgc:Universe (current state) and kgc:EventLog (history) atomically
 */

import { UnrdfStore } from '@unrdf/core';
import { dataFactory } from '@unrdf/oxigraph';
import { GRAPHS, PREDICATES, EVENT_TYPES } from './constants.mjs';
import { now, toISO } from './time.mjs';

export class KGCStore extends UnrdfStore {
  constructor(options = {}) {
    super(options);
    this.eventCount = 0;
  }

  /**
   * Atomic append: Add event to log and apply deltas to universe in one transaction
   * Follows ACID semantics via UnrdfStore.transaction()
   */
  async appendEvent(eventData, deltas = []) {
    return this.transaction((tx) => {
      const eventId = this._generateEventId();
      const t_ns = now();

      // 1. Serialize event to RDF quads for EventLog
      const eventQuads = this._serializeEvent({
        id: eventId,
        t_ns,
        type: eventData.type || 'CREATE',
        payload: eventData.payload || {},
        git_ref: eventData.git_ref || null,
      });

      // Add all event quads to EventLog named graph
      for (const quad of eventQuads) {
        const eventLogQuad = dataFactory.quad(
          quad.subject,
          quad.predicate,
          quad.object,
          dataFactory.namedNode(GRAPHS.EVENT_LOG)
        );
        tx.add(eventLogQuad);
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
          tx.add(universeQuad);
        } else if (delta.type === 'delete') {
          tx.delete(universeQuad);
        }
      }

      this.eventCount++;

      // 3. Generate and return receipt
      return {
        receipt: {
          id: eventId,
          t_ns: t_ns.toString(),
          timestamp_iso: toISO(t_ns),
          event_count: this.eventCount,
        },
      };
    });
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
    // Node.js fallback
    const { randomUUID } = await import('crypto');
    return randomUUID();
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
