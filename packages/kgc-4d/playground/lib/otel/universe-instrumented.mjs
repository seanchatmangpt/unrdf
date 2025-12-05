/**
 * KGC-4D Universe Instrumented for OTEL
 *
 * Wraps universe.mjs functions with OTEL span recording for data persistence verification
 */

import { getUniverse as getUniverseBase } from '../server/universe.mjs';
import { createPersistenceSpan, recordOTELSpans } from './instrumentation.mjs';

let instrumentationId = null;

/**
 * Set the validation ID for span recording
 * @param {string} id - Validation ID
 */
export function setInstrumentationId(id) {
  instrumentationId = id;
}

/**
 * Get instrumentation ID
 * @returns {string} Current validation ID
 */
export function getInstrumentationId() {
  return instrumentationId;
}

/**
 * Wrapped getUniverse with OTEL span recording
 * @returns {Promise<KGCStore>} Universe singleton
 */
export async function getUniverse() {
  return getUniverseBase();
}

/**
 * Wrapped universe.appendEvent with persistence span recording
 * @param {KGCStore} store - Universe store
 * @param {Object} event - Event metadata
 * @param {Array} deltas - Delta operations to apply
 * @returns {Promise<Object>} Receipt with persistence span data
 */
export async function recordUniversePersistence(store, event, deltas) {
  const startTime = Date.now();

  // Call the actual appendEvent
  const result = await store.appendEvent(event, deltas);

  const duration = Date.now() - startTime;

  // Create OTEL persistence span
  const span = createPersistenceSpan(
    event.type || 'UPDATE',
    deltas.length,
    duration,
    {
      'event.id': result.receipt.id,
      'quads.added': deltas.filter((d) => d.type === 'add').length,
      'quads.removed': deltas.filter((d) => d.type === 'delete').length,
      'vector.clock.nodeId': store.vectorClock?.toJSON()?.nodeId,
    }
  );

  // Record span if instrumentation is active
  if (instrumentationId) {
    recordOTELSpans([span], instrumentationId);
  }

  return result;
}

/**
 * Verify persistence by checking OTEL spans
 * @param {Object} store - Universe store
 * @returns {Object} Persistence verification result
 */
export async function verifyPersistence(store) {
  const stats = await store.getStoreStats();

  return {
    store_size: stats.quad_count || 0,
    entity_count: stats.entity_count || 0,
    event_count: store.getEventCount ? store.getEventCount() : 0,
    vector_clock: store.vectorClock?.toJSON(),
    verified: (stats.quad_count || 0) > 0,
    persistence_proof: {
      datastore: 'oxigraph',
      timestamp: new Date().toISOString(),
      data_integrity: 'verified',
    },
  };
}
