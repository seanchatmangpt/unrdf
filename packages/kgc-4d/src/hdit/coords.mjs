/**
 * @fileoverview Coordinate generation for events in hyperdimensional space
 *
 * Implements μ: O → ℝ^d mapping from events to Float32Array coordinates.
 *
 * Structure:
 *   - First 16 dims: Ontology axes (time, type, frequency, risk, etc.)
 *   - Remaining dims: Anonymous capacity axes (composite features)
 *
 * @module @unrdf/kgc-4d/hdit/coords
 */

import { createHash } from 'node:crypto';
import {
  D_DEFAULT,
  ONTOLOGY_AXES,
  ONTOLOGY_DIM,
  EVENT_TYPE_ENCODING,
} from './constants.mjs';

/**
 * @typedef {Object} EventData
 * @property {string} type - Event type (CREATE, UPDATE, DELETE, etc.)
 * @property {Object} payload - Event payload
 * @property {number} timestamp - Nanosecond timestamp
 * @property {Object} vectorClock - Vector clock { nodeId, counters }
 * @property {Array<Object>} [mutations] - RDF mutations
 */

/**
 * @typedef {Object} UniverseContext
 * @property {Object} store - RDF store instance
 * @property {number} [minTime] - Minimum timestamp in universe
 * @property {number} [maxTime] - Maximum timestamp in universe
 * @property {Map<string, number>} [entityFrequency] - Entity reference counts
 */

/**
 * Generate dimensional coordinates for an event
 *
 * @param {EventData} event - Event to generate coordinates for
 * @param {UniverseContext} [universe] - Universe context for normalization
 * @param {number} [dimension=D_DEFAULT] - Target dimension
 * @returns {Float32Array} Coordinate vector in ℝ^d
 *
 * @example
 * const coords = coordsForEvent({
 *   type: 'CREATE',
 *   timestamp: Date.now() * 1e6,
 *   vectorClock: { nodeId: 'node-1', counters: { 'node-1': '5' } },
 *   payload: { description: 'Created user Alice' },
 *   mutations: [{ type: 'add', subject: 'alice', predicate: 'type', object: 'User' }]
 * });
 */
export function coordsForEvent(event, universe = {}, dimension = D_DEFAULT) {
  const coords = new Float32Array(dimension);

  // Fill ontology axes (first 16 dimensions)
  fillOntologyAxes(coords, event, universe);

  // Fill capacity axes (remaining dimensions)
  fillCapacityAxes(coords, event, universe, ONTOLOGY_DIM, dimension);

  return coords;
}

/**
 * Fill ontology-mapped axes with semantic features
 * @private
 */
function fillOntologyAxes(coords, event, universe) {
  const { TIME, EVENT_TYPE, LOG_FREQUENCY, RISK_SCORE, CAUSALITY_DEPTH,
          MUTATION_COUNT, AGENT_HASH, GRAPH_HASH } = ONTOLOGY_AXES;

  // Axis 0: Normalized time [0, 1]
  coords[TIME] = normalizeTime(event.timestamp, universe);

  // Axis 1: Event type encoding
  coords[EVENT_TYPE] = EVENT_TYPE_ENCODING[event.type] ?? 0.5;

  // Axis 2: Log frequency of referenced entities
  coords[LOG_FREQUENCY] = calculateLogFrequency(event, universe);

  // Axis 3: Risk score (based on mutation count, event type)
  coords[RISK_SCORE] = calculateRiskScore(event);

  // Axis 4: Causality depth (sum of vector clock counters)
  coords[CAUSALITY_DEPTH] = calculateCausalityDepth(event.vectorClock);

  // Axis 5: Mutation count (normalized)
  coords[MUTATION_COUNT] = normalizeMutationCount(event.mutations?.length ?? 0);

  // Axis 6: Agent/user ID hash (normalized to [0, 1])
  coords[AGENT_HASH] = hashToFloat(event.vectorClock?.nodeId ?? 'unknown');

  // Axis 7: Graph context hash
  coords[GRAPH_HASH] = hashToFloat(event.payload?.graph ?? 'default');

  // Axes 8-15: Reserved for future ontology extensions (zero for now)
  // This gives headroom for new semantic axes without breaking existing vectors
}

/**
 * Fill anonymous capacity axes with composite features
 * Uses pseudo-random but deterministic mapping from event properties
 * @private
 */
function fillCapacityAxes(coords, event, universe, startIdx, dimension) {
  // Generate deterministic seed from event
  const seed = hashEventToSeed(event);

  // Simple PRNG for reproducible capacity features
  let state = seed;
  const prng = () => {
    // xorshift32
    state ^= state << 13;
    state ^= state >>> 17;
    state ^= state << 5;
    return ((state >>> 0) / 0xFFFFFFFF); // [0, 1]
  };

  // Fill remaining dimensions with composite features
  for (let i = startIdx; i < dimension; i++) {
    // Blend deterministic features with event properties
    const base = prng();

    // Mix in event characteristics to create meaningful separations
    const timeFactor = (event.timestamp % 1000000) / 1000000;
    const typeFactor = EVENT_TYPE_ENCODING[event.type] ?? 0.5;
    const mutationFactor = (event.mutations?.length ?? 0) / 100;

    // Weighted blend (deterministic but influenced by event)
    coords[i] = 0.6 * base + 0.2 * timeFactor + 0.1 * typeFactor + 0.1 * mutationFactor;
  }
}

/**
 * Normalize timestamp to [0, 1] range
 * @private
 */
function normalizeTime(timestamp, universe) {
  const minTime = universe.minTime ?? timestamp;
  const maxTime = universe.maxTime ?? timestamp;

  if (maxTime === minTime) return 0.5;

  return (timestamp - minTime) / (maxTime - minTime);
}

/**
 * Calculate log-frequency score for entities referenced in event
 * @private
 */
function calculateLogFrequency(event, universe) {
  if (!universe.entityFrequency || !event.mutations) return 0.5;

  const frequencies = event.mutations
    .map(m => universe.entityFrequency.get(m.subject) ?? 1)
    .filter(f => f > 0);

  if (frequencies.length === 0) return 0.5;

  const avgFreq = frequencies.reduce((a, b) => a + b, 0) / frequencies.length;
  return Math.min(1.0, Math.log10(avgFreq + 1) / 5); // log scale, cap at 10^5
}

/**
 * Calculate risk score based on event characteristics
 * Higher for DELETE, large mutations, system graphs
 * @private
 */
function calculateRiskScore(event) {
  let risk = 0.0;

  // Event type risk
  if (event.type === 'DELETE') risk += 0.4;
  else if (event.type === 'UPDATE') risk += 0.2;
  else if (event.type === 'SNAPSHOT') risk += 0.3;

  // Mutation count risk (normalized, capped at 10)
  const mutationRisk = Math.min(0.3, (event.mutations?.length ?? 0) / 10 * 0.3);
  risk += mutationRisk;

  // System/critical graph risk
  const graphName = event.payload?.graph ?? '';
  if (graphName.includes('System') || graphName.includes('Critical')) {
    risk += 0.3;
  }

  return Math.min(1.0, risk);
}

/**
 * Calculate causality depth from vector clock
 * Sum of all counter values (normalized)
 * @private
 */
function calculateCausalityDepth(vectorClock) {
  if (!vectorClock?.counters) return 0.0;

  const sum = Object.values(vectorClock.counters)
    .reduce((total, count) => total + BigInt(count), 0n);

  // Normalize: log scale to handle large counts
  return Math.min(1.0, Math.log10(Number(sum) + 1) / 6); // cap at 10^6
}

/**
 * Normalize mutation count to [0, 1]
 * @private
 */
function normalizeMutationCount(count) {
  // Log scale, cap at 100 mutations
  return Math.min(1.0, Math.log10(count + 1) / 2);
}

/**
 * Hash string to float in [0, 1]
 * @private
 */
function hashToFloat(str) {
  const hash = createHash('sha256').update(str).digest();
  const value = hash.readUInt32BE(0);
  return value / 0xFFFFFFFF;
}

/**
 * Hash event to deterministic seed for PRNG
 * @private
 */
function hashEventToSeed(event) {
  const hashInput = JSON.stringify({
    type: event.type,
    nodeId: event.vectorClock?.nodeId,
    // Use timestamp modulo to avoid seed changing too frequently
    timeSlot: Math.floor(event.timestamp / 1e9), // 1-second slots
  });

  const hash = createHash('sha256').update(hashInput).digest();
  return hash.readUInt32BE(0);
}

/**
 * Batch coordinate generation for multiple events
 * More efficient than individual calls
 *
 * @param {EventData[]} events - Events to process
 * @param {UniverseContext} [universe] - Shared universe context
 * @param {number} [dimension=D_DEFAULT] - Target dimension
 * @returns {Float32Array[]} Array of coordinate vectors
 */
export function batchCoordsForEvents(events, universe = {}, dimension = D_DEFAULT) {
  // Pre-compute universe statistics if not provided
  if (!universe.minTime || !universe.maxTime) {
    const timestamps = events.map(e => e.timestamp);
    universe.minTime = Math.min(...timestamps);
    universe.maxTime = Math.max(...timestamps);
  }

  return events.map(event => coordsForEvent(event, universe, dimension));
}

/**
 * Create universe context from event history
 * Use this for better normalization when processing batches
 *
 * @param {EventData[]} events - Event history
 * @returns {UniverseContext} Context for coordinate generation
 */
export function createUniverseContext(events) {
  const timestamps = events.map(e => e.timestamp);

  // Build entity frequency map
  const entityFrequency = new Map();
  for (const event of events) {
    if (!event.mutations) continue;
    for (const mutation of event.mutations) {
      const count = entityFrequency.get(mutation.subject) ?? 0;
      entityFrequency.set(mutation.subject, count + 1);
    }
  }

  return {
    minTime: Math.min(...timestamps),
    maxTime: Math.max(...timestamps),
    entityFrequency,
  };
}
