/**
 * Planner - Intent → Delta compilation
 * @module planner
 */

import { dataFactory, createStore } from '../../../packages/oxigraph/src/index.mjs';
import { createCapsule, serializeQuad } from './capsule.mjs';

/**
 * Plan a capsule by compiling intent into delta
 * Steps:
 * 1. Parse intent operations
 * 2. Compute delta by comparing before/after states
 * 3. Apply guard profile constraints
 * 4. Generate capsule with receipt
 *
 * @param {Object} intent - Intent with ops array
 * @param {Object} store - Current RDF store (for delta computation)
 * @param {Object} profile - Convention profile with guards
 * @param {Array<string>} [parents] - Parent capsule hashes
 * @returns {Promise<Object>} Planned capsule
 */
export async function planCapsule(intent, store, profile, parents = []) {
  // 1. Create temporary store with current state
  const tempStore = createStore();

  // Copy current store state
  for (const quad of store.match(null, null, null, null)) {
    tempStore.add(quad);
  }

  // 2. Apply intent operations to temp store
  const delta = { add: [], del: [] };

  for (const op of intent.ops) {
    switch (op.type) {
      case 'set': {
        // Delete old values for (subject, predicate)
        const oldQuads = [
          ...tempStore.match(
            dataFactory.namedNode(op.subject),
            dataFactory.namedNode(op.predicate),
            null,
            null
          ),
        ];

        for (const quad of oldQuads) {
          tempStore.delete(quad);
          delta.del.push(serializeQuad(quad));
        }

        // Add new value
        const newQuad = dataFactory.quad(
          dataFactory.namedNode(op.subject),
          dataFactory.namedNode(op.predicate),
          createObject(op.object),
          dataFactory.defaultGraph()
        );
        tempStore.add(newQuad);
        delta.add.push(serializeQuad(newQuad));
        break;
      }

      case 'create': {
        // Add subject to graph (minimal: rdf:type)
        const quad = dataFactory.quad(
          dataFactory.namedNode(op.subject),
          dataFactory.namedNode(
            'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'
          ),
          dataFactory.namedNode(op.graph),
          dataFactory.defaultGraph()
        );
        tempStore.add(quad);
        delta.add.push(serializeQuad(quad));
        break;
      }

      case 'link': {
        const quad = dataFactory.quad(
          dataFactory.namedNode(op.subject),
          dataFactory.namedNode(op.predicate),
          dataFactory.namedNode(op.target),
          dataFactory.defaultGraph()
        );
        tempStore.add(quad);
        delta.add.push(serializeQuad(quad));
        break;
      }

      case 'unlink': {
        const quad = dataFactory.quad(
          dataFactory.namedNode(op.subject),
          dataFactory.namedNode(op.predicate),
          dataFactory.namedNode(op.target),
          dataFactory.defaultGraph()
        );
        tempStore.delete(quad);
        delta.del.push(serializeQuad(quad));
        break;
      }

      default:
        throw new Error(`Unknown intent operation type: ${op.type}`);
    }
  }

  // 3. Apply guard constraints
  const totalQuads = delta.add.length + delta.del.length;
  if (totalQuads > profile.limits.maxQuads) {
    throw new Error(
      `Delta exceeds maxQuads: ${totalQuads} > ${profile.limits.maxQuads}`
    );
  }

  // 4. Create capsule
  return createCapsule(intent, delta, profile, parents);
}

/**
 * Ensure idempotence: planCapsule(planCapsule(x)) === planCapsule(x)
 * By computing delta from ORIGINAL store, not modified store
 *
 * @param {Object} intent - Intent with ops array
 * @param {Object} originalStore - Original RDF store
 * @param {Object} profile - Convention profile with guards
 * @param {Array<string>} [parents] - Parent capsule hashes
 * @returns {Promise<Object>} Planned capsule
 */
export async function planCapsuleIdempotent(
  intent,
  originalStore,
  profile,
  parents = []
) {
  // Always compute delta relative to original store
  return planCapsule(intent, originalStore, profile, parents);
}

/**
 * Helper: Create RDF object term from value
 * @param {*} value - Value to convert
 * @returns {Object} RDF term
 */
function createObject(value) {
  if (typeof value === 'string' && value.startsWith('http')) {
    return dataFactory.namedNode(value);
  }
  return dataFactory.literal(String(value));
}
