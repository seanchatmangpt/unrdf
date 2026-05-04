/**
 * @unrdf/pictl-semantics v26.4.23
 *
 * PICTL Semantics Integration with @unrdf Federation
 * Ontology-driven process mining with cryptographic quorum consensus
 *
 * @module @unrdf/pictl-semantics
 */

import { loadPictlOntology, queryPictlKnowledge } from './ontology-loader.mjs';
import {
  proposeResult,
  votePictlResult,
  getQuorumStatus,
  validateVoteTally,
  QuorumConfigSchema,
} from './quorum.mjs';
import { validatePictlResult, validateAgainstShapes } from './result-validator.mjs';

/**
 * Initialize PICTL semantics with federation support
 *
 * @param {Object} config - Configuration object
 * @param {string} config.nodeId - Unique node identifier in the quorum
 * @param {number} [config.quorumThreshold=2] - Minimum votes needed (default 2/3)
 * @param {string} [config.ontologyPath] - Path to pictl-ontology.ttl (optional for now)
 * @param {string} [config.shapesPath] - Path to pictl-shapes.ttl (optional for now)
 * @returns {Promise<Object>} Initialized PICTL context
 *
 * @example
 * const pictl = await initializePictlSemantics({
 *   nodeId: 'pictl-node-1',
 *   quorumThreshold: 2
 * });
 * await pictl.loadOntology();
 */
export async function initializePictlSemantics(config) {
  const validated = QuorumConfigSchema.parse(config);

  // Initialize quorum state
  const quorumState = {
    nodeId: validated.nodeId,
    quorumThreshold: validated.quorumThreshold || 2,
    results: new Map(),
    votes: new Map(),
    receipts: [],
    peers: new Map(),
    ontology: null,
    shapes: null,
  };

  return {
    nodeId: validated.nodeId,
    quorumState,
    // Lazy-load ontology on demand
    loadOntology: async () => {
      quorumState.ontology = await loadPictlOntology();
      return quorumState.ontology;
    },
    // Query the loaded ontology
    query: async sparql => queryPictlKnowledge(sparql, quorumState.ontology),
    // Federation quorum operations
    proposeResult: (result, nodeId) => proposeResult(result, nodeId, quorumState),
    votePictlResult: (resultId, vote, reason) => votePictlResult(resultId, vote, reason, quorumState),
    getQuorumStatus: () => getQuorumStatus(quorumState),
    // Result validation
    validateResult: result => validatePictlResult(result),
    validateAgainstShapes: (data, shapeUri) => validateAgainstShapes(data, shapeUri, quorumState),
  };
}

/**
 * Shutdown PICTL semantics context
 *
 * @param {Object} pictlContext - PICTL context from initializePictlSemantics
 * @returns {Promise<void>}
 */
export async function shutdownPictlSemantics(pictlContext) {
  if (pictlContext && pictlContext.quorumState) {
    pictlContext.quorumState.results.clear();
    pictlContext.quorumState.votes.clear();
    pictlContext.quorumState.peers.clear();
    pictlContext.quorumState.receipts = [];
    pictlContext.quorumState.ontology = null;
    pictlContext.quorumState.shapes = null;
  }
}

// Re-export key types and utilities
export { loadPictlOntology, queryPictlKnowledge } from './ontology-loader.mjs';
export {
  proposeResult,
  votePictlResult,
  getQuorumStatus,
  validateVoteTally,
  QuorumConfigSchema,
} from './quorum.mjs';
export { validatePictlResult, validateAgainstShapes } from './result-validator.mjs';
