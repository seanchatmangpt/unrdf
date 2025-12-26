/**
 * @file In-memory substrate store implementation
 * @module agent-9/substrate-store
 */

import { OperationType, validateOperations } from './operation-types.mjs';

/**
 * @typedef {Object} Triple
 * @property {string} subject - Subject IRI
 * @property {string} predicate - Predicate IRI
 * @property {string} object - Object literal or IRI
 * @property {string} [graph] - Named graph IRI
 */

/**
 * @typedef {Object} Substrate
 * @property {Map<string, Triple>} triples - Triple store
 * @property {Map<string, Set<string>>} indexes - Index structures
 * @property {*} [transaction] - Current transaction
 * @property {boolean} frozen - Whether substrate is frozen
 * @property {Map<string, *>} metadata - Substrate metadata
 */

/**
 * Create a new substrate instance
 * @returns {Substrate} New substrate
 */
export function createSubstrate() {
  return {
    triples: new Map(),
    indexes: new Map([
      ['subject', new Map()],
      ['predicate', new Map()],
      ['object', new Map()]
    ]),
    transaction: null,
    frozen: false,
    metadata: new Map()
  };
}

/**
 * Apply operations atomically to substrate
 * @param {Substrate} substrate - Target substrate
 * @param {Array<import('./operation-types.mjs').Operation>} operations - Operations to apply
 * @returns {Object} Result with success status and affected count
 * @throws {Error} If substrate is frozen or operations are invalid
 */
export function apply(substrate, operations) {
  if (substrate.frozen) {
    throw new Error('Cannot apply operations to frozen substrate');
  }

  validateOperations(operations);

  // Store original state for rollback
  const originalTriples = new Map(substrate.triples);
  const originalIndexes = cloneIndexes(substrate.indexes);

  let affected = 0;

  try {
    for (const operation of operations) {
      switch (operation.type) {
        case OperationType.INSERT:
          affected += applyInsert(substrate, operation.payload);
          break;

        case OperationType.UPDATE:
          affected += applyUpdate(substrate, operation.payload);
          break;

        case OperationType.DELETE:
          affected += applyDelete(substrate, operation.payload);
          break;

        case OperationType.QUERY:
          // Query operations don't modify state
          break;

        default:
          throw new Error(`Unknown operation type: ${operation.type}`);
      }
    }

    return { success: true, affected };
  } catch (error) {
    // Rollback on error
    substrate.triples = originalTriples;
    substrate.indexes = originalIndexes;
    throw new Error(`Failed to apply operations: ${error.message}`);
  }
}

/**
 * Query substrate
 * @param {Substrate} substrate - Substrate to query
 * @param {string} queryString - Query string (simplified SPARQL-like)
 * @returns {Array<Object>} Query results
 */
export function query(substrate, queryString) {
  // Simple pattern matching query implementation
  // Supports basic SELECT * WHERE { ?s ?p ?o } patterns

  const results = [];

  // Parse simple triple pattern
  const pattern = parseSimpleQuery(queryString);

  if (!pattern) {
    throw new Error('Invalid query format');
  }

  // Match against triples
  for (const [, triple] of substrate.triples) {
    if (matchesPattern(triple, pattern)) {
      results.push({
        subject: triple.subject,
        predicate: triple.predicate,
        object: triple.object,
        graph: triple.graph
      });
    }
  }

  return results;
}

/**
 * Create immutable snapshot of substrate
 * @param {Substrate} substrate - Substrate to snapshot
 * @returns {Substrate} Frozen snapshot
 */
export function snapshot(substrate) {
  const snap = {
    triples: new Map(substrate.triples),
    indexes: cloneIndexes(substrate.indexes),
    transaction: null,
    frozen: true,
    metadata: new Map(substrate.metadata)
  };

  return snap;
}

/**
 * Get substrate statistics
 * @param {Substrate} substrate - Substrate to analyze
 * @returns {Object} Statistics
 */
export function getStats(substrate) {
  return {
    tripleCount: substrate.triples.size,
    frozen: substrate.frozen,
    inTransaction: substrate.transaction !== null,
    subjectCount: substrate.indexes.get('subject').size,
    predicateCount: substrate.indexes.get('predicate').size,
    objectCount: substrate.indexes.get('object').size
  };
}

// --- Internal helpers ---

/**
 * Apply INSERT operation
 * @param {Substrate} substrate - Target substrate
 * @param {Object} payload - Operation payload
 * @returns {number} Number of triples inserted
 */
function applyInsert(substrate, payload) {
  const { subject, predicate, object, graph } = payload;
  const key = makeTripleKey(subject, predicate, object, graph);

  if (substrate.triples.has(key)) {
    return 0; // Already exists
  }

  const triple = { subject, predicate, object, graph };
  substrate.triples.set(key, triple);
  updateIndexes(substrate.indexes, triple, 'add');

  return 1;
}

/**
 * Apply UPDATE operation
 * @param {Substrate} substrate - Target substrate
 * @param {Object} payload - Operation payload
 * @returns {number} Number of triples updated
 */
function applyUpdate(substrate, payload) {
  const { subject, predicate, object, graph } = payload;

  // Find matching triples
  let updated = 0;
  for (const [key, triple] of substrate.triples) {
    if (triple.subject === subject && triple.predicate === predicate) {
      // Remove old
      updateIndexes(substrate.indexes, triple, 'remove');
      substrate.triples.delete(key);

      // Insert new
      const newKey = makeTripleKey(subject, predicate, object, graph);
      const newTriple = { subject, predicate, object, graph };
      substrate.triples.set(newKey, newTriple);
      updateIndexes(substrate.indexes, newTriple, 'add');

      updated++;
    }
  }

  return updated;
}

/**
 * Apply DELETE operation
 * @param {Substrate} substrate - Target substrate
 * @param {Object} payload - Operation payload
 * @returns {number} Number of triples deleted
 */
function applyDelete(substrate, payload) {
  const { subject, predicate, object, graph } = payload;
  const key = makeTripleKey(subject, predicate, object, graph);

  const triple = substrate.triples.get(key);
  if (!triple) {
    return 0; // Doesn't exist
  }

  substrate.triples.delete(key);
  updateIndexes(substrate.indexes, triple, 'remove');

  return 1;
}

/**
 * Make triple key
 * @param {string} subject - Subject
 * @param {string} predicate - Predicate
 * @param {string} object - Object
 * @param {string} [graph] - Graph
 * @returns {string} Triple key
 */
function makeTripleKey(subject, predicate, object, graph) {
  return graph
    ? `${subject}|${predicate}|${object}|${graph}`
    : `${subject}|${predicate}|${object}`;
}

/**
 * Update indexes
 * @param {Map} indexes - Index map
 * @param {Triple} triple - Triple to index
 * @param {'add' | 'remove'} action - Action to perform
 */
function updateIndexes(indexes, triple, action) {
  const key = makeTripleKey(triple.subject, triple.predicate, triple.object, triple.graph);

  for (const [indexType, index] of indexes) {
    const value = triple[indexType];
    if (!value) continue;

    if (action === 'add') {
      if (!index.has(value)) {
        index.set(value, new Set());
      }
      index.get(value).add(key);
    } else {
      const set = index.get(value);
      if (set) {
        set.delete(key);
        if (set.size === 0) {
          index.delete(value);
        }
      }
    }
  }
}

/**
 * Clone indexes
 * @param {Map} indexes - Indexes to clone
 * @returns {Map} Cloned indexes
 */
function cloneIndexes(indexes) {
  const cloned = new Map();
  for (const [key, index] of indexes) {
    const clonedIndex = new Map();
    for (const [k, set] of index) {
      clonedIndex.set(k, new Set(set));
    }
    cloned.set(key, clonedIndex);
  }
  return cloned;
}

/**
 * Parse simple query
 * @param {string} queryString - Query string
 * @returns {Object|null} Parsed pattern
 */
function parseSimpleQuery(queryString) {
  // Match: SELECT * WHERE { ?s ?p ?o }
  const match = queryString.match(/SELECT\s+\*\s+WHERE\s*\{\s*\?(\w+)\s+\?(\w+)\s+\?(\w+)\s*\}/i);

  if (match) {
    return {
      subject: match[1],
      predicate: match[2],
      object: match[3]
    };
  }

  return null;
}

/**
 * Check if triple matches pattern
 * @param {Triple} triple - Triple to check
 * @param {Object} pattern - Pattern to match
 * @returns {boolean} True if matches
 */
function matchesPattern(triple, pattern) {
  // Variables (starting with ?) match anything
  // For simple implementation, all variables match
  return true;
}
