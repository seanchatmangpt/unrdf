/**
 * @file Oxigraph adapter for substrate operations
 * @module agent-9/oxigraph-adapter
 */

import { createStore, dataFactory } from '../../packages/oxigraph/src/index.mjs';
import { OperationType } from './operation-types.mjs';

const { quad, namedNode, literal } = dataFactory;

/**
 * @typedef {Object} OxigraphSubstrate
 * @property {*} store - Oxigraph store instance
 * @property {'oxigraph'} type - Substrate type
 * @property {boolean} frozen - Whether substrate is frozen
 * @property {*} [transaction] - Current transaction
 * @property {Map<string, *>} metadata - Substrate metadata
 */

/**
 * Create Oxigraph-backed substrate
 * @param {Object} [options] - Configuration options
 * @param {string} [options.path] - Path for persistent storage
 * @returns {OxigraphSubstrate} New Oxigraph substrate
 */
export function createOxigraphSubstrate(options = {}) {
  const store = createStore(options.path);

  return {
    store,
    type: 'oxigraph',
    frozen: false,
    transaction: null,
    metadata: new Map()
  };
}

/**
 * Convert operations to Oxigraph operations
 * @param {Array<import('./operation-types.mjs').Operation>} operations - Operations to convert
 * @returns {Array<Object>} Oxigraph operations
 */
export function toOxigraphOps(operations) {
  return operations.map(op => {
    switch (op.type) {
      case OperationType.INSERT:
        return {
          type: 'insert',
          quad: createQuad(op.payload)
        };

      case OperationType.DELETE:
        return {
          type: 'delete',
          quad: createQuad(op.payload)
        };

      case OperationType.UPDATE:
        // Update = delete old + insert new
        return [
          {
            type: 'delete',
            pattern: {
              subject: op.payload.subject,
              predicate: op.payload.predicate
            }
          },
          {
            type: 'insert',
            quad: createQuad(op.payload)
          }
        ];

      case OperationType.QUERY:
        return {
          type: 'query',
          sparql: op.payload.query
        };

      default:
        throw new Error(`Unknown operation type: ${op.type}`);
    }
  }).flat();
}

/**
 * Convert Oxigraph results to standard format
 * @param {*} result - Oxigraph result
 * @returns {Array<Object>} Converted results
 */
export function fromOxigraphResult(result) {
  if (!result) {
    return [];
  }

  // Handle different result types
  if (Array.isArray(result)) {
    return result.map(binding => {
      const row = {};
      for (const [key, value] of Object.entries(binding)) {
        row[key] = termToString(value);
      }
      return row;
    });
  }

  // Handle single result
  if (typeof result === 'object') {
    const converted = {};
    for (const [key, value] of Object.entries(result)) {
      converted[key] = termToString(value);
    }
    return [converted];
  }

  return [];
}

/**
 * Apply operations to Oxigraph substrate
 * @param {OxigraphSubstrate} substrate - Target substrate
 * @param {Array<import('./operation-types.mjs').Operation>} operations - Operations to apply
 * @returns {Object} Result with success status
 */
export function applyToOxigraph(substrate, operations) {
  if (substrate.frozen) {
    throw new Error('Cannot apply operations to frozen substrate');
  }

  const oxOps = toOxigraphOps(operations);
  let affected = 0;

  for (const op of oxOps) {
    if (op.type === 'insert') {
      substrate.store.add(op.quad);
      affected++;
    } else if (op.type === 'delete') {
      if (op.quad) {
        substrate.store.delete(op.quad);
        affected++;
      } else if (op.pattern) {
        // Delete by pattern
        const matches = queryPattern(substrate.store, op.pattern);
        for (const q of matches) {
          substrate.store.delete(q);
          affected++;
        }
      }
    } else if (op.type === 'query') {
      // Query operations don't modify state
    }
  }

  return { success: true, affected };
}

/**
 * Query Oxigraph substrate
 * @param {OxigraphSubstrate} substrate - Substrate to query
 * @param {string} sparql - SPARQL query
 * @returns {Array<Object>} Query results
 */
export function queryOxigraph(substrate, sparql) {
  try {
    const result = substrate.store.query(sparql);
    return fromOxigraphResult(result);
  } catch (error) {
    throw new Error(`Query failed: ${error.message}`);
  }
}

/**
 * Create immutable snapshot of Oxigraph substrate
 * @param {OxigraphSubstrate} substrate - Substrate to snapshot
 * @returns {OxigraphSubstrate} Frozen snapshot
 */
export function snapshotOxigraph(substrate) {
  // Create new store and copy all quads
  const newStore = createStore();

  for (const quad of substrate.store) {
    newStore.add(quad);
  }

  return {
    store: newStore,
    type: 'oxigraph',
    frozen: true,
    transaction: null,
    metadata: new Map(substrate.metadata)
  };
}

/**
 * Get Oxigraph substrate statistics
 * @param {OxigraphSubstrate} substrate - Substrate to analyze
 * @returns {Object} Statistics
 */
export function getOxigraphStats(substrate) {
  let tripleCount = 0;

  // Count all quads
  for (const _ of substrate.store) {
    tripleCount++;
  }

  return {
    tripleCount,
    type: 'oxigraph',
    frozen: substrate.frozen,
    inTransaction: substrate.transaction !== null
  };
}

// --- Internal helpers ---

/**
 * Create RDF quad from operation payload
 * @param {Object} payload - Operation payload
 * @returns {*} RDF quad
 */
function createQuad(payload) {
  const { subject, predicate, object, graph } = payload;

  const s = namedNode(subject);
  const p = namedNode(predicate);
  const o = object.startsWith('"') ? parseLiteral(object) : namedNode(object);
  const g = graph ? namedNode(graph) : undefined;

  return quad(s, p, o, g);
}

/**
 * Parse literal value
 * @param {string} value - Literal string
 * @returns {*} Literal term
 */
function parseLiteral(value) {
  // Remove quotes
  const cleaned = value.replace(/^"(.*)"$/, '$1');

  // Check for datatype
  const datatypeMatch = value.match(/"\^\^<(.+)>$/);
  if (datatypeMatch) {
    return literal(cleaned, namedNode(datatypeMatch[1]));
  }

  // Check for language tag
  const langMatch = value.match(/"@(\w+)$/);
  if (langMatch) {
    return literal(cleaned, langMatch[1]);
  }

  return literal(cleaned);
}

/**
 * Convert RDF term to string
 * @param {*} term - RDF term
 * @returns {string} String representation
 */
function termToString(term) {
  if (!term) {
    return '';
  }

  if (term.termType === 'NamedNode') {
    return term.value;
  }

  if (term.termType === 'Literal') {
    let value = `"${term.value}"`;
    if (term.language) {
      value += `@${term.language}`;
    } else if (term.datatype && term.datatype.value !== 'http://www.w3.org/2001/XMLSchema#string') {
      value += `^^<${term.datatype.value}>`;
    }
    return value;
  }

  if (term.termType === 'BlankNode') {
    return `_:${term.value}`;
  }

  return String(term.value || term);
}

/**
 * Query store by pattern
 * @param {*} store - Oxigraph store
 * @param {Object} pattern - Query pattern
 * @returns {Array<*>} Matching quads
 */
function queryPattern(store, pattern) {
  const matches = [];
  const { subject, predicate, object, graph } = pattern;

  for (const quad of store) {
    if (subject && quad.subject.value !== subject) continue;
    if (predicate && quad.predicate.value !== predicate) continue;
    if (object && quad.object.value !== object) continue;
    if (graph && quad.graph?.value !== graph) continue;

    matches.push(quad);
  }

  return matches;
}
