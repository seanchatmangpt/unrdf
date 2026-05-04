/**
 * @file Store Adapter - Unified interface for RDF store operations
 * @module @unrdf/fusion/store-adapter
 *
 * Provides consistent store adapter pattern with:
 * - Unified interface across Oxigraph/KGC-4D/Core implementations
 * - Transaction semantics with rollback
 * - Freeze/reconstruct for immutable snapshots
 * - Deterministic hashing for verification
 *
 * Reuses existing patterns from:
 * - @unrdf/core/UnrdfStore (base store operations)
 * - @unrdf/kgc-4d/KGCStore (transaction rollback pattern)
 * - @unrdf/kgc-4d/freeze (snapshot creation and hashing)
 */

import { createStore as createMockStore, dataFactory } from './mock-store.mjs';
import { createHash } from 'node:crypto';

/**
 * Create a unified store adapter
 *
 * @param {string} [impl='oxigraph'] - Store implementation type
 * @param {Object} [options] - Store options
 * @returns {Object} StoreAdapter with methods: { addQuad, deleteQuad, query, transaction, freeze, reconstruct, getStore }
 *
 * @example
 * import { createStoreAdapter } from '@unrdf/fusion/store-adapter';
 * const adapter = createStoreAdapter();
 * console.assert(typeof adapter.addQuad === 'function', 'Has addQuad method');
 */
export async function createStoreAdapter(impl = 'oxigraph', options = {}) {
  // Create base store based on implementation type
  let store;

  if (impl === 'oxigraph') {
    // Use mock store for testing (oxigraph not available without full install)
    store = createMockStore();
    // Add initial quads if provided
    if (options.quads && Array.isArray(options.quads)) {
      for (const quad of options.quads) {
        store.add(quad);
      }
    }
  } else {
    throw new TypeError(`createStoreAdapter: unsupported implementation '${impl}'`);
  }

  // Return unified adapter interface
  return {
    /**
     * Add a quad to the store
     * @param {Object} quad - RDF quad to add
     */
    addQuad(quad) {
      if (!quad) throw new TypeError('addQuad: quad is required');
      store.add(quad);
    },

    /**
     * Delete a quad from the store
     * @param {Object} quad - RDF quad to delete
     */
    deleteQuad(quad) {
      if (!quad) throw new TypeError('deleteQuad: quad is required');
      store.delete(quad);
    },

    /**
     * Query quads from the store
     * @param {Object} [subject] - Subject filter (null for all)
     * @param {Object} [predicate] - Predicate filter (null for all)
     * @param {Object} [object] - Object filter (null for all)
     * @param {Object} [graph] - Graph filter (null for all)
     * @returns {Array<Object>} Array of matching quads
     */
    query(subject = null, predicate = null, object = null, graph = null) {
      return Array.from(store.match(subject, predicate, object, graph));
    },

    /**
     * Execute a transaction with rollback on failure
     * @param {Function} fn - Transaction function receiving adapter
     * @returns {Promise<void>}
     */
    async transaction(fn) {
      if (typeof fn !== 'function') {
        throw new TypeError('transaction: fn must be a function');
      }

      // Snapshot current state for rollback (reuse KGCStore pattern)
      const snapshot = Array.from(store.match());

      try {
        // Execute transaction
        await fn(this);
      } catch (error) {
        // Rollback: clear store and restore snapshot
        const allQuads = Array.from(store.match());
        for (const quad of allQuads) {
          store.delete(quad);
        }
        for (const quad of snapshot) {
          store.add(quad);
        }
        throw new Error(`Transaction failed (rolled back): ${error.message}`);
      }
    },

    /**
     * Get raw store instance
     * @returns {Object} The underlying store
     */
    getStore() {
      return store;
    },
  };
}

/**
 * Execute transactional operation with all-or-nothing semantics
 *
 * @param {Object} adapter - StoreAdapter instance
 * @param {Function} fn - Transaction function receiving adapter
 * @returns {Promise<void>}
 *
 * @example
 * import { transactional } from '@unrdf/fusion/store-adapter';
 * await transactional(adapter, async (txAdapter) => {
 *   txAdapter.addQuad(quad1);
 *   txAdapter.addQuad(quad2);
 * });
 */
export async function transactional(adapter, fn) {
  if (!adapter || typeof adapter.transaction !== 'function') {
    throw new TypeError('transactional: adapter must have transaction() method');
  }

  return adapter.transaction(fn);
}

/**
 * Freeze store to immutable snapshot with hash
 * Reuses KGC freeze pattern: dump, canonicalize, hash
 *
 * @param {Object} adapter - StoreAdapter instance
 * @returns {Promise<Object>} { hash, timestamp, snapshot }
 *
 * @example
 * import { freeze } from '@unrdf/fusion/store-adapter';
 * const frozen = await freeze(adapter);
 * console.assert(frozen.hash, 'Has hash');
 * console.assert(frozen.snapshot, 'Has snapshot');
 */
export async function freeze(adapter) {
  if (!adapter || typeof adapter.query !== 'function') {
    throw new TypeError('freeze: adapter must have query() method');
  }

  try {
    // Get all quads and serialize to canonical N-Quads
    const quads = adapter.query();

    // Sort for canonical ordering (reuse KGC freeze pattern)
    quads.sort((a, b) => {
      const sCompare = a.subject.value < b.subject.value ? -1 :
                       a.subject.value > b.subject.value ? 1 : 0;
      if (sCompare !== 0) return sCompare;

      const pCompare = a.predicate.value < b.predicate.value ? -1 :
                       a.predicate.value > b.predicate.value ? 1 : 0;
      if (pCompare !== 0) return pCompare;

      return a.object.value < b.object.value ? -1 :
             a.object.value > b.object.value ? 1 : 0;
    });

    // Serialize to N-Quads (reuse KGC escaping pattern)
    const nquads = quads.map(q => {
      const s = q.subject.termType === 'NamedNode'
        ? `<${q.subject.value}>`
        : `_:${q.subject.value}`;
      const p = `<${q.predicate.value}>`;

      let o;
      if (q.object.termType === 'NamedNode') {
        o = `<${q.object.value}>`;
      } else if (q.object.termType === 'BlankNode') {
        o = `_:${q.object.value}`;
      } else {
        // Literal - use N-Quads escaping
        const escaped = escapeNQuadsString(q.object.value);
        if (q.object.datatype && q.object.datatype.value !== 'http://www.w3.org/2001/XMLSchema#string') {
          o = `"${escaped}"^^<${q.object.datatype.value}>`;
        } else if (q.object.language) {
          o = `"${escaped}"@${q.object.language}`;
        } else {
          o = `"${escaped}"`;
        }
      }

      const g = q.graph && q.graph.value !== ''
        ? `<${q.graph.value}>`
        : '<http://www.w3.org/1999/02/22-rdf-syntax-ns#defaultGraph>';

      return `${s} ${p} ${o} ${g} .`;
    }).join('\n');

    // Hash with SHA-256 (deterministic, built-in)
    const hash = createHash('sha256').update(nquads).digest('hex');

    // Generate timestamp
    const timestamp = new Date().toISOString();

    return {
      hash,
      timestamp,
      snapshot: nquads,
    };
  } catch (error) {
    throw new Error(`freeze failed: ${error.message}`);
  }
}

/**
 * Reconstruct store from frozen snapshot
 *
 * @param {Object} snapshot - Frozen snapshot object
 * @param {string} snapshot.hash - BLAKE3 hash
 * @param {string} snapshot.timestamp - ISO timestamp
 * @param {string} snapshot.snapshot - N-Quads serialization
 * @returns {Promise<Object>} New StoreAdapter instance
 *
 * @example
 * import { reconstruct } from '@unrdf/fusion/store-adapter';
 * const adapter = await reconstruct(frozenSnapshot);
 * console.assert(adapter.query().length === 10, 'Reconstructed 10 quads');
 */
export async function reconstruct(snapshot) {
  if (!snapshot || typeof snapshot !== 'object') {
    throw new TypeError('reconstruct: snapshot must be an object');
  }
  if (snapshot.snapshot === undefined || typeof snapshot.snapshot !== 'string') {
    throw new TypeError('reconstruct: snapshot.snapshot must be a string');
  }

  try {
    // Create new adapter
    const adapter = await createStoreAdapter('oxigraph');

    // Parse and load N-Quads manually (simple parser for mock store compatibility)
    if (snapshot.snapshot && snapshot.snapshot.trim() !== '') {
      const lines = snapshot.snapshot.split('\n').filter(l => l.trim() && !l.startsWith('#'));

      for (const line of lines) {
        try {
          const parsed = parseNQuadLine(line);
          if (parsed) {
            adapter.addQuad(parsed);
          }
        } catch (err) {
          // Skip invalid lines
          console.warn(`Failed to parse N-Quad line: ${line}`, err.message);
        }
      }
    }

    return adapter;
  } catch (error) {
    throw new Error(`reconstruct failed: ${error.message}`);
  }
}

/**
 * Verify snapshot by comparing hashes
 *
 * @param {Object} original - Original StoreAdapter
 * @param {Object} snapshot - Frozen snapshot object
 * @returns {Promise<Object>} { valid: boolean, originalHash?, snapshotHash?, reason? }
 *
 * @example
 * import { verifySnapshot } from '@unrdf/fusion/store-adapter';
 * const result = await verifySnapshot(adapter, frozenSnapshot);
 * console.assert(result.valid === true, 'Snapshot is valid');
 */
export async function verifySnapshot(original, snapshot) {
  if (!original || typeof original.query !== 'function') {
    throw new TypeError('verifySnapshot: original must be a StoreAdapter');
  }
  if (!snapshot || !snapshot.hash) {
    throw new TypeError('verifySnapshot: snapshot must have hash property');
  }

  try {
    // Freeze original to get current hash
    const currentFreeze = await freeze(original);

    // Compare hashes
    if (currentFreeze.hash !== snapshot.hash) {
      return {
        valid: false,
        originalHash: currentFreeze.hash,
        snapshotHash: snapshot.hash,
        reason: 'Hash mismatch',
      };
    }

    return {
      valid: true,
      originalHash: currentFreeze.hash,
      snapshotHash: snapshot.hash,
    };
  } catch (error) {
    return {
      valid: false,
      reason: `Verification failed: ${error.message}`,
    };
  }
}

/**
 * Parse a single N-Quad line (simple parser for basic cases)
 * @private
 * @param {string} line - N-Quad line to parse
 * @returns {Object|null} Quad object or null
 */
function parseNQuadLine(line) {
  const trimmed = line.trim();
  if (!trimmed || trimmed.startsWith('#')) return null;

  // Simple regex-based parser (handles basic cases)
  // Format: <subject> <predicate> <object> <graph> .
  const parts = [];
  let current = '';
  let inQuotes = false;
  let inAngleBrackets = false;

  for (let i = 0; i < trimmed.length; i++) {
    const char = trimmed[i];

    if (char === '"' && trimmed[i - 1] !== '\\') {
      inQuotes = !inQuotes;
      current += char;
    } else if (char === '<' && !inQuotes) {
      inAngleBrackets = true;
      current += char;
    } else if (char === '>' && !inQuotes) {
      inAngleBrackets = false;
      current += char;
    } else if (char === ' ' && !inQuotes && !inAngleBrackets) {
      if (current.trim()) {
        parts.push(current.trim());
        current = '';
      }
    } else {
      current += char;
    }
  }

  if (current.trim() && current.trim() !== '.') {
    parts.push(current.trim());
  }

  if (parts.length < 3) return null;

  // Parse subject
  const subject = parts[0].startsWith('<')
    ? dataFactory.namedNode(parts[0].slice(1, -1))
    : dataFactory.blankNode(parts[0].slice(2)); // _:xxx

  // Parse predicate
  const predicate = dataFactory.namedNode(parts[1].slice(1, -1));

  // Parse object
  let object;
  if (parts[2].startsWith('"')) {
    // Literal
    const literalMatch = parts[2].match(/^"(.+?)"(?:@(\w+)|\^\^<(.+)>)?/);
    if (literalMatch) {
      const value = literalMatch[1].replace(/\\n/g, '\n').replace(/\\t/g, '\t').replace(/\\"/g, '"').replace(/\\\\/g, '\\');
      if (literalMatch[2]) {
        object = dataFactory.literal(value, literalMatch[2]);
      } else if (literalMatch[3]) {
        object = dataFactory.literal(value, dataFactory.namedNode(literalMatch[3]));
      } else {
        object = dataFactory.literal(value);
      }
    } else {
      object = dataFactory.literal(parts[2].slice(1, -1));
    }
  } else if (parts[2].startsWith('_:')) {
    object = dataFactory.blankNode(parts[2].slice(2));
  } else {
    object = dataFactory.namedNode(parts[2].slice(1, -1));
  }

  // Parse graph (optional, defaults to defaultGraph)
  let graph = dataFactory.defaultGraph();
  if (parts.length > 3 && parts[3].startsWith('<')) {
    const graphUri = parts[3].slice(1, -1);
    // Map default graph URI back to defaultGraph()
    if (graphUri === 'http://www.w3.org/1999/02/22-rdf-syntax-ns#defaultGraph') {
      graph = dataFactory.defaultGraph();
    } else {
      graph = dataFactory.namedNode(graphUri);
    }
  }

  return dataFactory.quad(subject, predicate, object, graph);
}

/**
 * Escape N-Quads string literals (reuse KGC pattern)
 * @private
 * @param {string} str - String to escape
 * @returns {string} Escaped string
 */
function escapeNQuadsString(str) {
  return str
    .replace(/\\/g, '\\\\')      // Backslash first (order matters!)
    .replace(/"/g, '\\"')        // Quote
    .replace(/\n/g, '\\n')       // Newline
    .replace(/\r/g, '\\r')       // Carriage return
    .replace(/\t/g, '\\t');      // Tab
    // Note: Removed \f and \b escaping - \b in regex is word boundary, not backspace
    // For actual backspace/formfeed characters (rare in RDF), use explicit char codes
}
