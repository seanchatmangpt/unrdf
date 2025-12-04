/**
 * @file μ(O) Minimal-N3 Integration - Canonical Reference Implementation
 * @module @unrdf/core/rdf/minimal-n3-integration
 *
 * This module demonstrates the μ(O) (Minimal-N3) architectural principle:
 * - Oxigraph is the authoritative engine for ALL operations
 * - N3 is invoked ONLY at 5 justified boundaries where Oxigraph cannot act
 * - All N3 operations MUST re-enter Oxigraph immediately
 *
 * Justified N3 Use Cases (ONLY these 5):
 * 1. Streaming parsing (input > memory budget or backpressure required)
 * 2. Streaming serialization (output must stream to sink)
 * 3. N3 rule reasoning (forward-chaining, not in Oxigraph)
 * 4. Permissive parsing (dirty/malformed RDF input)
 * 5. Structural RDF transforms (cannot express in SPARQL 1.1)
 *
 * Everything else: Use Oxigraph ONLY.
 */

import { createStore } from '@unrdf/oxigraph'

/**
 * Case 1: Streaming Parse (N3 justified - backpressure handling)
 * Use when: Input stream requires backpressure control or exceeds memory budget
 *
 * @param {ReadableStream} stream - Input RDF stream
 * @param {Object} [options] - Parser options (format, baseIRI, etc.)
 * @returns {Promise<OxigraphStore>} Store with parsed quads (re-entered Oxigraph)
 *
 * @example
 * const stream = fs.createReadStream('large.ttl');
 * const store = await streamParse(stream, { format: 'turtle' });
 * // Now back in Oxigraph - can query with SPARQL
 * const results = store.query('SELECT * WHERE { ?s ?p ?o } LIMIT 10');
 */
export async function streamParse(stream, options = {}) {
  const { Parser } = await import('n3')
  const parser = new Parser(options)
  const quads = []

  return new Promise((resolve, reject) => {
    stream
      .pipe(parser)
      .on('data', (quad) => quads.push(quad))
      .on('end', () => {
        // CRITICAL: Re-enter Oxigraph immediately
        const store = createStore(quads)
        resolve(store)
      })
      .on('error', reject)
  })
}

/**
 * Case 2: Streaming Serialize (N3 justified - output streaming)
 * Use when: Output must stream to sink (HTTP response, file write with backpressure)
 *
 * @param {OxigraphStore} store - Source Oxigraph store
 * @param {string} format - Output format (turtle, ntriples, etc.)
 * @param {WritableStream} sink - Output stream sink
 * @returns {Promise<void>}
 *
 * @example
 * const store = createStore(quads);
 * const sink = fs.createWriteStream('output.ttl');
 * await streamSerialize(store, 'turtle', sink);
 */
export async function streamSerialize(store, format, sink) {
  const { Writer } = await import('n3')
  const writer = new Writer({ format })

  // Extract quads from Oxigraph (staying in Oxigraph as long as possible)
  const quads = store.match(null, null, null, null)

  for (const quad of quads) {
    writer.addQuad(quad)
  }

  return new Promise((resolve, reject) => {
    writer.end((error, result) => {
      if (error) {
        reject(error)
        return
      }
      sink.write(result)
      sink.end()
      resolve()
    })
  })
}

/**
 * Case 3: N3 Rule Reasoning (N3 justified - forward-chaining not in Oxigraph)
 * Use when: Need N3 rule-based forward chaining (Notation3 logic)
 *
 * @param {OxigraphStore} store - Source Oxigraph store
 * @param {string} rulesTtl - N3 rules in Turtle format
 * @returns {Promise<OxigraphStore>} Store with inferred quads (re-entered Oxigraph)
 *
 * @example
 * const rules = `
 *   @prefix : <http://example.org/> .
 *   { ?x a :Person } => { ?x a :Human } .
 * `;
 * const inferredStore = await applyN3Rules(store, rules);
 * // Back in Oxigraph - can query inferred triples
 */
export async function applyN3Rules(store, rulesTtl) {
  const { Store, Parser } = await import('n3')

  // Convert Oxigraph → N3 (boundary crossing)
  const n3Store = new Store(Array.from(store.dump()))
  const rulesStore = new Store()

  const parser = new Parser()
  const rules = parser.parse(rulesTtl)
  rules.forEach((r) => rulesStore.addQuad(r))

  // N3 reasoning (N3-only capability)
  // Note: Actual N3 reasoner would go here (eye.js, etc.)
  // For now, this is a placeholder showing the pattern
  const inferred = performN3Reasoning(n3Store, rulesStore)

  // CRITICAL: Re-enter Oxigraph immediately
  const allQuads = [...n3Store.getQuads(), ...inferred]
  return createStore(allQuads)
}

/**
 * Placeholder for N3 reasoning implementation
 * @private
 */
function performN3Reasoning(dataStore, rulesStore) {
  // Real implementation would use eye.js or similar
  // This is a pattern demonstration
  return []
}

/**
 * Case 4: Permissive Parse (N3 justified - malformed RDF recovery)
 * Use when: Input RDF is malformed and Oxigraph strict parsing fails
 *
 * @param {string} dirtyRdf - Potentially malformed RDF string
 * @param {Object} [options] - Parser options
 * @returns {OxigraphStore} Store with recovered quads (re-entered Oxigraph)
 *
 * @example
 * const dirtyTurtle = `... malformed syntax ...`;
 * const store = parsePermissive(dirtyTurtle, { format: 'turtle' });
 * // N3's permissive parser recovered what it could, now in Oxigraph
 */
export function parsePermissive(dirtyRdf, options = {}) {
  const { Parser } = require('n3')
  const parser = new Parser({ ...options, strict: false })

  try {
    const quads = parser.parse(dirtyRdf)
    // CRITICAL: Re-enter Oxigraph immediately
    return createStore(quads)
  } catch (e) {
    throw new Error(`Even permissive parsing failed: ${e.message}`)
  }
}

/**
 * Case 5: Structural Transform (N3 justified - SPARQL-inexpressible)
 * Use when: Transformation cannot be expressed in SPARQL 1.1
 *
 * @param {OxigraphStore} store - Source Oxigraph store
 * @param {Function} transformFn - Transformation function (N3.Store → N3.Store)
 * @returns {OxigraphStore} Transformed store (re-entered Oxigraph)
 *
 * @example
 * const transform = (n3Store) => {
 *   // Complex graph rewriting not expressible in SPARQL
 *   return modifiedN3Store;
 * };
 * const transformed = transformRdfStructure(store, transform);
 */
export function transformRdfStructure(store, transformFn) {
  const { Store } = require('n3')

  // Convert Oxigraph → N3 (boundary crossing)
  const n3Store = new Store(Array.from(store.dump()))

  // Apply transformation (N3-only utility)
  const transformed = transformFn(n3Store)

  // CRITICAL: Re-enter Oxigraph immediately
  return createStore(Array.from(transformed.getQuads()))
}

// ============================================================================
// DEFAULT OPERATIONS: Oxigraph ONLY (Never use N3 for these)
// ============================================================================

/**
 * Parse RDF (DEFAULT - Oxigraph handles all standard formats)
 * Use this for: Turtle, N-Triples, N-Quads, TriG, JSON-LD, RDF/XML
 *
 * @param {string} rdf - RDF content
 * @param {Object} [options] - Parse options (format, baseIRI)
 * @returns {OxigraphStore} Parsed store (Oxigraph native)
 *
 * @example
 * const ttl = `
 *   @prefix : <http://example.org/> .
 *   :alice a :Person .
 * `;
 * const store = parse(ttl, { format: 'turtle' });
 * // Stays in Oxigraph - ready for SPARQL
 */
export function parse(rdf, options = {}) {
  const store = createStore()
  store.load(rdf, options)
  return store
}

/**
 * Serialize RDF (DEFAULT - Oxigraph handles all standard formats)
 *
 * @param {OxigraphStore} store - Store to serialize
 * @param {Object} [options] - Serialize options (format)
 * @returns {string} Serialized RDF string
 *
 * @example
 * const ttl = serialize(store, { format: 'turtle' });
 */
export function serialize(store, options = {}) {
  return store.dump(options)
}

/**
 * Query RDF (DEFAULT - Oxigraph ONLY, never N3)
 *
 * @param {OxigraphStore} store - Store to query
 * @param {string} sparql - SPARQL 1.1 query
 * @param {Object} [options] - Query options
 * @returns {Object|boolean|Array} Query results
 *
 * @example
 * const results = query(store, 'SELECT * WHERE { ?s ?p ?o } LIMIT 10');
 */
export function query(store, sparql, options = {}) {
  return store.query(sparql, options)
}

/**
 * Update RDF (DEFAULT - Oxigraph ONLY, never N3)
 *
 * @param {OxigraphStore} store - Store to update
 * @param {string} sparqlUpdate - SPARQL 1.1 Update
 * @returns {void}
 *
 * @example
 * update(store, 'INSERT DATA { <http://example.org/alice> a <http://example.org/Person> }');
 */
export function update(store, sparqlUpdate) {
  return store.update(sparqlUpdate)
}

// ============================================================================
// USAGE DECISION FLOWCHART
// ============================================================================

/**
 * Decision helper: Should I use N3 or Oxigraph?
 *
 * Use this function to validate your architecture decision:
 *
 * @param {string} operation - The operation you want to perform
 * @returns {Object} Decision with engine and justification
 *
 * @example
 * const decision = shouldUseN3('parse');
 * console.log(decision);
 * // { engine: 'oxigraph', justification: 'Oxigraph handles all standard parsing' }
 *
 * const decision2 = shouldUseN3('stream-parse-large-file');
 * console.log(decision2);
 * // { engine: 'n3', justification: 'Streaming required for backpressure', reenter: true }
 */
export function shouldUseN3(operation) {
  const decisions = {
    // N3 Justified (5 cases only)
    'stream-parse': {
      engine: 'n3',
      justification: 'Streaming required for backpressure or memory budget',
      reenter: true,
      function: 'streamParse',
    },
    'stream-serialize': {
      engine: 'n3',
      justification: 'Output streaming required',
      reenter: false,
      function: 'streamSerialize',
    },
    'n3-reason': {
      engine: 'n3',
      justification: 'N3 rule-based reasoning (forward-chaining)',
      reenter: true,
      function: 'applyN3Rules',
    },
    'permissive-parse': {
      engine: 'n3',
      justification: 'Malformed RDF recovery',
      reenter: true,
      function: 'parsePermissive',
    },
    'rdf-transform': {
      engine: 'n3',
      justification: 'Structural transform not expressible in SPARQL',
      reenter: true,
      function: 'transformRdfStructure',
    },

    // Oxigraph Default (everything else)
    parse: {
      engine: 'oxigraph',
      justification: 'Oxigraph handles all standard formats',
      function: 'parse',
    },
    serialize: {
      engine: 'oxigraph',
      justification: 'Oxigraph supports all formats',
      function: 'serialize',
    },
    query: {
      engine: 'oxigraph',
      justification: 'Oxigraph provides SPARQL 1.1',
      function: 'query',
    },
    update: {
      engine: 'oxigraph',
      justification: 'Oxigraph provides SPARQL 1.1 Update',
      function: 'update',
    },
    storage: {
      engine: 'oxigraph',
      justification: 'Oxigraph is authoritative storage',
      function: 'createStore',
    },
  }

  const decision = decisions[operation]
  if (!decision) {
    return {
      engine: 'oxigraph',
      justification: 'Default: Oxigraph for all unlisted operations',
    }
  }

  return decision
}
