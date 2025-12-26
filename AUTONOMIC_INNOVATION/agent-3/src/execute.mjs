/**
 * @fileoverview Execute compiled lens (bidirectional transformation)
 * @module @unrdf/lens/execute
 */

import { dataFactory } from '@unrdf/oxigraph';
import { createStableIRI, extractFromIRI } from './skolem.mjs';

const { namedNode, literal, quad: createQuad } = dataFactory;

/**
 * Transform API payload to RDF quads using compiled lens
 * @param {Object} payload - API payload (e.g., { id: "123", name: "Alice" })
 * @param {import('./compiler.mjs').CompiledLens} compiledLens - Compiled lens program
 * @param {import('@unrdf/oxigraph').Store} store - Oxigraph store to add quads
 * @param {string} [entityType] - Entity type (auto-detected if not provided)
 * @returns {import('@unrdf/oxigraph').Quad[]} Generated quads (sorted)
 *
 * @example
 * const payload = { id: "123", name: "Alice", email: "alice@example.com" };
 * const quads = executeLensToGraph(payload, compiledLens, store);
 * console.log(quads.length); // 4 (type + 3 properties)
 */
export function executeLensToGraph(payload, compiledLens, store, entityType) {
  // Validate inputs
  if (!payload || typeof payload !== 'object') {
    throw new Error('Invalid payload: must be object');
  }

  if (!compiledLens || !compiledLens.compiledMappings) {
    throw new Error('Invalid compiled lens');
  }

  // Determine entity type
  if (!entityType) {
    // Try to detect from payload root key or use first mapping
    const mappingKeys = Object.keys(compiledLens.compiledMappings);
    entityType = mappingKeys[0]; // Default to first mapping

    // Check if payload has a type field
    if (payload.type &amp;&amp; compiledLens.compiledMappings[payload.type]) {
      entityType = payload.type;
    }
  }

  // Get compiled mapping
  const mapping = compiledLens.compiledMappings[entityType];
  if (!mapping) {
    throw new Error(`No mapping found for entity type: ${entityType}`);
  }

  // Generate subject IRI
  const subjectIRI = createStableIRI(
    payload,
    compiledLens.lensId,
    mapping.subject,
    compiledLens.profile.namespace,
    entityType
  );

  const subject = namedNode(subjectIRI);
  const quads = [];

  // Add rdf:type triple if specified
  if (mapping.type) {
    const typeQuad = createQuad(
      subject,
      namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      namedNode(mapping.type)
    );
    quads.push(typeQuad);
    store.add(typeQuad);
  }

  // Process predicates (already sorted in compilation)
  for (const predicate of mapping.predicates) {
    const value = payload[predicate.property];

    // Skip if value is undefined/null (unless required)
    if (value === undefined || value === null) {
      if (predicate.required) {
        throw new Error(`Missing required field: ${predicate.property}`);
      }
      continue;
    }

    // Skip empty strings unless required
    if (value === '' && !predicate.required) {
      continue;
    }

    // Create object (literal with optional datatype)
    let object;
    if (predicate.datatype) {
      object = literal(String(value), namedNode(predicate.datatype));
    } else {
      // Infer datatype from value
      if (typeof value === 'number') {
        const dt = Number.isInteger(value)
          ? 'http://www.w3.org/2001/XMLSchema#integer'
          : 'http://www.w3.org/2001/XMLSchema#decimal';
        object = literal(String(value), namedNode(dt));
      } else if (typeof value === 'boolean') {
        object = literal(String(value), namedNode('http://www.w3.org/2001/XMLSchema#boolean'));
      } else {
        object = literal(String(value), namedNode('http://www.w3.org/2001/XMLSchema#string'));
      }
    }

    // Create quad
    const q = createQuad(
      subject,
      namedNode(predicate.iri),
      object
    );

    quads.push(q);
    store.add(q);
  }

  // Sort quads by predicate IRI for determinism
  quads.sort((a, b) => {
    return a.predicate.value.localeCompare(b.predicate.value);
  });

  return quads;
}

/**
 * Extract API payload from RDF quads using compiled lens
 * @param {string} subjectIRI - Subject IRI to extract
 * @param {import('./compiler.mjs').CompiledLens} compiledLens - Compiled lens program
 * @param {import('@unrdf/oxigraph').Store} store - Oxigraph store with quads
 * @param {string} [entityType] - Entity type (auto-detected from rdf:type if not provided)
 * @returns {Object} Reconstructed payload
 *
 * @example
 * const payload = executeLensFromGraph(
 *   "https://example.org/Customer/123",
 *   compiledLens,
 *   store
 * );
 * console.log(payload); // { id: "123", name: "Alice", email: "alice@example.com" }
 */
export function executeLensFromGraph(subjectIRI, compiledLens, store, entityType) {
  // Validate inputs
  if (!subjectIRI || typeof subjectIRI !== 'string') {
    throw new Error('Invalid subject IRI');
  }

  if (!compiledLens || !compiledLens.compiledMappings) {
    throw new Error('Invalid compiled lens');
  }

  const subject = namedNode(subjectIRI);

  // Determine entity type from rdf:type if not provided
  if (!entityType) {
    const typeQuads = store.match(
      subject,
      namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
      null
    );

    for (const quad of typeQuads) {
      const typeIRI = quad.object.value;
      // Find mapping with matching type
      for (const [mappingKey, mapping] of Object.entries(compiledLens.compiledMappings)) {
        if (mapping.type === typeIRI) {
          entityType = mappingKey;
          break;
        }
      }
      if (entityType) break;
    }

    // Fallback: use first mapping
    if (!entityType) {
      entityType = Object.keys(compiledLens.compiledMappings)[0];
    }
  }

  // Get compiled mapping
  const mapping = compiledLens.compiledMappings[entityType];
  if (!mapping) {
    throw new Error(`No mapping found for entity type: ${entityType}`);
  }

  // Initialize payload
  const payload = {};

  // Extract ID from IRI using subject pattern
  const extracted = extractFromIRI(
    subjectIRI,
    mapping.subject.pattern,
    compiledLens.profile.namespace
  );

  // Add extracted ID fields to payload
  for (const key of mapping.subject.keys) {
    if (extracted[key]) {
      payload[key] = extracted[key];
    }
  }

  // Process predicates
  for (const predicate of mapping.predicates) {
    const quads = store.match(
      subject,
      namedNode(predicate.iri),
      null
    );

    for (const quad of quads) {
      // Extract value from literal
      let value = quad.object.value;

      // Type coercion based on datatype
      if (quad.object.datatype) {
        const dt = quad.object.datatype.value;
        if (dt === 'http://www.w3.org/2001/XMLSchema#integer') {
          value = parseInt(value, 10);
        } else if (dt === 'http://www.w3.org/2001/XMLSchema#decimal' || dt === 'http://www.w3.org/2001/XMLSchema#double') {
          value = parseFloat(value);
        } else if (dt === 'http://www.w3.org/2001/XMLSchema#boolean') {
          value = value === 'true';
        }
      }

      payload[predicate.property] = value;
      break; // Take first value (assuming single-valued predicates)
    }
  }

  return payload;
}

/**
 * Verify round-trip losslessness
 * @param {Object} payload - Original payload
 * @param {import('./compiler.mjs').CompiledLens} compiledLens - Compiled lens
 * @param {import('@unrdf/oxigraph').Store} store - Store instance
 * @param {string} [entityType] - Entity type
 * @returns {boolean} True if round-trip is lossless
 */
export function verifyRoundTrip(payload, compiledLens, store, entityType) {
  // Transform to graph
  const quads = executeLensToGraph(payload, compiledLens, store, entityType);

  if (quads.length === 0) {
    return false;
  }

  // Transform back to payload
  const reconstructed = executeLensFromGraph(quads[0].subject.value, compiledLens, store, entityType);

  // Deep equality check
  return JSON.stringify(payload) === JSON.stringify(reconstructed);
}
