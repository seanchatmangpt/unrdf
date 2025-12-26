/**
 * @file lens.mjs
 * @description Lens definition, compilation, and execution for API ↔ RDF mapping
 * @module agent-3/lens
 */

import { dataFactory } from '../../packages/oxigraph/src/index.mjs';
import { stableIRI, stableSkolem } from './stable-ids.mjs';

const { quad, namedNode, literal } = dataFactory;

/**
 * Define a lens mapping between DTO fields and RDF predicates
 *
 * @param {string} name - Lens name (e.g., "CustomerLens")
 * @param {Object} config - Lens configuration
 * @param {string} config.domain - Domain namespace (e.g., "kgc-facade")
 * @param {string} config.entity - Entity type (e.g., "customer")
 * @param {Array<Object>} config.rules - Mapping rules
 * @returns {Object} Lens object with metadata
 *
 * @example
 * const lens = defineLens("CustomerLens", {
 *   domain: "kgc-facade",
 *   entity: "customer",
 *   rules: [
 *     { dto_field: "id", rdf_predicate: "http://schema.org/identifier", type: "string" },
 *     { dto_field: "name", rdf_predicate: "http://schema.org/name", type: "string" }
 *   ]
 * });
 */
export function defineLens(name, config) {
  if (!name || typeof name !== 'string') {
    throw new Error('defineLens: name must be a non-empty string');
  }
  if (!config || typeof config !== 'object') {
    throw new Error('defineLens: config must be an object');
  }
  if (!config.domain || !config.entity) {
    throw new Error('defineLens: config must have domain and entity');
  }
  if (!Array.isArray(config.rules) || config.rules.length === 0) {
    throw new Error('defineLens: config.rules must be a non-empty array');
  }

  // Validate each rule
  for (const rule of config.rules) {
    if (!rule.dto_field || !rule.rdf_predicate || !rule.type) {
      throw new Error('defineLens: each rule must have dto_field, rdf_predicate, and type');
    }
  }

  return {
    name,
    domain: config.domain,
    entity: config.entity,
    rules: config.rules,
    version: '1.0.0',
    createdAt: new Date().toISOString()
  };
}

/**
 * Compile a lens definition into an executable JSON program
 *
 * LensProgram is fully serializable (no closures) and portable
 * Can be stored, transmitted, and executed on any system
 *
 * @param {Object} lens - Lens object from defineLens
 * @returns {Object} LensProgram (JSON-serializable)
 *
 * @example
 * const program = compileLens(customerLens);
 * // program.toGraph and program.fromGraph contain executable rules
 */
export function compileLens(lens) {
  if (!lens || !lens.name || !lens.rules) {
    throw new Error('compileLens: invalid lens object');
  }

  // Compile toGraph rules (DTO → RDF)
  const toGraph = lens.rules.map(rule => ({
    dto_field: rule.dto_field,
    rdf_predicate: rule.rdf_predicate,
    type: rule.type,
    transform: rule.transform || 'identity',
    validator: rule.validator || null
  }));

  // Compile fromGraph rules (RDF → DTO)
  const fromGraph = lens.rules.map(rule => ({
    rdf_predicate: rule.rdf_predicate,
    dto_field: rule.dto_field,
    type: rule.type,
    transform: rule.transform || 'identity',
    validator: rule.validator || null
  }));

  return {
    name: lens.name,
    version: lens.version || '1.0.0',
    stableIds: {
      domain: lens.domain,
      entity: lens.entity
    },
    toGraph,
    fromGraph
  };
}

/**
 * Execute lens to transform DTO payload → RDF quads
 *
 * Generates stable IRIs for subjects using stableIRI()
 * Returns both quads and subject IRIs for downstream processing
 *
 * @param {Object} dtoPayload - DTO object to transform
 * @param {Object} lensProgram - Compiled lens program
 * @returns {Object} { quads: Array<Quad>, subjects: Array<string> }
 *
 * @example
 * const result = executeLensToGraph(
 *   { id: "123", name: "Alice" },
 *   customerLensProgram
 * );
 * // result.quads → array of RDF quads
 * // result.subjects → ["http://kgc.internal/...#abc123"]
 */
export function executeLensToGraph(dtoPayload, lensProgram) {
  if (!dtoPayload || typeof dtoPayload !== 'object') {
    throw new Error('executeLensToGraph: dtoPayload must be an object');
  }
  if (!lensProgram || !lensProgram.toGraph || !lensProgram.stableIds) {
    throw new Error('executeLensToGraph: invalid lensProgram');
  }

  const quads = [];
  const subjects = [];

  // Generate stable subject IRI
  // Use the ID field or a composite key for stability
  const idField = lensProgram.toGraph.find(rule =>
    rule.dto_field === 'id' || rule.dto_field.includes('Id')
  );

  const subjectId = idField
    ? dtoPayload[idField.dto_field]
    : JSON.stringify(dtoPayload); // Fallback: hash entire object

  const subject = stableIRI(
    lensProgram.stableIds.domain,
    lensProgram.stableIds.entity,
    String(subjectId)
  );
  subjects.push(subject);

  const subjectNode = namedNode(subject);

  // Transform each DTO field → RDF quad
  for (const rule of lensProgram.toGraph) {
    const value = dtoPayload[rule.dto_field];

    if (value === undefined || value === null) {
      continue; // Skip undefined/null values
    }

    const predicate = namedNode(rule.rdf_predicate);

    // Apply transform (currently only 'identity' supported)
    const transformedValue = applyTransform(value, rule.transform);

    // Create literal with appropriate datatype
    const object = createLiteral(transformedValue, rule.type);

    quads.push(quad(subjectNode, predicate, object));
  }

  return { quads, subjects };
}

/**
 * Execute lens to transform RDF quads → DTO payload
 *
 * Queries store for subject IRIs and projects back to DTO shape
 * Reconstructs original DTO structure with round-trip fidelity
 *
 * @param {Array<string>} subjects - Subject IRIs to query
 * @param {Object} store - Oxigraph store instance
 * @param {Object} lensProgram - Compiled lens program
 * @returns {Object} Reconstructed DTO payload
 *
 * @example
 * const dto = executeLensFromGraph(
 *   ["http://kgc.internal/...#abc123"],
 *   store,
 *   customerLensProgram
 * );
 * // dto → { id: "123", name: "Alice" }
 */
export function executeLensFromGraph(subjects, store, lensProgram) {
  if (!Array.isArray(subjects) || subjects.length === 0) {
    throw new Error('executeLensFromGraph: subjects must be a non-empty array');
  }
  if (!store || typeof store.match !== 'function') {
    throw new Error('executeLensFromGraph: store must have match() method');
  }
  if (!lensProgram || !lensProgram.fromGraph) {
    throw new Error('executeLensFromGraph: invalid lensProgram');
  }

  const dto = {};
  const subject = namedNode(subjects[0]); // Use first subject

  // Query store for all predicates
  for (const rule of lensProgram.fromGraph) {
    const predicate = namedNode(rule.rdf_predicate);

    // Query: (subject, predicate, ?)
    const quads = Array.from(store.match(subject, predicate, null));

    if (quads.length === 0) {
      continue; // Field not present in RDF
    }

    // Extract object value
    const quad = quads[0];
    const objectValue = extractLiteralValue(quad.object, rule.type);

    // Apply reverse transform
    const transformedValue = applyTransform(objectValue, rule.transform);

    dto[rule.dto_field] = transformedValue;
  }

  return dto;
}

/**
 * Apply a transform function to a value
 *
 * @param {*} value - Input value
 * @param {string} transform - Transform type ("identity", etc.)
 * @returns {*} Transformed value
 * @private
 */
function applyTransform(value, transform) {
  switch (transform) {
    case 'identity':
      return value;
    // Future: Add more transforms (uppercase, lowercase, date parsing, etc.)
    default:
      return value;
  }
}

/**
 * Create an RDF literal with appropriate datatype
 *
 * @param {*} value - JavaScript value
 * @param {string} type - Type hint ("string", "number", "boolean", "date")
 * @returns {Literal} RDF literal term
 * @private
 */
function createLiteral(value, type) {
  switch (type) {
    case 'string':
      return literal(String(value));
    case 'number':
      return literal(String(value), namedNode('http://www.w3.org/2001/XMLSchema#double'));
    case 'integer':
      return literal(String(value), namedNode('http://www.w3.org/2001/XMLSchema#integer'));
    case 'boolean':
      return literal(String(value), namedNode('http://www.w3.org/2001/XMLSchema#boolean'));
    case 'date':
    case 'datetime':
      return literal(String(value), namedNode('http://www.w3.org/2001/XMLSchema#dateTime'));
    default:
      return literal(String(value));
  }
}

/**
 * Extract JavaScript value from RDF literal
 *
 * @param {Term} term - RDF term (literal or named node)
 * @param {string} type - Expected type
 * @returns {*} JavaScript value
 * @private
 */
function extractLiteralValue(term, type) {
  if (term.termType !== 'Literal') {
    return term.value; // Return as-is for named nodes
  }

  const value = term.value;

  switch (type) {
    case 'string':
      return value;
    case 'number':
    case 'integer':
      return Number(value);
    case 'boolean':
      return value === 'true';
    case 'date':
    case 'datetime':
      return value; // Keep as ISO string for round-trip fidelity
    default:
      return value;
  }
}
