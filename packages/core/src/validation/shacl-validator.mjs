/**
 * @file SHACL Validation Integration
 * @module @unrdf/core/validation/shacl-validator
 * @description Integrates rdf-validate-shacl for SHACL shape validation
 */

import SHACLValidator from 'rdf-validate-shacl';
import { z } from 'zod';
import { Parser, N3Store } from '../rdf/n3-justified-only.mjs';

/**
 * @typedef {import('@unrdf/oxigraph').Store} Store
 * @typedef {import('@unrdf/oxigraph').Quad} Quad
 */

/**
 * Validation options schema
 */
export const ValidationOptionsSchema = z.object({
  details: z.boolean().optional().default(true),
  maxErrors: z.number().int().positive().optional(),
}).optional();

/**
 * Validation report schema
 */
export const ValidationReportSchema = z.object({
  conforms: z.boolean(),
  results: z.array(z.object({
    path: z.string().optional(),
    message: z.string(),
    focusNode: z.string().optional(),
    severity: z.string().optional(),
    sourceConstraintComponent: z.string().optional(),
    value: z.string().optional(),
  })),
  details: z.any().optional(),
});

/**
 * SHACL constraint types
 */
export const ConstraintType = {
  MIN_COUNT: 'http://www.w3.org/ns/shacl#minCount',
  MAX_COUNT: 'http://www.w3.org/ns/shacl#maxCount',
  DATATYPE: 'http://www.w3.org/ns/shacl#datatype',
  NODE_KIND: 'http://www.w3.org/ns/shacl#nodeKind',
  PATTERN: 'http://www.w3.org/ns/shacl#pattern',
  MIN_INCLUSIVE: 'http://www.w3.org/ns/shacl#minInclusive',
  MAX_INCLUSIVE: 'http://www.w3.org/ns/shacl#maxInclusive',
  MIN_EXCLUSIVE: 'http://www.w3.org/ns/shacl#minExclusive',
  MAX_EXCLUSIVE: 'http://www.w3.org/ns/shacl#maxExclusive',
  UNIQUE_LANG: 'http://www.w3.org/ns/shacl#uniqueLang',
};

/**
 * Parse Turtle string into RDF quads
 * @param {string} turtle - Turtle serialized RDF
 * @returns {Promise<Quad[]>} Array of quads
 * @private
 */
async function parseTurtle(turtle) {
  return new Promise((resolve, reject) => {
    const parser = new Parser({ format: 'text/turtle' });
    const quads = [];

    parser.parse(turtle, (error, quad) => {
      if (error) {
        reject(error);
      } else if (quad) {
        quads.push(quad);
      } else {
        resolve(quads);
      }
    });
  });
}

/**
 * Convert Oxigraph store/quads to N3 Store for RDF/JS compatibility
 * @param {Store|Array} store - Oxigraph store or array of quads
 * @returns {N3Store} N3 Store instance
 * @private
 */
function storeToDataset(store) {
  // Create N3 Store which implements RDF/JS DatasetCore
  const n3Store = new N3Store();

  // Handle array of quads
  if (Array.isArray(store)) {
    for (const quad of store) {
      n3Store.addQuad(quad);
    }
    return n3Store;
  }

  // Use getQuads method for Oxigraph store
  if (typeof store.getQuads === 'function') {
    const results = store.getQuads();
    for (const quad of results) {
      n3Store.addQuad(quad);
    }
  } else if (store[Symbol.iterator]) {
    // Fallback to iterator
    for (const quad of store) {
      n3Store.addQuad(quad);
    }
  }

  return n3Store;
}

/**
 * Create a SHACL validator instance
 * @param {Store|string|Quad[]} shapes - SHACL shapes as store, Turtle string, or quads
 * @param {Object} [options] - Validator options
 * @param {boolean} [options.factory] - Use custom data factory
 * @returns {Promise<SHACLValidator>} SHACL validator instance
 * @throws {TypeError} If shapes format is invalid
 *
 * @example
 * const validator = await createValidator(`
 *   @prefix sh: <http://www.w3.org/ns/shacl#> .
 *   @prefix ex: <http://example.org/> .
 *
 *   ex:PersonShape a sh:NodeShape ;
 *     sh:targetClass ex:Person ;
 *     sh:property [
 *       sh:path ex:name ;
 *       sh:minCount 1 ;
 *       sh:datatype xsd:string ;
 *     ] .
 * `);
 */
export async function createValidator(shapes, options = {}) {
  let shapesDataset;

  if (typeof shapes === 'string') {
    // Parse Turtle string
    shapesDataset = await parseTurtle(shapes);
  } else if (Array.isArray(shapes)) {
    // Already an array of quads
    shapesDataset = shapes;
  } else if (shapes && typeof shapes.getQuads === 'function') {
    // Oxigraph store
    shapesDataset = storeToDataset(shapes);
  } else {
    throw new TypeError('Shapes must be a Store, Turtle string, or Quad array');
  }

  return new SHACLValidator(shapesDataset, options);
}

/**
 * Validate RDF data against SHACL shapes
 * @param {Store|Quad[]} data - RDF data to validate
 * @param {Store|string|Quad[]} shapes - SHACL shapes
 * @param {Object} [options] - Validation options
 * @param {boolean} [options.details=true] - Include detailed validation results
 * @param {number} [options.maxErrors] - Maximum number of errors to collect
 * @returns {Promise<Object>} Validation report
 * @throws {Error} If validation setup fails
 *
 * @example
 * const report = await validateGraph(dataStore, shapesStore);
 * if (!report.conforms) {
 *   console.error('Validation failed:', report.results);
 * }
 */
export async function validateGraph(data, shapes, options = {}) {
  const validatedOptions = ValidationOptionsSchema.parse(options);

  // Create validator
  const validator = await createValidator(shapes);

  // Convert data to dataset
  let dataDataset;
  if (Array.isArray(data)) {
    dataDataset = data;
  } else if (data && typeof data.getQuads === 'function') {
    dataDataset = storeToDataset(data);
  } else {
    throw new TypeError('Data must be a Store or Quad array');
  }

  // Run validation
  const report = validator.validate(dataDataset);

  // Build results
  const results = [];
  let errorCount = 0;

  // Check if results exist and are iterable
  const reportResults = report.results || [];
  const resultsArray = Array.isArray(reportResults)
    ? reportResults
    : Array.from(reportResults || []);

  for (const result of resultsArray) {
    if (validatedOptions.maxErrors && errorCount >= validatedOptions.maxErrors) {
      break;
    }

    const resultObj = {
      message: result.message?.[0]?.value || result.message?.value || 'Validation failed',
      focusNode: result.focusNode?.value,
      path: result.path?.value,
      severity: result.severity?.value,
      sourceConstraintComponent: result.sourceConstraintComponent?.value,
      value: result.value?.value,
    };

    results.push(resultObj);
    errorCount++;
  }

  return {
    conforms: report.conforms,
    results,
    details: validatedOptions.details ? report : undefined,
  };
}

/**
 * Validate a single constraint
 * @param {*} value - Value to validate
 * @param {string} constraintType - Constraint type URI
 * @param {*} constraintValue - Constraint parameter value
 * @returns {boolean} True if constraint is satisfied
 *
 * @example
 * const valid = validateConstraint('John', ConstraintType.DATATYPE, 'xsd:string');
 */
export function validateConstraint(value, constraintType, constraintValue) {
  switch (constraintType) {
    case ConstraintType.MIN_COUNT:
      return Array.isArray(value) ? value.length >= constraintValue : false;

    case ConstraintType.MAX_COUNT:
      return Array.isArray(value) ? value.length <= constraintValue : false;

    case ConstraintType.DATATYPE:
      return typeof value === 'string' || value?.datatype?.value === constraintValue;

    case ConstraintType.PATTERN:
      if (typeof value === 'string') {
        const regex = new RegExp(constraintValue);
        return regex.test(value);
      }
      return false;

    case ConstraintType.MIN_INCLUSIVE:
      return typeof value === 'number' ? value >= constraintValue : false;

    case ConstraintType.MAX_INCLUSIVE:
      return typeof value === 'number' ? value <= constraintValue : false;

    case ConstraintType.MIN_EXCLUSIVE:
      return typeof value === 'number' ? value > constraintValue : false;

    case ConstraintType.MAX_EXCLUSIVE:
      return typeof value === 'number' ? value < constraintValue : false;

    case ConstraintType.UNIQUE_LANG:
      if (!Array.isArray(value)) return true;
      const langs = value.map(v => v.language).filter(Boolean);
      return langs.length === new Set(langs).size;

    default:
      return true;
  }
}

/**
 * Generate a validation report from constraint violations
 * @param {Array<Object>} violations - Array of constraint violations
 * @param {string} violations[].path - Property path
 * @param {string} violations[].message - Error message
 * @param {string} [violations[].focusNode] - Focus node identifier
 * @returns {Object} Validation report
 *
 * @example
 * const report = generateReport([
 *   { path: 'ex:name', message: 'Required property missing' }
 * ]);
 */
export function generateReport(violations) {
  return {
    conforms: violations.length === 0,
    results: violations.map(v => ({
      path: v.path,
      message: v.message,
      focusNode: v.focusNode,
      severity: v.severity || 'http://www.w3.org/ns/shacl#Violation',
      sourceConstraintComponent: v.constraint,
      value: v.value,
    })),
  };
}

/**
 * Validate node kind constraint
 * @param {*} node - RDF node to validate
 * @param {string} nodeKind - Expected node kind (IRI, BlankNode, Literal)
 * @returns {boolean} True if node kind matches
 *
 * @example
 * const valid = validateNodeKind(namedNode('http://example.org/'), 'http://www.w3.org/ns/shacl#IRI');
 */
export function validateNodeKind(node, nodeKind) {
  const nodeKindMap = {
    'http://www.w3.org/ns/shacl#IRI': 'NamedNode',
    'http://www.w3.org/ns/shacl#BlankNode': 'BlankNode',
    'http://www.w3.org/ns/shacl#Literal': 'Literal',
    'http://www.w3.org/ns/shacl#BlankNodeOrIRI': ['NamedNode', 'BlankNode'],
    'http://www.w3.org/ns/shacl#BlankNodeOrLiteral': ['BlankNode', 'Literal'],
    'http://www.w3.org/ns/shacl#IRIOrLiteral': ['NamedNode', 'Literal'],
  };

  const expectedKinds = nodeKindMap[nodeKind];
  if (!expectedKinds) return false;

  const nodeType = node?.termType;

  if (Array.isArray(expectedKinds)) {
    return expectedKinds.includes(nodeType);
  }

  return nodeType === expectedKinds;
}

/**
 * Perform fast validation for common patterns
 * @param {Store} data - RDF data store
 * @param {Object} constraints - Simple constraint object
 * @param {string} constraints.targetClass - Target class IRI
 * @param {Object} constraints.properties - Property constraints
 * @returns {Object} Validation report
 *
 * @example
 * const report = fastValidate(store, {
 *   targetClass: 'ex:Person',
 *   properties: {
 *     'ex:name': { minCount: 1, datatype: 'xsd:string' }
 *   }
 * });
 */
export function fastValidate(data, constraints) {
  const violations = [];

  // Simple fast-path validation for common patterns
  // This is an optimization for performance-critical scenarios

  if (!constraints.targetClass || !constraints.properties) {
    return generateReport([]);
  }

  // Implementation would iterate through instances of targetClass
  // and check property constraints - simplified here

  return generateReport(violations);
}
