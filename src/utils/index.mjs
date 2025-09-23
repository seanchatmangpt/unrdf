/**
 * UNRDF Utils - Comprehensive utility library for RDF operations
 * 
 * This module provides a unified interface to all utility functions
 * organized by domain: terms, quads, graphs, validation, I/O, debugging, IDs,
 * namespaces, SPARQL, transformations, merging, and quality assessment.
 * 
 * @example
 * ```javascript
 * import { 
 *   asNamedNode, quadToJSON, getObjects, validateQuadJSON,
 *   createNamespaceManager, createSPARQLBuilder, assessDataQuality
 * } from 'unrdf/utils';
 * 
 * // Term utilities
 * const subject = asNamedNode('http://example.org/person');
 * 
 * // Quad utilities
 * const json = quadToJSON(quad);
 * 
 * // Graph utilities
 * const objects = getObjects(store, subject, 'http://example.org/name');
 * 
 * // Validation
 * const validated = validateQuadJSON(quadData);
 * 
 * // Namespace management
 * const nsManager = createNamespaceManager();
 * 
 * // SPARQL query building
 * const query = createSPARQLBuilder()
 *   .select('?s', '?p', '?o')
 *   .where('?s', '?p', '?o')
 *   .build();
 * 
 * // Quality assessment
 * const quality = assessDataQuality(store);
 * ```
 */

// Term utilities - RDF term manipulation and conversion
export * from "./term-utils.mjs";

// Quad utilities - Quad/JSON transformations and filtering
export * from "./quad-utils.mjs";

// Graph utilities - Store operations and query helpers
export * from "./graph-utils.mjs";

// Validation utilities - Zod schemas and validation functions
export * from "./validation-utils.mjs";

// I/O utilities - File operations for RDF formats
export * from "./io-utils.mjs";

// Debug utilities - Introspection, logging, and performance measurement
export * from "./debug-utils.mjs";

// ID utilities - Blank nodes, UUIDs, and IRI generation
export * from "./id-utils.mjs";

// Namespace utilities - Vocabulary and namespace management
export * from "./namespace-utils.mjs";

// SPARQL utilities - Query building and SPARQL operations
export * from "./sparql-utils.mjs";

// Transform utilities - Data transformations and conversions
export * from "./transform-utils.mjs";

// Merge utilities - Store operations and data merging
export * from "./merge-utils.mjs";

// Quality utilities - Data quality assessment and improvement
export * from "./quality-utils.mjs";