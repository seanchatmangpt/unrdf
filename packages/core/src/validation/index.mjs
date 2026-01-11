/**
 * @file Validation utilities module
 * @module @unrdf/core/validation
 */

import { z } from 'zod';

/**
 * Quad schema for validation
 */
export const QuadSchema = z.object({
  subject: z.object({ value: z.string() }),
  predicate: z.object({ value: z.string() }),
  object: z.object({ value: z.string() }),
  graph: z.object({ value: z.string() }).optional(),
});

/**
 * Store schema for validation
 */
export const StoreSchema = z.object({
  getQuads: z.function(),
  addQuad: z.function().optional(),
  removeQuad: z.function().optional(),
  countQuads: z.function().optional(),
});

/**
 * SPARQL query options schema
 */
export const QueryOptionsSchema = z
  .object({
    limit: z.number().optional(),
    offset: z.number().optional(),
    signal: z.instanceof(AbortSignal).optional(),
  })
  .optional();

/**
 * Validate a quad
 * @param {*} quad - Quad to validate
 * @returns {boolean} True if valid
 */
export function validateQuad(quad) {
  try {
    QuadSchema.parse(quad);
    return true;
  } catch {
    return false;
  }
}

/**
 * Validate a store
 * @param {*} store - Store to validate
 * @returns {boolean} True if valid
 */
export function validateStore(store) {
  try {
    StoreSchema.parse(store);
    return true;
  } catch {
    return false;
  }
}

// SHACL Validator exports
export {
  createValidator,
  validateGraph,
  validateConstraint,
  generateReport,
  validateNodeKind,
  fastValidate,
  ConstraintType,
  ValidationOptionsSchema,
  ValidationReportSchema,
} from './shacl-validator.mjs';

// RDF Schema Builder exports
export {
  shacl,
  CommonShapes,
  ShapeBuilder,
  PropertyBuilder,
} from './rdf-schema-builder.mjs';
