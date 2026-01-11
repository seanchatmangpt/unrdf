/**
 * @file RDF-star Zod validation schemas
 * @module @unrdf/core/rdf-star-schemas
 * @description Validation schemas for RDF-star (W3C RDF 1.2) quoted triples and annotations
 */

import { z } from 'zod';

/**
 * RDF Term schema (named node, blank node, or literal)
 */
export const RDFTermSchema = z.object({
  termType: z.enum(['NamedNode', 'BlankNode', 'Literal', 'DefaultGraph']),
  value: z.string(),
  language: z.string().optional(),
  datatype: z.object({
    value: z.string(),
  }).optional(),
});

/**
 * Triple schema (subject, predicate, object)
 */
export const TripleSchema = z.object({
  subject: RDFTermSchema,
  predicate: RDFTermSchema,
  object: RDFTermSchema,
});

/**
 * Quoted triple schema (RDF-star)
 */
export const QuotedTripleSchema = z.object({
  termType: z.literal('Quad'),
  subject: RDFTermSchema,
  predicate: RDFTermSchema,
  object: RDFTermSchema,
  graph: RDFTermSchema.optional(),
});

/**
 * Provenance annotation schema
 */
export const ProvenanceSchema = z.object({
  source: z.string().url().optional(),
  creator: z.string().optional(),
  created: z.string().datetime().optional(),
  modified: z.string().datetime().optional(),
  method: z.string().optional(),
});

/**
 * Temporal annotation schema
 */
export const TemporalSchema = z.object({
  validFrom: z.string().datetime().optional(),
  validTo: z.string().datetime().optional(),
  recordedAt: z.string().datetime().optional(),
});

/**
 * Confidence/certainty annotation schema
 */
export const ConfidenceSchema = z.object({
  confidence: z.number().min(0).max(1),
  method: z.string().optional(),
  basis: z.string().optional(),
});

/**
 * Multi-source annotation schema
 */
export const MultiSourceSchema = z.object({
  sources: z.array(z.string().url()),
  agreement: z.number().min(0).max(1).optional(),
  conflicting: z.boolean().optional(),
});

/**
 * Complete annotation schema (all annotation types)
 */
export const AnnotationSchema = z.object({
  provenance: ProvenanceSchema.nullable().optional(),
  temporal: TemporalSchema.nullable().optional(),
  confidence: ConfidenceSchema.nullable().optional(),
  multiSource: MultiSourceSchema.nullable().optional(),
  custom: z.union([z.record(z.string(), z.any()), z.null()]).optional(),
});

/**
 * Annotated triple schema (quoted triple + annotations)
 */
export const AnnotatedTripleSchema = z.object({
  quotedTriple: QuotedTripleSchema,
  annotations: AnnotationSchema,
});

/**
 * SPARQL-star query options schema
 */
export const SPARQLStarOptionsSchema = z.object({
  includeAnnotations: z.boolean().optional(),
  confidenceThreshold: z.number().min(0).max(1).optional(),
  temporalFilter: TemporalSchema.optional(),
  sourceFilter: z.array(z.string().url()).optional(),
});

/**
 * Validation functions
 */

/**
 * Validate a quoted triple
 * @param {unknown} value - Value to validate
 * @returns {Object} Validated quoted triple
 * @throws {z.ZodError} If validation fails
 */
export function validateQuotedTriple(value) {
  return QuotedTripleSchema.parse(value);
}

/**
 * Validate provenance annotation
 * @param {unknown} value - Value to validate
 * @returns {Object} Validated provenance
 * @throws {z.ZodError} If validation fails
 */
export function validateProvenance(value) {
  return ProvenanceSchema.parse(value);
}

/**
 * Validate temporal annotation
 * @param {unknown} value - Value to validate
 * @returns {Object} Validated temporal annotation
 * @throws {z.ZodError} If validation fails
 */
export function validateTemporal(value) {
  return TemporalSchema.parse(value);
}

/**
 * Validate confidence annotation
 * @param {unknown} value - Value to validate
 * @returns {Object} Validated confidence
 * @throws {z.ZodError} If validation fails
 */
export function validateConfidence(value) {
  return ConfidenceSchema.parse(value);
}

/**
 * Validate annotation object
 * @param {unknown} value - Value to validate
 * @returns {Object} Validated annotation
 * @throws {z.ZodError} If validation fails
 */
export function validateAnnotation(value) {
  return AnnotationSchema.parse(value);
}

/**
 * Safe parse quoted triple (returns success/error result)
 * @param {unknown} value - Value to validate
 * @returns {Object} Parse result with success flag and data/error
 */
export function safeParseQuotedTriple(value) {
  return QuotedTripleSchema.safeParse(value);
}

/**
 * Safe parse annotation (returns success/error result)
 * @param {unknown} value - Value to validate
 * @returns {Object} Parse result with success flag and data/error
 */
export function safeParseAnnotation(value) {
  return AnnotationSchema.safeParse(value);
}
