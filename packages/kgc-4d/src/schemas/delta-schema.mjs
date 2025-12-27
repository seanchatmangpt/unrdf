/**
 * Delta Schema - Zod Type-Level Guards
 * Validates RDF deltas before serialization/after deserialization
 * Prevents type errors during time-travel replay
 */

import { z } from 'zod';

/**
 * RDF Term schemas
 */
const RDFNamedNodeSchema = z.object({
  termType: z.literal('NamedNode'),
  value: z.string().url(),
});

const RDFBlankNodeSchema = z.object({
  termType: z.literal('BlankNode'),
  value: z.string().min(1),
});

const RDFLiteralSchema = z.object({
  termType: z.literal('Literal'),
  value: z.string(),
  language: z.string().optional(),
  datatype: z.string().url().optional(),
});

const RDFTermSchema = z.union([
  RDFNamedNodeSchema,
  RDFBlankNodeSchema,
  RDFLiteralSchema,
]);

/**
 * Delta schema (runtime + type-level validation)
 * Supports both live quad form and serialized form
 */
export const DeltaSchema = z.object({
  type: z.enum(['add', 'delete']),
  subject: z.union([
    z.string().min(1), // Serialized form
    RDFTermSchema,     // Live quad form
  ]),
  subjectType: z.enum(['NamedNode', 'BlankNode']),
  predicate: z.union([
    z.string().url(),  // Serialized form
    RDFNamedNodeSchema, // Live quad form
  ]),
  object: z.union([
    z.object({        // Serialized form
      value: z.string(),
      type: z.string(),
      datatype: z.string().optional(),
      language: z.string().optional(),
    }),
    RDFTermSchema,    // Live quad form
  ]),
});

/**
 * Serialized delta schema (for storage/transmission)
 * Stricter than DeltaSchema - only accepts serialized form
 */
export const SerializedDeltaSchema = z.object({
  type: z.enum(['add', 'delete']),
  subject: z.string().min(1),
  subjectType: z.enum(['NamedNode', 'BlankNode']),
  predicate: z.string().url(),
  object: z.object({
    value: z.string(),
    type: z.string(),
    datatype: z.string().optional(),
    language: z.string().optional(),
  }),
});

/**
 * Guard: Validate delta before serialization
 * @param {Object} delta - Delta to validate
 * @returns {Object} Validated delta
 * @throws {z.ZodError} if delta is invalid
 * 
 * @example
 * import { guardDeltaValid } from './schemas/delta-schema.mjs';
 * const delta = {
 *   type: 'add',
 *   subject: { termType: 'NamedNode', value: 'http://ex.org/s' },
 *   subjectType: 'NamedNode',
 *   predicate: { termType: 'NamedNode', value: 'http://ex.org/p' },
 *   object: { termType: 'Literal', value: 'test' },
 * };
 * guardDeltaValid(delta); // OK
 */
export function guardDeltaValid(delta) {
  return DeltaSchema.parse(delta);
}

/**
 * Guard: Validate serialized delta before deserialization
 * @param {Object} serializedDelta - Serialized delta to validate
 * @returns {Object} Validated serialized delta
 * @throws {z.ZodError} if serialized delta is malformed
 * 
 * @example
 * import { guardSerializedDeltaValid } from './schemas/delta-schema.mjs';
 * const delta = {
 *   type: 'add',
 *   subject: 'http://ex.org/s',
 *   subjectType: 'NamedNode',
 *   predicate: 'http://ex.org/p',
 *   object: { value: 'test', type: 'Literal' },
 * };
 * guardSerializedDeltaValid(delta); // OK
 */
export function guardSerializedDeltaValid(serializedDelta) {
  return SerializedDeltaSchema.parse(serializedDelta);
}
