/**
 * @file Zod schemas for SPARQL Query Builder
 * @module @unrdf/core/sparql/query-builder.schema
 */

import { z } from 'zod';

/**
 * Zod schema for query type (reserved for future validation)
 */
export const QueryTypeSchema = z.enum(['SELECT', 'INSERT', 'DELETE', 'CONSTRUCT', 'ASK']);

/**
 * Zod schema for prefix
 */
export const PrefixSchema = z.object({
  prefix: z.string().min(1),
  uri: z.string().url(),
});

/**
 * Zod schema for triple pattern
 */
export const TriplePatternSchema = z.object({
  subject: z.string().min(1),
  predicate: z.string().min(1),
  object: z.string().min(1),
});

/**
 * Zod schema for filter
 */
export const FilterSchema = z.string().min(1);

/**
 * Zod schema for query builder options
 */
export const QueryBuilderOptionsSchema = z.object({
  prefixes: z.record(z.string(), z.string()).optional(),
  baseIRI: z.string().url().optional(),
}).strict();
