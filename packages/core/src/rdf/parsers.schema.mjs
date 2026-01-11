/**
 * @file Parser Configuration Schemas
 * @module @unrdf/core/rdf/parsers.schema
 */

import { z } from 'zod';

/**
 * RDF Format enumeration for parsing
 */
export const ParseFormatSchema = z.enum([
  'turtle',
  'ntriples',
  'nquads',
  'trig',
  'jsonld',
  'rdfxml',
]);

/**
 * Base parser options
 */
export const BaseParserOptionsSchema = z.object({
  /** RDF format to parse */
  format: ParseFormatSchema.default('turtle'),

  /** Base IRI for relative IRIs */
  baseIRI: z.string().url().optional(),

  /** Strict parsing (throw on errors) vs permissive */
  strict: z.boolean().default(true),

  /** Chunk size for batch processing */
  chunkSize: z.number().int().positive().default(1000),

  /** Progress callback interval (number of quads) */
  progressInterval: z.number().int().positive().default(10000),

  /** Validate quads during parsing */
  validate: z.boolean().default(true),
});

/**
 * Streaming parser options
 */
export const StreamParserOptionsSchema = BaseParserOptionsSchema.extend({
  /** High water mark for backpressure */
  highWaterMark: z.number().int().positive().default(16384),

  /** Input encoding */
  encoding: z.enum(['utf8', 'utf-8', 'ascii', 'latin1']).default('utf8'),

  /** Maximum parser buffer size */
  maxBufferSize: z.number().int().positive().default(1024 * 1024 * 10), // 10MB
});

/**
 * Error recovery options
 */
export const ErrorRecoveryOptionsSchema = z.object({
  /** Skip invalid quads instead of throwing */
  skipInvalid: z.boolean().default(false),

  /** Maximum errors before aborting */
  maxErrors: z.number().int().positive().default(100),

  /** Error callback function */
  onError: z.function().optional(),

  /** Collect errors instead of throwing */
  collectErrors: z.boolean().default(false),
});

/**
 * Parallel parser options
 */
export const ParallelParserOptionsSchema = BaseParserOptionsSchema.extend({
  /** Number of parallel workers */
  workers: z.number().int().positive().max(10).default(4),

  /** Split input into N chunks */
  splits: z.number().int().positive().default(10),

  /** Worker timeout in milliseconds */
  workerTimeout: z.number().int().positive().default(30000),
});

/**
 * Progress callback schema
 */
export const ProgressCallbackSchema = z.function();
