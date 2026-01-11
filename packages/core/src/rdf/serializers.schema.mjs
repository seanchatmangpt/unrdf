/**
 * @file Serializer Configuration Schemas
 * @module @unrdf/core/rdf/serializers.schema
 */

import { z } from 'zod';

/**
 * RDF Format enumeration
 */
export const RdfFormatSchema = z.enum([
  'turtle',
  'ntriples',
  'nquads',
  'trig',
  'jsonld',
]);

/**
 * Base serializer options
 */
export const BaseSerializerOptionsSchema = z.object({
  /** RDF format to serialize to */
  format: RdfFormatSchema.default('turtle'),

  /** Base IRI for relative IRIs */
  baseIRI: z.string().url().optional(),

  /** Prefixes for Turtle/TriG format */
  prefixes: z.record(z.string(), z.string()).optional(),

  /** Chunk size for batch processing (number of quads) */
  chunkSize: z.number().int().positive().default(1000),

  /** Enable gzip compression */
  compress: z.boolean().default(false),

  /** Progress callback invoked every N quads */
  progressInterval: z.number().int().positive().default(10000),
});

/**
 * Turtle-specific serializer options
 */
export const TurtleSerializerOptionsSchema = BaseSerializerOptionsSchema.extend({
  format: z.literal('turtle'),

  /** Use compact notation where possible */
  compact: z.boolean().default(true),

  /** Include comments in output */
  includeComments: z.boolean().default(false),
});

/**
 * JSON-LD serializer options
 */
export const JsonLdSerializerOptionsSchema = BaseSerializerOptionsSchema.extend({
  format: z.literal('jsonld'),

  /** JSON-LD context */
  context: z.record(z.string(), z.any()).optional(),

  /** JSON-LD processing mode */
  processingMode: z.enum(['json-ld-1.0', 'json-ld-1.1']).default('json-ld-1.1'),

  /** Compact, expanded, or flattened output */
  outputForm: z.enum(['compact', 'expanded', 'flattened']).default('compact'),
});

/**
 * N-Triples/N-Quads serializer options
 */
export const NTriplesSerializerOptionsSchema = BaseSerializerOptionsSchema.extend({
  format: z.enum(['ntriples', 'nquads']),

  /** Sort output quads for deterministic serialization */
  sorted: z.boolean().default(false),
});

/**
 * Streaming serializer options
 */
export const StreamSerializerOptionsSchema = BaseSerializerOptionsSchema.extend({
  /** High water mark for stream backpressure */
  highWaterMark: z.number().int().positive().default(16384),

  /** Encoding for output stream */
  encoding: z.enum(['utf8', 'utf-8', 'ascii']).default('utf8'),
});

/**
 * Batch serialization options
 */
export const BatchSerializerOptionsSchema = BaseSerializerOptionsSchema.extend({
  /** Maximum batch size in bytes */
  maxBatchSize: z.number().int().positive().default(1024 * 1024), // 1MB

  /** Parallel serialization workers */
  parallel: z.number().int().positive().max(10).default(4),
});
