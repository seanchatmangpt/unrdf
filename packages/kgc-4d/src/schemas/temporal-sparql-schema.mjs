/**
 * @file Temporal SPARQL Schemas - Zod validation for temporal queries
 * @module @unrdf/kgc-4d/schemas/temporal-sparql-schema
 * @description Type-safe Zod schemas for temporal SPARQL operations
 *
 * @example
 * import { TemporalQuerySchema } from './schemas/temporal-sparql-schema.mjs';
 *
 * const query = TemporalQuerySchema.parse({
 *   mode: 'point-in-time',
 *   timestamp: '2026-01-01T00:00:00Z',
 *   timestampNs: 1735689600000000000n,
 *   baseSparql: 'SELECT * WHERE { ?s ?p ?o }',
 *   originalQuery: '...'
 * });
 */

import { z } from 'zod';

/**
 * Temporal query mode schema
 */
const TemporalModeSchema = z.enum(['current', 'point-in-time', 'time-range']);

/**
 * ISO 8601 timestamp string schema
 */
const ISO8601Schema = z.string().refine(
  (val) => {
    try {
      const date = new Date(val);
      return !isNaN(date.getTime());
    } catch {
      return false;
    }
  },
  { message: 'Must be valid ISO 8601 timestamp' }
);

/**
 * Nanosecond timestamp schema (bigint)
 */
const NanosecondTimestampSchema = z.bigint().positive();

/**
 * Base temporal query schema
 */
const BaseTemporalQuerySchema = z.object({
  mode: TemporalModeSchema,
  baseSparql: z.string().min(1),
  originalQuery: z.string().min(1),
});

/**
 * Point-in-time query schema
 */
const PointInTimeQuerySchema = BaseTemporalQuerySchema.extend({
  mode: z.literal('point-in-time'),
  timestamp: ISO8601Schema,
  timestampNs: NanosecondTimestampSchema,
});

/**
 * Time-range query schema
 */
const TimeRangeQuerySchema = BaseTemporalQuerySchema.extend({
  mode: z.literal('time-range'),
  startTimestamp: ISO8601Schema,
  startTimestampNs: NanosecondTimestampSchema,
  endTimestamp: ISO8601Schema,
  endTimestampNs: NanosecondTimestampSchema,
}).refine(
  (data) => data.startTimestampNs < data.endTimestampNs,
  { message: 'Start timestamp must be before end timestamp' }
);

/**
 * Current time query schema
 */
const CurrentQuerySchema = BaseTemporalQuerySchema.extend({
  mode: z.literal('current'),
});

/**
 * Temporal query schema (discriminated union)
 */
export const TemporalQuerySchema = z.discriminatedUnion('mode', [
  CurrentQuerySchema,
  PointInTimeQuerySchema,
  TimeRangeQuerySchema,
]);

/**
 * Temporal cache configuration schema
 */
export const TemporalCacheConfigSchema = z.object({
  maxSize: z.number().int().positive().default(1000),
  ttl: z.number().int().positive().default(300000), // 5 minutes
  enabled: z.boolean().default(true),
});

/**
 * Temporal query result metadata schema
 */
export const TemporalResultMetadataSchema = z.object({
  queryDuration_ms: z.number().nonnegative(),
  resultCount: z.number().int().nonnegative(),
  cached: z.boolean(),
  targetTime: z.string().optional(),
  targetTimeISO: z.string().optional(),
  reconstructionTime_ms: z.number().nonnegative().optional(),
});

/**
 * Temporal query result schema
 */
export const TemporalResultSchema = z.object({
  results: z.array(z.any()),
  metadata: TemporalResultMetadataSchema,
});

/**
 * Time-range query result schema
 */
export const TimeRangeResultSchema = z.object({
  changes: z.array(z.object({
    timestamp: ISO8601Schema,
    timestamp_ns: NanosecondTimestampSchema,
    additions: z.number().int().nonnegative(),
    deletions: z.number().int().nonnegative(),
    netChange: z.number().int(),
  })),
  metadata: z.object({
    startTime: ISO8601Schema,
    endTime: ISO8601Schema,
    sampleCount: z.number().int().positive(),
    totalAdditions: z.number().int().nonnegative(),
    totalDeletions: z.number().int().nonnegative(),
    queryDuration_ms: z.number().nonnegative(),
  }),
});

/**
 * Temporal SPARQL engine options schema
 */
export const TemporalEngineOptionsSchema = z.object({
  cache: TemporalCacheConfigSchema.optional(),
  enableOTEL: z.boolean().default(false),
  reconstructor: z.object({
    cacheSize: z.number().int().positive().default(100),
    enableCache: z.boolean().default(true),
  }).optional(),
});

/**
 * Guard: Validate temporal query
 * @param {Object} query - Query to validate
 * @returns {Object} Validated query
 * @throws {z.ZodError} if query is invalid
 *
 * @example
 * import { guardTemporalQueryValid } from './schemas/temporal-sparql-schema.mjs';
 * guardTemporalQueryValid({ mode: 'point-in-time', ... });
 */
export function guardTemporalQueryValid(query) {
  return TemporalQuerySchema.parse(query);
}

/**
 * Guard: Validate temporal result
 * @param {Object} result - Result to validate
 * @returns {Object} Validated result
 * @throws {z.ZodError} if result is invalid
 *
 * @example
 * import { guardTemporalResultValid } from './schemas/temporal-sparql-schema.mjs';
 * guardTemporalResultValid({ results: [], metadata: {...} });
 */
export function guardTemporalResultValid(result) {
  return TemporalResultSchema.parse(result);
}

/**
 * Guard: Validate temporal cache config
 * @param {Object} config - Config to validate
 * @returns {Object} Validated config
 * @throws {z.ZodError} if config is invalid
 *
 * @example
 * import { guardCacheConfigValid } from './schemas/temporal-sparql-schema.mjs';
 * guardCacheConfigValid({ maxSize: 1000, ttl: 300000 });
 */
export function guardCacheConfigValid(config) {
  return TemporalCacheConfigSchema.parse(config);
}
