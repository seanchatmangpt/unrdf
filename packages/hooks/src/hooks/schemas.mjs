/**
 * @fileoverview Schema definitions for knowledge hooks
 * @module hooks/schemas
 *
 * @description
 * Zod schemas for validating hook definitions, policy packs, and observability configurations.
 */

import { z } from 'zod';

/**
 * Schema for hook metadata
 */
export const HookMetaSchema = z.object({
  name: z.string().min(1).max(100),
  version: z.string().regex(/^\d+\.\d+\.\d+$/, 'Version must be semantic version format'),
  description: z.string().min(1).max(1000).optional(),
  author: z.string().min(1).max(100).optional(),
  license: z.string().min(1).max(100).optional(),
  tags: z.array(z.string().min(1).max(50)).max(20).optional(),
});

/**
 * Schema for hook condition reference
 */
export const HookConditionRefSchema = z.object({
  uri: z.string().min(1),
  sha256: z.string().regex(/^[a-f0-9]{64}$/, 'SHA-256 hash must be 64 hex characters').optional(),
  mediaType: z.string().optional(),
});

/**
 * Schema for hook condition
 */
export const HookConditionSchema = z.object({
  kind: z.enum(['sparql-ask', 'sparql-select', 'shacl', 'delta', 'threshold', 'count', 'window']),
  ref: HookConditionRefSchema.optional(),
  query: z.string().optional(),
  shapes: z.string().optional(),
  spec: z.record(z.any()).optional(),
});

/**
 * Schema for hook effect reference
 */
export const HookEffectRefSchema = z.object({
  uri: z.string().min(1),
  sha256: z.string().regex(/^[a-f0-9]{64}$/, 'SHA-256 hash must be 64 hex characters').optional(),
  mediaType: z.string().optional(),
});

/**
 * Schema for hook effect
 */
export const HookEffectSchema = z.object({
  ref: HookEffectRefSchema.optional(),
  inline: z.function().optional(),
  timeout: z.number().int().positive().max(300000).default(30000),
  retries: z.number().int().nonnegative().max(5).default(1),
  sandbox: z.boolean().default(false),
});

/**
 * Complete knowledge hook schema
 */
export const KnowledgeHookSchema = z.object({
  id: z.string().uuid(),
  meta: HookMetaSchema,
  condition: HookConditionSchema,
  effect: HookEffectSchema,
  priority: z.number().int().min(0).max(100).default(50),
  enabled: z.boolean().default(true),
  dependsOn: z.array(z.string().uuid()).optional(),
  run: z.function().optional(),
});

/**
 * Schema for observability configuration
 */
export const ObservabilityConfigSchema = z.object({
  enableTracing: z.boolean().default(true),
  enableMetrics: z.boolean().default(true),
  enableLogging: z.boolean().default(true),
  tracingProvider: z.enum(['opentelemetry', 'jaeger', 'zipkin', 'none']).default('opentelemetry'),
  metricsProvider: z.enum(['prometheus', 'statsd', 'none']).default('prometheus'),
  loggingProvider: z.enum(['winston', 'pino', 'bunyan', 'console', 'none']).default('console'),
  serviceName: z.string().default('@unrdf/hooks'),
  environment: z.string().default('development'),
  sampleRate: z.number().min(0).max(1).default(1.0),
  batchSize: z.number().int().positive().default(100),
  flushInterval: z.number().int().positive().default(10000),
});

/**
 * Schema for performance metrics
 */
export const PerformanceMetricsSchema = z.object({
  timestamp: z.number().int().positive(),
  hookId: z.string(),
  operation: z.string(),
  duration: z.number().nonnegative(),
  success: z.boolean(),
  errorMessage: z.string().optional(),
  metadata: z.record(z.any()).optional(),
});

/**
 * Create a knowledge hook with validation
 *
 * @param {object} hookDef - Hook definition
 * @returns {object} Validated hook
 * @throws {Error} If validation fails
 */
export function createKnowledgeHook(hookDef) {
  return KnowledgeHookSchema.parse(hookDef);
}

/**
 * Validate a knowledge hook definition
 *
 * @param {object} hookDef - Hook definition
 * @returns {object} Validation result with { success, errors }
 */
export function validateKnowledgeHook(hookDef) {
  const result = KnowledgeHookSchema.safeParse(hookDef);

  if (result.success) {
    return { success: true, data: result.data };
  }

  return {
    success: false,
    errors: result.error.errors.map(err => ({
      path: err.path.join('.'),
      message: err.message,
    })),
  };
}

/**
 * Validate observability configuration
 *
 * @param {object} config - Observability config
 * @returns {object} Validated config
 */
export function validateObservabilityConfig(config) {
  return ObservabilityConfigSchema.parse(config);
}

/**
 * Validate performance metrics
 *
 * @param {object} metrics - Performance metrics
 * @returns {object} Validated metrics
 */
export function validatePerformanceMetrics(metrics) {
  return PerformanceMetricsSchema.parse(metrics);
}
