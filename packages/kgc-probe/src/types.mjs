/**
 * @file KGC Probe Types - Zod schemas for probe observations
 * @module @unrdf/kgc-probe/types
 *
 * @description
 * Type-safe Zod schemas for KGC probe observations:
 * - Observation: Probe result with guard decisions
 * - GuardDecision: Access control decision with rationale
 * - ProbeConfig: Configuration for probe execution
 */

import { z } from 'zod';

// =============================================================================
// Guard Decision Schema
// =============================================================================

/**
 * Guard decision schema for access control
 *
 * @example
 * {
 *   path: '/home/user/output/test.txt',
 *   allowed: true,
 *   reason: 'Within config.out directory',
 *   policy: 'output-only'
 * }
 *
 * @constant
 * @type {z.ZodObject}
 */
export const GuardDecisionSchema = z.object({
  /** Path being accessed */
  path: z.string(),
  /** Whether access is allowed */
  allowed: z.boolean(),
  /** Reason for decision */
  reason: z.string(),
  /** Policy that was applied */
  policy: z.string().optional(),
  /** Timestamp of decision */
  timestamp: z.number().int().positive().optional(),
});

// =============================================================================
// Observation Schema
// =============================================================================

/**
 * Observation schema for probe results
 *
 * @example
 * {
 *   probeName: 'persistence',
 *   timestamp: 1703001600000,
 *   category: 'storage',
 *   observation: 'Write persistence confirmed across runs',
 *   value: true,
 *   guardDecision: {
 *     path: '/home/user/output/test.txt',
 *     allowed: true,
 *     reason: 'Within config.out directory'
 *   },
 *   metadata: {
 *     testFile: 'persistence-marker.txt',
 *     fileSize: 1024
 *   }
 * }
 *
 * @constant
 * @type {z.ZodObject}
 */
export const ObservationSchema = z.object({
  /** Name of the probe that created this observation */
  probeName: z.string().min(1).max(100),
  /** Observation timestamp (Unix epoch ms) */
  timestamp: z.number().int().positive(),
  /** Category of observation */
  category: z.enum([
    'storage',
    'network',
    'memory',
    'cpu',
    'filesystem',
    'permissions',
    'quota',
    'performance',
    'security',
  ]),
  /** Human-readable observation description */
  observation: z.string().min(1),
  /** Observed value (flexible type) */
  value: z.any(),
  /** Guard decision for path access */
  guardDecision: GuardDecisionSchema.optional(),
  /** Error information if observation failed */
  error: z
    .object({
      message: z.string(),
      code: z.string().optional(),
      stack: z.string().optional(),
    })
    .optional(),
  /** Additional metadata */
  metadata: z.record(z.string(), z.any()).optional(),
});

// =============================================================================
// Probe Config Schema
// =============================================================================

/**
 * Probe configuration schema
 *
 * @example
 * {
 *   out: '/home/user/output',
 *   timeout: 5000,
 *   maxQuota: 104857600
 * }
 *
 * @constant
 * @type {z.ZodObject}
 */
export const ProbeConfigSchema = z.object({
  /** Output directory for probe test files */
  out: z.string().min(1),
  /** Timeout per operation (milliseconds) */
  timeout: z.number().int().positive().default(5000),
  /** Maximum quota to test (bytes) */
  maxQuota: z.number().int().positive().default(100 * 1024 * 1024), // 100 MB
  /** Chunk size for quota testing (bytes) */
  chunkSize: z.number().int().positive().default(1024 * 1024), // 1 MB
  /** Additional metadata */
  metadata: z.record(z.string(), z.any()).optional(),
});

// =============================================================================
// Validation Helper Functions
// =============================================================================

/**
 * Validate an observation
 * @param {any} observation - The observation to validate
 * @returns {Object} Validation result { success, data, errors }
 */
export function validateObservation(observation) {
  try {
    const validated = ObservationSchema.parse(observation);
    return { success: true, data: validated, errors: [] };
  } catch (error) {
    if (error instanceof z.ZodError) {
      return {
        success: false,
        data: null,
        errors: (error.issues || error.errors || []).map(err => ({
          path: err.path?.join('.') || 'unknown',
          message: err.message || 'Unknown error',
          code: err.code || 'unknown',
        })),
      };
    }
    throw error;
  }
}

/**
 * Validate a probe config
 * @param {any} config - The config to validate
 * @returns {Object} Validation result { success, data, errors }
 */
export function validateProbeConfig(config) {
  try {
    const validated = ProbeConfigSchema.parse(config);
    return { success: true, data: validated, errors: [] };
  } catch (error) {
    if (error instanceof z.ZodError) {
      return {
        success: false,
        data: null,
        errors: (error.issues || error.errors || []).map(err => ({
          path: err.path?.join('.') || 'unknown',
          message: err.message || 'Unknown error',
          code: err.code || 'unknown',
        })),
      };
    }
    throw error;
  }
}

// =============================================================================
// Module Exports
// =============================================================================

export default {
  ObservationSchema,
  GuardDecisionSchema,
  ProbeConfigSchema,
  validateObservation,
  validateProbeConfig,
};
