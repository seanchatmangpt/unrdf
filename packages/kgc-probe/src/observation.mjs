#!/usr/bin/env node
/**
 * @fileoverview Canonical Observation record schema for KGC Probe
 *
 * Observation = atomic unit of knowledge extraction from codebase analysis.
 * All probes emit Observation records that conform to this schema.
 *
 * Design principles:
 * - Immutable: Once created, never modified
 * - Deterministic: Same input → same Observation
 * - Composable: Multiple Observations → Knowledge Graph
 * - Receipt-driven: Each Observation includes hash for verification
 */

import { z } from 'zod';

/**
 * Observation severity levels (aligned with OTEL)
 * @typedef {'trace' | 'debug' | 'info' | 'warn' | 'error' | 'fatal'} ObservationSeverity
 */
export const ObservationSeveritySchema = z.enum([
  'trace',   // Fine-grained execution flow
  'debug',   // Developer diagnostics
  'info',    // Informational observations
  'warn',    // Potential issues
  'error',   // Actionable problems
  'fatal'    // Critical failures
]);

/**
 * Observation category - what aspect of codebase is being observed
 * @typedef {'file' | 'dependency' | 'pattern' | 'metric' | 'security' | 'quality' | 'performance' | 'test' | 'documentation' | 'guard'} ObservationCategory
 */
export const ObservationCategorySchema = z.enum([
  'file',          // File-level observations (imports, exports, structure)
  'dependency',    // Package dependencies, versions, vulnerabilities
  'pattern',       // Code patterns, anti-patterns, idioms
  'metric',        // Complexity, LOC, coupling metrics
  'security',      // Security issues, secrets, vulnerabilities
  'quality',       // Code quality, style violations
  'performance',   // Performance characteristics, benchmarks
  'test',          // Test coverage, assertions, flakiness
  'documentation', // Docs completeness, accuracy
  'guard'          // Poka-yoke guard denials (forbidden operations)
]);

/**
 * Source location within a file
 */
export const SourceLocationSchema = z.object({
  file: z.string().describe('Absolute file path'),
  line: z.number().int().positive().optional().describe('Line number (1-indexed)'),
  column: z.number().int().positive().optional().describe('Column number (1-indexed)'),
  endLine: z.number().int().positive().optional().describe('End line number'),
  endColumn: z.number().int().positive().optional().describe('End column number')
});

/**
 * Observation metadata (who, when, how)
 */
export const ObservationMetadataSchema = z.object({
  agentId: z.string().describe('Agent that created this observation (e.g., "agent-2-import-analyzer")'),
  timestamp: z.string().datetime().describe('ISO 8601 timestamp'),
  probeVersion: z.string().describe('KGC Probe version'),
  budgetMs: z.number().int().positive().describe('Time budget allocated (ms)'),
  actualMs: z.number().int().nonnegative().describe('Actual time spent (ms)')
});

/**
 * Canonical Observation record
 *
 * @typedef {Object} Observation
 * @property {string} id - Unique observation ID (hash of deterministic content)
 * @property {ObservationCategory} category - What aspect is being observed
 * @property {ObservationSeverity} severity - Importance level
 * @property {string} message - Human-readable summary
 * @property {SourceLocation} [location] - Where in codebase (optional for global observations)
 * @property {Record<string, any>} data - Structured observation payload
 * @property {ObservationMetadata} metadata - Who/when/how
 * @property {string[]} tags - Searchable tags for filtering
 * @property {string} [receiptHash] - Hash of this observation for verification
 */
export const ObservationSchema = z.object({
  id: z.string().describe('Unique observation ID (deterministic hash)'),
  category: ObservationCategorySchema,
  severity: ObservationSeveritySchema,
  message: z.string().min(1).describe('Human-readable summary'),
  location: SourceLocationSchema.optional().describe('Source location (optional for global observations)'),
  data: z.record(z.string(), z.any()).describe('Structured observation payload'),
  metadata: ObservationMetadataSchema,
  tags: z.array(z.string()).default([]).describe('Searchable tags'),
  receiptHash: z.string().optional().describe('Hash of this observation for verification')
});

/**
 * Type exports
 */
export const ObservationType = ObservationSchema;

/**
 * Factory function to create Observations with defaults
 *
 * @param {Partial<z.infer<typeof ObservationSchema>>} obs - Partial observation
 * @returns {z.infer<typeof ObservationSchema>} - Validated observation
 */
export function createObservation(obs) {
  const defaults = {
    id: obs.id || crypto.randomUUID(), // Will be replaced by hash in receipt.mjs
    severity: obs.severity || 'info',
    tags: obs.tags || [],
    data: obs.data || {},
    metadata: {
      timestamp: new Date().toISOString(),
      ...obs.metadata
    }
  };

  const merged = { ...defaults, ...obs };
  return ObservationSchema.parse(merged);
}

/**
 * Guard denial observation - emitted when poka-yoke guards block an operation
 *
 * @param {Object} params
 * @param {string} params.guardName - Name of guard that triggered
 * @param {string} params.reason - Why operation was denied
 * @param {string} params.agentId - Agent that attempted the operation
 * @param {Record<string, any>} params.context - Additional context (sanitized)
 * @returns {z.infer<typeof ObservationSchema>}
 */
export function createGuardDenial({ guardName, reason, agentId, context = {} }) {
  return createObservation({
    category: 'guard',
    severity: 'warn',
    message: `Guard '${guardName}' denied operation: ${reason}`,
    data: {
      guardName,
      reason,
      context: sanitizeContext(context)
    },
    metadata: {
      agentId,
      probeVersion: '0.1.0',
      budgetMs: 1, // Guards have minimal overhead
      actualMs: 0,
      timestamp: new Date().toISOString()
    },
    tags: ['guard-denial', guardName]
  });
}

/**
 * Sanitize context to remove sensitive data before logging
 *
 * @param {Record<string, any>} context
 * @returns {Record<string, any>}
 */
function sanitizeContext(context) {
  const sanitized = { ...context };

  // Remove sensitive keys
  const sensitiveKeys = ['password', 'token', 'secret', 'key', 'auth', 'credential'];
  for (const key of Object.keys(sanitized)) {
    if (sensitiveKeys.some(sk => key.toLowerCase().includes(sk))) {
      sanitized[key] = '[REDACTED]';
    }
  }

  return sanitized;
}

/**
 * Validate observation record
 *
 * @param {unknown} data
 * @returns {z.infer<typeof ObservationSchema>}
 * @throws {z.ZodError} if validation fails
 */
export function validateObservation(data) {
  return ObservationSchema.parse(data);
}

export default {
  ObservationSchema,
  ObservationSeveritySchema,
  ObservationCategorySchema,
  SourceLocationSchema,
  ObservationMetadataSchema,
  createObservation,
  createGuardDenial,
  validateObservation
};
