/**
 * @fileoverview KGC Probe - Type definitions and Zod schemas
 *
 * Runtime-validatable schemas for:
 * - Observations (agent outputs)
 * - Artifacts (scan results)
 * - Configuration objects
 *
 * @module @unrdf/kgc-probe/types
 */

import { z } from 'zod';

// ============================================================================
// OBSERVATION SCHEMA
// ============================================================================

/**
 * Individual observation from an agent scan
 * @type {z.ZodSchema}
 */
export const ObservationSchema = z.object({
  id: z.string().uuid('Observation must have UUID').describe('Globally unique observation ID'),
  agent: z.string().describe('Agent identifier that produced this observation'),
  timestamp: z.string().datetime().describe('ISO8601 timestamp when observation was made'),
  kind: z.enum([
    'completeness',
    'consistency',
    'conformance',
    'coverage',
    'caching',
    'completeness_level',
    'coherence',
    'clustering',
    'classification',
    'collaboration'
  ]).describe('Type of observation (agent-specific)'),
  severity: z.enum(['critical', 'warning', 'info']).default('info').describe('Impact severity'),
  subject: z.string().describe('RDF node under observation'),
  predicate: z.string().optional().describe('RDF property (optional)'),
  object: z.string().optional().describe('RDF value (optional)'),
  evidence: z.object({
    query: z.string().describe('SPARQL query or algorithm used'),
    result: z.unknown().describe('Computation result'),
    witnesses: z.array(z.string()).describe('Confirming triple/node references')
  }).describe('Proof and context for observation'),
  metrics: z.object({
    confidence: z.number().min(0).max(1).describe('Confidence score [0, 1]'),
    coverage: z.number().min(0).max(1).describe('Coverage ratio [0, 1]'),
    latency_ms: z.number().nonnegative().describe('Execution latency in ms')
  }).describe('Quantitative metrics'),
  tags: z.array(z.string()).default([]).describe('Searchable tags'),
  xid: z.string().optional().describe('Correlation ID for tracing')
}).strict();

/**
 * Type representing validated observation
 * @typedef {z.infer<typeof ObservationSchema>} Observation
 */
export const ObservationType = ObservationSchema;

// ============================================================================
// ARTIFACT SCHEMA
// ============================================================================

/**
 * Artifact summary statistics
 * @type {z.ZodSchema}
 */
export const ArtifactSummarySchema = z.object({
  total: z.number().nonnegative().describe('Total observation count'),
  by_kind: z.record(z.string(), z.number().nonnegative()).describe('Count per observation kind'),
  by_severity: z.record(z.enum(['critical', 'warning', 'info']), z.number().nonnegative()).describe('Count per severity'),
  confidence_mean: z.number().min(0).max(1).describe('Mean confidence score'),
  coverage_mean: z.number().min(0).max(1).describe('Mean coverage ratio')
}).describe('Summary statistics for artifact');

/**
 * Complete artifact with observations and metadata
 * @type {z.ZodSchema}
 */
export const ArtifactSchema = z.object({
  version: z.literal('1.0').describe('Artifact schema version'),
  universe_id: z.string().describe('4D universe reference'),
  snapshot_id: z.string().describe('KGC-4D snapshot hash'),
  generated_at: z.string().datetime().describe('Generation timestamp'),
  probe_run_id: z.string().uuid().describe('Unique probe run ID'),
  shard_count: z.number().nonnegative().describe('Number of shards merged'),
  shard_hash: z.string().regex(/^[a-f0-9]{64}$/).describe('Blake3 hash of all shards'),
  observations: z.array(ObservationSchema).describe('All observations from this scan'),
  summary: ArtifactSummarySchema.describe('Aggregated statistics'),
  metadata: z.object({
    agents_run: z.array(z.string()).describe('Agent IDs executed'),
    guards_applied: z.array(z.string()).describe('Guard IDs applied'),
    execution_time_ms: z.number().nonnegative().describe('Total execution time'),
    storage_backend: z.string().describe('Storage implementation used'),
    config: z.unknown().optional().describe('Scan configuration')
  }).describe('Execution metadata'),
  integrity: z.object({
    checksum: z.string().regex(/^[a-f0-9]{64}$/).describe('Blake3 hash of observations'),
    signature: z.string().optional().describe('Optional cryptographic signature'),
    verified_at: z.string().datetime().optional().describe('Verification timestamp')
  }).describe('Integrity verification')
}).strict();

/**
 * Type representing validated artifact
 * @typedef {z.infer<typeof ArtifactSchema>} Artifact
 */
export const ArtifactType = ArtifactSchema;

// ============================================================================
// CONFIGURATION SCHEMAS
// ============================================================================

/**
 * Probe scan configuration
 * @type {z.ZodSchema}
 */
export const ProbeConfigSchema = z.object({
  universe_id: z.string().describe('Universe to scan'),
  snapshot_id: z.string().optional().describe('Optional snapshot reference'),
  agents: z.array(z.string()).optional().describe('Agent IDs to run (all if omitted)'),
  guards: z.array(z.string()).optional().describe('Guard IDs to apply'),
  distributed: z.boolean().default(false).describe('Enable multi-shard merge'),
  persist: z.boolean().default(true).describe('Save artifact to storage'),
  timeout_ms: z.number().positive().default(300000).describe('Scan timeout'),
  batch_size: z.number().positive().default(100).describe('Observation batch size')
}).describe('Probe scan configuration');

/**
 * Guard configuration
 * @type {z.ZodSchema}
 */
export const GuardConfigSchema = z.object({
  quality_check: z.object({
    critical_observations_threshold: z.number().positive().default(50),
    confidence_min: z.number().min(0).max(1).default(0.6)
  }).optional(),
  completeness_check: z.object({
    coverage_min: z.number().min(0).max(1).default(0.7)
  }).optional(),
  severity_limit: z.object({
    critical_limit: z.number().positive().default(10)
  }).optional()
}).describe('Guard thresholds and limits');

/**
 * Storage configuration
 * @type {z.ZodSchema}
 */
export const StorageConfigSchema = z.object({
  type: z.enum(['memory', 'file', 'database']).describe('Storage backend type'),
  path: z.string().optional().describe('File system path (for file storage)'),
  connectionString: z.string().optional().describe('Database connection (for database storage)')
}).describe('Storage backend configuration');

// ============================================================================
// AGENT INTERFACE
// ============================================================================

/**
 * Base agent interface definition
 * @type {z.ZodSchema}
 */
export const AgentSchema = z.object({
  id: z.string().describe('Agent identifier'),
  kind: z.string().describe('Observation kind produced'),
  description: z.string().describe('Human-readable description'),
  scan: z.function().describe('Async scan function')
}).describe('Agent interface specification');

// ============================================================================
// GUARD VIOLATION
// ============================================================================

/**
 * Guard validation violation
 * @type {z.ZodSchema}
 */
export const GuardViolationSchema = z.object({
  guard_id: z.string().describe('Guard that detected violation'),
  severity: z.enum(['critical', 'warning', 'info']).describe('Violation severity'),
  details: z.object({
    message: z.string().describe('Human-readable message'),
    actual: z.unknown().describe('Actual value observed'),
    threshold: z.unknown().describe('Expected/threshold value')
  }).describe('Violation details')
}).describe('Guard validation violation');

/**
 * Type representing guard violation
 * @typedef {z.infer<typeof GuardViolationSchema>} GuardViolation
 */
export const GuardViolationType = GuardViolationSchema;

// ============================================================================
// DIFF RESULT
// ============================================================================

/**
 * Diff between two artifacts
 * @type {z.ZodSchema}
 */
export const DiffResultSchema = z.object({
  added: z.array(ObservationSchema).describe('Observations in artifact2 but not artifact1'),
  removed: z.array(ObservationSchema).describe('Observations in artifact1 but not artifact2'),
  modified: z.array(z.object({
    original: ObservationSchema,
    updated: ObservationSchema,
    changes: z.record(z.string(), z.unknown()).describe('Field changes')
  })).describe('Observations present in both but with changes'),
  summary: z.object({
    total_changes: z.number().nonnegative(),
    similarity_ratio: z.number().min(0).max(1).describe('Jaccard similarity')
  }).describe('Diff summary')
}).describe('Artifact comparison result');

/**
 * Type representing diff result
 * @typedef {z.infer<typeof DiffResultSchema>} DiffResult
 */
export const DiffResultType = DiffResultSchema;

// ============================================================================
// VALIDATION RESULTS
// ============================================================================

/**
 * Scan execution result
 * @type {z.ZodSchema}
 */
export const ScanResultSchema = z.object({
  artifact: ArtifactSchema.describe('Generated artifact'),
  status: z.enum(['success', 'partial', 'failed']).describe('Execution status'),
  errors: z.array(z.object({
    agent: z.string().optional(),
    guard: z.string().optional(),
    error: z.string().describe('Error message')
  })).describe('Execution errors')
}).describe('Complete scan result');

/**
 * Type representing scan result
 * @typedef {z.infer<typeof ScanResultSchema>} ScanResult
 */
export const ScanResultType = ScanResultSchema;

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

/**
 * Validate observation against schema
 * @param {unknown} data - Data to validate
 * @returns {Observation} Validated observation
 * @throws {z.ZodError} If validation fails
 */
export function validateObservation(data) {
  return ObservationSchema.parse(data);
}

/**
 * Validate artifact against schema
 * @param {unknown} data - Data to validate
 * @returns {Artifact} Validated artifact
 * @throws {z.ZodError} If validation fails
 */
export function validateArtifact(data) {
  return ArtifactSchema.parse(data);
}

/**
 * Validate probe configuration
 * @param {unknown} data - Data to validate
 * @returns {ProbeConfig} Validated config
 * @throws {z.ZodError} If validation fails
 */
export function validateProbeConfig(data) {
  return ProbeConfigSchema.parse(data);
}

/**
 * Safe validation (returns null instead of throwing)
 * @param {unknown} data - Data to validate
 * @returns {Observation | null} Validated observation or null
 */
export function tryValidateObservation(data) {
  const result = ObservationSchema.safeParse(data);
  return result.success ? result.data : null;
}
