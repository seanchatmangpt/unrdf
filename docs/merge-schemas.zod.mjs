/**
 * Zod Validation Schemas for Orchestrator Merge Algorithm
 * SPARC Phase: Data Structure Definitions
 * Runtime validation for all shard inputs and merge outputs
 */

import { z } from 'zod';

// ============================================================================
// 1. PROOF & EVIDENCE SCHEMAS
// ============================================================================

/** Evidence backing a claim (observational data) */
export const EvidenceSchema = z.object({
  evidence_id: z.string().uuid(),
  type: z.enum(['direct', 'calculated', 'inferred']),
  timestamp_ns: z.number().positive().int(),
  source: z.string().min(1),
  data: z.unknown(), // Raw evidence data
  method: z.string().min(1),
});

/** Proof structure with reasoning chain */
export const ProofSchema = z.object({
  type: z.enum(['direct', 'inferred', 'aggregated']),
  evidence: z.array(EvidenceSchema),
  reasoning: z.string().min(1).max(10000),
  confidence_delta: z.number().min(0).max(1).optional(),
});

// ============================================================================
// 2. CLAIM SCHEMAS
// ============================================================================

/** A single claim from an agent (primary assertion) */
export const ClaimSchema = z.object({
  key: z.string().min(3),  // domain:resource:method format
  domain: z.string().regex(/^[a-z]+:[a-z_]+$/),
  method: z.string().min(1),
  value: z.unknown(),
  timestamp_ns: z.number().positive().int(),
  proof: ProofSchema,
  confidence: z.number().min(0).max(1),
  agent_id: z.string().min(1),
  tags: z.array(z.string()).optional(),
});

// ============================================================================
// 3. OBSERVATION SCHEMAS
// ============================================================================

/** Raw observation data from agent measurement */
export const ObservationSchema = z.object({
  observation_id: z.string().uuid(),
  timestamp_ns: z.number().positive().int(),
  method: z.string().min(1),
  result: z.unknown(),
  error: z.string().nullable().optional(),
  duration_ns: z.number().nonnegative().int().optional(),
});

// ============================================================================
// 4. SHARD METADATA & MANIFEST
// ============================================================================

/** Metadata about agent execution */
export const AgentMetadataSchema = z.object({
  cpu_usage_ns: z.number().nonnegative(),
  memory_bytes: z.number().nonnegative(),
  execution_time_ns: z.number().nonnegative(),
  method: z.enum(['exhaustive', 'heuristic', 'sample']),
  status: z.enum(['success', 'partial', 'failed']).optional(),
});

/** Individual shard from one agent */
export const AgentShardSchema = z.object({
  agent_id: z.string().min(1),
  domain: z.string().min(1),
  timestamp_ns: z.number().positive().int(),
  claims: z.array(ClaimSchema),
  observations: z.array(ObservationSchema).optional(),
  metadata: AgentMetadataSchema,
  shard_hash: z.string().optional(),  // SHA-256 computed later
});

// ============================================================================
// 5. CONFLICT SCHEMAS
// ============================================================================

/** Resolution decision for a conflict */
export const ResolutionSchema = z.object({
  resolved_at_ns: z.number().positive().int(),
  strategy: z.enum(['lww', 'manual', 'heuristic', 'merge']),
  chosen_value: z.unknown(),
  reason: z.string().min(1).max(5000),
  resolved_by: z.string().min(1),
  confidence_override: z.number().min(0).max(1).optional(),
});

/** Record of detected conflict between claims */
export const ConflictRecordSchema = z.object({
  conflict_id: z.string().uuid(),
  resource_key: z.string().min(1),

  claim_a: ClaimSchema,
  claim_b: ClaimSchema,

  timestamp_a_ns: z.number().positive().int(),
  timestamp_b_ns: z.number().positive().int(),

  agent_a: z.string().min(1),
  agent_b: z.string().min(1),

  value_a: z.unknown(),
  value_b: z.unknown(),

  conflict_type: z.enum([
    'value_divergence',
    'proof_mismatch',
    'confidence_drop',
    'method_change',
  ]),

  first_timestamp_ns: z.number().positive().int(),
  resolution: ResolutionSchema.nullable().optional(),

  metadata: z.object({
    value_a_hash: z.string(),
    value_b_hash: z.string(),
    similarity: z.number().min(0).max(1),
    is_transitive: z.boolean().optional(),
  }).optional(),
});

// ============================================================================
// 6. MERGED CLAIM ENTRY
// ============================================================================

/** Entry in the merged claims map (latest value for key) */
export const MergedClaimEntrySchema = z.object({
  claim: ClaimSchema,
  timestamp_ns: z.number().positive().int(),
  agent_id: z.string().min(1),
  version: z.number().positive().int(),
});

// ============================================================================
// 7. RDF QUAD SCHEMA
// ============================================================================

/** RDF quad (subject, predicate, object, graph) */
export const RDFQuadSchema = z.object({
  subject: z.string().min(1),
  predicate: z.string().min(1),
  object: z.unknown(),
  graph: z.string().optional(),
  timestamp_ns: z.number().positive().int(),
  confidence: z.number().min(0).max(1),
  agent_id: z.string().min(1),
});

// ============================================================================
// 8. RESOURCE & UNIVERSE SCHEMAS
// ============================================================================

/** Value in resource property (may be array of values) */
export const ResourceValueSchema = z.object({
  value: z.unknown(),
  timestamp_ns: z.number().positive().int(),
  agent_id: z.string(),
  confidence: z.number().min(0).max(1),
});

/** Resource in the RDF universe */
export const ResourceSchema = z.object({
  id: z.string().min(1),
  type: z.array(z.string()),
  properties: z.record(z.array(ResourceValueSchema)),
  updated_at_ns: z.number().positive().int(),
  version: z.number().positive().int(),
  agent_id: z.string().min(1),
});

/** Agent contribution tracking */
export const AgentContributionSchema = z.object({
  claims_count: z.number().nonnegative().int(),
  domains: z.array(z.string()),
  last_update_ns: z.number().positive().int(),
  conflicts_count: z.number().nonnegative().int().optional(),
});

/** Metrics data point (time series) */
export const MetricValueSchema = z.object({
  timestamp_ns: z.number().positive().int(),
  value: z.number(),
  agent_id: z.string(),
  tags: z.record(z.string()).optional(),
});

/** Complete universe (RDF + metrics) */
export const ProbeUniverseSchema = z.object({
  metadata: z.object({
    created_at_ns: z.number().positive().int(),
    version: z.string(),
    total_agents: z.number().positive().int(),
    total_claims: z.number().nonnegative().int(),
  }),

  rdf_quads: z.array(RDFQuadSchema),
  metrics_store: z.record(z.array(MetricValueSchema)),
  resource_index: z.record(ResourceSchema),
  agent_contributions: z.record(AgentContributionSchema),
});

// ============================================================================
// 9. MERKLE RECEIPT SCHEMAS
// ============================================================================

/** Input shard information in receipt */
export const ReceiptShardInfoSchema = z.object({
  shard_hash: z.string(),
  claim_count: z.number().nonnegative().int(),
  timestamp_ns: z.number().positive().int(),
});

/** Merkle tree structure */
export const MerkleTreeSchema = z.object({
  root_hash: z.string(),
  levels: z.number().nonnegative().int(),
  leaf_hashes: z.array(z.string()),
});

/** Merkle receipt for verification */
export const MerkleReceiptSchema = z.object({
  algorithm_version: z.string(),

  input_shards: z.record(ReceiptShardInfoSchema),

  merge_tree: MerkleTreeSchema,

  claims_hash: z.string(),
  conflicts_hash: z.string(),

  algorithm_params: z.object({
    conflict_ttl_ns: z.number().positive().int(),
    max_conflicts: z.number().positive().int(),
    sort_key_schema: z.string(),
  }),

  computed_at_ns: z.number().positive().int(),
  computed_by: z.string().min(1),
  verification_token: z.string().min(1),
});

// ============================================================================
// 10. MERGE OUTPUT SCHEMAS
// ============================================================================

/** Statistics from merge operation */
export const MergeStatisticsSchema = z.object({
  total_shards: z.number().positive().int(),
  total_claims: z.number().nonnegative().int(),
  merged_unique_keys: z.number().nonnegative().int(),
  conflicts_detected: z.number().nonnegative().int(),
  resolved_conflicts: z.number().nonnegative().int(),
  unresolved_conflicts: z.number().nonnegative().int(),
  shard_agreement_ratio: z.number().min(0).max(1),
});

/** Performance timing breakdown */
export const PerformanceMetricsSchema = z.object({
  phase_1_flatten_ms: z.number().nonnegative(),
  phase_2_sort_ms: z.number().nonnegative(),
  phase_3_merge_ms: z.number().nonnegative(),
  phase_4_universe_ms: z.number().nonnegative(),
  phase_5_merkle_ms: z.number().nonnegative(),
  total_ms: z.number().nonnegative(),
  throughput_claims_per_sec: z.number().nonnegative(),
});

/** Merged world model (main result) */
export const MergedWorldModelSchema = z.object({
  claims: z.record(MergedClaimEntrySchema),
  universe: ProbeUniverseSchema,
  conflicts: z.array(ConflictRecordSchema),
  receipt: MerkleReceiptSchema,
  stats: MergeStatisticsSchema,
});

/** Complete orchestration output */
export const MergeOutputSchema = z.object({
  orchestration: z.object({
    request_id: z.string().min(1),
    timestamp_ns: z.number().positive().int(),
    algorithm: z.string().min(1),
  }),

  merged_world_model: MergedWorldModelSchema,
  statistics: MergeStatisticsSchema,
  performance: PerformanceMetricsSchema,
});

// ============================================================================
// 11. CONFIGURATION SCHEMAS
// ============================================================================

/** Configuration for merge operation */
export const MergeConfigSchema = z.object({
  algorithm_version: z.string(),

  conflict_strategy: z.enum([
    'lww',
    'majority_vote',
    'quorum',
  ]),

  conflict_ttl_ns: z.number().positive().int(),
  max_conflicts: z.number().positive().int(),

  enable_incremental: z.boolean().default(true),
  verify_receipt: z.boolean().default(true),

  logging: z.object({
    log_level: z.enum(['debug', 'info', 'warn', 'error']),
    log_conflicts: z.boolean(),
    log_timing: z.boolean(),
  }),
});

// ============================================================================
// 12. SORT KEY SCHEMAS
// ============================================================================

/** Deterministic sort key for claims */
export const SortKeySchema = z.object({
  domain: z.string(),
  method: z.string(),
  resource_key: z.string(),
  timestamp_ns: z.number().positive().int(),
  agent_id: z.string(),
});

// ============================================================================
// EXPORTS: Type inferences from Zod schemas
// ============================================================================

export type Claim = z.infer<typeof ClaimSchema>;
export type AgentShard = z.infer<typeof AgentShardSchema>;
export type ConflictRecord = z.infer<typeof ConflictRecordSchema>;
export type ProbeUniverse = z.infer<typeof ProbeUniverseSchema>;
export type MerkleReceipt = z.infer<typeof MerkleReceiptSchema>;
export type MergedWorldModel = z.infer<typeof MergedWorldModelSchema>;
export type MergeOutput = z.infer<typeof MergeOutputSchema>;
export type MergeConfig = z.infer<typeof MergeConfigSchema>;
export type RDFQuad = z.infer<typeof RDFQuadSchema>;

/**
 * Validation helper function
 */
export function validateShard(shard: unknown): AgentShard {
  return AgentShardSchema.parse(shard);
}

export function validateClaim(claim: unknown): Claim {
  return ClaimSchema.parse(claim);
}

export function validateOutput(output: unknown): MergeOutput {
  return MergeOutputSchema.parse(output);
}

export function validateConfig(config: unknown): MergeConfig {
  return MergeConfigSchema.parse(config);
}

export default {
  ClaimSchema,
  AgentShardSchema,
  ConflictRecordSchema,
  ProbeUniverseSchema,
  MerkleReceiptSchema,
  MergedWorldModelSchema,
  MergeOutputSchema,
  MergeConfigSchema,
  validateShard,
  validateClaim,
  validateOutput,
  validateConfig,
};
