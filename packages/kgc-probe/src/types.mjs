/**
 * @fileoverview KGC Probe - Type definitions and Zod schemas
 *
 * Runtime-validatable schemas for:
 * - KGC Markdown Frontmatter (o_hash, policy_id, receipts, bounds, etc.)
 * - Block Metadata (query, extract, render, proof blocks)
 * - Receipts (cryptographic proofs)
 * - Observations (agent outputs)
 * - Artifacts (scan results)
 * - Configuration objects
 *
 * @module @unrdf/kgc-probe/types
 */

import { z } from 'zod';

// ============================================================================
// CONSTANTS & REGEXES (per SPARC spec)
// ============================================================================

/**
 * SHA-256 hash pattern (64 lowercase hex characters)
 * @type {RegExp}
 */
export const SHA256_REGEX = /^[a-f0-9]{64}$/;

/**
 * UUID v4 pattern
 * @type {RegExp}
 */
export const UUID_V4_REGEX = /^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i;

/**
 * Semver pattern (e.g., 1.0.0, 2.1.3-beta.1+build.123)
 * @type {RegExp}
 */
export const SEMVER_REGEX = /^(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)(?:-((?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\.(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\+([0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*))?$/;

// ============================================================================
// KGC MARKDOWN FRONTMATTER SCHEMAS (SPARC Domain 1, 4, 8)
// ============================================================================

/**
 * Source file reference schema
 * References a specific range of lines in a source file with hash
 * @type {z.ZodSchema}
 */
export const SourceSchema = z.object({
  path: z.string().min(1).describe('Path to source file (no ../ allowed)'),
  lineStart: z.number().int().min(1).describe('Starting line number'),
  lineEnd: z.number().int().min(1).describe('Ending line number'),
  hash: z.string().regex(SHA256_REGEX, 'Must be 64-char hex SHA-256').describe('SHA-256 hash of source content')
}).refine(
  (data) => data.lineEnd >= data.lineStart,
  { message: 'lineEnd must be >= lineStart', path: ['lineEnd'] }
).refine(
  (data) => !data.path.includes('..'),
  { message: 'Path must not contain directory traversal (..)', path: ['path'] }
);

/**
 * Resource bounds schema
 * Limits for queries, runtime, and file scans
 * @type {z.ZodSchema}
 */
export const BoundsSchema = z.object({
  maxQueries: z.number().int().min(1).max(10000).describe('Maximum query result count [1, 10000]'),
  maxRuntime: z.number().int().min(100).max(60000).describe('Maximum runtime in ms [100, 60000]'),
  maxFileScans: z.number().int().min(1).max(1000).describe('Maximum file scans [1, 1000]')
});

/**
 * Author schema
 * @type {z.ZodSchema}
 */
export const AuthorSchema = z.object({
  name: z.string().min(1).describe('Author name'),
  role: z.string().optional().describe('Author role (optional)')
});

/**
 * Diataxis view types
 * @type {z.ZodEnum}
 */
export const DiatasisViewSchema = z.enum(['tutorial', 'how-to', 'reference', 'explanation']);

/**
 * KGC Markdown Frontmatter schema (Domain 1: Frontmatter Parsing)
 * Validates all frontmatter fields per SPARC specification
 * @type {z.ZodSchema}
 */
export const FrontmatterSchema = z.object({
  o_hash: z.string()
    .regex(SHA256_REGEX, 'o_hash must be 64-char lowercase hex SHA-256')
    .describe('Universe snapshot hash'),
  policy_id: z.string()
    .regex(UUID_V4_REGEX, 'policy_id must be valid UUID v4')
    .describe('Policy pack UUID'),
  receipts: z.array(
    z.string().regex(SHA256_REGEX, 'Receipt ID must be 64-char hex')
  ).min(0).max(1000).describe('Array of receipt IDs [0-1000 items]'),
  bounds: BoundsSchema.describe('Resource limits'),
  views: z.array(DiatasisViewSchema)
    .min(1).max(4)
    .describe('Diataxis view types'),
  sources: z.array(SourceSchema)
    .min(0).max(100)
    .describe('Source file references'),
  version: z.string()
    .regex(SEMVER_REGEX, 'version must be valid semver')
    .describe('Document version'),
  createdAt: z.string().datetime({ offset: true }).describe('Creation timestamp (ISO 8601)'),
  lastProved: z.string().datetime({ offset: true }).describe('Last proof timestamp (ISO 8601)'),
  tags: z.array(z.string().min(1).max(50)).max(20).optional().describe('Optional tags'),
  authors: z.array(AuthorSchema).optional().describe('Optional authors')
}).refine(
  (data) => new Date(data.lastProved) >= new Date(data.createdAt),
  { message: 'lastProved must be >= createdAt', path: ['lastProved'] }
);

// ============================================================================
// BLOCK METADATA SCHEMAS (SPARC Domain 2, 5, 8)
// ============================================================================

/**
 * Block types supported by KGC Markdown
 * @type {z.ZodEnum}
 */
export const BlockTypeSchema = z.enum(['kgc:query', 'kgc:proof', 'kgc:extract', 'kgc:render']);

/**
 * Output format options
 * @type {z.ZodEnum}
 */
export const OutputFormatSchema = z.enum(['json', 'markdown', 'text']);

/**
 * Determinism level options
 * @type {z.ZodEnum}
 */
export const DeterminismLevelSchema = z.enum(['strict', 'lenient', 'best-effort']);

/**
 * Base block metadata schema (shared by all block types)
 * @type {z.ZodSchema}
 */
export const BlockMetadataSchema = z.object({
  receiptId: z.string()
    .regex(SHA256_REGEX, 'receiptId must be 64-char hex')
    .describe('Receipt ID (SHA-256)'),
  expectedOutputFormat: OutputFormatSchema.describe('Expected output format'),
  determinismLevel: DeterminismLevelSchema.describe('Determinism guarantee level'),
  metadata: z.record(z.any()).optional().describe('Block-type-specific metadata')
});

/**
 * Query type options
 * @type {z.ZodEnum}
 */
export const QueryTypeSchema = z.enum(['sparql', 'n3', 'shacl']);

/**
 * Result bounds for queries
 * @type {z.ZodSchema}
 */
export const ResultBoundsSchema = z.object({
  minResults: z.number().int().min(0).describe('Minimum expected results'),
  maxResults: z.number().int().min(0).describe('Maximum expected results')
});

/**
 * Query block metadata schema (kgc:query)
 * @type {z.ZodSchema}
 */
export const QueryMetadataSchema = BlockMetadataSchema.extend({
  queryType: QueryTypeSchema.describe('Query language type'),
  resultBounds: ResultBoundsSchema.describe('Expected result count range'),
  timeout: z.number().int().min(100).max(60000).describe('Query timeout in ms')
});

/**
 * Extraction type options
 * @type {z.ZodEnum}
 */
export const ExtractionTypeSchema = z.enum(['exports', 'functions', 'classes', 'types', 'all']);

/**
 * Extract block metadata schema (kgc:extract)
 * @type {z.ZodSchema}
 */
export const ExtractMetadataSchema = BlockMetadataSchema.extend({
  extractionType: ExtractionTypeSchema.describe('What to extract'),
  fileGlobs: z.array(z.string()).min(1).describe('File patterns to scan'),
  includePrivate: z.boolean().default(false).describe('Include private members'),
  includeDocstrings: z.boolean().default(true).describe('Include documentation')
});

/**
 * Render block metadata schema (kgc:render)
 * @type {z.ZodSchema}
 */
export const RenderMetadataSchema = BlockMetadataSchema.extend({
  templateName: z.string().min(1).describe('Template to use'),
  sectionTitle: z.string().min(1).describe('Section heading'),
  includeTableOfContents: z.boolean().default(false).describe('Include TOC')
});

/**
 * Proof type options
 * @type {z.ZodEnum}
 */
export const ProofTypeSchema = z.enum(['merkle', 'hash-chain', 'single']);

/**
 * Proof block metadata schema (kgc:proof)
 * @type {z.ZodSchema}
 */
export const ProofMetadataSchema = BlockMetadataSchema.extend({
  proofType: ProofTypeSchema.describe('Type of cryptographic proof'),
  verifyChain: z.boolean().default(true).describe('Verify dependency chain'),
  validateSignatures: z.boolean().default(false).describe('Validate signatures')
});

// ============================================================================
// RECEIPT SCHEMAS (SPARC Domain 3, 7)
// ============================================================================

/**
 * Receipt decision options
 * @type {z.ZodEnum}
 */
export const DecisionSchema = z.enum(['ADMIT', 'REJECT', 'PARTIAL']);

/**
 * Merkle proof schema (optional in receipts)
 * @type {z.ZodSchema}
 */
export const MerkleProofSchema = z.object({
  siblings: z.array(z.string().regex(SHA256_REGEX)).describe('Sibling hashes'),
  root: z.string().regex(SHA256_REGEX).describe('Merkle root hash'),
  index: z.number().int().min(0).describe('Leaf index'),
  totalLeaves: z.number().int().min(1).describe('Total leaf count')
});

/**
 * Receipt schema (Domain 3: Receipt Validation)
 * Cryptographic proof of block execution
 * @type {z.ZodSchema}
 */
export const ReceiptSchema = z.object({
  id: z.string()
    .regex(SHA256_REGEX, 'Receipt ID must be SHA-256 of canonical body')
    .describe('Receipt ID (SHA-256)'),
  timestamp: z.string().datetime({ offset: true }).describe('Execution timestamp'),
  o_hash: z.string()
    .regex(SHA256_REGEX, 'o_hash must match frontmatter')
    .describe('Universe snapshot hash'),
  block_type: BlockTypeSchema.describe('Block type that produced this receipt'),
  input_hash: z.string()
    .regex(SHA256_REGEX, 'input_hash is SHA-256 of block metadata + body')
    .describe('Hash of block input'),
  output_hash: z.string()
    .regex(SHA256_REGEX, 'output_hash is SHA-256 of generated content (RFC 8785)')
    .describe('Hash of block output'),
  decision: DecisionSchema.describe('Execution decision'),
  metadata: z.record(z.any()).optional().describe('Block-specific metadata'),
  dependencies: z.array(
    z.string().regex(SHA256_REGEX)
  ).optional().describe('Receipt IDs this depends on'),
  merkle_proof: MerkleProofSchema.optional().describe('Merkle proof (batch receipts)')
});

// ============================================================================
// DYNAMIC SECTION SCHEMAS (SPARC Domain 4)
// ============================================================================

/**
 * Dynamic section schema
 * Content between <!-- kgc:dynamic --> markers
 * @type {z.ZodSchema}
 */
export const DynamicSectionSchema = z.object({
  name: z.string().min(1).describe('Section identifier'),
  receiptId: z.string().regex(SHA256_REGEX).describe('Associated receipt ID'),
  lineStart: z.number().int().min(1).describe('Opening marker line'),
  lineEnd: z.number().int().min(1).describe('Closing marker line'),
  contentHash: z.string().regex(SHA256_REGEX).optional().describe('Computed content hash')
});

// ============================================================================
// PROBE ERROR SCHEMAS (SPARC Domain 10)
// ============================================================================

/**
 * Error severity levels
 * @type {z.ZodEnum}
 */
export const ErrorSeveritySchema = z.enum(['error', 'warning', 'info']);

/**
 * Error type enumeration (all possible probe errors)
 * @type {z.ZodEnum}
 */
export const ErrorTypeSchema = z.enum([
  'InvalidFrontmatter',
  'MissingReceipt',
  'MismatchedHash',
  'BoundsExceeded',
  'NonDeterministic',
  'InvalidBlockStructure',
  'CyclicDependency',
  'MissingDependency',
  'TimestampOrderViolation',
  'UniverseHashMismatch',
  'OrphanedSection',
  'UnmappedReceipt',
  'BrokenLink',
  'HeadingHierarchyViolation',
  'DuplicateHeading',
  'SchemaViolation'
]);

/**
 * Probe error schema with location and remediation
 * @type {z.ZodSchema}
 */
export const ProbeErrorSchema = z.object({
  type: ErrorTypeSchema.describe('Error classification'),
  severity: ErrorSeveritySchema.describe('Impact severity'),
  message: z.string().min(1).describe('Human-readable message'),
  location: z.object({
    file: z.string().optional().describe('File path'),
    line: z.number().int().min(1).optional().describe('Line number'),
    field: z.string().optional().describe('Field name')
  }).optional().describe('Error location'),
  actual: z.unknown().optional().describe('Actual value observed'),
  expected: z.unknown().optional().describe('Expected value'),
  remediation: z.string().optional().describe('How to fix')
});

/**
 * Probe warning (non-blocking issue)
 * @type {z.ZodSchema}
 */
export const ProbeWarningSchema = ProbeErrorSchema.extend({
  severity: z.literal('warning')
});

// ============================================================================
// PROBE REPORT SCHEMAS (SPARC Success Metrics)
// ============================================================================

/**
 * Domain validation result
 * @type {z.ZodSchema}
 */
export const DomainResultSchema = z.object({
  domain: z.string().describe('Domain name'),
  passed: z.boolean().describe('Whether domain checks passed'),
  checks: z.number().int().min(0).describe('Number of checks run'),
  errors: z.array(ProbeErrorSchema).describe('Errors found'),
  warnings: z.array(ProbeWarningSchema).describe('Warnings found')
});

/**
 * Complete probe report schema
 * @type {z.ZodSchema}
 */
export const ProbeReportSchema = z.object({
  version: z.literal('1.0').describe('Report schema version'),
  documentPath: z.string().describe('Scanned document path'),
  timestamp: z.string().datetime({ offset: true }).describe('Scan timestamp'),
  status: z.enum(['PASS', 'FAIL', 'PARTIAL']).describe('Overall status'),
  domains: z.array(DomainResultSchema).describe('Results per domain'),
  summary: z.object({
    totalChecks: z.number().int().min(0).describe('Total checks run'),
    totalPassed: z.number().int().min(0).describe('Checks passed'),
    totalErrors: z.number().int().min(0).describe('Error count'),
    totalWarnings: z.number().int().min(0).describe('Warning count'),
    coverage: z.number().min(0).max(1).describe('Document coverage ratio')
  }).describe('Summary statistics'),
  metadata: z.object({
    probeVersion: z.string().describe('Probe version'),
    executionTimeMs: z.number().int().min(0).describe('Execution time'),
    documentSize: z.number().int().min(0).optional().describe('Document size in bytes')
  }).describe('Execution metadata')
});

// ============================================================================
// OBSERVATION SCHEMA (Agent outputs - existing)
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
    'collaboration',
    'guard_violation'
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
// FACTORY FUNCTIONS (25 factories per SPARC spec)
// ============================================================================

/**
 * Create valid frontmatter object with defaults
 * @param {Partial<z.infer<typeof FrontmatterSchema>>} data - Partial frontmatter
 * @returns {z.infer<typeof FrontmatterSchema>} Complete frontmatter
 * @example
 * const fm = createFrontmatter({
 *   o_hash: 'a'.repeat(64),
 *   policy_id: '550e8400-e29b-41d4-a716-446655440000'
 * });
 */
export function createFrontmatter(data) {
  const now = new Date().toISOString();
  const defaults = {
    o_hash: '0'.repeat(64),
    policy_id: '00000000-0000-4000-8000-000000000000',
    receipts: [],
    bounds: { maxQueries: 100, maxRuntime: 5000, maxFileScans: 50 },
    views: ['reference'],
    sources: [],
    version: '1.0.0',
    createdAt: now,
    lastProved: now
  };
  return FrontmatterSchema.parse({ ...defaults, ...data });
}

/**
 * Create valid bounds object
 * @param {Partial<z.infer<typeof BoundsSchema>>} data - Partial bounds
 * @returns {z.infer<typeof BoundsSchema>} Complete bounds
 */
export function createBounds(data = {}) {
  const defaults = { maxQueries: 100, maxRuntime: 5000, maxFileScans: 50 };
  return BoundsSchema.parse({ ...defaults, ...data });
}

/**
 * Create valid source reference
 * @param {Partial<z.infer<typeof SourceSchema>>} data - Partial source
 * @returns {z.infer<typeof SourceSchema>} Complete source
 */
export function createSource(data) {
  const defaults = {
    path: 'src/index.mjs',
    lineStart: 1,
    lineEnd: 100,
    hash: '0'.repeat(64)
  };
  return SourceSchema.parse({ ...defaults, ...data });
}

/**
 * Create valid author object
 * @param {Partial<z.infer<typeof AuthorSchema>>} data - Partial author
 * @returns {z.infer<typeof AuthorSchema>} Complete author
 */
export function createAuthor(data) {
  const defaults = { name: 'Anonymous' };
  return AuthorSchema.parse({ ...defaults, ...data });
}

/**
 * Create valid block metadata
 * @param {Partial<z.infer<typeof BlockMetadataSchema>>} data - Partial metadata
 * @returns {z.infer<typeof BlockMetadataSchema>} Complete metadata
 */
export function createBlockMetadata(data) {
  const defaults = {
    receiptId: '0'.repeat(64),
    expectedOutputFormat: 'json',
    determinismLevel: 'strict'
  };
  return BlockMetadataSchema.parse({ ...defaults, ...data });
}

/**
 * Create valid query metadata
 * @param {Partial<z.infer<typeof QueryMetadataSchema>>} data - Partial metadata
 * @returns {z.infer<typeof QueryMetadataSchema>} Complete metadata
 */
export function createQueryMetadata(data) {
  const defaults = {
    receiptId: '0'.repeat(64),
    expectedOutputFormat: 'json',
    determinismLevel: 'strict',
    queryType: 'sparql',
    resultBounds: { minResults: 0, maxResults: 1000 },
    timeout: 5000
  };
  return QueryMetadataSchema.parse({ ...defaults, ...data });
}

/**
 * Create valid extract metadata
 * @param {Partial<z.infer<typeof ExtractMetadataSchema>>} data - Partial metadata
 * @returns {z.infer<typeof ExtractMetadataSchema>} Complete metadata
 */
export function createExtractMetadata(data) {
  const defaults = {
    receiptId: '0'.repeat(64),
    expectedOutputFormat: 'json',
    determinismLevel: 'lenient',
    extractionType: 'exports',
    fileGlobs: ['src/**/*.mjs'],
    includePrivate: false,
    includeDocstrings: true
  };
  return ExtractMetadataSchema.parse({ ...defaults, ...data });
}

/**
 * Create valid render metadata
 * @param {Partial<z.infer<typeof RenderMetadataSchema>>} data - Partial metadata
 * @returns {z.infer<typeof RenderMetadataSchema>} Complete metadata
 */
export function createRenderMetadata(data) {
  const defaults = {
    receiptId: '0'.repeat(64),
    expectedOutputFormat: 'markdown',
    determinismLevel: 'strict',
    templateName: 'api-reference',
    sectionTitle: 'API Reference',
    includeTableOfContents: true
  };
  return RenderMetadataSchema.parse({ ...defaults, ...data });
}

/**
 * Create valid proof metadata
 * @param {Partial<z.infer<typeof ProofMetadataSchema>>} data - Partial metadata
 * @returns {z.infer<typeof ProofMetadataSchema>} Complete metadata
 */
export function createProofMetadata(data) {
  const defaults = {
    receiptId: '0'.repeat(64),
    expectedOutputFormat: 'json',
    determinismLevel: 'strict',
    proofType: 'merkle',
    verifyChain: true,
    validateSignatures: false
  };
  return ProofMetadataSchema.parse({ ...defaults, ...data });
}

/**
 * Create valid receipt object
 * @param {Partial<z.infer<typeof ReceiptSchema>>} data - Partial receipt
 * @returns {z.infer<typeof ReceiptSchema>} Complete receipt
 */
export function createReceipt(data) {
  const now = new Date().toISOString();
  const defaults = {
    id: '0'.repeat(64),
    timestamp: now,
    o_hash: '0'.repeat(64),
    block_type: 'kgc:query',
    input_hash: '0'.repeat(64),
    output_hash: '0'.repeat(64),
    decision: 'ADMIT'
  };
  return ReceiptSchema.parse({ ...defaults, ...data });
}

/**
 * Create valid merkle proof
 * @param {Partial<z.infer<typeof MerkleProofSchema>>} data - Partial proof
 * @returns {z.infer<typeof MerkleProofSchema>} Complete proof
 */
export function createMerkleProof(data) {
  const defaults = {
    siblings: [],
    root: '0'.repeat(64),
    index: 0,
    totalLeaves: 1
  };
  return MerkleProofSchema.parse({ ...defaults, ...data });
}

/**
 * Create valid dynamic section
 * @param {Partial<z.infer<typeof DynamicSectionSchema>>} data - Partial section
 * @returns {z.infer<typeof DynamicSectionSchema>} Complete section
 */
export function createDynamicSection(data) {
  const defaults = {
    name: 'section',
    receiptId: '0'.repeat(64),
    lineStart: 1,
    lineEnd: 100
  };
  return DynamicSectionSchema.parse({ ...defaults, ...data });
}

/**
 * Create valid probe error
 * @param {Partial<z.infer<typeof ProbeErrorSchema>>} data - Partial error
 * @returns {z.infer<typeof ProbeErrorSchema>} Complete error
 */
export function createProbeError(data) {
  const defaults = {
    type: 'SchemaViolation',
    severity: 'error',
    message: 'Validation failed'
  };
  return ProbeErrorSchema.parse({ ...defaults, ...data });
}

/**
 * Create valid probe warning
 * @param {Partial<z.infer<typeof ProbeWarningSchema>>} data - Partial warning
 * @returns {z.infer<typeof ProbeWarningSchema>} Complete warning
 */
export function createProbeWarning(data) {
  const defaults = {
    type: 'BoundsExceeded',
    severity: 'warning',
    message: 'Resource utilization high'
  };
  return ProbeWarningSchema.parse({ ...defaults, ...data });
}

/**
 * Create valid domain result
 * @param {Partial<z.infer<typeof DomainResultSchema>>} data - Partial result
 * @returns {z.infer<typeof DomainResultSchema>} Complete result
 */
export function createDomainResult(data) {
  const defaults = {
    domain: 'frontmatter',
    passed: true,
    checks: 10,
    errors: [],
    warnings: []
  };
  return DomainResultSchema.parse({ ...defaults, ...data });
}

/**
 * Create valid probe report
 * @param {Partial<z.infer<typeof ProbeReportSchema>>} data - Partial report
 * @returns {z.infer<typeof ProbeReportSchema>} Complete report
 */
export function createProbeReport(data) {
  const now = new Date().toISOString();
  const defaults = {
    version: '1.0',
    documentPath: 'docs/example.kgcmd',
    timestamp: now,
    status: 'PASS',
    domains: [],
    summary: {
      totalChecks: 0,
      totalPassed: 0,
      totalErrors: 0,
      totalWarnings: 0,
      coverage: 1.0
    },
    metadata: {
      probeVersion: '1.0.0',
      executionTimeMs: 0
    }
  };
  return ProbeReportSchema.parse({ ...defaults, ...data });
}

/**
 * Create probe configuration
 * @param {Partial<z.infer<typeof ProbeConfigSchema>>} data - Partial config
 * @returns {z.infer<typeof ProbeConfigSchema>} Complete config
 */
export function createProbeConfig(data) {
  const defaults = {
    universe_id: 'default-universe',
    distributed: false,
    persist: true,
    timeout_ms: 300000,
    batch_size: 100
  };
  return ProbeConfigSchema.parse({ ...defaults, ...data });
}

/**
 * Create valid observation
 * @param {Partial<z.infer<typeof ObservationSchema>>} data - Partial observation
 * @returns {z.infer<typeof ObservationSchema>} Complete observation
 */
export function createObservation(data) {
  const now = new Date().toISOString();
  const generateUUID = () => {
    return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
      const r = (Math.random() * 16) | 0;
      const v = c === 'x' ? r : (r & 0x3) | 0x8;
      return v.toString(16);
    });
  };
  const defaults = {
    id: generateUUID(),
    agent: 'unknown',
    timestamp: now,
    kind: 'completeness',
    severity: 'info',
    subject: '',
    evidence: {
      query: '',
      result: null,
      witnesses: []
    },
    metrics: {
      confidence: 0.5,
      coverage: 0.5,
      latency_ms: 0
    },
    tags: []
  };
  return ObservationSchema.parse({ ...defaults, ...data });
}

// ============================================================================
// VALIDATION HELPER FUNCTIONS
// ============================================================================

/**
 * Validate observation against schema
 * @param {unknown} data - Data to validate
 * @returns {z.infer<typeof ObservationSchema>} Validated observation
 * @throws {z.ZodError} If validation fails
 */
export function validateObservation(data) {
  return ObservationSchema.parse(data);
}

/**
 * Validate artifact against schema
 * @param {unknown} data - Data to validate
 * @returns {z.infer<typeof ArtifactSchema>} Validated artifact
 * @throws {z.ZodError} If validation fails
 */
export function validateArtifact(data) {
  return ArtifactSchema.parse(data);
}

/**
 * Validate probe configuration
 * @param {unknown} data - Data to validate
 * @returns {z.infer<typeof ProbeConfigSchema>} Validated config
 * @throws {z.ZodError} If validation fails
 */
export function validateProbeConfig(data) {
  return ProbeConfigSchema.parse(data);
}

/**
 * Validate frontmatter
 * @param {unknown} data - Data to validate
 * @returns {z.infer<typeof FrontmatterSchema>} Validated frontmatter
 * @throws {z.ZodError} If validation fails
 */
export function validateFrontmatter(data) {
  return FrontmatterSchema.parse(data);
}

/**
 * Validate receipt
 * @param {unknown} data - Data to validate
 * @returns {z.infer<typeof ReceiptSchema>} Validated receipt
 * @throws {z.ZodError} If validation fails
 */
export function validateReceipt(data) {
  return ReceiptSchema.parse(data);
}

/**
 * Safe validation (returns null instead of throwing)
 * @param {unknown} data - Data to validate
 * @returns {z.infer<typeof ObservationSchema> | null} Validated observation or null
 */
export function tryValidateObservation(data) {
  const result = ObservationSchema.safeParse(data);
  return result.success ? result.data : null;
}

/**
 * Safe frontmatter validation
 * @param {unknown} data - Data to validate
 * @returns {z.infer<typeof FrontmatterSchema> | null} Validated or null
 */
export function tryValidateFrontmatter(data) {
  const result = FrontmatterSchema.safeParse(data);
  return result.success ? result.data : null;
}

/**
 * Safe receipt validation
 * @param {unknown} data - Data to validate
 * @returns {z.infer<typeof ReceiptSchema> | null} Validated or null
 */
export function tryValidateReceipt(data) {
  const result = ReceiptSchema.safeParse(data);
  return result.success ? result.data : null;
}
