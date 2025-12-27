/**
 * @fileoverview KGC Probe - Public API
 *
 * High-level entry points for:
 * - Creating probe orchestrator instances
 * - Registering guards and agents
 * - Running scans and merging results
 * - Validating artifacts
 * - KGC Markdown document validation (frontmatter, blocks, receipts)
 *
 * @module @unrdf/kgc-probe
 */

// ============================================================================
// ORCHESTRATOR
// ============================================================================

export { createProbeOrchestrator, ProbeOrchestrator } from './orchestrator.mjs';

// ============================================================================
// GUARDS REGISTRY
// ============================================================================

export { createGuardRegistry, GuardRegistry, PATTERNS } from './guards.mjs';

// ============================================================================
// OBSERVATION VALIDATOR
// ============================================================================

export { createObservationValidator, ObservationValidator } from './artifact.mjs';

// ============================================================================
// AGENTS (Factory Functions for 10 probe agents)
// ============================================================================

export {
  // Registry
  createAgentRegistry,
  AgentRegistry,
  Agent,

  // 10 Domain Agents
  OrchestratorAgent,
  RuntimeAgent,
  FilesystemAgent,
  WasmAgent,
  PerformanceAgent,
  NetworkAgent,
  ToolingAgent,
  StorageAgent,
  ConcurrencyAgent,
  SystemAgent,

  // Factory functions
  createOrchestratorAgent,
  createRuntimeAgent,
  createFilesystemAgent,
  createWasmAgent,
  createPerformanceAgent,
  createNetworkAgent,
  createToolingAgent,
  createStorageAgent,
  createConcurrencyAgent,
  createSystemAgent,

  // Backward compatibility
  CompletionAgent,
  ConsistencyAgent,
  ConformanceAgent,
  CoverageAgent,
  CachingAgent,
  CompletenessAgent,
  CoherenceAgent,
  ClusteringAgent,
  ClassificationAgent,
  CollaborationAgent,
  createCompletionAgent,
  createConsistencyAgent,
  createConformanceAgent,
  createCoverageAgent,
  createCachingAgent,
  createCompletenessAgent,
  createCoherenceAgent,
  createClusteringAgent,
  createClassificationAgent,
  createCollaborationAgent
} from './agents/index.mjs';

// ============================================================================
// STORAGE BACKENDS
// ============================================================================

export {
  MemoryStorage,
  FileStorage,
  DatabaseStorage,
  createMemoryStorage,
  createFileStorage,
  createDatabaseStorage,
  createStorage
} from './storage/index.mjs';

// ============================================================================
// RECEIPTS
// ============================================================================

export {
  // Constants
  HASH_ALGORITHM,
  RECEIPT_VERSION,
  RECEIPT_TYPES,

  // Hash utilities
  computeHash,
  computeChainHash,
  deterministicSerialize,

  // Merkle
  buildMerkleTree,
  verifyMerkleRoot,

  // Receipt creation
  createObservationReceipt,
  createMergeReceipt,
  createVerificationReceipt,

  // Verification
  verifyObservationReceipt,
  verifyMergeReceipt,
  verifyVerificationReceipt,

  // Confidence
  calculateConfidenceScore,
  getFailedChecks,
  summarizeVerification,

  // Chain builder
  ReceiptChainBuilder,
  createReceiptChainBuilder
} from './receipts/index.mjs';

// ============================================================================
// ARTIFACT OPERATIONS
// ============================================================================

export {
  mergeShards,
  diffArtifacts,
  verifyArtifact,
  serializeArtifact,
  deserializeArtifact,
  hashObservations,
  computeArtifactSummary
} from './artifact.mjs';

// ============================================================================
// CONVENIENCE: FULL SCAN
// ============================================================================

export { runProbe } from './probe.mjs';

// ============================================================================
// ZOD SCHEMAS (All schemas from SPARC specification)
// ============================================================================

export {
  // Constants & Regexes
  SHA256_REGEX,
  UUID_V4_REGEX,
  SEMVER_REGEX,

  // KGC Markdown Frontmatter Schemas (Domain 1)
  SourceSchema,
  BoundsSchema,
  AuthorSchema,
  DiatasisViewSchema,
  FrontmatterSchema,

  // Block Metadata Schemas (Domain 2)
  BlockTypeSchema,
  OutputFormatSchema,
  DeterminismLevelSchema,
  BlockMetadataSchema,
  QueryTypeSchema,
  ResultBoundsSchema,
  QueryMetadataSchema,
  ExtractionTypeSchema,
  ExtractMetadataSchema,
  RenderMetadataSchema,
  ProofTypeSchema,
  ProofMetadataSchema,

  // Receipt Schemas (Domain 3)
  DecisionSchema,
  MerkleProofSchema,
  ReceiptSchema,

  // Dynamic Section Schemas (Domain 4)
  DynamicSectionSchema,

  // Error Schemas (Domain 10)
  ErrorSeveritySchema,
  ErrorTypeSchema,
  ProbeErrorSchema,
  ProbeWarningSchema,

  // Probe Report Schemas
  DomainResultSchema,
  ProbeReportSchema,

  // Observation & Artifact Schemas
  ObservationSchema,
  ObservationType,
  ArtifactSummarySchema,
  ArtifactSchema,
  ArtifactType,

  // Configuration Schemas
  ProbeConfigSchema,
  GuardConfigSchema,
  StorageConfigSchema,

  // Agent & Guard Schemas
  AgentSchema,
  GuardViolationSchema,
  GuardViolationType,

  // Diff & Result Schemas
  DiffResultSchema,
  DiffResultType,
  ScanResultSchema,
  ScanResultType
} from './types.mjs';

// ============================================================================
// FACTORY FUNCTIONS (25 factories per SPARC specification)
// ============================================================================

export {
  // Frontmatter & Sources
  createFrontmatter,
  createBounds,
  createSource,
  createAuthor,

  // Block Metadata
  createBlockMetadata,
  createQueryMetadata,
  createExtractMetadata,
  createRenderMetadata,
  createProofMetadata,

  // Receipts & Proofs
  createReceipt,
  createMerkleProof,

  // Observations
  createObservation,

  // Dynamic Sections
  createDynamicSection,

  // Errors & Warnings
  createProbeError,
  createProbeWarning,

  // Reports & Results
  createDomainResult,
  createProbeReport,

  // Configuration
  createProbeConfig
} from './types.mjs';

// ============================================================================
// VALIDATION FUNCTIONS
// ============================================================================

export {
  validateObservation,
  validateArtifact,
  validateProbeConfig,
  validateFrontmatter,
  validateReceipt,
  tryValidateObservation,
  tryValidateFrontmatter,
  tryValidateReceipt
} from './types.mjs';

// ============================================================================
// CLI COMMANDS
// ============================================================================

export {
  scanCommand,
  mergeCommand,
  diffCommand,
  reportCommand,
  verifyCommand,
  probeExtension,
  ScanArgsSchema,
  MergeArgsSchema,
  DiffArgsSchema,
  ReportArgsSchema,
  VerifyArgsSchema
} from './cli.mjs';

// ============================================================================
// UTILITIES (Logger, Errors)
// ============================================================================

export {
  Logger,
  createLogger,
  defaultLogger,
  LOG_LEVELS
} from './utils/logger.mjs';

export {
  ProbeError,
  GuardViolationError,
  ValidationError,
  MergeConflictError,
  ReceiptError,
  ArtifactNotFoundError,
  TimeoutError,
  AgentError,
  StorageError,
  ConfigurationError,
  ErrorCodes,
  isProbeError,
  wrapError
} from './utils/errors.mjs';

// ============================================================================
// DEFAULT EXPORT (Package Metadata)
// ============================================================================

/**
 * Package metadata
 * @type {Object}
 */
export default {
  name: '@unrdf/kgc-probe',
  version: '1.0.0',
  description: 'KGC Probe - Automated knowledge graph integrity scanning with 10 agents and artifact validation'
};
