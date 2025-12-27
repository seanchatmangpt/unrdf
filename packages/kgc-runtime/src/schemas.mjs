/**
 * @file KGC Runtime Schemas - Comprehensive Zod validation for governance substrate
 * @module @unrdf/kgc-runtime/schemas
 *
 * @description
 * Type-safe Zod schemas for KGC (Knowledge Graph Computation) governance runtime:
 * - Receipt: Versioned, immutable audit records with cryptographic anchoring
 * - RunCapsule: Δ_run representation capturing input/output/trace/artifacts
 * - ToolTraceEntry: Atomic tool call records for deterministic replay
 * - Bounds: Capacity limits enforcing resource constraints
 * - WorkItem: Async task node states for distributed execution
 * - ProjectionManifest: Surface definitions for CLI/docs/IDE integration
 * - KGCMarkdown AST: Front matter + fenced block structured documents
 *
 * All schemas are v1 format with strict validation and comprehensive examples.
 *
 * @example
 * import { ReceiptSchema, RunCapsuleSchema } from '@unrdf/kgc-runtime/schemas';
 *
 * const receipt = ReceiptSchema.parse({
 *   version: '1.0.0',
 *   id: crypto.randomUUID(),
 *   timestamp: Date.now(),
 *   runId: 'run-001',
 *   actor: 'agent:orchestrator',
 *   action: 'execute',
 *   payload: { command: 'test' },
 * });
 */

import { z } from 'zod';

// =============================================================================
// Core Type Definitions
// =============================================================================

/**
 * ISO 8601 timestamp string schema
 * @constant
 */
const TimestampSchema = z.coerce.date();

/**
 * Semantic version schema (semver)
 * @constant
 */
const SemanticVersionSchema = z
  .string()
  .regex(/^\d+\.\d+\.\d+$/, 'Must be semantic version format (x.y.z)');

/**
 * UUID v4 schema
 * @constant
 */
const UUIDSchema = z.string().uuid('Must be a valid UUID v4');

/**
 * Content-addressable hash schema (SHA-256)
 * @constant
 */
const SHA256HashSchema = z
  .string()
  .length(64)
  .regex(/^[a-f0-9]{64}$/, 'Must be a valid SHA-256 hash (64 hex chars)');

/**
 * Actor identifier schema (agent:name, user:id, system:component)
 * @constant
 */
const ActorIdSchema = z
  .string()
  .regex(/^(agent|user|system):[a-zA-Z0-9_-]+$/, 'Must be formatted as type:identifier');

// =============================================================================
// Receipt Schema - Versioned Immutable Audit Records
// =============================================================================

/**
 * Receipt schema for versioned, immutable audit records
 *
 * Captures complete execution trace with cryptographic anchoring:
 * - Immutable event log (never modified, only appended)
 * - Content-addressable storage (hash-based retrieval)
 * - Cryptographic verification (signatures + merkle proofs)
 * - Version-aware format (v1.0.0 schema)
 * - Actor attribution (who executed what when)
 *
 * @example
 * {
 *   version: '1.0.0',
 *   id: '550e8400-e29b-41d4-a716-446655440000',
 *   timestamp: 1703001600000,
 *   runId: 'run-2024-001',
 *   actor: 'agent:orchestrator',
 *   action: 'execute_workflow',
 *   payload: { workflowId: 'wf-001', input: { x: 42 } },
 *   result: { success: true, output: { y: 84 } },
 *   contentHash: 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855',
 *   previousHash: 'a1b2c3d4...',
 *   signature: {
 *     algorithm: 'ed25519',
 *     publicKey: '0x1234...',
 *     value: '0xabcd...'
 *   }
 * }
 *
 * @constant
 * @type {z.ZodObject}
 */
export const ReceiptSchema = z.object({
  /** Schema version for forward compatibility */
  version: SemanticVersionSchema.default('1.0.0'),
  /** Unique receipt identifier (UUID v4) */
  id: UUIDSchema,
  /** Receipt creation timestamp (Unix epoch ms) */
  timestamp: z.number().int().positive(),
  /** Associated run/execution identifier */
  runId: z.string().min(1).max(200),
  /** Actor who initiated the action */
  actor: ActorIdSchema,
  /** Action performed (execute, validate, commit, rollback, etc.) */
  action: z.enum([
    'execute',
    'validate',
    'commit',
    'rollback',
    'checkpoint',
    'snapshot',
    'merge',
    'fork',
  ]),
  /** Action-specific payload (flexible structure) */
  payload: z.record(z.string(), z.any()),
  /** Execution result (success/failure + output) */
  result: z
    .object({
      success: z.boolean(),
      output: z.any().optional(),
      error: z.string().optional(),
      duration: z.number().nonnegative().optional(),
    })
    .optional(),
  /** SHA-256 hash of receipt content (for integrity) */
  contentHash: SHA256HashSchema.optional(),
  /** Hash of previous receipt (blockchain-style linking) */
  previousHash: SHA256HashSchema.optional().nullable(),
  /** Cryptographic signature for authenticity */
  signature: z
    .object({
      algorithm: z.enum(['ed25519', 'ecdsa', 'rsa']),
      publicKey: z.string().min(1),
      value: z.string().min(1),
    })
    .optional(),
  /** Merkle tree proof for batch verification */
  merkleProof: z
    .object({
      root: SHA256HashSchema,
      path: z.array(SHA256HashSchema),
      index: z.number().int().nonnegative(),
    })
    .optional(),
  /** External anchoring references (git commit, blockchain tx, etc.) */
  anchors: z
    .array(
      z.object({
        type: z.enum(['git', 'blockchain', 'timestamp', 'ipfs']),
        reference: z.string().min(1),
        timestamp: z.number().int().positive().optional(),
      })
    )
    .optional(),
  /** Additional metadata */
  metadata: z.record(z.string(), z.any()).optional(),
});

// =============================================================================
// ToolTraceEntry Schema - Atomic Tool Call Records
// =============================================================================

/**
 * Tool trace entry schema for atomic tool call records
 *
 * Captures individual tool invocations for:
 * - Deterministic replay (recreate exact execution)
 * - Debugging/audit trails (what happened when)
 * - Performance profiling (tool-level metrics)
 * - Causality tracking (input → output chains)
 *
 * @example
 * {
 *   id: '123e4567-e89b-12d3-a456-426614174000',
 *   timestamp: 1703001600000,
 *   toolName: 'Bash',
 *   input: { command: 'npm test', timeout: 5000 },
 *   output: { stdout: '✅ All tests passed', stderr: '', exitCode: 0 },
 *   duration: 2314,
 *   status: 'success',
 *   parentId: 'parent-trace-001'
 * }
 *
 * @constant
 * @type {z.ZodObject}
 */
export const ToolTraceEntrySchema = z.object({
  /** Unique trace entry identifier */
  id: UUIDSchema,
  /** Tool invocation timestamp (Unix epoch ms) */
  timestamp: z.number().int().positive(),
  /** Tool name (Bash, Read, Write, Grep, etc.) */
  toolName: z.string().min(1).max(100),
  /** Tool input parameters (preserves exact call signature) */
  input: z.record(z.string(), z.any()),
  /** Tool output/result */
  output: z.any().optional(),
  /** Execution duration in milliseconds */
  duration: z.number().nonnegative(),
  /** Execution status */
  status: z.enum(['success', 'error', 'timeout', 'cancelled']),
  /** Error details if status is 'error' */
  error: z
    .object({
      message: z.string(),
      code: z.string().optional(),
      stack: z.string().optional(),
    })
    .optional(),
  /** Parent trace entry (for nested calls) */
  parentId: UUIDSchema.optional().nullable(),
  /** Causal dependencies (trace IDs this call depends on) */
  dependencies: z.array(UUIDSchema).optional(),
  /** Resource usage metrics */
  resources: z
    .object({
      cpuTime: z.number().nonnegative().optional(),
      memoryPeak: z.number().nonnegative().optional(),
      ioBytes: z.number().nonnegative().optional(),
    })
    .optional(),
  /** Metadata (tags, annotations, etc.) */
  metadata: z.record(z.string(), z.any()).optional(),
});

// =============================================================================
// RunCapsule Schema - Δ_run Representation
// =============================================================================

/**
 * Run capsule schema for complete execution snapshot (Δ_run)
 *
 * Encapsulates entire run lifecycle:
 * - Input: Initial state + parameters
 * - Output: Final results + artifacts
 * - Trace: Execution path (tool calls, decisions)
 * - Artifacts: Generated files, data, proofs
 * - Provenance: Who/what/when/why attribution
 *
 * Enables:
 * - Reproducibility: Re-execute from capsule
 * - Auditing: Complete execution history
 * - Caching: Reuse results for identical inputs
 * - Debugging: Replay with full context
 *
 * @example
 * {
 *   id: 'run-2024-12-26-001',
 *   version: '1.0.0',
 *   startTime: 1703001600000,
 *   endTime: 1703001620000,
 *   status: 'completed',
 *   input: {
 *     task: 'implement feature X',
 *     parameters: { timeout: 5000 },
 *     context: { workingDir: '/home/user/project' }
 *   },
 *   output: {
 *     success: true,
 *     results: { filesChanged: 5, testsAdded: 12 },
 *     artifacts: [
 *       { type: 'file', path: '/home/user/project/src/feature.mjs', hash: 'abc123...' }
 *     ]
 *   },
 *   trace: [
 *     { id: 'trace-001', toolName: 'Bash', input: { command: 'npm test' }, ... },
 *     { id: 'trace-002', toolName: 'Write', input: { file_path: '...' }, ... }
 *   ],
 *   bounds: {
 *     maxFiles: 100,
 *     maxBytes: 10485760,
 *     maxOps: 1000,
 *     maxRuntime: 300000
 *   },
 *   actor: 'agent:backend-dev',
 *   receipt: { id: 'receipt-001', ... }
 * }
 *
 * @constant
 * @type {z.ZodObject}
 */
export const RunCapsuleSchema = z.object({
  /** Unique run identifier */
  id: z.string().min(1).max(200),
  /** Schema version */
  version: SemanticVersionSchema.default('1.0.0'),
  /** Run start timestamp (Unix epoch ms) */
  startTime: z.number().int().positive(),
  /** Run end timestamp (Unix epoch ms, null if in progress) */
  endTime: z.number().int().positive().optional().nullable(),
  /** Run execution status */
  status: z.enum(['pending', 'running', 'completed', 'failed', 'cancelled', 'timeout']),
  /** Input specification */
  input: z.object({
    /** Task description/objective */
    task: z.string().min(1).max(10000),
    /** Execution parameters */
    parameters: z.record(z.string(), z.any()).optional(),
    /** Initial context (env vars, working dir, etc.) */
    context: z.record(z.string(), z.any()).optional(),
    /** Input artifacts/files */
    artifacts: z
      .array(
        z.object({
          type: z.enum(['file', 'directory', 'url', 'inline']),
          path: z.string().optional(),
          content: z.string().optional(),
          hash: SHA256HashSchema.optional(),
          metadata: z.record(z.string(), z.any()).optional(),
        })
      )
      .optional(),
  }),
  /** Output results */
  output: z
    .object({
      /** Whether run succeeded */
      success: z.boolean(),
      /** Structured results */
      results: z.record(z.string(), z.any()).optional(),
      /** Generated artifacts */
      artifacts: z
        .array(
          z.object({
            type: z.enum(['file', 'directory', 'url', 'inline', 'proof', 'receipt']),
            path: z.string().optional(),
            content: z.string().optional(),
            hash: SHA256HashSchema.optional(),
            size: z.number().nonnegative().optional(),
            metadata: z.record(z.string(), z.any()).optional(),
          })
        )
        .optional(),
      /** Error information if failed */
      error: z
        .object({
          message: z.string(),
          code: z.string().optional(),
          stack: z.string().optional(),
          recoverable: z.boolean().optional(),
        })
        .optional(),
    })
    .optional(),
  /** Execution trace (ordered tool calls) */
  trace: z.array(ToolTraceEntrySchema).optional(),
  /** Resource bounds enforced during execution */
  bounds: z
    .object({
      maxFiles: z.number().int().positive().optional(),
      maxBytes: z.number().int().positive().optional(),
      maxOps: z.number().int().positive().optional(),
      maxRuntime: z.number().int().positive().optional(),
      maxGraphRewrites: z.number().int().positive().optional(),
    })
    .optional(),
  /** Actor who initiated the run */
  actor: ActorIdSchema,
  /** Associated receipt */
  receipt: ReceiptSchema.optional(),
  /** Provenance chain (parent runs, dependencies) */
  provenance: z
    .object({
      parentRunId: z.string().optional(),
      dependencies: z.array(z.string()).optional(),
      derivedFrom: z.array(z.string()).optional(),
    })
    .optional(),
  /** Checkpoints for long-running tasks */
  checkpoints: z
    .array(
      z.object({
        id: UUIDSchema,
        timestamp: z.number().int().positive(),
        state: z.record(z.string(), z.any()),
        progress: z.number().min(0).max(1).optional(),
      })
    )
    .optional(),
  /** Metadata */
  metadata: z.record(z.string(), z.any()).optional(),
});

// =============================================================================
// Bounds Schema - Capacity Limits
// =============================================================================

/**
 * Bounds schema for resource capacity limits
 *
 * Enforces governance constraints:
 * - File limits: Max number of files created/modified
 * - Byte limits: Max storage used
 * - Operation limits: Max tool calls/graph operations
 * - Runtime limits: Max execution time
 * - Graph rewrite limits: Max structural changes to knowledge graph
 *
 * Prevents:
 * - Resource exhaustion attacks
 * - Runaway processes
 * - Storage abuse
 * - Performance degradation
 *
 * @example
 * {
 *   maxFiles: 100,
 *   maxBytes: 10485760, // 10 MB
 *   maxOps: 1000,
 *   maxRuntime: 300000, // 5 minutes
 *   maxGraphRewrites: 50,
 *   enforcementPolicy: 'strict',
 *   warnings: {
 *     filesThreshold: 0.8,
 *     bytesThreshold: 0.9,
 *     opsThreshold: 0.75,
 *     runtimeThreshold: 0.9
 *   }
 * }
 *
 * @constant
 * @type {z.ZodObject}
 */
export const BoundsSchema = z.object({
  /** Maximum number of files that can be created/modified */
  maxFiles: z.number().int().positive().max(10000).default(100),
  /** Maximum total bytes that can be written */
  maxBytes: z
    .number()
    .int()
    .positive()
    .max(1024 * 1024 * 1024)
    .default(10 * 1024 * 1024), // 10 MB
  /** Maximum number of operations/tool calls */
  maxOps: z.number().int().positive().max(100000).default(1000),
  /** Maximum runtime in milliseconds */
  maxRuntime: z.number().int().positive().max(3600000).default(300000), // 5 minutes
  /** Maximum graph structure rewrites */
  maxGraphRewrites: z.number().int().positive().max(1000).default(50),
  /** Enforcement policy */
  enforcementPolicy: z.enum(['strict', 'soft', 'monitor']).default('strict'),
  /** Warning thresholds (0.0-1.0 as fraction of limit) */
  warnings: z
    .object({
      filesThreshold: z.number().min(0).max(1).default(0.8),
      bytesThreshold: z.number().min(0).max(1).default(0.9),
      opsThreshold: z.number().min(0).max(1).default(0.75),
      runtimeThreshold: z.number().min(0).max(1).default(0.9),
      graphRewritesThreshold: z.number().min(0).max(1).default(0.8),
    })
    .optional(),
  /** Current usage (for monitoring) */
  currentUsage: z
    .object({
      files: z.number().int().nonnegative().default(0),
      bytes: z.number().int().nonnegative().default(0),
      ops: z.number().int().nonnegative().default(0),
      runtime: z.number().nonnegative().default(0),
      graphRewrites: z.number().int().nonnegative().default(0),
    })
    .optional(),
  /** Metadata */
  metadata: z.record(z.string(), z.any()).optional(),
});

// =============================================================================
// WorkItem Schema - Async Task Node States
// =============================================================================

/**
 * Work item schema for async task node states
 *
 * Represents distributed task execution:
 * - State machine: queued → running → succeeded/failed/denied
 * - Priority queue: Higher priority = earlier execution
 * - Dependencies: Task dependencies graph (DAG)
 * - Retries: Automatic retry with exponential backoff
 * - Timeout: Per-task execution time limits
 *
 * State transitions:
 * - queued: Waiting for execution slot
 * - running: Currently executing
 * - succeeded: Completed successfully
 * - failed: Failed (may retry if retries remain)
 * - denied: Governance policy denied execution
 *
 * @example
 * {
 *   id: 'work-001',
 *   type: 'file_operation',
 *   state: 'running',
 *   priority: 75,
 *   createdAt: 1703001600000,
 *   startedAt: 1703001605000,
 *   payload: {
 *     operation: 'write',
 *     path: '/home/user/file.txt',
 *     content: 'Hello World'
 *   },
 *   dependencies: ['work-000'],
 *   retries: { max: 3, current: 0, backoff: 'exponential' },
 *   timeout: 30000,
 *   assignedTo: 'agent:worker-01'
 * }
 *
 * @constant
 * @type {z.ZodObject}
 */
export const WorkItemSchema = z.object({
  /** Unique work item identifier */
  id: UUIDSchema,
  /** Work item type/category */
  type: z.string().min(1).max(100),
  /** Current state */
  state: z.enum(['queued', 'running', 'succeeded', 'failed', 'denied', 'cancelled']),
  /** Priority (0-100, higher = more important) */
  priority: z.number().int().min(0).max(100).default(50),
  /** Creation timestamp (Unix epoch ms) */
  createdAt: z.number().int().positive(),
  /** Start timestamp (when state became 'running') */
  startedAt: z.number().int().positive().optional().nullable(),
  /** Completion timestamp (when state became terminal) */
  completedAt: z.number().int().positive().optional().nullable(),
  /** Work item payload (task-specific data) */
  payload: z.record(z.string(), z.any()),
  /** Work item result (populated on completion) */
  result: z.any().optional(),
  /** Error information if failed */
  error: z
    .object({
      message: z.string(),
      code: z.string().optional(),
      stack: z.string().optional(),
      retryable: z.boolean().default(true),
    })
    .optional(),
  /** Dependency work item IDs (must complete before this can run) */
  dependencies: z.array(UUIDSchema).optional(),
  /** Retry configuration */
  retries: z
    .object({
      max: z.number().int().nonnegative().max(10).default(3),
      current: z.number().int().nonnegative().default(0),
      backoff: z.enum(['constant', 'linear', 'exponential']).default('exponential'),
      delay: z.number().int().positive().default(1000), // Base delay in ms
    })
    .optional(),
  /** Execution timeout (milliseconds) */
  timeout: z.number().int().positive().max(3600000).default(30000),
  /** Agent/worker assigned to execute this work item */
  assignedTo: ActorIdSchema.optional().nullable(),
  /** Governance policy evaluation result */
  policyEvaluation: z
    .object({
      allowed: z.boolean(),
      reason: z.string().optional(),
      constraints: z.record(z.string(), z.any()).optional(),
    })
    .optional(),
  /** Progress tracking (0.0-1.0) */
  progress: z.number().min(0).max(1).optional(),
  /** Metadata */
  metadata: z.record(z.string(), z.any()).optional(),
});

// =============================================================================
// ProjectionManifest Schema - Surface Definitions
// =============================================================================

/**
 * Projection manifest schema for surface definitions
 *
 * Defines how KGC runtime projects to different surfaces:
 * - CLI: Command-line interface configuration
 * - Docs: Documentation generation settings
 * - IDE: Editor integration (LSP, snippets, etc.)
 * - API: HTTP/GraphQL endpoints
 * - UI: Web dashboard configuration
 *
 * Enables multi-surface governance:
 * - Consistent UX across interfaces
 * - Auto-generated integrations
 * - Policy-driven access control
 * - Version-aware compatibility
 *
 * @example
 * {
 *   version: '1.0.0',
 *   surfaces: {
 *     cli: {
 *       commands: [
 *         {
 *           name: 'run',
 *           description: 'Execute a workflow',
 *           options: [{ name: 'file', required: true, type: 'string' }]
 *         }
 *       ]
 *     },
 *     docs: {
 *       generator: 'typedoc',
 *       outputDir: './docs',
 *       includes: ['**\/*.mjs'],
 *       theme: 'default'
 *     },
 *     ide: {
 *       lsp: { enabled: true, port: 9000 },
 *       snippets: [
 *         { prefix: 'run', body: 'RunCapsuleSchema.parse({ ... })' }
 *       ]
 *     }
 *   }
 * }
 *
 * @constant
 * @type {z.ZodObject}
 */
export const ProjectionManifestSchema = z.object({
  /** Schema version */
  version: SemanticVersionSchema.default('1.0.0'),
  /** Surface definitions */
  surfaces: z.object({
    /** CLI surface configuration */
    cli: z
      .object({
        commands: z
          .array(
            z.object({
              name: z.string().min(1).max(100),
              description: z.string().max(500).optional(),
              aliases: z.array(z.string()).optional(),
              options: z
                .array(
                  z.object({
                    name: z.string().min(1),
                    type: z.enum(['string', 'number', 'boolean', 'array']),
                    required: z.boolean().default(false),
                    default: z.any().optional(),
                    description: z.string().optional(),
                  })
                )
                .optional(),
              examples: z.array(z.string()).optional(),
            })
          )
          .optional(),
        globalOptions: z.record(z.string(), z.any()).optional(),
      })
      .optional(),
    /** Documentation surface configuration */
    docs: z
      .object({
        generator: z.enum(['typedoc', 'jsdoc', 'docusaurus', 'mkdocs']).optional(),
        outputDir: z.string().default('./docs'),
        includes: z.array(z.string()).optional(),
        excludes: z.array(z.string()).optional(),
        theme: z.string().optional(),
        navigation: z
          .array(
            z.object({
              title: z.string(),
              path: z.string(),
              children: z.array(z.any()).optional(),
            })
          )
          .optional(),
      })
      .optional(),
    /** IDE surface configuration */
    ide: z
      .object({
        lsp: z
          .object({
            enabled: z.boolean().default(true),
            port: z.number().int().positive().default(9000),
            features: z
              .array(z.enum(['completion', 'hover', 'goto', 'diagnostics', 'formatting']))
              .optional(),
          })
          .optional(),
        snippets: z
          .array(
            z.object({
              prefix: z.string().min(1),
              body: z.string().min(1),
              description: z.string().optional(),
              scope: z.string().optional(),
            })
          )
          .optional(),
        schemas: z
          .array(
            z.object({
              fileMatch: z.array(z.string()),
              schema: z.record(z.string(), z.any()),
            })
          )
          .optional(),
      })
      .optional(),
    /** API surface configuration */
    api: z
      .object({
        type: z.enum(['rest', 'graphql', 'grpc']).default('rest'),
        baseUrl: z.string().url().optional(),
        endpoints: z
          .array(
            z.object({
              path: z.string(),
              method: z.enum(['GET', 'POST', 'PUT', 'DELETE', 'PATCH']).optional(),
              schema: z.record(z.string(), z.any()).optional(),
              auth: z.enum(['none', 'bearer', 'api-key', 'oauth2']).optional(),
            })
          )
          .optional(),
        versioning: z
          .object({
            strategy: z.enum(['url', 'header', 'query']),
            current: z.string(),
          })
          .optional(),
      })
      .optional(),
    /** UI surface configuration */
    ui: z
      .object({
        type: z.enum(['web', 'desktop', 'mobile']).default('web'),
        framework: z.string().optional(),
        routes: z
          .array(
            z.object({
              path: z.string(),
              component: z.string(),
              title: z.string().optional(),
            })
          )
          .optional(),
        theme: z.record(z.string(), z.any()).optional(),
      })
      .optional(),
  }),
  /** Access control per surface */
  accessControl: z
    .object({
      default: z.enum(['allow', 'deny']).default('allow'),
      rules: z
        .array(
          z.object({
            surface: z.string(),
            action: z.string(),
            allowed: z.boolean(),
            roles: z.array(z.string()).optional(),
          })
        )
        .optional(),
    })
    .optional(),
  /** Metadata */
  metadata: z.record(z.string(), z.any()).optional(),
});

// =============================================================================
// KGCMarkdown AST Schema - Front Matter + Fenced Blocks
// =============================================================================

/**
 * KGC Markdown AST node schema
 *
 * Structured representation of KGC-flavored markdown:
 * - Front matter: YAML metadata block
 * - Fenced blocks: Code blocks with type annotations
 * - Content blocks: Prose, lists, tables
 * - Semantic annotations: RDF/semantic web metadata
 *
 * Supports:
 * - Literate programming (code + docs interleaved)
 * - Executable notebooks (runnable code blocks)
 * - Semantic documentation (RDF-annotated content)
 * - Multi-format export (HTML, PDF, slides)
 *
 * @example
 * {
 *   type: 'document',
 *   frontMatter: {
 *     title: 'KGC Example',
 *     version: '1.0.0',
 *     ontology: ['http://schema.org/']
 *   },
 *   children: [
 *     {
 *       type: 'heading',
 *       level: 1,
 *       content: 'Introduction',
 *       id: 'intro',
 *       metadata: {}
 *     },
 *     {
 *       type: 'fenced-block',
 *       language: 'javascript',
 *       attributes: { executable: true, output: 'inline' },
 *       content: 'console.log("Hello KGC");',
 *       metadata: { runId: 'run-001' }
 *     },
 *     {
 *       type: 'paragraph',
 *       content: 'This is a paragraph with **bold** text.',
 *       semanticAnnotations: [
 *         {
 *           predicate: 'http://schema.org/description',
 *           object: 'Introduction paragraph'
 *         }
 *       ]
 *     }
 *   ],
 *   metadata: {}
 * }
 *
 * @constant
 * @type {z.ZodObject}
 */

// Define base node schema (all AST nodes share these properties)
const BaseASTNodeSchema = z.object({
  /** Node type */
  type: z.string().min(1),
  /** Node metadata */
  metadata: z.record(z.string(), z.any()).optional(),
});

// Recursive AST node schema (allows nested children)
const ASTNodeSchema = BaseASTNodeSchema.extend({
  /** Text content (for leaf nodes) */
  content: z.string().optional(),
  /** Child nodes (for container nodes) */
  children: z.lazy(() => z.array(ASTNodeSchema).optional()),
});

/**
 * KGC Markdown document AST schema
 */
export const KGCMarkdownSchema = z.object({
  /** Root node type */
  type: z.literal('document'),
  /** Front matter (YAML metadata) */
  frontMatter: z
    .object({
      title: z.string().max(200).optional(),
      version: SemanticVersionSchema.optional(),
      author: z.string().max(100).optional(),
      date: TimestampSchema.optional(),
      ontology: z.array(z.string().url()).optional(),
      tags: z.array(z.string()).optional(),
      custom: z.record(z.string(), z.any()).optional(),
    })
    .optional(),
  /** Document child nodes */
  children: z.array(
    z.discriminatedUnion('type', [
      // Heading node
      BaseASTNodeSchema.extend({
        type: z.literal('heading'),
        level: z.number().int().min(1).max(6),
        content: z.string().min(1),
        id: z.string().optional(),
      }),
      // Paragraph node
      BaseASTNodeSchema.extend({
        type: z.literal('paragraph'),
        content: z.string(),
        semanticAnnotations: z
          .array(
            z.object({
              predicate: z.string().url(),
              object: z.string(),
              datatype: z.string().optional(),
            })
          )
          .optional(),
      }),
      // Fenced block node (code blocks)
      BaseASTNodeSchema.extend({
        type: z.literal('fenced-block'),
        language: z.string().max(50).optional(),
        attributes: z.record(z.string(), z.any()).optional(),
        content: z.string(),
        output: z.string().optional(),
        executable: z.boolean().default(false),
      }),
      // List node
      BaseASTNodeSchema.extend({
        type: z.literal('list'),
        ordered: z.boolean().default(false),
        items: z.array(z.string()),
      }),
      // Table node
      BaseASTNodeSchema.extend({
        type: z.literal('table'),
        headers: z.array(z.string()).optional(),
        rows: z.array(z.array(z.string())),
        alignment: z.array(z.enum(['left', 'center', 'right'])).optional(),
      }),
      // Link node
      BaseASTNodeSchema.extend({
        type: z.literal('link'),
        url: z.string().url(),
        text: z.string(),
        title: z.string().optional(),
      }),
      // Image node
      BaseASTNodeSchema.extend({
        type: z.literal('image'),
        url: z.string().url(),
        alt: z.string().optional(),
        title: z.string().optional(),
        width: z.number().int().positive().optional(),
        height: z.number().int().positive().optional(),
      }),
      // Blockquote node
      BaseASTNodeSchema.extend({
        type: z.literal('blockquote'),
        content: z.string(),
      }),
    ])
  ),
  /** Document metadata */
  metadata: z.record(z.string(), z.any()).optional(),
});

// =============================================================================
// Validation Helper Functions
// =============================================================================

/**
 * Validate a receipt
 * @param {any} receipt - The receipt to validate
 * @returns {Object} Validation result { success, data, errors }
 */
export function validateReceipt(receipt) {
  try {
    const validated = ReceiptSchema.parse(receipt);
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
 * Validate a run capsule
 * @param {any} capsule - The run capsule to validate
 * @returns {Object} Validation result { success, data, errors }
 */
export function validateRunCapsule(capsule) {
  try {
    const validated = RunCapsuleSchema.parse(capsule);
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
 * Validate a tool trace entry
 * @param {any} entry - The tool trace entry to validate
 * @returns {Object} Validation result { success, data, errors }
 */
export function validateToolTraceEntry(entry) {
  try {
    const validated = ToolTraceEntrySchema.parse(entry);
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
 * Validate bounds
 * @param {any} bounds - The bounds to validate
 * @returns {Object} Validation result { success, data, errors }
 */
export function validateBounds(bounds) {
  try {
    const validated = BoundsSchema.parse(bounds);
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
 * Validate a work item
 * @param {any} workItem - The work item to validate
 * @returns {Object} Validation result { success, data, errors }
 */
export function validateWorkItem(workItem) {
  try {
    const validated = WorkItemSchema.parse(workItem);
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
 * Validate a projection manifest
 * @param {any} manifest - The projection manifest to validate
 * @returns {Object} Validation result { success, data, errors }
 */
export function validateProjectionManifest(manifest) {
  try {
    const validated = ProjectionManifestSchema.parse(manifest);
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
 * Validate a KGC Markdown AST
 * @param {any} ast - The AST to validate
 * @returns {Object} Validation result { success, data, errors }
 */
export function validateKGCMarkdown(ast) {
  try {
    const validated = KGCMarkdownSchema.parse(ast);
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
  // Schemas
  ReceiptSchema,
  RunCapsuleSchema,
  ToolTraceEntrySchema,
  BoundsSchema,
  WorkItemSchema,
  ProjectionManifestSchema,
  KGCMarkdownSchema,

  // Validation functions
  validateReceipt,
  validateRunCapsule,
  validateToolTraceEntry,
  validateBounds,
  validateWorkItem,
  validateProjectionManifest,
  validateKGCMarkdown,
};
