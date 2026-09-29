/**
 * @fileoverview Receipt Schema Definitions - Zod schemas for receipt validation
 *
 * @module receipts/receipt-schemas
 */

import { z } from 'zod';

// Zod v4 compatibility - use z.unknown() instead of z.any() for better compatibility
const anySchema = z.unknown();

// ============================================================================
// Receipt Type Definitions
// ============================================================================

/**
 * All supported receipt types in the UNRDF ecosystem
 * @type {readonly string[]}
 */
export const RECEIPT_TYPES = Object.freeze([
  'admission',     // GOS gate admission decisions
  'test',          // Test execution results
  'build',         // Build/compilation results
  'deployment',    // Deployment to environment
  'projection',    // Documentation generation
  'query',         // SPARQL query execution
  'workflow',      // Workflow/task execution
  'validation',    // Schema/data validation
  'migration',     // Data migration operations
  'sync',          // Synchronization operations
  'audit',         // Audit log entries
  'custom',        // Custom/extension receipts
]);

/**
 * Receipt decision outcomes
 */
export const DECISION_OUTCOMES = Object.freeze([
  'ALLOW',    // Operation permitted/succeeded
  'DENY',     // Operation blocked/failed
  'WARN',     // Operation allowed with warnings
  'SKIP',     // Operation skipped
  'PENDING',  // Operation in progress
]);

/**
 * Receipt severity levels
 */
export const SEVERITY_LEVELS = Object.freeze([
  'critical',
  'error',
  'warning',
  'info',
  'debug',
]);

// ============================================================================
// Zod Schemas
// ============================================================================

/**
 * Base provenance schema - tracks origin of operation
 */
export const ProvenanceSchema = z.object({
  agent: z.string(),
  source: z.string().optional(),
  version: z.string().optional(),
  sessionId: z.string().optional(),
  requestId: z.string().optional(),
});

/**
 * Toolchain version schema - captures environment
 */
export const ToolchainSchema = z.object({
  node: z.string(),
  platform: z.string().optional(),
  arch: z.string().optional(),
  packages: z.record(z.string(), z.string()).optional(),
});

/**
 * Input specification schema
 */
export const InputSpecSchema = z.object({
  hashes: z.record(z.string(), z.string()),
  sources: z.array(z.string()).optional(),
  metadata: z.record(z.string(), anySchema).optional(),
});

/**
 * Output specification schema
 */
export const OutputSpecSchema = z.object({
  hash: z.string(),
  artifacts: z.array(z.string()).optional(),
  metadata: z.record(z.string(), anySchema).optional(),
});

/**
 * Check result schema - individual check within receipt
 */
export const CheckResultSchema = z.object({
  name: z.string(),
  passed: z.boolean(),
  severity: z.enum(['critical', 'error', 'warning', 'info', 'debug']).optional(),
  message: z.string().optional(),
  details: z.record(z.string(), anySchema).optional(),
  duration: z.number().optional(),
});

/**
 * Metrics schema - operation performance metrics
 */
export const MetricsSchema = z.object({
  duration: z.number(),
  startTime: z.string(),
  endTime: z.string(),
  memory: z.object({
    heapUsed: z.number().optional(),
    heapTotal: z.number().optional(),
    external: z.number().optional(),
  }).optional(),
  cpu: z.object({
    user: z.number().optional(),
    system: z.number().optional(),
  }).optional(),
  custom: z.record(z.string(), z.number()).optional(),
});

/**
 * Violation schema - detailed violation information
 */
export const ViolationSchema = z.object({
  code: z.string(),
  severity: z.enum(['critical', 'error', 'warning', 'info']),
  message: z.string(),
  location: z.string().optional(),
  rule: z.string().optional(),
  fix: z.string().optional(),
});

// ============================================================================
// Type-Specific Extension Schemas
// ============================================================================

/**
 * Admission receipt extension
 */
export const AdmissionExtensionSchema = z.object({
  capsuleId: z.string().describe('Delta capsule ID'),
  partition: z.string().optional().describe('Target partition'),
  guards: z.array(z.object({
    name: z.string(),
    passed: z.boolean(),
    blockedBy: z.array(z.string()).optional(),
  })).optional(),
  invariants: z.array(z.object({
    name: z.string(),
    passed: z.boolean(),
    severity: z.string().optional(),
  })).optional(),
  quadCount: z.number().optional().describe('Number of quads in capsule'),
  beforeHash: z.string().optional().describe('Universe hash before'),
  afterHash: z.string().optional().describe('Universe hash after'),
});

/**
 * Test receipt extension
 */
export const TestExtensionSchema = z.object({
  suite: z.string().optional().describe('Test suite name'),
  file: z.string().optional().describe('Test file path'),
  total: z.number().describe('Total test count'),
  passed: z.number().describe('Passed test count'),
  failed: z.number().describe('Failed test count'),
  skipped: z.number().optional().describe('Skipped test count'),
  coverage: z.object({
    lines: z.number().optional(),
    branches: z.number().optional(),
    functions: z.number().optional(),
    statements: z.number().optional(),
  }).optional(),
  failures: z.array(z.object({
    name: z.string(),
    message: z.string(),
    stack: z.string().optional(),
  })).optional(),
});

/**
 * Build receipt extension
 */
export const BuildExtensionSchema = z.object({
  target: z.string().optional().describe('Build target'),
  mode: z.enum(['development', 'production', 'test']).optional(),
  bundleSize: z.number().optional().describe('Bundle size in bytes'),
  chunks: z.array(z.object({
    name: z.string(),
    size: z.number(),
  })).optional(),
  warnings: z.array(z.string()).optional(),
  errors: z.array(z.string()).optional(),
});

/**
 * Deployment receipt extension
 */
export const DeploymentExtensionSchema = z.object({
  environment: z.string().describe('Target environment'),
  version: z.string().describe('Deployed version'),
  rollbackVersion: z.string().optional().describe('Previous version for rollback'),
  instances: z.number().optional().describe('Number of instances'),
  region: z.string().optional().describe('Deployment region'),
  url: z.string().optional().describe('Deployed URL'),
  healthCheck: z.object({
    passed: z.boolean(),
    endpoint: z.string().optional(),
    responseTime: z.number().optional(),
  }).optional(),
});

/**
 * Projection receipt extension (documentation generation)
 */
export const ProjectionExtensionSchema = z.object({
  format: z.string().describe('Output format (markdown, html, etc)'),
  template: z.string().optional().describe('Template used'),
  pages: z.number().optional().describe('Pages generated'),
  files: z.array(z.string()).optional().describe('Generated files'),
  wordCount: z.number().optional().describe('Total word count'),
  sections: z.array(z.object({
    name: z.string(),
    size: z.number(),
  })).optional(),
});

/**
 * Query receipt extension (SPARQL, workflow)
 */
export const QueryExtensionSchema = z.object({
  queryType: z.enum(['SELECT', 'CONSTRUCT', 'ASK', 'DESCRIBE', 'INSERT', 'DELETE', 'UPDATE']).optional(),
  queryHash: z.string().optional().describe('Query content hash'),
  resultCount: z.number().optional().describe('Number of results'),
  bindings: z.number().optional().describe('Number of bindings'),
  graphsAccessed: z.array(z.string()).optional(),
  optimizations: z.array(z.string()).optional(),
});

/**
 * Workflow receipt extension
 */
export const WorkflowExtensionSchema = z.object({
  workflowId: z.string().describe('Workflow identifier'),
  workflowName: z.string().optional().describe('Workflow name'),
  taskCount: z.number().optional().describe('Total tasks'),
  completedTasks: z.number().optional().describe('Completed tasks'),
  failedTasks: z.number().optional().describe('Failed tasks'),
  steps: z.array(z.object({
    name: z.string(),
    status: z.enum(['pending', 'running', 'completed', 'failed', 'skipped']),
    duration: z.number().optional(),
  })).optional(),
});

/**
 * Type-specific extension schema (simplified for Zod v4 compatibility)
 */
export const ExtensionSchema = z.object({
  type: z.string(),
  data: z.record(z.string(), anySchema),
}).optional();

// ============================================================================
// Universal Receipt Schema
// ============================================================================

/**
 * Universal Receipt Schema - the canonical format for all receipts
 */
export const UniversalReceiptSchema = z.object({
  // Identity
  id: z.string(),
  type: z.enum(['admission', 'test', 'build', 'deployment', 'projection', 'query', 'workflow', 'validation', 'migration', 'sync', 'audit', 'custom']),
  version: z.literal('1.0.0'),

  // Package context
  package: z.string(),
  namespace: z.string().optional(),

  // Temporal
  epoch: z.string(),
  timestamp: z.string(),

  // Decision
  decision: z.enum(['ALLOW', 'DENY', 'WARN', 'SKIP', 'PENDING']),
  reason: z.string(),

  // Provenance
  provenance: ProvenanceSchema,
  toolchain: ToolchainSchema,

  // Input/Output
  input: InputSpecSchema,
  output: OutputSpecSchema,

  // Checks and violations
  checks: z.array(CheckResultSchema).optional(),
  violations: z.array(ViolationSchema).optional(),

  // Performance
  metrics: MetricsSchema.optional(),

  // Chain linkage
  beforeHash: z.string().nullable(),
  merkleRoot: z.string().nullable().optional(),

  // Type-specific extension
  extension: ExtensionSchema,

  // Computed hash (added after creation)
  receiptHash: z.string().optional(),
});

/**
 * @typedef {z.infer<typeof UniversalReceiptSchema>} UniversalReceipt
 */
