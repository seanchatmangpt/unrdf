/**
 * @fileoverview Universal Receipt Standard - Standardized receipts for all UNRDF operations
 *
 * **Purpose**: Define a universal receipt format that works across all 42+ packages:
 * - Admission receipts (GOS gates)
 * - Test execution receipts
 * - Build/compilation receipts
 * - Deployment receipts
 * - Projection receipts (documentation generation)
 * - Query/operation receipts (SPARQL, workflow)
 *
 * **Design Principles**:
 * 1. Single canonical format with extensible type-specific fields
 * 2. Deterministic hashing via canonical JSON serialization
 * 3. Chain linkage via beforeHash for temporal ordering
 * 4. Merkle batching for efficient verification
 * 5. JSON-LD and TTL serialization for RDF compatibility
 *
 * @module receipts/receipt-standard
 */

import { blake3 } from 'hash-wasm';
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
const ProvenanceSchema = z.object({
  agent: z.string(),
  source: z.string().optional(),
  version: z.string().optional(),
  sessionId: z.string().optional(),
  requestId: z.string().optional(),
});

/**
 * Toolchain version schema - captures environment
 */
const ToolchainSchema = z.object({
  node: z.string(),
  platform: z.string().optional(),
  arch: z.string().optional(),
  packages: z.record(z.string(), z.string()).optional(),
});

/**
 * Input specification schema
 */
const InputSpecSchema = z.object({
  hashes: z.record(z.string(), z.string()),
  sources: z.array(z.string()).optional(),
  metadata: z.record(z.string(), anySchema).optional(),
});

/**
 * Output specification schema
 */
const OutputSpecSchema = z.object({
  hash: z.string(),
  artifacts: z.array(z.string()).optional(),
  metadata: z.record(z.string(), anySchema).optional(),
});

/**
 * Check result schema - individual check within receipt
 */
const CheckResultSchema = z.object({
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
const MetricsSchema = z.object({
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
const ViolationSchema = z.object({
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
const AdmissionExtensionSchema = z.object({
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
const TestExtensionSchema = z.object({
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
const BuildExtensionSchema = z.object({
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
const DeploymentExtensionSchema = z.object({
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
const ProjectionExtensionSchema = z.object({
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
const QueryExtensionSchema = z.object({
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
const WorkflowExtensionSchema = z.object({
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
const ExtensionSchema = z.object({
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

// ============================================================================
// Receipt Builder
// ============================================================================

/**
 * Generate deterministic epoch from timestamp
 *
 * Format: tau_YYYY_MM_DD_HHmmss_SSS (includes seconds for precision)
 *
 * @param {Date} timestamp - Timestamp
 * @returns {string} Epoch string
 */
export function generateEpoch(timestamp = new Date()) {
  const year = timestamp.getUTCFullYear();
  const month = String(timestamp.getUTCMonth() + 1).padStart(2, '0');
  const day = String(timestamp.getUTCDate()).padStart(2, '0');
  const hour = String(timestamp.getUTCHours()).padStart(2, '0');
  const minute = String(timestamp.getUTCMinutes()).padStart(2, '0');
  const second = String(timestamp.getUTCSeconds()).padStart(2, '0');
  const ms = String(timestamp.getUTCMilliseconds()).padStart(3, '0');

  return `tau_${year}_${month}_${day}_${hour}${minute}${second}_${ms}`;
}

/**
 * Generate unique receipt ID
 *
 * @param {string} type - Receipt type
 * @param {string} pkg - Package name
 * @param {Date} timestamp - Timestamp
 * @returns {string} Receipt URN
 */
export function generateReceiptId(type, pkg, timestamp = new Date()) {
  const epoch = generateEpoch(timestamp);
  const random = Math.random().toString(36).substring(2, 10);
  return `urn:receipt:${pkg}:${type}:${epoch}:${random}`;
}

/**
 * Compute deterministic hash of receipt data
 *
 * @param {object} receiptData - Receipt data (without receiptHash)
 * @returns {Promise<string>} BLAKE3 hash
 */
export async function computeReceiptHash(receiptData) {
  // Remove receiptHash if present
  const { receiptHash: _, ...dataWithoutHash } = receiptData;

  // Canonical JSON serialization (sorted keys)
  const keys = Object.keys(dataWithoutHash).sort();
  const canonical = JSON.stringify(dataWithoutHash, keys);

  return blake3(canonical);
}

/**
 * Universal Receipt Builder - fluent API for creating receipts
 *
 * @example
 * const receipt = await new ReceiptBuilder('test', '@unrdf/core')
 *   .decision('ALLOW', 'All tests passed')
 *   .provenance({ agent: 'vitest' })
 *   .input({ hashes: { testFile: 'abc123' } })
 *   .output({ hash: 'def456' })
 *   .extension({ type: 'test', data: { total: 10, passed: 10, failed: 0 } })
 *   .build();
 */
export class ReceiptBuilder {
  /**
   * Create a new receipt builder
   *
   * @param {string} type - Receipt type
   * @param {string} pkg - Package name
   * @param {object} [options] - Builder options
   */
  constructor(type, pkg, options = {}) {
    this._timestamp = options.timestamp || new Date();
    this._data = {
      id: generateReceiptId(type, pkg, this._timestamp),
      type,
      version: '1.0.0',
      package: pkg,
      epoch: generateEpoch(this._timestamp),
      timestamp: this._timestamp.toISOString(),
      decision: 'PENDING',
      reason: '',
      provenance: {
        agent: 'unknown',
      },
      toolchain: {
        node: process.version,
        platform: process.platform,
        arch: process.arch,
      },
      input: { hashes: {} },
      output: { hash: '' },
      beforeHash: null,
      merkleRoot: null,
    };
  }

  /**
   * Set namespace
   * @param {string} namespace
   * @returns {ReceiptBuilder}
   */
  namespace(namespace) {
    this._data.namespace = namespace;
    return this;
  }

  /**
   * Set decision and reason
   * @param {'ALLOW'|'DENY'|'WARN'|'SKIP'|'PENDING'} decision
   * @param {string} reason
   * @returns {ReceiptBuilder}
   */
  decision(decision, reason) {
    this._data.decision = decision;
    this._data.reason = reason;
    return this;
  }

  /**
   * Set provenance
   * @param {object} provenance
   * @returns {ReceiptBuilder}
   */
  provenance(provenance) {
    this._data.provenance = { ...this._data.provenance, ...provenance };
    return this;
  }

  /**
   * Set toolchain
   * @param {object} toolchain
   * @returns {ReceiptBuilder}
   */
  toolchain(toolchain) {
    this._data.toolchain = { ...this._data.toolchain, ...toolchain };
    return this;
  }

  /**
   * Set input specification
   * @param {object} input
   * @returns {ReceiptBuilder}
   */
  input(input) {
    this._data.input = { ...this._data.input, ...input };
    return this;
  }

  /**
   * Set output specification
   * @param {object} output
   * @returns {ReceiptBuilder}
   */
  output(output) {
    this._data.output = { ...this._data.output, ...output };
    return this;
  }

  /**
   * Add checks
   * @param {Array} checks
   * @returns {ReceiptBuilder}
   */
  checks(checks) {
    this._data.checks = checks;
    return this;
  }

  /**
   * Add violations
   * @param {Array} violations
   * @returns {ReceiptBuilder}
   */
  violations(violations) {
    this._data.violations = violations;
    return this;
  }

  /**
   * Set metrics
   * @param {object} metrics
   * @returns {ReceiptBuilder}
   */
  metrics(metrics) {
    this._data.metrics = metrics;
    return this;
  }

  /**
   * Set chain linkage
   * @param {string|null} beforeHash
   * @returns {ReceiptBuilder}
   */
  chain(beforeHash) {
    this._data.beforeHash = beforeHash;
    return this;
  }

  /**
   * Set merkle root
   * @param {string|null} merkleRoot
   * @returns {ReceiptBuilder}
   */
  merkle(merkleRoot) {
    this._data.merkleRoot = merkleRoot;
    return this;
  }

  /**
   * Set type-specific extension
   * @param {object} extension
   * @returns {ReceiptBuilder}
   */
  extension(extension) {
    this._data.extension = extension;
    return this;
  }

  /**
   * Build the receipt
   * @returns {Promise<UniversalReceipt>}
   */
  async build() {
    // Validate with schema
    const validated = UniversalReceiptSchema.parse(this._data);

    // Compute hash
    const receiptHash = await computeReceiptHash(validated);

    // Return immutable receipt
    const receipt = { ...validated, receiptHash };
    return Object.freeze(receipt);
  }
}

// ============================================================================
// Type-Specific Receipt Factories
// ============================================================================

/**
 * Create an admission receipt
 *
 * @param {object} options - Receipt options
 * @returns {Promise<UniversalReceipt>}
 */
export async function createAdmissionReceipt(options) {
  const {
    pkg,
    capsuleId,
    decision,
    reason,
    partition,
    guards = [],
    invariants = [],
    quadCount,
    beforeHash,
    afterHash,
    beforeReceiptHash = null,
    provenance = {},
    timestamp,
  } = options;

  const builder = new ReceiptBuilder('admission', pkg, { timestamp });

  return builder
    .decision(decision, reason)
    .provenance({ agent: 'admission-engine', ...provenance })
    .input({ hashes: { capsule: capsuleId, before: beforeHash || '' } })
    .output({ hash: afterHash || beforeHash || '' })
    .chain(beforeReceiptHash)
    .extension({
      type: 'admission',
      data: {
        capsuleId,
        partition,
        guards,
        invariants,
        quadCount,
        beforeHash,
        afterHash,
      },
    })
    .build();
}

/**
 * Create a test execution receipt
 *
 * @param {object} options - Receipt options
 * @returns {Promise<UniversalReceipt>}
 */
export async function createTestReceipt(options) {
  const {
    pkg,
    suite,
    file,
    total,
    passed,
    failed,
    skipped = 0,
    coverage,
    failures = [],
    duration,
    beforeReceiptHash = null,
    provenance = {},
    timestamp,
  } = options;

  const decision = failed === 0 ? 'ALLOW' : 'DENY';
  const reason = failed === 0
    ? `All ${total} tests passed`
    : `${failed} of ${total} tests failed`;

  const builder = new ReceiptBuilder('test', pkg, { timestamp });
  const startTime = timestamp || new Date();
  const endTime = new Date(startTime.getTime() + (duration || 0));

  return builder
    .decision(decision, reason)
    .provenance({ agent: 'test-runner', ...provenance })
    .input({ hashes: { testFile: file || 'unknown' } })
    .output({ hash: `test-result-${total}-${passed}-${failed}` })
    .metrics({
      duration: duration || 0,
      startTime: startTime.toISOString(),
      endTime: endTime.toISOString(),
    })
    .chain(beforeReceiptHash)
    .extension({
      type: 'test',
      data: {
        suite,
        file,
        total,
        passed,
        failed,
        skipped,
        coverage,
        failures,
      },
    })
    .build();
}

/**
 * Create a build receipt
 *
 * @param {object} options - Receipt options
 * @returns {Promise<UniversalReceipt>}
 */
export async function createBuildReceipt(options) {
  const {
    pkg,
    target,
    mode = 'production',
    success,
    bundleSize,
    chunks = [],
    warnings = [],
    errors = [],
    duration,
    beforeReceiptHash = null,
    provenance = {},
    timestamp,
  } = options;

  const decision = success ? (warnings.length > 0 ? 'WARN' : 'ALLOW') : 'DENY';
  const reason = success
    ? `Build completed${warnings.length > 0 ? ` with ${warnings.length} warning(s)` : ''}`
    : `Build failed: ${errors.join(', ')}`;

  const builder = new ReceiptBuilder('build', pkg, { timestamp });
  const startTime = timestamp || new Date();
  const endTime = new Date(startTime.getTime() + (duration || 0));

  return builder
    .decision(decision, reason)
    .provenance({ agent: 'build-system', ...provenance })
    .input({ hashes: { source: 'src-hash' } })
    .output({ hash: `build-${bundleSize || 0}` })
    .metrics({
      duration: duration || 0,
      startTime: startTime.toISOString(),
      endTime: endTime.toISOString(),
    })
    .chain(beforeReceiptHash)
    .extension({
      type: 'build',
      data: {
        target,
        mode,
        bundleSize,
        chunks,
        warnings,
        errors,
      },
    })
    .build();
}

/**
 * Create a deployment receipt
 *
 * @param {object} options - Receipt options
 * @returns {Promise<UniversalReceipt>}
 */
export async function createDeploymentReceipt(options) {
  const {
    pkg,
    environment,
    version,
    rollbackVersion,
    success,
    instances,
    region,
    url,
    healthCheck,
    duration,
    beforeReceiptHash = null,
    provenance = {},
    timestamp,
  } = options;

  const decision = success ? 'ALLOW' : 'DENY';
  const reason = success
    ? `Deployed v${version} to ${environment}`
    : `Deployment to ${environment} failed`;

  const builder = new ReceiptBuilder('deployment', pkg, { timestamp });
  const startTime = timestamp || new Date();
  const endTime = new Date(startTime.getTime() + (duration || 0));

  return builder
    .decision(decision, reason)
    .provenance({ agent: 'deploy-system', ...provenance })
    .input({ hashes: { artifact: version } })
    .output({ hash: `deploy-${environment}-${version}` })
    .metrics({
      duration: duration || 0,
      startTime: startTime.toISOString(),
      endTime: endTime.toISOString(),
    })
    .chain(beforeReceiptHash)
    .extension({
      type: 'deployment',
      data: {
        environment,
        version,
        rollbackVersion,
        instances,
        region,
        url,
        healthCheck,
      },
    })
    .build();
}

/**
 * Create a projection receipt (documentation generation)
 *
 * @param {object} options - Receipt options
 * @returns {Promise<UniversalReceipt>}
 */
export async function createProjectionReceipt(options) {
  const {
    pkg,
    format,
    template,
    pages,
    files = [],
    wordCount,
    success,
    duration,
    beforeReceiptHash = null,
    provenance = {},
    timestamp,
  } = options;

  const decision = success ? 'ALLOW' : 'DENY';
  const reason = success
    ? `Generated ${pages || files.length} ${format} page(s)`
    : `Projection failed`;

  const builder = new ReceiptBuilder('projection', pkg, { timestamp });
  const startTime = timestamp || new Date();
  const endTime = new Date(startTime.getTime() + (duration || 0));

  return builder
    .decision(decision, reason)
    .provenance({ agent: 'projection-engine', ...provenance })
    .input({ hashes: { template: template || 'default' } })
    .output({ hash: `projection-${format}-${pages || files.length}`, artifacts: files })
    .metrics({
      duration: duration || 0,
      startTime: startTime.toISOString(),
      endTime: endTime.toISOString(),
    })
    .chain(beforeReceiptHash)
    .extension({
      type: 'projection',
      data: {
        format,
        template,
        pages,
        files,
        wordCount,
      },
    })
    .build();
}

/**
 * Create a query receipt (SPARQL, etc.)
 *
 * @param {object} options - Receipt options
 * @returns {Promise<UniversalReceipt>}
 */
export async function createQueryReceipt(options) {
  const {
    pkg,
    queryType,
    queryHash,
    resultCount,
    bindings,
    graphsAccessed = [],
    success,
    duration,
    beforeReceiptHash = null,
    provenance = {},
    timestamp,
  } = options;

  const decision = success ? 'ALLOW' : 'DENY';
  const reason = success
    ? `${queryType} returned ${resultCount} result(s)`
    : `Query execution failed`;

  const builder = new ReceiptBuilder('query', pkg, { timestamp });
  const startTime = timestamp || new Date();
  const endTime = new Date(startTime.getTime() + (duration || 0));

  return builder
    .decision(decision, reason)
    .provenance({ agent: 'query-engine', ...provenance })
    .input({ hashes: { query: queryHash || 'unknown' } })
    .output({ hash: `query-result-${resultCount || 0}` })
    .metrics({
      duration: duration || 0,
      startTime: startTime.toISOString(),
      endTime: endTime.toISOString(),
    })
    .chain(beforeReceiptHash)
    .extension({
      type: 'query',
      data: {
        queryType,
        queryHash,
        resultCount,
        bindings,
        graphsAccessed,
      },
    })
    .build();
}

/**
 * Create a workflow receipt
 *
 * @param {object} options - Receipt options
 * @returns {Promise<UniversalReceipt>}
 */
export async function createWorkflowReceipt(options) {
  const {
    pkg,
    workflowId,
    workflowName,
    taskCount,
    completedTasks,
    failedTasks = 0,
    steps = [],
    success,
    duration,
    beforeReceiptHash = null,
    provenance = {},
    timestamp,
  } = options;

  const decision = success ? 'ALLOW' : 'DENY';
  const reason = success
    ? `Workflow ${workflowName || workflowId} completed ${completedTasks}/${taskCount} tasks`
    : `Workflow failed: ${failedTasks} task(s) failed`;

  const builder = new ReceiptBuilder('workflow', pkg, { timestamp });
  const startTime = timestamp || new Date();
  const endTime = new Date(startTime.getTime() + (duration || 0));

  return builder
    .decision(decision, reason)
    .provenance({ agent: 'workflow-engine', ...provenance })
    .input({ hashes: { workflow: workflowId } })
    .output({ hash: `workflow-${workflowId}-${completedTasks}` })
    .metrics({
      duration: duration || 0,
      startTime: startTime.toISOString(),
      endTime: endTime.toISOString(),
    })
    .chain(beforeReceiptHash)
    .extension({
      type: 'workflow',
      data: {
        workflowId,
        workflowName,
        taskCount,
        completedTasks,
        failedTasks,
        steps,
      },
    })
    .build();
}

// ============================================================================
// Serialization
// ============================================================================

/**
 * RDF Namespace prefixes
 */
const RDF_PREFIXES = {
  unrdf: 'https://unrdf.org/receipts#',
  rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  xsd: 'http://www.w3.org/2001/XMLSchema#',
  prov: 'http://www.w3.org/ns/prov#',
  dct: 'http://purl.org/dc/terms/',
};

/**
 * Convert receipt to JSON-LD format
 *
 * @param {UniversalReceipt} receipt
 * @returns {object} JSON-LD representation
 */
export function receiptToJSONLD(receipt) {
  return {
    '@context': {
      unrdf: RDF_PREFIXES.unrdf,
      rdf: RDF_PREFIXES.rdf,
      xsd: RDF_PREFIXES.xsd,
      prov: RDF_PREFIXES.prov,
      dct: RDF_PREFIXES.dct,
    },
    '@id': receipt.id,
    '@type': `unrdf:${capitalize(receipt.type)}Receipt`,
    'unrdf:version': receipt.version,
    'unrdf:package': receipt.package,
    'unrdf:epoch': receipt.epoch,
    'prov:generatedAtTime': {
      '@type': 'xsd:dateTime',
      '@value': receipt.timestamp,
    },
    'unrdf:decision': receipt.decision,
    'dct:description': receipt.reason,
    'prov:wasAttributedTo': {
      '@type': 'prov:Agent',
      'prov:label': receipt.provenance.agent,
    },
    'unrdf:toolchain': {
      'unrdf:nodeVersion': receipt.toolchain.node,
      'unrdf:platform': receipt.toolchain.platform,
    },
    'unrdf:inputHash': receipt.input.hashes,
    'unrdf:outputHash': receipt.output.hash,
    'unrdf:beforeHash': receipt.beforeHash,
    'unrdf:merkleRoot': receipt.merkleRoot,
    'unrdf:receiptHash': receipt.receiptHash,
    'unrdf:extension': receipt.extension,
  };
}

/**
 * Convert receipt to Turtle (TTL) format
 *
 * @param {UniversalReceipt} receipt
 * @returns {string} Turtle serialization
 */
export function receiptToTurtle(receipt) {
  const lines = [
    `@prefix unrdf: <${RDF_PREFIXES.unrdf}> .`,
    `@prefix rdf: <${RDF_PREFIXES.rdf}> .`,
    `@prefix xsd: <${RDF_PREFIXES.xsd}> .`,
    `@prefix prov: <${RDF_PREFIXES.prov}> .`,
    `@prefix dct: <${RDF_PREFIXES.dct}> .`,
    '',
    `<${receipt.id}> a unrdf:${capitalize(receipt.type)}Receipt ;`,
    `  unrdf:version "${receipt.version}" ;`,
    `  unrdf:package "${receipt.package}" ;`,
    `  unrdf:epoch "${receipt.epoch}" ;`,
    `  prov:generatedAtTime "${receipt.timestamp}"^^xsd:dateTime ;`,
    `  unrdf:decision "${receipt.decision}" ;`,
    `  dct:description "${escapeString(receipt.reason)}" ;`,
    `  prov:wasAttributedTo [ prov:label "${receipt.provenance.agent}" ] ;`,
    `  unrdf:outputHash "${receipt.output.hash}" ;`,
  ];

  if (receipt.beforeHash) {
    lines.push(`  unrdf:beforeHash "${receipt.beforeHash}" ;`);
  }

  if (receipt.merkleRoot) {
    lines.push(`  unrdf:merkleRoot "${receipt.merkleRoot}" ;`);
  }

  lines.push(`  unrdf:receiptHash "${receipt.receiptHash}" .`);

  return lines.join('\n');
}

/**
 * Convert receipt to binary format (for efficient storage)
 *
 * @param {UniversalReceipt} receipt
 * @returns {Uint8Array} Binary representation
 */
export function receiptToBinary(receipt) {
  const json = JSON.stringify(receipt);
  return new TextEncoder().encode(json);
}

/**
 * Parse receipt from binary format
 *
 * @param {Uint8Array} binary
 * @returns {UniversalReceipt}
 */
export function receiptFromBinary(binary) {
  const json = new TextDecoder().decode(binary);
  return UniversalReceiptSchema.parse(JSON.parse(json));
}

// ============================================================================
// Verification
// ============================================================================

/**
 * Verify receipt hash integrity
 *
 * @param {UniversalReceipt} receipt
 * @returns {Promise<boolean>} True if hash is valid
 */
export async function verifyReceiptHash(receipt) {
  const computedHash = await computeReceiptHash(receipt);
  return computedHash === receipt.receiptHash;
}

/**
 * Validate receipt against schema
 *
 * @param {object} receipt
 * @returns {{ valid: boolean, errors: string[] }}
 */
export function validateReceipt(receipt) {
  const result = UniversalReceiptSchema.safeParse(receipt);

  if (result.success) {
    return { valid: true, errors: [] };
  }

  return {
    valid: false,
    errors: result.error.errors.map(e => `${e.path.join('.')}: ${e.message}`),
  };
}

// ============================================================================
// Utilities
// ============================================================================

/**
 * Capitalize first letter
 * @param {string} str
 * @returns {string}
 */
function capitalize(str) {
  return str.charAt(0).toUpperCase() + str.slice(1);
}

/**
 * Escape string for TTL
 * @param {string} str
 * @returns {string}
 */
function escapeString(str) {
  return str
    .replace(/\\/g, '\\\\')
    .replace(/"/g, '\\"')
    .replace(/\n/g, '\\n')
    .replace(/\r/g, '\\r');
}
