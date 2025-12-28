/**
 * OTEL Observation Schemas - Zod Implementation
 *
 * Complete schema definitions for SPARC Pseudocode Phase
 * Includes base Observation, 10 domain-specific outputs, Receipt chain,
 * Capability, and Constraint schemas with comprehensive validation.
 *
 * @module schemas/observation-schemas
 */

import { z } from 'zod';
import { v4 as uuidv4, validate as validateUuid } from 'uuid';

// ============================================================================
// CONSTANTS & ENUMS
// ============================================================================

/**
 * Valid agent identifiers (agents 1-54)
 * @type {string[]}
 */
const VALID_AGENTS = Array.from(
  { length: 54 },
  (_, i) => `agent-${i + 1}`
);

/**
 * Valid observation domains
 * @type {string[]}
 */
const VALID_DOMAINS = [
  'fs',           // Filesystem observations
  'runtime',      // Runtime/JavaScript engine observations
  'net',          // Network observations
  'wasm',         // WebAssembly observations
  'perf',         // Performance metrics
  'tooling',      // Tool availability and versions
  'limits',       // System limits and quotas
  'storage',      // Storage capacity and availability
  'concurrency',  // Concurrency capabilities
  'system'        // System information
];

/**
 * Valid guard types
 * @type {string[]}
 */
const VALID_GUARD_TYPES = ['allow', 'deny'];

/**
 * Valid filesystem symlink behaviors
 * @type {string[]}
 */
const SYMLINK_BEHAVIORS = ['followed', 'denied', 'chrooted'];

/**
 * Valid storage types
 * @type {string[]}
 */
const STORAGE_TYPES = ['memory', 'disk', 'db'];

/**
 * Valid platforms
 * @type {string[]}
 */
const PLATFORMS = ['linux', 'darwin', 'win32', 'freebsd'];

/**
 * Valid JavaScript engines
 * @type {string[]}
 */
const JS_ENGINES = ['v8', 'spidermonkey', 'jsc', 'chakra'];

/**
 * Valid constraint severity levels
 * @type {string[]}
 */
const CONSTRAINT_SEVERITIES = ['warn', 'error'];

// ============================================================================
// CUSTOM ZOD REFINEMENTS & VALIDATORS
// ============================================================================

/**
 * UUID v4 validator
 * Matches RFC 4122 format
 * @type {z.ZodSchema}
 */
const uuidV4 = z.string().uuid('v4').refine(
  (val) => validateUuid(val),
  { message: 'Invalid UUID v4 format' }
);

/**
 * BLAKE3 hex hash validator
 * Exactly 64 hexadecimal characters
 * @type {z.ZodSchema}
 */
const blake3Hash = z.string()
  .length(64, 'BLAKE3 hash must be exactly 64 characters')
  .regex(/^[0-9a-f]{64}$/i, 'BLAKE3 hash must be lowercase hexadecimal');

/**
 * BigInt string validator
 * Parses and validates decimal representation of BigInt
 * Range: 0 to 2^63 - 1 (nanosecond timestamp range)
 * @type {z.ZodSchema}
 */
const bigintString = z.string().refine(
  (val) => {
    try {
      const n = BigInt(val);
      return n >= 0n && n < (1n << 63n);
    } catch {
      return false;
    }
  },
  { message: 'Must be a valid BigInt string between 0 and 2^63-1' }
);

/**
 * Method identifier validator
 * Format: {domain}.{methodName}
 * Pattern: lowercase letters and underscores separated by dot
 * @type {z.ZodSchema}
 */
const methodId = z.string()
  .regex(/^[a-z]+\.[a-z_]+$/, 'Method must match pattern: domain.methodName');

/**
 * Semantic version validator
 * Matches: major.minor.patch[-prerelease][+build]
 * @type {z.ZodSchema}
 */
const semanticVersion = z.string()
  .regex(/^\d+\.\d+\.\d+(-[a-zA-Z0-9.]+)?(\+[a-zA-Z0-9.]+)?$/);

/**
 * Absolute filesystem path validator
 * Must start with / (Unix-like systems)
 * @type {z.ZodSchema}
 */
const absolutePath = z.string().startsWith('/');

/**
 * URL pattern validator
 * Accepts domain patterns with wildcards
 * Examples: "https://example.com", "https://*.api.example.com"
 * @type {z.ZodSchema}
 */
const urlPattern = z.string()
  .regex(/^https?:\/\/[\w.*\-/]+$/, 'Invalid URL pattern');

/**
 * Input redaction utility
 * Removes or obscures sensitive fields
 * @param {object} input - Raw input object
 * @returns {object} Redacted input
 */
function redactSecrets(input) {
  const redacted = {};
  const sensitivePatterns = [
    /password|secret|token|apikey|auth|credential|bearer/i
  ];

  for (const [key, value] of Object.entries(input)) {
    // Check if key matches sensitive pattern
    const isSensitive = sensitivePatterns.some(pattern => pattern.test(key));

    if (isSensitive) {
      // Redact by type size, not value
      if (typeof value === 'string') {
        redacted[key] = `[REDACTED:${value.length}]`;
      } else if (typeof value === 'object') {
        redacted[key] = '[REDACTED_OBJECT]';
      } else {
        redacted[key] = '[REDACTED]';
      }
    } else if (typeof value === 'object' && value !== null) {
      // Recurse into nested objects
      redacted[key] = redactSecrets(value);
    } else {
      // Safe field - pass through
      redacted[key] = value;
    }
  }

  return redacted;
}

/**
 * Input validator with redaction
 * @type {z.ZodSchema}
 */
const redactedInput = z.record(z.any())
  .transform(redactSecrets);

// ============================================================================
// DOMAIN-SPECIFIC OUTPUT SCHEMAS
// ============================================================================

/**
 * Runtime domain output schema
 * Captures Node.js/JavaScript engine capabilities
 *
 * @typedef {object} RuntimeOutput
 * @property {string} nodeVersion - Semantic version (e.g., "20.10.0")
 * @property {string} jsEngine - JavaScript engine (v8|spidermonkey|jsc|chakra)
 * @property {boolean} wasm - WebAssembly support
 * @property {number} workers - Available worker threads (0-32768)
 * @property {number} timersResolution - Minimum timer resolution in nanoseconds
 * @property {boolean} icu - ICU support enabled
 *
 * @type {z.ZodSchema}
 */
const RuntimeOutput = z.object({
  nodeVersion: semanticVersion,
  jsEngine: z.enum(JS_ENGINES),
  wasm: z.boolean(),
  workers: z.number().int().min(0).max(32768),
  timersResolution: z.number().int().min(100000).max(1000000),
  icu: z.boolean()
});

/**
 * Filesystem domain output schema
 * Captures filesystem characteristics and capabilities
 *
 * @typedef {object} FilesystemOutput
 * @property {string} root - Filesystem root path
 * @property {number} maxPathLength - Maximum path component length (255-65535)
 * @property {number} fileCount - Number of accessible files
 * @property {string} symlinkBehavior - How symlinks are handled
 * @property {boolean} writeTest - Whether test write succeeded
 *
 * @type {z.ZodSchema}
 */
const FilesystemOutput = z.object({
  root: absolutePath,
  maxPathLength: z.number().int().min(255).max(65535),
  fileCount: z.number().int().min(0),
  symlinkBehavior: z.enum(SYMLINK_BEHAVIORS),
  writeTest: z.boolean()
});

/**
 * WASM domain output schema
 * Captures WebAssembly module instantiation and memory
 *
 * @typedef {object} WasmOutput
 * @property {boolean} instantiated - Module instantiation success
 * @property {number} startupMs - Time to instantiate (milliseconds)
 * @property {boolean} sharedArrayBuffer - SharedArrayBuffer availability
 * @property {number} memoryGrowth - Peak memory growth (bytes)
 *
 * @type {z.ZodSchema}
 */
const WasmOutput = z.object({
  instantiated: z.boolean(),
  startupMs: z.number().min(0),
  sharedArrayBuffer: z.boolean(),
  memoryGrowth: z.number().int().min(0)
});

/**
 * Performance domain output schema
 * Captures throughput and latency metrics
 *
 * @typedef {object} PerformanceOutput
 * @property {string} domain - What was measured
 * @property {number} throughput - Operations per second
 * @property {number} latency_p50 - Median latency (milliseconds)
 * @property {number} latency_p99 - 99th percentile latency (milliseconds)
 * @property {number} variance - Latency variance/std dev (milliseconds)
 *
 * @type {z.ZodSchema}
 */
const PerformanceOutput = z.object({
  domain: z.string(),
  throughput: z.number().positive(),
  latency_p50: z.number().nonnegative(),
  latency_p99: z.number().nonnegative(),
  variance: z.number().nonnegative()
})
.refine(
  (data) => data.latency_p99 >= data.latency_p50,
  {
    message: 'latency_p99 must be >= latency_p50',
    path: ['latency_p99']
  }
);

/**
 * Network domain output schema
 * Captures network capabilities and restrictions
 *
 * @typedef {object} NetworkOutput
 * @property {string[]} urlAllowlist - Array of allowed URL patterns
 * @property {boolean} dnsResolution - DNS resolution working
 * @property {number|null} maxPayloadBytes - Max request payload
 *
 * @type {z.ZodSchema}
 */
const NetworkOutput = z.object({
  urlAllowlist: z.array(urlPattern).default([]),
  dnsResolution: z.boolean(),
  maxPayloadBytes: z.number().int().positive().nullable().default(null)
});

/**
 * Tooling domain output schema
 * Captures tool availability and versions
 *
 * @typedef {object} ToolingOutput
 * @property {string} command - Tool command name
 * @property {boolean} accessible - Tool is in PATH
 * @property {string|null} version - Tool version string
 *
 * @type {z.ZodSchema}
 */
const ToolingOutput = z.object({
  command: z.string().min(1),
  accessible: z.boolean(),
  version: semanticVersion.nullable().default(null)
});

/**
 * Storage domain output schema
 * Captures storage capacity and quota information
 *
 * @typedef {object} StorageOutput
 * @property {string} type - Storage type (memory|disk|db)
 * @property {number} quota - Total quota (bytes)
 * @property {number} available - Available space (bytes)
 *
 * @type {z.ZodSchema}
 */
const StorageOutput = z.object({
  type: z.enum(STORAGE_TYPES),
  quota: z.number().int().nonnegative(),
  available: z.number().int().nonnegative()
})
.refine(
  (data) => data.available <= data.quota,
  {
    message: 'available must be <= quota',
    path: ['available']
  }
);

/**
 * Throttle information for concurrency
 *
 * @typedef {object} ThrottleInfo
 * @property {string} name - Throttle identifier
 * @property {number} ratePerSecond - Operations per second
 * @property {number} burst - Max concurrent requests
 *
 * @type {z.ZodSchema}
 */
const ThrottleInfo = z.object({
  name: z.string().min(1),
  ratePerSecond: z.number().positive(),
  burst: z.number().int().positive()
});

/**
 * Concurrency domain output schema
 * Captures parallelism capabilities and throttles
 *
 * @typedef {object} ConcurrencyOutput
 * @property {boolean} hasWorkers - Worker threads available
 * @property {number} parallelism - Max concurrent operations
 * @property {ThrottleInfo[]} throttles - Active rate limits
 *
 * @type {z.ZodSchema}
 */
const ConcurrencyOutput = z.object({
  hasWorkers: z.boolean(),
  parallelism: z.number().int().positive(),
  throttles: z.array(ThrottleInfo).default([])
});

/**
 * Limits domain output schema
 * Captures system resource limits
 *
 * @typedef {object} LimitsOutput
 * @property {number} memoryMB - Memory limit (megabytes)
 * @property {number} cpuShares - CPU allocation (relative shares)
 * @property {number} fsQuota - Filesystem quota (bytes)
 *
 * @type {z.ZodSchema}
 */
const LimitsOutput = z.object({
  memoryMB: z.number().int().positive(),
  cpuShares: z.number().int().positive(),
  fsQuota: z.number().int().nonnegative()
});

/**
 * System domain output schema
 * Captures system information
 *
 * @typedef {object} SystemOutput
 * @property {string} platform - Operating system (linux|darwin|win32|freebsd)
 * @property {string} osVersion - OS version string
 * @property {boolean} containerized - Running in container/VM
 *
 * @type {z.ZodSchema}
 */
const SystemOutput = z.object({
  platform: z.enum(PLATFORMS),
  osVersion: z.string().min(1),
  containerized: z.boolean()
});

/**
 * Domain output router
 * Selects appropriate schema based on domain
 * @param {string} domain - Observation domain
 * @returns {z.ZodSchema} Domain-specific output schema
 * @throws {Error} If domain is unknown
 */
function getDomainSchema(domain) {
  const schemas = {
    runtime: RuntimeOutput,
    fs: FilesystemOutput,
    wasm: WasmOutput,
    perf: PerformanceOutput,
    net: NetworkOutput,
    tooling: ToolingOutput,
    storage: StorageOutput,
    concurrency: ConcurrencyOutput,
    limits: LimitsOutput,
    system: SystemOutput
  };

  if (!schemas[domain]) {
    throw new Error(`Unknown domain: ${domain}`);
  }

  return schemas[domain];
}

// ============================================================================
// GUARD & RECEIPT SCHEMAS
// ============================================================================

/**
 * Guard schema - Capability/access control decision
 *
 * @typedef {object} Guard
 * @property {string} type - "allow" or "deny"
 * @property {string|undefined} reason - Reason for denial (required if type=deny)
 *
 * @type {z.ZodSchema}
 */
const Guard = z.discriminatedUnion('type', [
  z.object({
    type: z.literal('allow')
  }),
  z.object({
    type: z.literal('deny'),
    reason: z.string().min(1, 'Denial reason required')
  })
]);

/**
 * Receipt schema - Hash chain receipt for immutable audit log
 *
 * @typedef {object} Receipt
 * @property {string} obs_hash - BLAKE3 hash of observation
 * @property {string|null} prev_hash - Previous receipt hash (null for first)
 * @property {string} timestamp_ns - Creation timestamp (nanoseconds)
 * @property {string} agentId - Agent that created receipt
 *
 * @type {z.ZodSchema}
 */
const Receipt = z.object({
  obs_hash: blake3Hash,
  prev_hash: blake3Hash.nullable().default(null),
  timestamp_ns: bigintString,
  agentId: z.enum(VALID_AGENTS)
});

// ============================================================================
// BASE OBSERVATION SCHEMA
// ============================================================================

/**
 * Base Observation schema
 * Core observation with domain-specific output
 *
 * @typedef {object} Observation
 * @property {string} id - UUID v4 identifier
 * @property {string} agentId - Agent identifier (agent-1 to agent-54)
 * @property {string} domain - Domain enum (fs|runtime|net|wasm|perf|tooling|limits|storage|concurrency|system)
 * @property {string} timestamp_ns - Nanosecond timestamp (BigInt string)
 * @property {string} method - Method identifier (domain.name)
 * @property {object} input - Redacted input parameters
 * @property {object} output - Domain-specific output
 * @property {string} hash - BLAKE3 hash of observation
 * @property {Guard} guard - Access control decision
 * @property {Receipt|null} receipt - Optional hash chain receipt
 *
 * @type {z.ZodSchema}
 */
const Observation = z.object({
  id: uuidV4,
  agentId: z.enum(VALID_AGENTS),
  domain: z.enum(VALID_DOMAINS),
  timestamp_ns: bigintString,
  method: methodId,
  input: redactedInput,
  output: z.record(z.any()), // Will be validated by domain schema
  hash: blake3Hash,
  guard: Guard,
  receipt: Receipt.nullable().default(null)
})
.refine(
  (data) => {
    // Validate output against domain schema
    try {
      const domainSchema = getDomainSchema(data.domain);
      domainSchema.parse(data.output);
      return true;
    } catch {
      return false;
    }
  },
  {
    message: 'Output does not match domain schema',
    path: ['output']
  }
)
.refine(
  (data) => {
    // Validate method starts with domain
    const [methodDomain] = data.method.split('.');
    return methodDomain === data.domain;
  },
  {
    message: 'Method domain must match observation domain',
    path: ['method']
  }
);

// ============================================================================
// DERIVED SCHEMAS: CAPABILITY & CONSTRAINT
// ============================================================================

/**
 * Capability schema - Positive capability assertion
 * Derived from successful observations
 *
 * @typedef {object} Capability
 * @property {string} id - UUID v4 identifier
 * @property {string} domain - Domain this capability applies to
 * @property {string} method - Specific method
 * @property {number|null} maxThroughput - Max observed throughput (ops/sec)
 * @property {number|null} minLatency_ns - Min observed latency (nanoseconds)
 * @property {boolean} available - Currently available
 * @property {number} confidence - Confidence score (0-1)
 * @property {string} lastVerified - Last verification timestamp (nanoseconds)
 * @property {number} observationCount - Supporting observations
 *
 * @type {z.ZodSchema}
 */
const Capability = z.object({
  id: uuidV4,
  domain: z.enum(VALID_DOMAINS),
  method: methodId,
  maxThroughput: z.number().positive().nullable().default(null),
  minLatency_ns: z.number().int().nonnegative().nullable().default(null),
  available: z.boolean(),
  confidence: z.number().min(0).max(1),
  lastVerified: bigintString,
  observationCount: z.number().int().nonnegative()
});

/**
 * Constraint schema - Negative constraint assertion
 * Derived from failed observations or system limits
 *
 * @typedef {object} Constraint
 * @property {string} id - UUID v4 identifier
 * @property {string} domain - Domain this constraint applies to
 * @property {string} name - Constraint identifier
 * @property {any} value - Constraint value
 * @property {string} unit - Constraint unit (bytes, connections, seconds, etc.)
 * @property {boolean} enforced - Whether hard limit
 * @property {string} severity - warn|error
 * @property {string} discoveredAt - Discovery timestamp (nanoseconds)
 * @property {number} observationCount - Triggering observations
 *
 * @type {z.ZodSchema}
 */
const Constraint = z.object({
  id: uuidV4,
  domain: z.enum(VALID_DOMAINS),
  name: z.string().min(1),
  value: z.any(),
  unit: z.string().min(1),
  enforced: z.boolean(),
  severity: z.enum(CONSTRAINT_SEVERITIES),
  discoveredAt: bigintString,
  observationCount: z.number().int().nonnegative()
});

// ============================================================================
// BATCH OPERATIONS
// ============================================================================

/**
 * Batch observation validation schema
 * For validating multiple observations in sequence
 *
 * @typedef {object} BatchObservations
 * @property {Observation[]} observations - Array of observations
 *
 * @type {z.ZodSchema}
 */
const BatchObservations = z.object({
  observations: z.array(Observation)
});

/**
 * Receipt chain validation schema
 * For validating hash chain integrity
 *
 * @typedef {object} ReceiptChain
 * @property {Receipt[]} receipts - Ordered receipts
 *
 * @type {z.ZodSchema}
 */
const ReceiptChain = z.array(Receipt)
  .refine(
    (receipts) => {
      // Validate hash chain continuity
      for (let i = 1; i < receipts.length; i++) {
        if (receipts[i].prev_hash !== receipts[i - 1].obs_hash) {
          return false;
        }
      }
      return true;
    },
    { message: 'Receipt chain broken - hashes do not link' }
  )
  .refine(
    (receipts) => {
      // Validate timestamp ordering
      for (let i = 1; i < receipts.length; i++) {
        const currTs = BigInt(receipts[i].timestamp_ns);
        const prevTs = BigInt(receipts[i - 1].timestamp_ns);
        if (currTs <= prevTs) {
          return false;
        }
      }
      return true;
    },
    { message: 'Receipt timestamps not monotonically increasing' }
  );

// ============================================================================
// ERROR HANDLING
// ============================================================================

/**
 * ValidationError class
 * Enhanced error with path and context information
 *
 * @class ValidationError
 * @extends {Error}
 */
class ValidationError extends Error {
  /**
   * Create ValidationError
   * @param {string} message - Error message
   * @param {string[]} path - Path to failed field
   * @param {any} value - Value that failed validation
   * @param {string} code - Error code
   */
  constructor(message, path = [], value = null, code = 'VALIDATION_ERROR') {
    super(message);
    this.name = 'ValidationError';
    this.path = path;
    this.value = value;
    this.code = code;
  }

  /**
   * Convert to JSON-serializable format
   * @returns {object} Error object
   */
  toJSON() {
    return {
      name: this.name,
      message: this.message,
      code: this.code,
      path: this.path,
      value: this.value === undefined ? null : String(this.value).slice(0, 100)
    };
  }
}

/**
 * Validation result wrapper
 * Provides success/failure pattern
 *
 * @typedef {object} ValidationResult
 * @property {boolean} success - Validation passed
 * @property {Observation|null} data - Validated observation (if success)
 * @property {ValidationError|null} error - Error details (if failure)
 *
 * @type {object}
 */

/**
 * Validate observation with error wrapping
 * @param {object} raw - Raw observation input
 * @returns {ValidationResult} Validation result
 */
function validateObservation(raw) {
  try {
    const data = Observation.parse(raw);
    return {
      success: true,
      data,
      error: null
    };
  } catch (err) {
    if (err instanceof z.ZodError) {
      const issue = err.issues[0];
      return {
        success: false,
        data: null,
        error: new ValidationError(
          issue.message,
          issue.path,
          raw[issue.path[0]],
          'ZOD_VALIDATION_ERROR'
        )
      };
    }

    return {
      success: false,
      data: null,
      error: new ValidationError(err.message)
    };
  }
}

/**
 * Validate batch observations
 * @param {object[]} observations - Array of raw observations
 * @returns {{results: ValidationResult[], passCount: number, failCount: number}}
 */
function validateObservationBatch(observations) {
  const results = observations.map(validateObservation);
  const passCount = results.filter(r => r.success).length;
  const failCount = results.length - passCount;

  return {
    results,
    passCount,
    failCount
  };
}

/**
 * Validate receipt chain
 * @param {object[]} receipts - Array of raw receipts
 * @returns {ValidationResult}
 */
function validateReceiptChain(receipts) {
  try {
    const data = ReceiptChain.parse(receipts);
    return {
      success: true,
      data,
      error: null
    };
  } catch (err) {
    if (err instanceof z.ZodError) {
      const issue = err.issues[0];
      return {
        success: false,
        data: null,
        error: new ValidationError(
          issue.message,
          issue.path,
          null,
          'RECEIPT_CHAIN_ERROR'
        )
      };
    }

    return {
      success: false,
      data: null,
      error: new ValidationError(err.message)
    };
  }
}

// ============================================================================
// EXPORTS
// ============================================================================

export {
  // Constants
  VALID_AGENTS,
  VALID_DOMAINS,
  VALID_GUARD_TYPES,
  SYMLINK_BEHAVIORS,
  STORAGE_TYPES,
  PLATFORMS,
  JS_ENGINES,
  CONSTRAINT_SEVERITIES,

  // Custom validators
  uuidV4,
  blake3Hash,
  bigintString,
  methodId,
  semanticVersion,
  absolutePath,
  urlPattern,
  redactedInput,

  // Domain schemas
  RuntimeOutput,
  FilesystemOutput,
  WasmOutput,
  PerformanceOutput,
  NetworkOutput,
  ToolingOutput,
  StorageOutput,
  ThrottleInfo,
  ConcurrencyOutput,
  LimitsOutput,
  SystemOutput,
  getDomainSchema,

  // Core schemas
  Guard,
  Receipt,
  Observation,

  // Derived schemas
  Capability,
  Constraint,

  // Batch operations
  BatchObservations,
  ReceiptChain,

  // Error handling
  ValidationError,
  validateObservation,
  validateObservationBatch,
  validateReceiptChain,

  // Utilities
  redactSecrets
};
