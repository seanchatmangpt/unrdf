/**
 * @fileoverview KGC Probe - Custom Error Classes
 *
 * Provides structured error types with:
 * - Error codes for programmatic handling
 * - Context information for debugging
 * - Recovery suggestions for users
 *
 * @module @unrdf/kgc-probe/utils/errors
 */

/**
 * Base error for KGC Probe operations
 *
 * @class ProbeError
 * @extends Error
 * @example
 * throw new ProbeError('Operation failed', 'OP_FAILED', { operation: 'scan' });
 */
export class ProbeError extends Error {
  /**
   * @param {string} message - Error message
   * @param {string} [code='PROBE_ERROR'] - Error code
   * @param {Object} [context] - Additional context
   * @param {string} [recovery] - Recovery suggestion
   */
  constructor(message, code = 'PROBE_ERROR', context = {}, recovery = undefined) {
    super(message);
    this.name = 'ProbeError';

    /** @type {string} */
    this.code = code;

    /** @type {Object} */
    this.context = context;

    /** @type {string | undefined} */
    this.recovery = recovery;

    /** @type {string} */
    this.timestamp = new Date().toISOString();

    // Capture stack trace
    if (Error.captureStackTrace) {
      Error.captureStackTrace(this, this.constructor);
    }
  }

  /**
   * Convert to JSON for logging
   * @returns {Object}
   */
  toJSON() {
    return {
      name: this.name,
      code: this.code,
      message: this.message,
      context: this.context,
      recovery: this.recovery,
      timestamp: this.timestamp,
      stack: this.stack
    };
  }

  /**
   * Create from unknown error
   * @param {unknown} error - Unknown error
   * @param {string} [code] - Override code
   * @returns {ProbeError}
   */
  static from(error, code) {
    if (error instanceof ProbeError) {
      return error;
    }

    if (error instanceof Error) {
      return new ProbeError(error.message, code || 'UNKNOWN_ERROR', {
        originalName: error.name,
        originalStack: error.stack
      });
    }

    return new ProbeError(String(error), code || 'UNKNOWN_ERROR');
  }
}

/**
 * Error thrown when a guard blocks an operation
 *
 * @class GuardViolationError
 * @extends ProbeError
 * @example
 * throw new GuardViolationError(
 *   'Access to AWS credentials forbidden',
 *   'G-H1-ENV-TOKEN',
 *   { variable: 'AWS_SECRET_KEY', pattern: 'AWS_*' },
 *   'receipt-123'
 * );
 */
export class GuardViolationError extends ProbeError {
  /**
   * @param {string} message - Error message
   * @param {string} guardId - Guard that blocked the operation
   * @param {Object} [context] - Additional context
   * @param {string} [receiptId] - Denial receipt ID
   */
  constructor(message, guardId, context = {}, receiptId = undefined) {
    super(message, 'GUARD_VIOLATION', {
      ...context,
      guardId,
      receiptId
    }, 'Check guard policy configuration or request access exemption');

    this.name = 'GuardViolationError';

    /** @type {string} */
    this.guardId = guardId;

    /** @type {string | undefined} */
    this.receiptId = receiptId;
  }
}

/**
 * Error thrown when input validation fails
 *
 * @class ValidationError
 * @extends ProbeError
 * @example
 * throw new ValidationError('Invalid universe ID', { field: 'universe_id', value: null });
 */
export class ValidationError extends ProbeError {
  /**
   * @param {string} message - Error message
   * @param {Object} [context] - Validation context
   * @param {string} [field] - Field that failed validation
   */
  constructor(message, context = {}, field = undefined) {
    super(message, 'VALIDATION_ERROR', {
      ...context,
      field
    }, 'Check input format and required fields');

    this.name = 'ValidationError';

    /** @type {string | undefined} */
    this.field = field;

    /** @type {any} */
    this.issues = context.issues || [];
  }

  /**
   * Create from Zod error
   * @param {import('zod').ZodError} zodError - Zod validation error
   * @returns {ValidationError}
   */
  static fromZod(zodError) {
    const issues = zodError.issues.map(i => ({
      path: i.path.join('.'),
      message: i.message,
      code: i.code
    }));

    const firstIssue = issues[0];
    return new ValidationError(
      `Validation failed: ${firstIssue?.message || 'Invalid input'}`,
      { issues },
      firstIssue?.path
    );
  }
}

/**
 * Error thrown when shard merge conflicts occur
 *
 * @class MergeConflictError
 * @extends ProbeError
 * @example
 * throw new MergeConflictError('Conflicting claims detected', [
 *   { claimId: 'cap-1', agents: ['agent-1', 'agent-2'] }
 * ]);
 */
export class MergeConflictError extends ProbeError {
  /**
   * @param {string} message - Error message
   * @param {Array<{claimId: string, agents: string[]}>} conflicts - Conflict details
   */
  constructor(message, conflicts = []) {
    super(message, 'MERGE_CONFLICT', {
      conflicts,
      conflictCount: conflicts.length
    }, 'Use --on-conflict=list to review conflicts or --on-conflict=merge to auto-resolve');

    this.name = 'MergeConflictError';

    /** @type {Array<{claimId: string, agents: string[]}>} */
    this.conflicts = conflicts;
  }
}

/**
 * Error thrown when receipt verification fails
 *
 * @class ReceiptError
 * @extends ProbeError
 * @example
 * throw new ReceiptError('Hash chain verification failed', {
 *   expectedHash: '0xabc...',
 *   actualHash: '0xdef...'
 * });
 */
export class ReceiptError extends ProbeError {
  /**
   * @param {string} message - Error message
   * @param {Object} [context] - Receipt context
   * @param {string} [verificationStep] - Which verification step failed
   */
  constructor(message, context = {}, verificationStep = undefined) {
    super(message, 'RECEIPT_ERROR', {
      ...context,
      verificationStep
    }, 'Regenerate receipts or check artifact integrity');

    this.name = 'ReceiptError';

    /** @type {string | undefined} */
    this.verificationStep = verificationStep;
  }
}

/**
 * Error thrown when artifact is not found
 *
 * @class ArtifactNotFoundError
 * @extends ProbeError
 * @example
 * throw new ArtifactNotFoundError('run-123');
 */
export class ArtifactNotFoundError extends ProbeError {
  /**
   * @param {string} artifactId - Missing artifact ID
   * @param {string} [path] - Path searched
   */
  constructor(artifactId, path = undefined) {
    super(`Artifact not found: ${artifactId}`, 'ARTIFACT_NOT_FOUND', {
      artifactId,
      path
    }, 'Run a probe scan first or check the artifact path');

    this.name = 'ArtifactNotFoundError';

    /** @type {string} */
    this.artifactId = artifactId;
  }
}

/**
 * Error thrown when command execution times out
 *
 * @class TimeoutError
 * @extends ProbeError
 * @example
 * throw new TimeoutError('Scan timed out', 30000);
 */
export class TimeoutError extends ProbeError {
  /**
   * @param {string} message - Error message
   * @param {number} timeoutMs - Timeout in milliseconds
   * @param {string} [operation] - Operation that timed out
   */
  constructor(message, timeoutMs, operation = undefined) {
    super(message, 'TIMEOUT', {
      timeoutMs,
      operation
    }, 'Increase --timeout value or check for performance issues');

    this.name = 'TimeoutError';

    /** @type {number} */
    this.timeoutMs = timeoutMs;
  }
}

/**
 * Error thrown when agent execution fails
 *
 * @class AgentError
 * @extends ProbeError
 * @example
 * throw new AgentError('Agent failed', 'completeness-agent', new Error('Query failed'));
 */
export class AgentError extends ProbeError {
  /**
   * @param {string} message - Error message
   * @param {string} agentId - Agent that failed
   * @param {Error} [cause] - Underlying error
   */
  constructor(message, agentId, cause = undefined) {
    super(message, 'AGENT_ERROR', {
      agentId,
      cause: cause?.message
    }, 'Check agent configuration and dependencies');

    this.name = 'AgentError';

    /** @type {string} */
    this.agentId = agentId;

    /** @type {Error | undefined} */
    this.cause = cause;
  }
}

/**
 * Error thrown when storage operation fails
 *
 * @class StorageError
 * @extends ProbeError
 * @example
 * throw new StorageError('Failed to write artifact', 'write', { path: '/tmp/artifact.json' });
 */
export class StorageError extends ProbeError {
  /**
   * @param {string} message - Error message
   * @param {string} operation - Storage operation (read|write|delete)
   * @param {Object} [context] - Additional context
   */
  constructor(message, operation, context = {}) {
    super(message, 'STORAGE_ERROR', {
      ...context,
      operation
    }, 'Check storage permissions and disk space');

    this.name = 'StorageError';

    /** @type {string} */
    this.operation = operation;
  }
}

/**
 * Error thrown when configuration is invalid
 *
 * @class ConfigurationError
 * @extends ProbeError
 * @example
 * throw new ConfigurationError('Invalid config file', { path: '.kgc-probe.json' });
 */
export class ConfigurationError extends ProbeError {
  /**
   * @param {string} message - Error message
   * @param {Object} [context] - Configuration context
   */
  constructor(message, context = {}) {
    super(message, 'CONFIG_ERROR', context, 'Check configuration file format and required fields');

    this.name = 'ConfigurationError';
  }
}

/**
 * Error code constants
 * @type {Object<string, string>}
 */
export const ErrorCodes = {
  PROBE_ERROR: 'PROBE_ERROR',
  GUARD_VIOLATION: 'GUARD_VIOLATION',
  VALIDATION_ERROR: 'VALIDATION_ERROR',
  MERGE_CONFLICT: 'MERGE_CONFLICT',
  RECEIPT_ERROR: 'RECEIPT_ERROR',
  ARTIFACT_NOT_FOUND: 'ARTIFACT_NOT_FOUND',
  TIMEOUT: 'TIMEOUT',
  AGENT_ERROR: 'AGENT_ERROR',
  STORAGE_ERROR: 'STORAGE_ERROR',
  CONFIG_ERROR: 'CONFIG_ERROR',
  UNKNOWN_ERROR: 'UNKNOWN_ERROR'
};

/**
 * Check if error is a ProbeError
 * @param {unknown} error - Error to check
 * @returns {boolean}
 */
export function isProbeError(error) {
  return error instanceof ProbeError;
}

/**
 * Wrap error in ProbeError if not already
 * @param {unknown} error - Error to wrap
 * @param {string} [code] - Error code
 * @returns {ProbeError}
 */
export function wrapError(error, code) {
  return ProbeError.from(error, code);
}
