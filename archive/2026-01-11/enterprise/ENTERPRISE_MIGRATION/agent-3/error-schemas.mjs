/**
 * @fileoverview Error Schemas and Codes
 * Standardized error handling for migration system
 */

/**
 * Error codes for migration system
 * @enum {string}
 */
export const ErrorCode = {
  CONTRACT_DRIFT: 'CONTRACT_DRIFT',
  LENS_FAILURE: 'LENS_FAILURE',
  RECEIPT_TAMPER: 'RECEIPT_TAMPER',
  SHADOW_MISMATCH: 'SHADOW_MISMATCH',
  ROUTING_ERROR: 'ROUTING_ERROR',
  VALIDATION_ERROR: 'VALIDATION_ERROR',
  TIMEOUT_ERROR: 'TIMEOUT_ERROR',
  NETWORK_ERROR: 'NETWORK_ERROR',
  UNKNOWN_ERROR: 'UNKNOWN_ERROR',
};

/**
 * Error severity levels
 * @enum {string}
 */
export const ErrorSeverity = {
  CRITICAL: 'CRITICAL',
  ERROR: 'ERROR',
  WARNING: 'WARNING',
  INFO: 'INFO',
};

/**
 * @typedef {object} ErrorContext
 * @property {string} [domainId] - Associated domain ID
 * @property {string} [capsuleId] - Associated capsule ID
 * @property {string} [lensId] - Associated lens ID
 * @property {string} [receiptId] - Associated receipt ID
 * @property {string} [operation] - Operation being performed
 * @property {object} [metadata] - Additional context data
 */

/**
 * MigrationError class for structured error handling
 */
export class MigrationError extends Error {
  /**
   * @param {string} code - Error code from ErrorCode enum
   * @param {string} message - Human-readable error message
   * @param {object} [options] - Additional options
   * @param {string} [options.correlationId] - Correlation ID for tracing
   * @param {ErrorSeverity} [options.severity] - Error severity
   * @param {ErrorContext} [options.context] - Error context
   * @param {Error} [options.cause] - Original error if wrapping
   */
  constructor(code, message, options = {}) {
    super(message);
    this.name = 'MigrationError';
    this.code = code;
    this.correlationId = options.correlationId || null;
    this.severity = options.severity || ErrorSeverity.ERROR;
    this.context = options.context || {};
    this.timestamp = Date.now();
    this.cause = options.cause || null;

    // Maintain proper stack trace
    if (Error.captureStackTrace) {
      Error.captureStackTrace(this, MigrationError);
    }
  }

  /**
   * Serialize error to JSON
   * @returns {object} Serialized error
   */
  toJSON() {
    return {
      name: this.name,
      code: this.code,
      message: this.message,
      correlationId: this.correlationId,
      severity: this.severity,
      context: this.context,
      timestamp: this.timestamp,
      stack: this.stack,
      ...(this.cause && {
        cause: this.cause instanceof Error
          ? { message: this.cause.message, stack: this.cause.stack }
          : this.cause
      }),
    };
  }

  /**
   * Serialize error for logging
   * @returns {object} Log-friendly error object
   */
  toLogEntry() {
    return {
      error_code: this.code,
      error_message: this.message,
      error_severity: this.severity,
      correlation_id: this.correlationId,
      timestamp: this.timestamp,
      context: this.context,
    };
  }
}

/**
 * Deserialize error from JSON
 * @param {object} json - Serialized error
 * @returns {MigrationError} Reconstructed error
 */
export function deserializeError(json) {
  const error = new MigrationError(
    json.code,
    json.message,
    {
      correlationId: json.correlationId,
      severity: json.severity,
      context: json.context,
    }
  );

  error.timestamp = json.timestamp;
  error.stack = json.stack;

  if (json.cause) {
    const causeError = new Error(json.cause.message);
    causeError.stack = json.cause.stack;
    error.cause = causeError;
  }

  return error;
}

/**
 * Create CONTRACT_DRIFT error
 * @param {string} message - Error message
 * @param {ErrorContext} context - Error context
 * @param {string} [correlationId] - Correlation ID
 * @returns {MigrationError} Contract drift error
 */
export function createContractDriftError(message, context, correlationId) {
  return new MigrationError(ErrorCode.CONTRACT_DRIFT, message, {
    severity: ErrorSeverity.CRITICAL,
    context,
    correlationId,
  });
}

/**
 * Create LENS_FAILURE error
 * @param {string} message - Error message
 * @param {ErrorContext} context - Error context
 * @param {string} [correlationId] - Correlation ID
 * @param {Error} [cause] - Original error
 * @returns {MigrationError} Lens failure error
 */
export function createLensFailureError(message, context, correlationId, cause) {
  return new MigrationError(ErrorCode.LENS_FAILURE, message, {
    severity: ErrorSeverity.ERROR,
    context,
    correlationId,
    cause,
  });
}

/**
 * Create RECEIPT_TAMPER error
 * @param {string} message - Error message
 * @param {ErrorContext} context - Error context
 * @param {string} [correlationId] - Correlation ID
 * @returns {MigrationError} Receipt tamper error
 */
export function createReceiptTamperError(message, context, correlationId) {
  return new MigrationError(ErrorCode.RECEIPT_TAMPER, message, {
    severity: ErrorSeverity.CRITICAL,
    context,
    correlationId,
  });
}

/**
 * Create SHADOW_MISMATCH error
 * @param {string} message - Error message
 * @param {ErrorContext} context - Error context
 * @param {string} [correlationId] - Correlation ID
 * @returns {MigrationError} Shadow mismatch error
 */
export function createShadowMismatchError(message, context, correlationId) {
  return new MigrationError(ErrorCode.SHADOW_MISMATCH, message, {
    severity: ErrorSeverity.ERROR,
    context,
    correlationId,
  });
}

/**
 * Create ROUTING_ERROR
 * @param {string} message - Error message
 * @param {ErrorContext} context - Error context
 * @param {string} [correlationId] - Correlation ID
 * @returns {MigrationError} Routing error
 */
export function createRoutingError(message, context, correlationId) {
  return new MigrationError(ErrorCode.ROUTING_ERROR, message, {
    severity: ErrorSeverity.ERROR,
    context,
    correlationId,
  });
}

/**
 * Create VALIDATION_ERROR
 * @param {string} message - Error message
 * @param {ErrorContext} context - Error context
 * @param {string} [correlationId] - Correlation ID
 * @returns {MigrationError} Validation error
 */
export function createValidationError(message, context, correlationId) {
  return new MigrationError(ErrorCode.VALIDATION_ERROR, message, {
    severity: ErrorSeverity.WARNING,
    context,
    correlationId,
  });
}

/**
 * Wrap unknown error as MigrationError
 * @param {unknown} error - Unknown error
 * @param {string} [correlationId] - Correlation ID
 * @param {ErrorContext} [context] - Additional context
 * @returns {MigrationError} Wrapped error
 */
export function wrapError(error, correlationId, context = {}) {
  if (error instanceof MigrationError) {
    return error;
  }

  const message = error instanceof Error ? error.message : String(error);
  const cause = error instanceof Error ? error : undefined;

  return new MigrationError(ErrorCode.UNKNOWN_ERROR, message, {
    severity: ErrorSeverity.ERROR,
    correlationId,
    context,
    cause,
  });
}

/**
 * Check if error is recoverable
 * @param {MigrationError} error - Error to check
 * @returns {boolean} True if error is potentially recoverable
 */
export function isRecoverableError(error) {
  const recoverableCodes = [
    ErrorCode.TIMEOUT_ERROR,
    ErrorCode.NETWORK_ERROR,
  ];

  return error instanceof MigrationError && recoverableCodes.includes(error.code);
}

/**
 * Check if error is critical
 * @param {MigrationError} error - Error to check
 * @returns {boolean} True if error is critical
 */
export function isCriticalError(error) {
  return error instanceof MigrationError && error.severity === ErrorSeverity.CRITICAL;
}

/**
 * Aggregate multiple errors into summary
 * @param {MigrationError[]} errors - Errors to aggregate
 * @returns {object} Error summary
 */
export function aggregateErrors(errors) {
  const summary = {
    total: errors.length,
    bySeverity: {},
    byCode: {},
    critical: [],
  };

  for (const error of errors) {
    // Count by severity
    summary.bySeverity[error.severity] = (summary.bySeverity[error.severity] || 0) + 1;

    // Count by code
    summary.byCode[error.code] = (summary.byCode[error.code] || 0) + 1;

    // Track critical errors
    if (isCriticalError(error)) {
      summary.critical.push(error.toJSON());
    }
  }

  return summary;
}
