/**
 * @fileoverview YAWL Error Classes
 *
 * Centralized error definitions for YAWL workflow engine.
 * All errors extend YAWLError base class for consistent handling.
 *
 * @module @unrdf/yawl/errors
 * @version 1.0.0
 */

/**
 * Base error class for all YAWL errors
 */
export class YAWLError extends Error {
  /**
   * @param {string} message - Error message
   * @param {Object} [options={}] - Error options
   * @param {Error} [options.cause] - Underlying cause
   * @param {Object} [options.context] - Additional context
   */
  constructor(message, options = {}) {
    super(message, { cause: options.cause });
    this.name = 'YAWLError';
    this.context = options.context || {};

    // Capture stack trace
    if (Error.captureStackTrace) {
      Error.captureStackTrace(this, this.constructor);
    }
  }

  /**
   * Convert to JSON for logging
   */
  toJSON() {
    return {
      name: this.name,
      message: this.message,
      context: this.context,
      stack: this.stack,
      cause: this.cause?.message,
    };
  }
}

/**
 * Validation error - schema or constraint validation failed
 */
export class ValidationError extends YAWLError {
  constructor(message, options = {}) {
    super(message, options);
    this.name = 'ValidationError';
  }
}

/**
 * Workflow error - workflow definition or structure invalid
 */
export class WorkflowError extends YAWLError {
  constructor(message, options = {}) {
    super(message, options);
    this.name = 'WorkflowError';
  }
}

/**
 * Task execution error - task state transition or execution failed
 */
export class TaskExecutionError extends YAWLError {
  constructor(message, options = {}) {
    super(message, options);
    this.name = 'TaskExecutionError';
  }
}

/**
 * Storage error - RDF store operations failed
 */
export class StorageError extends YAWLError {
  constructor(message, options = {}) {
    super(message, options);
    this.name = 'StorageError';
  }
}

/**
 * Receipt error - receipt generation or verification failed
 */
export class ReceiptError extends YAWLError {
  constructor(message, options = {}) {
    super(message, options);
    this.name = 'ReceiptError';
  }
}

/**
 * Resource allocation error - resource allocation/deallocation failed
 */
export class ResourceError extends YAWLError {
  constructor(message, options = {}) {
    super(message, options);
    this.name = 'ResourceError';
  }
}

/**
 * Serialization error - RDF serialization/deserialization failed
 */
export class SerializationError extends YAWLError {
  constructor(message, options = {}) {
    super(message, options);
    this.name = 'SerializationError';
  }
}

/**
 * Engine error - workflow engine core operations failed
 */
export class EngineError extends YAWLError {
  constructor(message, options = {}) {
    super(message, options);
    this.name = 'EngineError';
  }
}

/**
 * Cancellation error - cancellation region operations failed
 */
export class CancellationError extends YAWLError {
  constructor(message, options = {}) {
    super(message, options);
    this.name = 'CancellationError';
  }
}
