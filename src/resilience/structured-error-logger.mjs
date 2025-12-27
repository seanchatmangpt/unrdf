/**
 * @fileoverview Structured Error Logging with Categorization
 *
 * **Purpose**: Comprehensive error logging with categorization and tracking
 * - Error categorization (transient vs permanent)
 * - Error rate tracking per category
 * - Severity levels (debug, info, warn, error, fatal)
 * - OTEL integration for error tracking
 * - Error aggregation and alerting
 *
 * @module resilience/structured-error-logger
 */

import { trace, SpanStatusCode } from '@opentelemetry/api';
import { z } from 'zod';

/**
 * Error category enumeration
 */
export const ErrorCategory = {
  TRANSIENT: 'transient', // Network timeouts, temporary failures
  PERMANENT: 'permanent', // Validation errors, not found
  INFRASTRUCTURE: 'infrastructure', // Database, cache, external service
  BUSINESS_LOGIC: 'business_logic', // Domain rule violations
  SECURITY: 'security', // Authentication, authorization
  UNKNOWN: 'unknown', // Unclassified
};

/**
 * Error severity enumeration
 */
export const ErrorSeverity = {
  DEBUG: 'debug',
  INFO: 'info',
  WARN: 'warn',
  ERROR: 'error',
  FATAL: 'fatal',
};

/**
 * Structured error schema
 */
export const StructuredErrorSchema = z.object({
  id: z.string(),
  timestamp: z.string(),
  category: z.enum([
    'transient',
    'permanent',
    'infrastructure',
    'business_logic',
    'security',
    'unknown',
  ]),
  severity: z.enum(['debug', 'info', 'warn', 'error', 'fatal']),
  message: z.string(),
  code: z.string().optional(),
  stack: z.string().optional(),
  context: z.record(z.any()).optional(),
  agentId: z.string().optional(),
  agentType: z.string().optional(),
  operation: z.string().optional(),
  correlationId: z.string().optional(),
});

/**
 * Structured Error Logger
 *
 * @example
 * const logger = new StructuredErrorLogger();
 * logger.logError(error, {
 *   category: ErrorCategory.TRANSIENT,
 *   severity: ErrorSeverity.WARN,
 *   context: { agentId: 'agent-001', operation: 'processTask' }
 * });
 */
export class StructuredErrorLogger {
  /**
   * Create a new structured error logger
   * @param {Object} [config] - Logger configuration
   */
  constructor(config = {}) {
    this.config = {
      maxLogSize: config.maxLogSize ?? 1000,
      enableConsole: config.enableConsole ?? true,
      enableOTEL: config.enableOTEL ?? true,
      alertThresholds: config.alertThresholds ?? {
        errorRate: 0.1, // 10% error rate
        fatalCount: 5, // 5 fatal errors
        errorWindow: 60000, // 1 minute window
      },
    };

    this.tracer = trace.getTracer('unrdf-error-logger');
    this.errorLog = [];
    this.metrics = {
      totalErrors: 0,
      byCategory: {
        transient: 0,
        permanent: 0,
        infrastructure: 0,
        business_logic: 0,
        security: 0,
        unknown: 0,
      },
      bySeverity: {
        debug: 0,
        info: 0,
        warn: 0,
        error: 0,
        fatal: 0,
      },
      recentErrorRate: 0,
      lastAlert: null,
    };
  }

  /**
   * Log an error with structured metadata
   * @param {Error} error - Error to log
   * @param {Object} [options] - Logging options
   * @returns {Object} Logged error record
   */
  logError(error, options = {}) {
    const {
      category = this._categorizeError(error),
      severity = this._determineSeverity(error, category),
      context = {},
      operation = null,
      agentId = null,
      agentType = null,
      correlationId = null,
    } = options;

    const structuredError = {
      id: `error_${Date.now()}_${Math.random().toString(36).slice(2, 9)}`,
      timestamp: new Date().toISOString(),
      category,
      severity,
      message: error.message || String(error),
      code: error.code || error.name || 'UNKNOWN_ERROR',
      stack: error.stack,
      context,
      agentId,
      agentType,
      operation,
      correlationId,
    };

    // Validate
    const validated = StructuredErrorSchema.parse(structuredError);

    // Store in log
    this.errorLog.push(validated);

    // Update metrics
    this.metrics.totalErrors++;
    this.metrics.byCategory[category]++;
    this.metrics.bySeverity[severity]++;

    // Cleanup old logs
    this._cleanupOldLogs();

    // Console logging if enabled
    if (this.config.enableConsole) {
      this._logToConsole(validated);
    }

    // OTEL logging if enabled
    if (this.config.enableOTEL) {
      this._logToOTEL(validated, error);
    }

    // Check alert thresholds
    this._checkAlertThresholds();

    return validated;
  }

  /**
   * Log to console with formatting
   * @param {Object} structuredError - Structured error
   * @private
   */
  _logToConsole(structuredError) {
    const prefix = `[${structuredError.severity.toUpperCase()}] [${structuredError.category}]`;
    const agentInfo = structuredError.agentId
      ? ` [Agent: ${structuredError.agentId}]`
      : '';
    const operationInfo = structuredError.operation
      ? ` [Op: ${structuredError.operation}]`
      : '';

    const message = `${prefix}${agentInfo}${operationInfo} ${structuredError.message}`;

    switch (structuredError.severity) {
      case ErrorSeverity.DEBUG:
        console.debug(message, structuredError.context);
        break;
      case ErrorSeverity.INFO:
        console.info(message, structuredError.context);
        break;
      case ErrorSeverity.WARN:
        console.warn(message, structuredError.context);
        break;
      case ErrorSeverity.ERROR:
      case ErrorSeverity.FATAL:
        console.error(message, structuredError.context);
        if (structuredError.stack) {
          console.error(structuredError.stack);
        }
        break;
    }
  }

  /**
   * Log to OTEL
   * @param {Object} structuredError - Structured error
   * @param {Error} originalError - Original error object
   * @private
   */
  _logToOTEL(structuredError, originalError) {
    const span = this.tracer.startSpan('error.logged');

    span.setAttributes({
      'error.id': structuredError.id,
      'error.category': structuredError.category,
      'error.severity': structuredError.severity,
      'error.message': structuredError.message,
      'error.code': structuredError.code,
      'error.agent_id': structuredError.agentId || 'unknown',
      'error.agent_type': structuredError.agentType || 'unknown',
      'error.operation': structuredError.operation || 'unknown',
    });

    if (originalError) {
      span.recordException(originalError);
    }

    span.setStatus({
      code:
        structuredError.severity === ErrorSeverity.FATAL
          ? SpanStatusCode.ERROR
          : SpanStatusCode.OK,
      message: structuredError.message,
    });

    span.end();
  }

  /**
   * Categorize error automatically
   * @param {Error} error - Error to categorize
   * @returns {string} Error category
   * @private
   */
  _categorizeError(error) {
    const message = (error.message || '').toLowerCase();
    const code = (error.code || '').toLowerCase();

    // Check for transient patterns
    const transientPatterns = [
      'timeout',
      'etimedout',
      'econnreset',
      'enotfound',
      'network',
      'unavailable',
      '503',
      '429',
    ];
    if (
      transientPatterns.some((p) => message.includes(p) || code.includes(p))
    ) {
      return ErrorCategory.TRANSIENT;
    }

    // Check for permanent patterns
    const permanentPatterns = [
      'not found',
      '404',
      'bad request',
      '400',
      'validation',
      'invalid',
      'schema',
    ];
    if (
      permanentPatterns.some((p) => message.includes(p) || code.includes(p))
    ) {
      return ErrorCategory.PERMANENT;
    }

    // Check for security patterns
    const securityPatterns = [
      'unauthorized',
      '401',
      'forbidden',
      '403',
      'authentication',
      'authorization',
      'permission',
    ];
    if (
      securityPatterns.some((p) => message.includes(p) || code.includes(p))
    ) {
      return ErrorCategory.SECURITY;
    }

    // Check for infrastructure patterns
    const infraPatterns = [
      'database',
      'redis',
      'cache',
      'connection',
      'econnrefused',
    ];
    if (infraPatterns.some((p) => message.includes(p) || code.includes(p))) {
      return ErrorCategory.INFRASTRUCTURE;
    }

    return ErrorCategory.UNKNOWN;
  }

  /**
   * Determine error severity
   * @param {Error} error - Error object
   * @param {string} category - Error category
   * @returns {string} Severity level
   * @private
   */
  _determineSeverity(error, category) {
    // Fatal if explicit
    if (error.fatal || error.name === 'FatalError') {
      return ErrorSeverity.FATAL;
    }

    // Security errors are always ERROR or higher
    if (category === ErrorCategory.SECURITY) {
      return ErrorSeverity.ERROR;
    }

    // Transient errors are usually WARN
    if (category === ErrorCategory.TRANSIENT) {
      return ErrorSeverity.WARN;
    }

    // Permanent errors are ERROR
    if (category === ErrorCategory.PERMANENT) {
      return ErrorSeverity.ERROR;
    }

    // Default to ERROR
    return ErrorSeverity.ERROR;
  }

  /**
   * Cleanup old logs
   * @private
   */
  _cleanupOldLogs() {
    if (this.errorLog.length > this.config.maxLogSize) {
      this.errorLog = this.errorLog.slice(-this.config.maxLogSize);
    }
  }

  /**
   * Check alert thresholds
   * @private
   */
  _checkAlertThresholds() {
    const now = Date.now();
    const windowMs = this.config.alertThresholds.errorWindow;

    // Calculate recent error rate
    const recentErrors = this.errorLog.filter(
      (e) => now - new Date(e.timestamp).getTime() < windowMs
    );

    const recentFatal = recentErrors.filter(
      (e) => e.severity === ErrorSeverity.FATAL
    ).length;

    this.metrics.recentErrorRate = recentErrors.length / (windowMs / 1000); // Errors per second

    // Check fatal count threshold
    if (recentFatal >= this.config.alertThresholds.fatalCount) {
      this._triggerAlert('FATAL_ERROR_THRESHOLD', {
        fatalCount: recentFatal,
        threshold: this.config.alertThresholds.fatalCount,
      });
    }

    // Check error rate threshold
    const totalOperations = this.metrics.totalErrors; // Simplified
    const errorRate =
      totalOperations > 0
        ? this.metrics.totalErrors / totalOperations
        : 0;

    if (errorRate >= this.config.alertThresholds.errorRate) {
      this._triggerAlert('ERROR_RATE_THRESHOLD', {
        errorRate: (errorRate * 100).toFixed(2) + '%',
        threshold:
          (this.config.alertThresholds.errorRate * 100).toFixed(2) + '%',
      });
    }
  }

  /**
   * Trigger alert
   * @param {string} alertType - Alert type
   * @param {Object} data - Alert data
   * @private
   */
  _triggerAlert(alertType, data) {
    const now = Date.now();

    // Rate limit alerts (1 per minute)
    if (
      this.metrics.lastAlert &&
      now - this.metrics.lastAlert < 60000
    ) {
      return;
    }

    this.metrics.lastAlert = now;

    console.error(
      `[ALERT] ${alertType}:`,
      JSON.stringify(data, null, 2)
    );

    // Could integrate with external alerting systems here
  }

  /**
   * Get errors by category
   * @param {string} category - Error category
   * @param {Object} [options] - Query options
   * @returns {Array<Object>} Errors
   */
  getErrorsByCategory(category, options = {}) {
    const { limit = 100, since = null } = options;

    let errors = this.errorLog.filter((e) => e.category === category);

    if (since) {
      const sinceTime = new Date(since).getTime();
      errors = errors.filter(
        (e) => new Date(e.timestamp).getTime() >= sinceTime
      );
    }

    return errors.slice(-limit);
  }

  /**
   * Get errors by severity
   * @param {string} severity - Error severity
   * @param {Object} [options] - Query options
   * @returns {Array<Object>} Errors
   */
  getErrorsBySeverity(severity, options = {}) {
    const { limit = 100, since = null } = options;

    let errors = this.errorLog.filter((e) => e.severity === severity);

    if (since) {
      const sinceTime = new Date(since).getTime();
      errors = errors.filter(
        (e) => new Date(e.timestamp).getTime() >= sinceTime
      );
    }

    return errors.slice(-limit);
  }

  /**
   * Get errors by agent
   * @param {string} agentId - Agent ID
   * @param {Object} [options] - Query options
   * @returns {Array<Object>} Errors
   */
  getErrorsByAgent(agentId, options = {}) {
    const { limit = 100, since = null } = options;

    let errors = this.errorLog.filter((e) => e.agentId === agentId);

    if (since) {
      const sinceTime = new Date(since).getTime();
      errors = errors.filter(
        (e) => new Date(e.timestamp).getTime() >= sinceTime
      );
    }

    return errors.slice(-limit);
  }

  /**
   * Get error metrics
   * @returns {Object} Metrics
   */
  getMetrics() {
    return {
      ...this.metrics,
      errorLog: {
        size: this.errorLog.length,
        maxSize: this.config.maxLogSize,
      },
    };
  }

  /**
   * Reset metrics
   */
  resetMetrics() {
    this.metrics = {
      totalErrors: 0,
      byCategory: {
        transient: 0,
        permanent: 0,
        infrastructure: 0,
        business_logic: 0,
        security: 0,
        unknown: 0,
      },
      bySeverity: {
        debug: 0,
        info: 0,
        warn: 0,
        error: 0,
        fatal: 0,
      },
      recentErrorRate: 0,
      lastAlert: null,
    };
  }

  /**
   * Clear error log
   */
  clearLog() {
    this.errorLog = [];
  }
}

/**
 * Default global error logger
 */
export const defaultErrorLogger = new StructuredErrorLogger();

/**
 * Convenience function to log error
 * @param {Error} error - Error to log
 * @param {Object} [options] - Logging options
 * @returns {Object} Logged error record
 */
export function logError(error, options = {}) {
  return defaultErrorLogger.logError(error, options);
}
