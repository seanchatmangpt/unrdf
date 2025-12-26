/**
 * @file Structured Logger
 * @module @unrdf/core/logger
 *
 * @description
 * Production-grade structured JSON logger with performance tracking,
 * context injection, and integration with OTEL traces.
 *
 * @example
 * ```javascript
 * import { createLogger } from '@unrdf/core/logger';
 *
 * const logger = createLogger({
 *   service: 'unrdf-api',
 *   level: 'info',
 *   context: { requestId: '123' }
 * });
 *
 * logger.info('User action', { userId: '456', action: 'login' });
 * logger.error('Database error', { error: err, query: sql });
 * logger.performance('Query execution', { duration: 150, rows: 100 });
 * ```
 */

import { z } from 'zod';

/**
 * Log levels (RFC 5424 syslog levels)
 * @enum {number}
 */
export const LogLevel = {
  TRACE: 10,
  DEBUG: 20,
  INFO: 30,
  WARN: 40,
  ERROR: 50,
  FATAL: 60
};

/**
 * Log level names
 */
const LogLevelNames = {
  [LogLevel.TRACE]: 'trace',
  [LogLevel.DEBUG]: 'debug',
  [LogLevel.INFO]: 'info',
  [LogLevel.WARN]: 'warn',
  [LogLevel.ERROR]: 'error',
  [LogLevel.FATAL]: 'fatal'
};

/**
 * Logger configuration schema
 */
const LoggerConfigSchema = z.object({
  service: z.string(),
  level: z.enum(['trace', 'debug', 'info', 'warn', 'error', 'fatal']).default('info'),
  context: z.record(z.string(), z.any()).default({}),
  pretty: z.boolean().default(false),
  destination: z.any()
});

/**
 * Create structured logger instance
 *
 * @param {Object} config - Logger configuration
 * @param {string} config.service - Service name
 * @param {string} [config.level='info'] - Minimum log level
 * @param {Object} [config.context={}] - Default context to include in all logs
 * @param {boolean} [config.pretty=false] - Pretty print for development
 * @param {Object} [config.destination=process.stdout] - Output destination
 * @returns {Object} Logger instance
 */
export function createLogger(config) {
  const validated = LoggerConfigSchema.parse(config);
  const minLevel = LogLevel[validated.level.toUpperCase()];
  const destination = validated.destination || process.stdout;

  /**
   * Format and write log entry
   *
   * @param {number} level - Log level
   * @param {string} message - Log message
   * @param {Object} data - Additional log data
   * @param {Error} [error] - Error object
   */
  function writeLog(level, message, data = {}, error = null) {
    if (level < minLevel) return;

    const entry = {
      timestamp: new Date().toISOString(),
      level: LogLevelNames[level],
      service: validated.service,
      message,
      ...validated.context,
      ...data
    };

    // Add OTEL trace context if available
    try {
      const { trace, context } = require('@opentelemetry/api');
      const span = trace.getSpan(context.active());
      if (span) {
        const spanContext = span.spanContext();
        entry.traceId = spanContext.traceId;
        entry.spanId = spanContext.spanId;
      }
    } catch {
      // OTEL not available, skip trace context
    }

    // Add error details if present
    if (error instanceof Error) {
      entry.error = {
        name: error.name,
        message: error.message,
        stack: error.stack,
        ...(error.cause ? { cause: error.cause } : {})
      };
    }

    // Write to destination
    const output = validated.pretty
      ? JSON.stringify(entry, null, 2)
      : JSON.stringify(entry);

    destination.write(output + '\n');
  }

  /**
   * Create child logger with additional context
   *
   * @param {Object} childContext - Additional context for child logger
   * @returns {Object} Child logger instance
   */
  function child(childContext) {
    return createLogger({
      ...validated,
      context: { ...validated.context, ...childContext }
    });
  }

  return {
    /**
     * Trace level log (level 10)
     * Use for very detailed debugging information
     */
    trace: (message, data) => writeLog(LogLevel.TRACE, message, data),

    /**
     * Debug level log (level 20)
     * Use for debugging information
     */
    debug: (message, data) => writeLog(LogLevel.DEBUG, message, data),

    /**
     * Info level log (level 30)
     * Use for general informational messages
     */
    info: (message, data) => writeLog(LogLevel.INFO, message, data),

    /**
     * Warn level log (level 40)
     * Use for warning messages
     */
    warn: (message, data) => writeLog(LogLevel.WARN, message, data),

    /**
     * Error level log (level 50)
     * Use for error messages
     */
    error: (message, data, error) => writeLog(LogLevel.ERROR, message, data, error),

    /**
     * Fatal level log (level 60)
     * Use for fatal error messages that require immediate attention
     */
    fatal: (message, data, error) => writeLog(LogLevel.FATAL, message, data, error),

    /**
     * Performance log - specialized for performance metrics
     * Always logs at INFO level with performance marker
     *
     * @param {string} operation - Operation name
     * @param {Object} metrics - Performance metrics
     */
    performance: (operation, metrics) => {
      writeLog(LogLevel.INFO, `Performance: ${operation}`, {
        type: 'performance',
        operation,
        ...metrics
      });
    },

    /**
     * Slow query detection
     * Logs warning if duration exceeds threshold
     *
     * @param {string} query - Query or operation name
     * @param {number} duration - Duration in milliseconds
     * @param {number} [threshold=100] - Threshold in milliseconds
     * @param {Object} [data={}] - Additional data
     */
    slowQuery: (query, duration, threshold = 100, data = {}) => {
      if (duration > threshold) {
        writeLog(LogLevel.WARN, `Slow query detected: ${query}`, {
          type: 'slow_query',
          query,
          duration,
          threshold,
          ...data
        });
      }
    },

    /**
     * Create child logger with additional context
     */
    child,

    /**
     * Get current log level
     */
    getLevel: () => validated.level,

    /**
     * Set log level
     */
    setLevel: (level) => {
      validated.level = level;
    }
  };
}

/**
 * Default logger for UNRDF
 */
export const logger = createLogger({
  service: 'unrdf',
  level: process.env.LOG_LEVEL || 'info',
  pretty: process.env.NODE_ENV !== 'production'
});

/**
 * Performance timer utility
 *
 * @example
 * ```javascript
 * const timer = performanceTimer();
 * await doWork();
 * logger.performance('Work completed', timer.end());
 * ```
 *
 * @returns {Object} Timer with end() method
 */
export function performanceTimer() {
  const start = process.hrtime.bigint();

  return {
    /**
     * End timer and return metrics
     * @returns {Object} Performance metrics
     */
    end: () => {
      const end = process.hrtime.bigint();
      const duration = Number(end - start) / 1000000; // Convert to milliseconds

      return {
        duration: Math.round(duration * 100) / 100,
        timestamp: new Date().toISOString()
      };
    },

    /**
     * Get current duration without ending timer
     * @returns {number} Current duration in milliseconds
     */
    elapsed: () => {
      const now = process.hrtime.bigint();
      return Math.round(Number(now - start) / 1000000 * 100) / 100;
    }
  };
}

/**
 * Express/Fastify middleware for request logging
 *
 * @param {Object} options - Middleware options
 * @param {Object} [options.logger] - Logger instance
 * @param {boolean} [options.logBody=false] - Log request/response bodies
 * @returns {Function} Middleware function
 */
export function requestLogger(options = {}) {
  const loggerInstance = options.logger || logger;
  const logBody = options.logBody || false;

  return (req, res, next) => {
    const timer = performanceTimer();
    const requestId = req.headers['x-request-id'] ||
                      req.id ||
                      `req_${Date.now()}_${Math.random().toString(36).slice(2)}`;

    // Create request-scoped logger
    req.log = loggerInstance.child({ requestId });

    // Log request
    req.log.info('Request started', {
      method: req.method,
      path: req.path || req.url,
      query: req.query,
      ip: req.ip || req.socket?.remoteAddress,
      userAgent: req.headers['user-agent'],
      ...(logBody && req.body ? { body: req.body } : {})
    });

    // Hook into response finish
    const originalEnd = res.end;
    res.end = function(...args) {
      const metrics = timer.end();

      req.log.info('Request completed', {
        method: req.method,
        path: req.path || req.url,
        statusCode: res.statusCode,
        duration: metrics.duration
      });

      // Check for slow requests (>100ms)
      if (metrics.duration > 100) {
        req.log.slowQuery(
          `${req.method} ${req.path || req.url}`,
          metrics.duration
        );
      }

      return originalEnd.apply(this, args);
    };

    next();
  };
}
