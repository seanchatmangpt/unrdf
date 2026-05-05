/**
 * @fileoverview Logging Schemas
 * Structured logging for migration operations
 */

/**
 * Log levels
 * @enum {string}
 */
export const LogLevel = {
  DEBUG: 'DEBUG',
  INFO: 'INFO',
  WARN: 'WARN',
  ERROR: 'ERROR',
  FATAL: 'FATAL',
};

/**
 * Migration phases
 * @enum {string}
 */
export const MigrationPhase = {
  INITIALIZATION: 'INITIALIZATION',
  VALIDATION: 'VALIDATION',
  LENS_APPLICATION: 'LENS_APPLICATION',
  CAPSULE_PROCESSING: 'CAPSULE_PROCESSING',
  RECEIPT_GENERATION: 'RECEIPT_GENERATION',
  SHADOW_VERIFICATION: 'SHADOW_VERIFICATION',
  FINALIZATION: 'FINALIZATION',
};

/**
 * @typedef {object} LogEntry
 * @property {number} timestamp - Unix timestamp in milliseconds
 * @property {LogLevel} level - Log level
 * @property {string} correlationId - Correlation ID for request tracing
 * @property {string} agentId - Agent identifier (e.g., 'agent-1', 'agent-2')
 * @property {MigrationPhase} phase - Current migration phase
 * @property {string} message - Log message
 * @property {number} [duration] - Operation duration in milliseconds
 * @property {object} [metrics] - Performance metrics
 * @property {object} [context] - Additional context data
 */

/**
 * @typedef {object} PerformanceMetrics
 * @property {number} [throughput] - Items per second
 * @property {number} [latencyMs] - Operation latency
 * @property {number} [memoryUsedMb] - Memory usage in MB
 * @property {number} [cpuPercent] - CPU usage percentage
 * @property {number} [errorRate] - Error rate (0-1)
 */

/**
 * Create structured log entry
 * @param {object} params - Log parameters
 * @param {LogLevel} params.level - Log level
 * @param {string} params.correlationId - Correlation ID
 * @param {string} params.agentId - Agent ID
 * @param {MigrationPhase} params.phase - Migration phase
 * @param {string} params.message - Log message
 * @param {number} [params.duration] - Duration in ms
 * @param {PerformanceMetrics} [params.metrics] - Performance metrics
 * @param {object} [params.context] - Additional context
 * @returns {LogEntry} Structured log entry
 */
export function createLogEntry(params) {
  return {
    timestamp: Date.now(),
    level: params.level,
    correlationId: params.correlationId,
    agentId: params.agentId,
    phase: params.phase,
    message: params.message,
    ...(params.duration !== undefined && { duration: params.duration }),
    ...(params.metrics && { metrics: params.metrics }),
    ...(params.context && { context: params.context }),
  };
}

/**
 * Create DEBUG log entry
 * @param {string} correlationId - Correlation ID
 * @param {string} agentId - Agent ID
 * @param {MigrationPhase} phase - Migration phase
 * @param {string} message - Log message
 * @param {object} [context] - Additional context
 * @returns {LogEntry} Debug log entry
 */
export function debug(correlationId, agentId, phase, message, context) {
  return createLogEntry({
    level: LogLevel.DEBUG,
    correlationId,
    agentId,
    phase,
    message,
    context,
  });
}

/**
 * Create INFO log entry
 * @param {string} correlationId - Correlation ID
 * @param {string} agentId - Agent ID
 * @param {MigrationPhase} phase - Migration phase
 * @param {string} message - Log message
 * @param {object} [context] - Additional context
 * @returns {LogEntry} Info log entry
 */
export function info(correlationId, agentId, phase, message, context) {
  return createLogEntry({
    level: LogLevel.INFO,
    correlationId,
    agentId,
    phase,
    message,
    context,
  });
}

/**
 * Create WARN log entry
 * @param {string} correlationId - Correlation ID
 * @param {string} agentId - Agent ID
 * @param {MigrationPhase} phase - Migration phase
 * @param {string} message - Log message
 * @param {object} [context] - Additional context
 * @returns {LogEntry} Warning log entry
 */
export function warn(correlationId, agentId, phase, message, context) {
  return createLogEntry({
    level: LogLevel.WARN,
    correlationId,
    agentId,
    phase,
    message,
    context,
  });
}

/**
 * Create ERROR log entry
 * @param {string} correlationId - Correlation ID
 * @param {string} agentId - Agent ID
 * @param {MigrationPhase} phase - Migration phase
 * @param {string} message - Log message
 * @param {object} [context] - Additional context
 * @returns {LogEntry} Error log entry
 */
export function error(correlationId, agentId, phase, message, context) {
  return createLogEntry({
    level: LogLevel.ERROR,
    correlationId,
    agentId,
    phase,
    message,
    context,
  });
}

/**
 * Create FATAL log entry
 * @param {string} correlationId - Correlation ID
 * @param {string} agentId - Agent ID
 * @param {MigrationPhase} phase - Migration phase
 * @param {string} message - Log message
 * @param {object} [context] - Additional context
 * @returns {LogEntry} Fatal log entry
 */
export function fatal(correlationId, agentId, phase, message, context) {
  return createLogEntry({
    level: LogLevel.FATAL,
    correlationId,
    agentId,
    phase,
    message,
    context,
  });
}

/**
 * Create performance log entry with metrics
 * @param {string} correlationId - Correlation ID
 * @param {string} agentId - Agent ID
 * @param {MigrationPhase} phase - Migration phase
 * @param {string} message - Log message
 * @param {number} duration - Duration in ms
 * @param {PerformanceMetrics} metrics - Performance metrics
 * @returns {LogEntry} Performance log entry
 */
export function performance(correlationId, agentId, phase, message, duration, metrics) {
  return createLogEntry({
    level: LogLevel.INFO,
    correlationId,
    agentId,
    phase,
    message,
    duration,
    metrics,
  });
}

/**
 * Format log entry for console output
 * @param {LogEntry} entry - Log entry
 * @returns {string} Formatted log string
 */
export function formatLogEntry(entry) {
  const timestamp = new Date(entry.timestamp).toISOString();
  const parts = [
    `[${timestamp}]`,
    `[${entry.level}]`,
    `[${entry.correlationId}]`,
    `[${entry.agentId}]`,
    `[${entry.phase}]`,
    entry.message,
  ];

  if (entry.duration !== undefined) {
    parts.push(`(${entry.duration}ms)`);
  }

  return parts.join(' ');
}

/**
 * Format log entry as JSON string
 * @param {LogEntry} entry - Log entry
 * @returns {string} JSON string
 */
export function formatLogEntryJSON(entry) {
  return JSON.stringify(entry);
}

/**
 * Validate log entry structure
 * @param {unknown} obj - Object to validate
 * @returns {{valid: boolean, errors: string[]}} Validation result
 */
export function validateLogEntry(obj) {
  const errors = [];

  if (!obj || typeof obj !== 'object') {
    return { valid: false, errors: ['Log entry must be an object'] };
  }

  if (typeof obj.timestamp !== 'number' || obj.timestamp <= 0) {
    errors.push('timestamp must be a positive number');
  }

  if (!Object.values(LogLevel).includes(obj.level)) {
    errors.push(`level must be one of: ${Object.values(LogLevel).join(', ')}`);
  }

  if (typeof obj.correlationId !== 'string' || obj.correlationId.length === 0) {
    errors.push('correlationId must be a non-empty string');
  }

  if (typeof obj.agentId !== 'string' || obj.agentId.length === 0) {
    errors.push('agentId must be a non-empty string');
  }

  if (!Object.values(MigrationPhase).includes(obj.phase)) {
    errors.push(`phase must be one of: ${Object.values(MigrationPhase).join(', ')}`);
  }

  if (typeof obj.message !== 'string' || obj.message.length === 0) {
    errors.push('message must be a non-empty string');
  }

  if (obj.duration !== undefined && (typeof obj.duration !== 'number' || obj.duration < 0)) {
    errors.push('duration must be a non-negative number if provided');
  }

  return { valid: errors.length === 0, errors };
}

/**
 * Create logger instance for specific agent
 * @param {string} agentId - Agent identifier
 * @returns {object} Logger instance with helper methods
 */
export function createLogger(agentId) {
  return {
    /**
     * Log debug message
     * @param {string} correlationId - Correlation ID
     * @param {MigrationPhase} phase - Migration phase
     * @param {string} message - Log message
     * @param {object} [context] - Additional context
     */
    debug: (correlationId, phase, message, context) =>
      debug(correlationId, agentId, phase, message, context),

    /**
     * Log info message
     * @param {string} correlationId - Correlation ID
     * @param {MigrationPhase} phase - Migration phase
     * @param {string} message - Log message
     * @param {object} [context] - Additional context
     */
    info: (correlationId, phase, message, context) =>
      info(correlationId, agentId, phase, message, context),

    /**
     * Log warning message
     * @param {string} correlationId - Correlation ID
     * @param {MigrationPhase} phase - Migration phase
     * @param {string} message - Log message
     * @param {object} [context] - Additional context
     */
    warn: (correlationId, phase, message, context) =>
      warn(correlationId, agentId, phase, message, context),

    /**
     * Log error message
     * @param {string} correlationId - Correlation ID
     * @param {MigrationPhase} phase - Migration phase
     * @param {string} message - Log message
     * @param {object} [context] - Additional context
     */
    error: (correlationId, phase, message, context) =>
      error(correlationId, agentId, phase, message, context),

    /**
     * Log fatal message
     * @param {string} correlationId - Correlation ID
     * @param {MigrationPhase} phase - Migration phase
     * @param {string} message - Log message
     * @param {object} [context] - Additional context
     */
    fatal: (correlationId, phase, message, context) =>
      fatal(correlationId, agentId, phase, message, context),

    /**
     * Log performance message
     * @param {string} correlationId - Correlation ID
     * @param {MigrationPhase} phase - Migration phase
     * @param {string} message - Log message
     * @param {number} duration - Duration in ms
     * @param {PerformanceMetrics} metrics - Performance metrics
     */
    performance: (correlationId, phase, message, duration, metrics) =>
      performance(correlationId, agentId, phase, message, duration, metrics),
  };
}

/**
 * Aggregate log entries by phase
 * @param {LogEntry[]} entries - Log entries
 * @returns {Map<MigrationPhase, LogEntry[]>} Entries grouped by phase
 */
export function aggregateByPhase(entries) {
  const map = new Map();

  for (const entry of entries) {
    if (!map.has(entry.phase)) {
      map.set(entry.phase, []);
    }
    map.get(entry.phase).push(entry);
  }

  return map;
}

/**
 * Calculate phase statistics from log entries
 * @param {LogEntry[]} entries - Log entries for a phase
 * @returns {object} Phase statistics
 */
export function calculatePhaseStats(entries) {
  const durations = entries.filter(e => e.duration !== undefined).map(e => e.duration);
  const errors = entries.filter(e => e.level === LogLevel.ERROR || e.level === LogLevel.FATAL);

  return {
    totalEntries: entries.length,
    errorCount: errors.length,
    avgDuration: durations.length > 0
      ? durations.reduce((a, b) => a + b, 0) / durations.length
      : null,
    minDuration: durations.length > 0 ? Math.min(...durations) : null,
    maxDuration: durations.length > 0 ? Math.max(...durations) : null,
  };
}
