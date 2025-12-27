/**
 * @fileoverview KGC Probe - Structured Logger
 *
 * Simple, structured logging utility for observability.
 * No external dependencies - outputs JSON for log aggregation.
 *
 * @module @unrdf/kgc-probe/utils/logger
 */

/**
 * Log levels in order of severity
 * @type {Object<string, number>}
 */
const LOG_LEVELS = {
  debug: 0,
  info: 1,
  warn: 2,
  error: 3,
  silent: 4
};

/**
 * Logger configuration
 * @typedef {Object} LoggerConfig
 * @property {string} [level='info'] - Minimum log level
 * @property {boolean} [json=true] - Output JSON format
 * @property {string} [prefix=''] - Log prefix
 * @property {boolean} [timestamps=true] - Include timestamps
 * @property {Function} [output] - Custom output function
 */

/**
 * Structured log entry
 * @typedef {Object} LogEntry
 * @property {string} level - Log level
 * @property {string} message - Log message
 * @property {string} [timestamp] - ISO timestamp
 * @property {Object} [context] - Additional context
 */

/**
 * Logger - Structured logging with JSON output
 *
 * @class Logger
 * @example
 * const logger = createLogger({ prefix: 'kgc-probe' });
 * logger.info('Scan started', { universe: 'my-universe' });
 * logger.error('Scan failed', { error: err.message });
 */
export class Logger {
  /**
   * Create logger instance
   * @param {LoggerConfig} [config] - Logger configuration
   */
  constructor(config = {}) {
    /** @type {string} */
    this.level = config.level || 'info';

    /** @type {boolean} */
    this.json = config.json !== false;

    /** @type {string} */
    this.prefix = config.prefix || '';

    /** @type {boolean} */
    this.timestamps = config.timestamps !== false;

    /** @type {Function} */
    this.output = config.output || console.log;

    /** @type {Function} */
    this.errorOutput = config.errorOutput || console.error;
  }

  /**
   * Check if level should be logged
   * @private
   * @param {string} level - Level to check
   * @returns {boolean}
   */
  shouldLog(level) {
    return LOG_LEVELS[level] >= LOG_LEVELS[this.level];
  }

  /**
   * Format log entry
   * @private
   * @param {string} level - Log level
   * @param {string} message - Log message
   * @param {Object} [context] - Additional context
   * @returns {string}
   */
  format(level, message, context) {
    /** @type {LogEntry} */
    const entry = {
      level,
      message: this.prefix ? `[${this.prefix}] ${message}` : message
    };

    if (this.timestamps) {
      entry.timestamp = new Date().toISOString();
    }

    if (context && Object.keys(context).length > 0) {
      entry.context = context;
    }

    if (this.json) {
      return JSON.stringify(entry);
    }

    // Human-readable format
    const ts = this.timestamps ? `[${entry.timestamp}] ` : '';
    const ctx = context ? ` ${JSON.stringify(context)}` : '';
    return `${ts}${level.toUpperCase()}: ${entry.message}${ctx}`;
  }

  /**
   * Log debug message
   * @param {string} message - Log message
   * @param {Object} [context] - Additional context
   */
  debug(message, context) {
    if (this.shouldLog('debug')) {
      this.output(this.format('debug', message, context));
    }
  }

  /**
   * Log info message
   * @param {string} message - Log message
   * @param {Object} [context] - Additional context
   */
  info(message, context) {
    if (this.shouldLog('info')) {
      this.output(this.format('info', message, context));
    }
  }

  /**
   * Log warning message
   * @param {string} message - Log message
   * @param {Object} [context] - Additional context
   */
  warn(message, context) {
    if (this.shouldLog('warn')) {
      this.errorOutput(this.format('warn', message, context));
    }
  }

  /**
   * Log error message
   * @param {string} message - Log message
   * @param {Object} [context] - Additional context
   */
  error(message, context) {
    if (this.shouldLog('error')) {
      this.errorOutput(this.format('error', message, context));
    }
  }

  /**
   * Create child logger with additional prefix
   * @param {string} childPrefix - Child prefix
   * @returns {Logger}
   */
  child(childPrefix) {
    const newPrefix = this.prefix
      ? `${this.prefix}:${childPrefix}`
      : childPrefix;

    return new Logger({
      level: this.level,
      json: this.json,
      prefix: newPrefix,
      timestamps: this.timestamps,
      output: this.output,
      errorOutput: this.errorOutput
    });
  }

  /**
   * Set log level
   * @param {string} level - New log level
   */
  setLevel(level) {
    if (LOG_LEVELS[level] === undefined) {
      throw new Error(`Invalid log level: ${level}`);
    }
    this.level = level;
  }

  /**
   * Log with timing
   * @param {string} message - Log message
   * @param {Function} fn - Function to time
   * @returns {Promise<any>} Function result
   */
  async timed(message, fn) {
    const start = Date.now();
    try {
      const result = await fn();
      const duration = Date.now() - start;
      this.info(message, { duration_ms: duration, status: 'success' });
      return result;
    } catch (err) {
      const duration = Date.now() - start;
      this.error(message, { duration_ms: duration, status: 'error', error: err.message });
      throw err;
    }
  }
}

/**
 * Create logger instance
 * @param {LoggerConfig} [config] - Logger configuration
 * @returns {Logger}
 * @example
 * const logger = createLogger({ prefix: 'my-module', level: 'debug' });
 * logger.info('Hello', { key: 'value' });
 */
export function createLogger(config) {
  return new Logger(config);
}

/**
 * Default logger instance
 * @type {Logger}
 */
export const defaultLogger = createLogger({ prefix: 'kgc-probe' });

/**
 * Log levels constant
 * @type {Object<string, number>}
 */
export { LOG_LEVELS };
