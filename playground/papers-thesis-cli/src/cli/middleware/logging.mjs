/**
 * @fileoverview Logging Middleware
 *
 * @description
 * Provides structured logging for CLI commands in JSON-LD format.
 * Logs command execution, arguments, results, and errors.
 * Respects --quiet flag for suppressing output.
 *
 * @module cli/middleware/logging
 * @version 1.0.0
 * @license MIT
 */

/**
 * @typedef {Object} LogEntry
 * @property {string} @type - JSON-LD type
 * @property {string} @context - JSON-LD context URL
 * @property {string} timestamp - ISO 8601 timestamp
 * @property {string} level - Log level (debug, info, warn, error)
 * @property {string} command - Command being executed
 * @property {Object} data - Additional log data
 */

/**
 * Sensitive argument patterns to mask
 * @type {Array<RegExp>}
 */
const SENSITIVE_PATTERNS = [
  /password/i,
  /secret/i,
  /token/i,
  /key/i,
  /auth/i,
  /credential/i,
  /api[_-]?key/i
];

/**
 * JSON-LD context for log entries
 */
const LOG_CONTEXT = {
  '@vocab': 'http://schema.org/',
  'log': 'http://purl.org/logging#',
  'cli': 'http://unrdf.io/cli#'
};

/**
 * Log levels with numeric priority
 * @type {Object<string, number>}
 */
const LOG_LEVELS = {
  debug: 0,
  info: 1,
  warn: 2,
  error: 3
};

/**
 * Current log level (configurable via environment or config)
 * @type {string}
 */
let currentLogLevel = process.env.LOG_LEVEL || 'info';

/**
 * Log output handlers
 * @type {Array<Function>}
 */
const logHandlers = [defaultConsoleHandler];

/**
 * Set the current log level
 * @param {string} level - Log level to set
 */
export function setLogLevel(level) {
  if (!LOG_LEVELS.hasOwnProperty(level)) {
    throw new Error(`Invalid log level: ${level}. Valid levels: ${Object.keys(LOG_LEVELS).join(', ')}`);
  }
  currentLogLevel = level;
}

/**
 * Check if a log level should be output
 * @param {string} level - Level to check
 * @returns {boolean}
 */
function shouldLog(level) {
  return LOG_LEVELS[level] >= LOG_LEVELS[currentLogLevel];
}

/**
 * Mask sensitive values in arguments
 * @param {Object} args - Arguments to mask
 * @returns {Object} Masked arguments
 */
export function maskSensitiveArgs(args) {
  const masked = { ...args };

  for (const key of Object.keys(masked)) {
    if (SENSITIVE_PATTERNS.some(pattern => pattern.test(key))) {
      masked[key] = '***MASKED***';
    }
  }

  return masked;
}

/**
 * Default console log handler
 * @param {LogEntry} entry - Log entry
 * @param {Object} context - Middleware context
 */
function defaultConsoleHandler(entry, context) {
  if (context.quiet) return;

  const prefix = {
    debug: '\x1b[90m[DEBUG]\x1b[0m',
    info: '\x1b[34m[INFO]\x1b[0m',
    warn: '\x1b[33m[WARN]\x1b[0m',
    error: '\x1b[31m[ERROR]\x1b[0m'
  }[entry.level] || '[LOG]';

  if (context.verbose || entry.level === 'error') {
    console.log(`${prefix} ${entry.timestamp} ${entry.message || JSON.stringify(entry.data)}`);
  } else if (entry.level !== 'debug') {
    console.log(`${prefix} ${entry.message || ''}`);
  }
}

/**
 * Add a custom log handler
 * @param {Function} handler - Handler function
 */
export function addLogHandler(handler) {
  if (typeof handler !== 'function') {
    throw new Error('Log handler must be a function');
  }
  logHandlers.push(handler);
}

/**
 * Remove a log handler
 * @param {Function} handler - Handler to remove
 * @returns {boolean} True if handler was removed
 */
export function removeLogHandler(handler) {
  const index = logHandlers.indexOf(handler);
  if (index > -1) {
    logHandlers.splice(index, 1);
    return true;
  }
  return false;
}

/**
 * Create a JSON-LD formatted log entry
 * @param {string} level - Log level
 * @param {string} message - Log message
 * @param {Object} data - Additional data
 * @param {Object} context - Middleware context
 * @returns {LogEntry}
 */
export function createLogEntry(level, message, data, context) {
  return {
    '@context': LOG_CONTEXT,
    '@type': 'log:LogEntry',
    'log:timestamp': new Date().toISOString(),
    'log:level': level,
    'log:message': message,
    'cli:command': context.command,
    'cli:phase': context.meta?.phase || 'unknown',
    'log:data': data,
    // Shorthand properties for easier access
    timestamp: new Date().toISOString(),
    level,
    message,
    command: context.command,
    data
  };
}

/**
 * Log a message
 * @param {string} level - Log level
 * @param {string} message - Message
 * @param {Object} data - Additional data
 * @param {Object} context - Middleware context
 */
export function log(level, message, data, context) {
  if (!shouldLog(level)) return;

  const entry = createLogEntry(level, message, data, context);

  // Store in context log history
  if (!context._logs) {
    context._logs = [];
  }
  context._logs.push(entry);

  // Call all handlers
  for (const handler of logHandlers) {
    try {
      handler(entry, context);
    } catch (err) {
      console.error('Log handler error:', err.message);
    }
  }
}

/**
 * Convenience logging functions
 */
export const debug = (msg, data, ctx) => log('debug', msg, data, ctx);
export const info = (msg, data, ctx) => log('info', msg, data, ctx);
export const warn = (msg, data, ctx) => log('warn', msg, data, ctx);
export const error = (msg, data, ctx) => log('error', msg, data, ctx);

/**
 * Logging middleware handler
 * @param {Object} context - Middleware context
 * @returns {Promise<Object>} Modified context
 */
export async function loggingMiddleware(context) {
  // Set log level from config if available
  if (context.config?.logging?.level) {
    setLogLevel(context.config.logging.level);
  }

  // Log command start
  log('info', `Starting command: ${context.command}`, {
    args: maskSensitiveArgs(context.args),
    phase: context.meta.phase
  }, context);

  // Log arguments in verbose mode
  if (context.verbose) {
    log('debug', 'Command arguments', {
      args: maskSensitiveArgs(context.args),
      options: context.options
    }, context);
  }

  // Add logging utilities to context
  context.log = {
    debug: (msg, data) => debug(msg, data, context),
    info: (msg, data) => info(msg, data, context),
    warn: (msg, data) => warn(msg, data, context),
    error: (msg, data) => error(msg, data, context)
  };

  // Add cleanup handler
  context._cleanup = context._cleanup || [];
  context._cleanup.push(() => {
    // Log command completion
    const duration = context.timing?.totalDuration
      ? `${context.timing.totalDuration.toFixed(2)}ms`
      : 'N/A';

    if (context.errors.length > 0) {
      log('error', `Command failed: ${context.command}`, {
        errors: context.errors,
        duration
      }, context);
    } else {
      log('info', `Command completed: ${context.command}`, {
        duration,
        success: true
      }, context);
    }
  });

  return context;
}

/**
 * Get all logs from context
 * @param {Object} context - Middleware context
 * @returns {Array<LogEntry>}
 */
export function getLogs(context) {
  return context._logs || [];
}

/**
 * Format logs as JSON-LD document
 * @param {Object} context - Middleware context
 * @returns {Object} JSON-LD document
 */
export function formatLogsAsJsonLd(context) {
  return {
    '@context': LOG_CONTEXT,
    '@type': 'log:LogCollection',
    'log:entries': getLogs(context),
    'log:command': context.command,
    'log:startTime': new Date(context.meta?.startTime).toISOString(),
    'log:endTime': new Date().toISOString()
  };
}

export default loggingMiddleware;
