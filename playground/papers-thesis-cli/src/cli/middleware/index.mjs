/**
 * @fileoverview CLI Middleware Orchestrator
 *
 * @description
 * Main middleware orchestrator that applies all middleware in sequence.
 * Supports custom middleware registration and provides a pipeline pattern
 * for processing CLI context before and after command execution.
 *
 * @module cli/middleware
 * @version 1.0.0
 * @license MIT
 */

import { loggingMiddleware } from './logging.mjs';
import { profilingMiddleware } from './profiling.mjs';
import { validationMiddleware } from './validation.mjs';
import { configMiddleware } from './config.mjs';

/**
 * @typedef {Object} MiddlewareContext
 * @property {string} command - The command being executed
 * @property {Object} args - Parsed command arguments
 * @property {Object} options - Global options
 * @property {Object} config - Loaded configuration
 * @property {Object} meta - Metadata about the execution
 * @property {Object} timing - Timing information
 * @property {Array<Object>} errors - Accumulated errors
 * @property {Array<Object>} warnings - Accumulated warnings
 * @property {boolean} quiet - Whether to suppress output
 * @property {boolean} verbose - Whether to enable verbose output
 */

/**
 * @typedef {Function} MiddlewareHandler
 * @param {MiddlewareContext} context - The middleware context
 * @returns {Promise<MiddlewareContext>} Modified context
 */

/** @type {Map<string, MiddlewareHandler>} */
const customMiddleware = new Map();

/**
 * Default middleware stack in execution order
 * @type {Array<{name: string, handler: MiddlewareHandler, phase: 'pre'|'post'}>}
 */
const defaultMiddlewareStack = [
  { name: 'config', handler: configMiddleware, phase: 'pre' },
  { name: 'validation', handler: validationMiddleware, phase: 'pre' },
  { name: 'logging', handler: loggingMiddleware, phase: 'pre' },
  { name: 'profiling', handler: profilingMiddleware, phase: 'pre' },
];

/**
 * Create initial middleware context
 * @param {Object} options - Initial options
 * @returns {MiddlewareContext}
 */
export function createContext(options = {}) {
  return {
    command: options.command || '',
    args: options.args || {},
    options: options.options || {},
    config: {},
    meta: {
      startTime: Date.now(),
      phase: 'init',
      middlewareExecuted: [],
    },
    timing: {
      start: process.hrtime.bigint(),
      checkpoints: [],
    },
    errors: [],
    warnings: [],
    quiet: options.quiet || false,
    verbose: options.verbose || false,
    ...options,
  };
}

/**
 * Apply all middleware in sequence
 * @param {MiddlewareContext} context - CLI context
 * @param {Object} options - Middleware options
 * @param {Array<string>} [options.skip] - Middleware names to skip
 * @param {Array<string>} [options.only] - Only run these middleware
 * @param {string} [options.phase] - Only run middleware for this phase
 * @returns {Promise<MiddlewareContext>} Modified context
 */
export async function applyMiddleware(context, options = {}) {
  const { skip = [], only = [], phase = null } = options;

  // Build middleware stack
  let stack = [...defaultMiddlewareStack];

  // Add custom middleware
  for (const [name, handler] of customMiddleware) {
    stack.push({ name, handler, phase: 'custom' });
  }

  // Filter by phase if specified
  if (phase) {
    stack = stack.filter(m => m.phase === phase || m.phase === 'custom');
  }

  // Filter by skip/only
  if (only.length > 0) {
    stack = stack.filter(m => only.includes(m.name));
  } else if (skip.length > 0) {
    stack = stack.filter(m => !skip.includes(m.name));
  }

  // Execute middleware in sequence
  let currentContext = { ...context };

  for (const { name, handler } of stack) {
    try {
      currentContext.meta.phase = `middleware:${name}`;
      currentContext.meta.middlewareExecuted.push(name);

      // Add timing checkpoint
      currentContext.timing.checkpoints.push({
        name,
        start: process.hrtime.bigint(),
      });

      currentContext = await handler(currentContext);

      // Update checkpoint with end time
      const lastCheckpoint =
        currentContext.timing.checkpoints[currentContext.timing.checkpoints.length - 1];
      lastCheckpoint.end = process.hrtime.bigint();
      lastCheckpoint.duration = Number(lastCheckpoint.end - lastCheckpoint.start) / 1e6;
    } catch (error) {
      currentContext.errors.push({
        middleware: name,
        message: error.message,
        stack: error.stack,
        timestamp: new Date().toISOString(),
      });

      // Optionally continue or abort on error
      if (currentContext.config?.middleware?.abortOnError !== false) {
        throw error;
      }
    }
  }

  currentContext.meta.phase = 'complete';
  return currentContext;
}

/**
 * Register custom middleware
 * @param {string} name - Middleware name
 * @param {MiddlewareHandler} handler - Middleware handler function
 * @throws {Error} If middleware name already exists
 */
export function registerMiddleware(name, handler) {
  if (typeof name !== 'string' || !name.trim()) {
    throw new Error('Middleware name must be a non-empty string');
  }
  if (typeof handler !== 'function') {
    throw new Error('Middleware handler must be a function');
  }
  if (customMiddleware.has(name)) {
    throw new Error(`Middleware "${name}" is already registered`);
  }
  customMiddleware.set(name, handler);
}

/**
 * Unregister custom middleware
 * @param {string} name - Middleware name to remove
 * @returns {boolean} True if middleware was removed
 */
export function unregisterMiddleware(name) {
  return customMiddleware.delete(name);
}

/**
 * Get list of registered middleware
 * @returns {Array<{name: string, phase: string}>}
 */
export function listMiddleware() {
  const result = defaultMiddlewareStack.map(m => ({
    name: m.name,
    phase: m.phase,
    builtin: true,
  }));

  for (const name of customMiddleware.keys()) {
    result.push({ name, phase: 'custom', builtin: false });
  }

  return result;
}

/**
 * Create a middleware chain for manual execution
 * @param {Array<string|MiddlewareHandler>} middleware - Middleware names or handlers
 * @returns {Function} Composed middleware function
 */
export function compose(middleware) {
  return async function composed(context) {
    let currentContext = { ...context };

    for (const item of middleware) {
      const handler =
        typeof item === 'string'
          ? customMiddleware.get(item) || defaultMiddlewareStack.find(m => m.name === item)?.handler
          : item;

      if (!handler) {
        throw new Error(`Unknown middleware: ${item}`);
      }

      currentContext = await handler(currentContext);
    }

    return currentContext;
  };
}

/**
 * Run a command with middleware
 * @param {Function} commandFn - Command function to execute
 * @param {Object} args - Command arguments
 * @param {Object} options - Additional options
 * @returns {Promise<*>} Command result
 */
export async function runWithMiddleware(commandFn, args, options = {}) {
  const context = createContext({
    command: options.command || 'unknown',
    args,
    options,
    quiet: args.quiet || options.quiet,
    verbose: args.verbose || options.verbose,
  });

  // Apply pre-execution middleware
  const preparedContext = await applyMiddleware(context);

  // Execute command
  let result;
  let error;
  try {
    preparedContext.meta.phase = 'execution';
    result = await commandFn(preparedContext);
    preparedContext.result = result;
  } catch (err) {
    error = err;
    preparedContext.errors.push({
      phase: 'execution',
      message: err.message,
      stack: err.stack,
      timestamp: new Date().toISOString(),
    });
  }

  // Apply post-execution hooks
  preparedContext.meta.phase = 'post';
  preparedContext.timing.end = process.hrtime.bigint();
  preparedContext.timing.totalDuration =
    Number(preparedContext.timing.end - preparedContext.timing.start) / 1e6;

  // Re-throw if there was an execution error
  if (error) {
    throw error;
  }

  return result;
}

// Re-export individual middleware for selective use
export { loggingMiddleware } from './logging.mjs';
export { profilingMiddleware } from './profiling.mjs';
export { validationMiddleware } from './validation.mjs';
export { configMiddleware } from './config.mjs';
