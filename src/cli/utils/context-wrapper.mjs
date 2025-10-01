/**
 * @file CLI Context Wrapper
 * @module cli/utils/context-wrapper
 *
 * @description
 * Wraps CLI commands with store context for dependency injection
 * and proper resource management.
 */

import { setStoreContext } from '../../index.mjs';
import { loadConfig } from './config-loader.mjs';
import { handleError } from './error-handler.mjs';

/**
 * Wrap command function with store context
 * @param {Function} commandFn - Command function
 * @param {string} commandName - Command name for error context
 * @returns {Function} Wrapped command function
 */
export function withContext(commandFn, commandName = 'command') {
  return async (ctx) => {
    try {
      const config = await loadConfig();
      // Initialize store context directly (more reliable than initStore for CLI usage)
      setStoreContext([], { baseIRI: config.baseIRI || 'http://example.org/' });

      await commandFn(ctx, config);
    } catch (error) {
      handleError(error, commandName);
    }
  };
}

/**
 * Create a command execution context
 * @param {Object} ctx - CLI context from citty
 * @param {Object} config - Loaded configuration
 * @returns {Object} Execution context
 */
export function createExecutionContext(ctx, config) {
  return {
    args: ctx.args,
    config,
    verbose: ctx.args.verbose || false,
    debug: ctx.args.debug || false
  };
}

/**
 * Validate required arguments
 * @param {Object} args - Command arguments
 * @param {Array<string>} required - Required argument names
 * @throws {Error} If required arguments are missing
 */
export function validateRequiredArgs(args, required) {
  const missing = required.filter(arg => !args[arg]);

  if (missing.length > 0) {
    throw new Error(`Missing required arguments: ${missing.join(', ')}`);
  }
}

/**
 * Get argument value with fallback
 * @param {Object} args - Command arguments
 * @param {string} name - Argument name
 * @param {any} defaultValue - Default value
 * @returns {any} Argument value or default
 */
export function getArg(args, name, defaultValue) {
  return args[name] !== undefined ? args[name] : defaultValue;
}
