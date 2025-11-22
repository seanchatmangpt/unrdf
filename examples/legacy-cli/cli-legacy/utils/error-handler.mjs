/**
 * @file CLI Error Handler
 * @module cli/utils/error-handler
 *
 * @description
 * Centralized error handling for CLI commands with proper
 * exit codes and user-friendly messages.
 */

import { ZodError } from 'zod';

/**
 * Error types and exit codes
 */
export const ErrorCodes = {
  SUCCESS: 0,
  GENERAL_ERROR: 1,
  INVALID_INPUT: 2,
  FILE_NOT_FOUND: 3,
  VALIDATION_ERROR: 4,
  PARSE_ERROR: 5,
  QUERY_ERROR: 6,
  CONVERSION_ERROR: 7,
  HOOK_ERROR: 8,
  CONFIG_ERROR: 9,
};

/**
 * Format Zod validation error
 * @param {ZodError} error - Zod error
 * @returns {string} Formatted error message
 */
function formatZodError(error) {
  const issues = error.issues
    .map(issue => {
      const path = issue.path.join('.');
      return `  - ${path}: ${issue.message}`;
    })
    .join('\n');

  return `Validation error:\n${issues}`;
}

/**
 * Handle CLI error with proper formatting and exit code
 * @param {Error} error - Error object
 * @param {string} context - Context where error occurred
 * @param {boolean} [exit=true] - Whether to exit process
 * @returns {number} Exit code
 */
export function handleError(error, context, exit = true) {
  let exitCode = ErrorCodes.GENERAL_ERROR;
  let message = error.message;

  // Determine error type and format message
  if (error instanceof ZodError) {
    exitCode = ErrorCodes.VALIDATION_ERROR;
    message = formatZodError(error);
  } else if (error.code === 'ENOENT') {
    exitCode = ErrorCodes.FILE_NOT_FOUND;
    message = `File not found: ${error.path || 'unknown'}`;
  } else if (error.name === 'ParseError') {
    exitCode = ErrorCodes.PARSE_ERROR;
  } else if (error.name === 'QueryError') {
    exitCode = ErrorCodes.QUERY_ERROR;
  } else if (error.name === 'ValidationError') {
    exitCode = ErrorCodes.VALIDATION_ERROR;
  } else if (error.name === 'ConversionError') {
    exitCode = ErrorCodes.CONVERSION_ERROR;
  }

  // Format final error message
  const errorMessage = `❌ ${context}: ${message}`;
  console.error(errorMessage);

  // Log stack trace in debug mode
  if (process.env.DEBUG || process.env.UNRDF_DEBUG) {
    console.error('\nStack trace:');
    console.error(error.stack);
  }

  if (exit) {
    process.exit(exitCode);
  }

  return exitCode;
}

/**
 * Wrap async command function with error handling
 * @param {Function} commandFn - Command function
 * @param {string} context - Command context
 * @returns {Function} Wrapped command function
 */
export function withErrorHandling(commandFn, context) {
  return async (...args) => {
    try {
      await commandFn(...args);
    } catch (error) {
      handleError(error, context);
    }
  };
}

/**
 * Create custom error class
 * @param {string} name - Error class name
 * @returns {class} Error class
 */
export function createErrorClass(name) {
  return class extends Error {
    constructor(message) {
      super(message);
      this.name = name;
    }
  };
}

// Export custom error classes
export const ParseError = createErrorClass('ParseError');
export const QueryError = createErrorClass('QueryError');
export const ValidationError = createErrorClass('ValidationError');
export const ConversionError = createErrorClass('ConversionError');
export const HookError = createErrorClass('HookError');
export const ConfigError = createErrorClass('ConfigError');
