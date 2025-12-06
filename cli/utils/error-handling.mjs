/**
 * @file Standardized Error Handling with Resource Cleanup
 * @module cli/utils/error-handling
 *
 * FM-008: Standardize error handling patterns across CLI
 * Ensures resources are always cleaned up even on error paths
 */

/**
 * Execute command with guaranteed cleanup
 *
 * @param {Function} fn - Async function to execute
 * @param {Object} options - Options
 * @param {Object} options.resources - Resources to clean up (locks, spans, files)
 * @param {Function} options.onError - Optional error handler
 * @returns {Promise<any>} Result of fn()
 *
 * @example
 * await withCleanup(async () => {
 *   const lock = await acquireLock();
 *   const span = startSpan();
 *   // ... operation ...
 * }, {
 *   resources: {
 *     lock: { release: () => lock.release() },
 *     span: { end: () => span.end() }
 *   }
 * });
 */
export async function withCleanup(fn, options = {}) {
  const { resources = {}, onError } = options;

  try {
    return await fn();
  } catch (error) {
    // Handle error if handler provided
    if (onError) {
      try {
        await onError(error);
      } catch (handlerError) {
        console.error('Error handler failed:', handlerError.message);
      }
    }

    throw error;
  } finally {
    // POKA-YOKE: Always cleanup, even on error
    for (const [name, resource] of Object.entries(resources)) {
      if (!resource) continue;

      try {
        // Support different cleanup methods
        if (typeof resource.release === 'function') {
          resource.release();
        } else if (typeof resource.end === 'function') {
          resource.end();
        } else if (typeof resource.close === 'function') {
          await resource.close();
        } else if (typeof resource.cleanup === 'function') {
          await resource.cleanup();
        }
      } catch (cleanupError) {
        // Don't throw during cleanup, just log
        console.warn(`Failed to cleanup ${name}:`, cleanupError.message);
      }
    }
  }
}

/**
 * Command error handler with user-friendly messages
 *
 * @param {Error} error - Error to handle
 * @param {Object} options - Options
 * @param {string} options.operation - Operation name for error message
 * @param {number} options.exitCode - Exit code (default: 1)
 * @param {boolean} options.exit - Whether to exit process (default: true)
 */
export function handleCommandError(error, options = {}) {
  const {
    operation = 'Operation',
    exitCode = 1,
    exit = true
  } = options;

  console.error(`❌ ${operation} failed: ${error.message}`);

  // Show helpful suggestions for common errors
  if (error.code === 'ENOENT') {
    console.log('💡 Check that the file or directory exists');
  } else if (error.code === 'EACCES') {
    console.log('💡 Check file permissions');
  } else if (error.code === 'EISDIR') {
    console.log('💡 Specify a file, not a directory');
  } else if (error.message.includes('SPARQL')) {
    console.log('💡 Check SPARQL syntax - use --validate flag to check without executing');
  } else if (error.message.includes('Context')) {
    console.log('💡 List contexts with: unrdf context list');
  }

  if (exit) {
    process.exit(exitCode);
  }
}

/**
 * Wrap command run function with error handling
 *
 * @param {Function} runFn - Command run function
 * @param {Object} options - Options
 * @returns {Function} Wrapped run function
 *
 * @example
 * export const myCommand = defineCommand({
 *   async run(ctx) {
 *     return await withErrorHandling(async () => {
 *       // ... command logic ...
 *     }, { operation: 'My Command' })(ctx);
 *   }
 * });
 */
export function withErrorHandling(runFn, options = {}) {
  return async function wrappedRun(ctx) {
    try {
      return await runFn(ctx);
    } catch (error) {
      handleCommandError(error, options);
    }
  };
}

/**
 * Validate required environment or preconditions
 *
 * @param {Object} checks - Checks to perform
 * @throws {Error} If any check fails
 *
 * @example
 * await validatePreconditions({
 *   'Store initialized': () => getStore() !== null,
 *   'Context available': async () => (await getContext()) !== null
 * });
 */
export async function validatePreconditions(checks) {
  for (const [description, checkFn] of Object.entries(checks)) {
    try {
      const result = typeof checkFn === 'function' ? await checkFn() : checkFn;
      if (!result) {
        throw new Error(`Precondition failed: ${description}`);
      }
    } catch (error) {
      throw new Error(`Precondition check failed for "${description}": ${error.message}`);
    }
  }
}

export default {
  withCleanup,
  handleCommandError,
  withErrorHandling,
  validatePreconditions
};
