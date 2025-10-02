/**
 * @file Error Handler Middleware
 * @module cli-v2/middleware/error-handler
 */

/**
 * Global error handler for CLI
 * @param {Error} error - Error to handle
 */
export function errorHandler(error) {
  // Log error with context
  console.error('❌ Error:', error.message);

  if (process.env.UNRDF_DEBUG === 'true') {
    console.error('\nStack trace:');
    console.error(error.stack);
  }

  // Provide helpful suggestions based on error type
  if (error.code === 'ENOENT') {
    console.error('\n💡 Tip: The file or directory does not exist.');
  } else if (error.code === 'EACCES') {
    console.error('\n💡 Tip: Permission denied. Try running with appropriate permissions.');
  } else if (error.name === 'ZodError') {
    console.error('\n💡 Tip: Check your input arguments and configuration.');
  }

  // Exit with error code
  process.exit(1);
}

/**
 * Wrap async function with error handling
 * @param {Function} fn - Async function to wrap
 * @returns {Function} Wrapped function
 */
export function withErrorHandler(fn) {
  return async (...args) => {
    try {
      return await fn(...args);
    } catch (error) {
      errorHandler(error);
    }
  };
}
