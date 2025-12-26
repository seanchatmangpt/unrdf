/**
 * @file Error Handler Middleware
 * @module cli-v2/middleware/error-handler
 *
 * ENHANCED: Uses enhanced-errors module for better UX
 */

import {
  getErrorRecoveryGuide,
  WorkflowError,
  ImportError
} from '@unrdf/core/utils/enhanced-errors';

/**
 * Global error handler for CLI
 * Enhanced with recovery guides and actionable suggestions
 * @param {Error} error - Error to handle
 */
export function errorHandler(error) {
  // Enhanced errors already have great formatting
  if (error.name === 'ValidationError' || error instanceof WorkflowError || error instanceof ImportError) {
    console.error(error.message);

    // Show recovery guide if available
    if (process.env.UNRDF_DEBUG === 'true') {
      console.error('\n📖 Recovery Guide:\n');
      console.error(getErrorRecoveryGuide(error));
    }
  } else {
    // Handle other errors with basic formatting
    console.error('❌ Error:', error.message);

    // Provide helpful suggestions based on error type
    if (error.code === 'ENOENT') {
      console.error('\n💡 Tip: The file or directory does not exist.');
      console.error('Check the path and try again.');
    } else if (error.code === 'EACCES') {
      console.error('\n💡 Tip: Permission denied.');
      console.error('Try running with appropriate permissions or check file ownership.');
    }
  }

  // Show stack trace in debug mode
  if (process.env.UNRDF_DEBUG === 'true') {
    console.error('\n🔍 Stack trace:');
    console.error(error.stack);
  } else {
    console.error('\n💡 For detailed debugging, run with: UNRDF_DEBUG=true');
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
