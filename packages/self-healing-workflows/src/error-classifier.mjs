/**
 * @file Error classification and pattern matching
 * @module @unrdf/self-healing-workflows/classifier
 * @description Classifies errors by category and severity for recovery decisions
 */

import {
  ErrorPatternSchema
} from './schemas.mjs';

/**
 * Default error patterns for common scenarios
 */
const DEFAULT_PATTERNS = [
  {
    name: 'NetworkError',
    category: 'network',
    severity: 'medium',
    pattern: /ECONNREFUSED|ENOTFOUND|ETIMEDOUT|ECONNRESET|network/i
  },
  {
    name: 'ResourceError',
    category: 'resource',
    severity: 'high',
    pattern: /ENOMEM|ENOSPC|out of memory|disk full|resource unavailable/i
  },
  {
    name: 'ValidationError',
    category: 'validation',
    severity: 'low',
    pattern: /\bvalidation\b|\binvalid\b|schema|parse error/i
  },
  {
    name: 'DependencyError',
    category: 'dependency',
    severity: 'high',
    pattern: /service unavailable|503|502|dependency failed/i
  },
  {
    name: 'BusinessLogicError',
    category: 'business-logic',
    severity: 'critical',
    pattern: /business rule|constraint violation|invariant/i
  },
  {
    name: 'TimeoutError',
    category: 'timeout',
    severity: 'medium',
    pattern: /\btimeout\b|timed out|deadline exceeded/i
  }
];

/**
 * Error classifier for pattern matching and categorization
 */
export class ErrorClassifier {
  /**
   * Creates a new error classifier
   * @param {Object} [options] - Configuration options
   * @param {Array<Object>} [options.patterns] - Custom error patterns
   */
  constructor(options = {}) {
    this.patterns = [
      ...DEFAULT_PATTERNS,
      ...(options.patterns || [])
    ].map(p => ErrorPatternSchema.parse(p));
  }

  /**
   * Classifies an error based on patterns
   * @param {Error} error - The error to classify
   * @returns {Object} Classified error object
   * @example
   * const classifier = new ErrorClassifier();
   * const classified = classifier.classify(new Error('ECONNREFUSED'));
   * console.log(classified.category); // 'network'
   */
  classify(error) {
    if (!(error instanceof Error)) {
      throw new TypeError('Expected Error instance');
    }

    const errorMessage = error.message || '';
    const errorName = error.name || '';
    // Only match against name and message, not stack trace
    const fullText = `${errorName} ${errorMessage}`;

    // Try to match against patterns
    for (const pattern of this.patterns) {
      const regex = pattern.pattern instanceof RegExp
        ? pattern.pattern
        : new RegExp(pattern.pattern, 'i');

      if (regex.test(fullText)) {
        return {
          originalError: error,
          category: pattern.category,
          severity: pattern.severity,
          matchedPattern: pattern.name,
          retryable: this.isRetryable(pattern.category, pattern.severity),
          timestamp: Date.now(),
          metadata: pattern.metadata
        };
      }
    }

    // Default classification for unknown errors
    return {
      originalError: error,
      category: 'unknown',
      severity: 'medium',
      retryable: false,
      timestamp: Date.now()
    };
  }

  /**
   * Determines if an error is retryable based on category and severity
   * @param {string} category - Error category
   * @param {string} severity - Error severity
   * @returns {boolean} True if error is retryable
   */
  isRetryable(category, severity) {
    // Critical errors are never retryable
    if (severity === 'critical') {
      return false;
    }

    // Business logic errors are not retryable
    if (category === 'business-logic') {
      return false;
    }

    // Validation errors are not retryable
    if (category === 'validation') {
      return false;
    }

    // Network, timeout, and resource errors are retryable
    return ['network', 'timeout', 'resource', 'dependency'].includes(category);
  }

  /**
   * Adds a custom error pattern
   * @param {Object} pattern - Error pattern to add
   * @returns {void}
   */
  addPattern(pattern) {
    const validated = ErrorPatternSchema.parse(pattern);
    this.patterns.push(validated);
  }

  /**
   * Gets all registered patterns
   * @returns {Array<Object>} Error patterns
   */
  getPatterns() {
    return [...this.patterns];
  }

  /**
   * Classifies multiple errors
   * @param {Array<Error>} errors - Errors to classify
   * @returns {Array<Object>} Classified errors
   */
  classifyBatch(errors) {
    return errors.map(error => this.classify(error));
  }

  /**
   * Gets error statistics by category
   * @param {Array<Object>} classifiedErrors - Array of classified errors
   * @returns {Object} Statistics by category
   */
  getStatsByCategory(classifiedErrors) {
    const stats = {};

    for (const classified of classifiedErrors) {
      const category = classified.category;
      stats[category] = (stats[category] || 0) + 1;
    }

    return stats;
  }

  /**
   * Filters retryable errors from a batch
   * @param {Array<Object>} classifiedErrors - Classified errors
   * @returns {Array<Object>} Retryable errors only
   */
  filterRetryable(classifiedErrors) {
    return classifiedErrors.filter(e => e.retryable);
  }
}

/**
 * Creates a new error classifier instance
 * @param {Object} [options] - Configuration options
 * @returns {ErrorClassifier} Error classifier instance
 */
export function createErrorClassifier(options) {
  return new ErrorClassifier(options);
}
