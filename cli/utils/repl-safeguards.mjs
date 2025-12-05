/**
 * @file REPL session safeguards
 * @module cli/utils/repl-safeguards
 *
 * Protects REPL from buffer overflow and hanging queries
 * FM-CLI-015: REPL infinite loop / buffer overflow
 */

/**
 * REPL session with safeguards
 */
export class REPLSession {
  constructor(options = {}) {
    this.maxBufferSize = options.maxBufferSize || 10 * 1024 * 1024; // 10MB
    this.maxQueryTimeout = options.maxQueryTimeout || 30000; // 30s
    this.maxHistoryLines = options.maxHistoryLines || 1000;
    this.maxConsecutiveErrors = options.maxConsecutiveErrors || 10;

    this.buffer = '';
    this.history = [];
    this.currentQuery = null;
    this.queryTimeout = null;
    this.consecutiveErrors = 0;
    this.sessionStarted = Date.now();
  }

  /**
   * Check buffer size before accepting input
   */
  checkBufferSize(input) {
    const newSize = this.buffer.length + input.length;

    if (newSize > this.maxBufferSize) {
      return {
        valid: false,
        error: `Buffer size exceeded: ${(newSize / 1024 / 1024).toFixed(2)}MB > ${(this.maxBufferSize / 1024 / 1024).toFixed(2)}MB`,
        suggestion: 'Clear history with "clear" command or start a new REPL session'
      };
    }

    return { valid: true };
  }

  /**
   * Add input to buffer with safeguards
   */
  addInput(input) {
    // Check buffer size
    const bufferCheck = this.checkBufferSize(input);
    if (!bufferCheck.valid) {
      return bufferCheck;
    }

    this.buffer += input + '\n';
    this.history.push(input);

    // Trim history if too long
    if (this.history.length > this.maxHistoryLines) {
      this.history = this.history.slice(-this.maxHistoryLines);
    }

    return { valid: true, bufferLength: this.buffer.length };
  }

  /**
   * Execute query with timeout protection
   */
  async executeWithTimeout(queryFunction) {
    let timeoutHandle;
    let timedOut = false;

    const timeoutPromise = new Promise((_, reject) => {
      timeoutHandle = setTimeout(() => {
        timedOut = true;
        reject(new Error(
          `Query timeout: exceeded ${this.maxQueryTimeout}ms limit. ` +
          `Consider breaking the query into smaller operations.`
        ));
      }, this.maxQueryTimeout);
    });

    try {
      const result = await Promise.race([
        queryFunction(),
        timeoutPromise
      ]);

      this.consecutiveErrors = 0;
      return { success: true, result };
    } catch (error) {
      this.consecutiveErrors++;

      if (this.consecutiveErrors > this.maxConsecutiveErrors) {
        return {
          success: false,
          error: error.message,
          suggestion: 'Too many consecutive errors. Consider starting a new REPL session.',
          sessionAbnormal: true
        };
      }

      return {
        success: false,
        error: error.message,
        errorCount: this.consecutiveErrors
      };
    } finally {
      clearTimeout(timeoutHandle);
    }
  }

  /**
   * Clear buffer and reset state
   */
  clearBuffer() {
    const oldSize = this.buffer.length;
    this.buffer = '';
    this.currentQuery = null;

    return {
      success: true,
      freedBytes: oldSize,
      historyKept: this.history.length
    };
  }

  /**
   * Get session diagnostics
   */
  getDiagnostics() {
    const sessionDuration = Date.now() - this.sessionStarted;
    const bufferUsage = (this.buffer.length / this.maxBufferSize * 100).toFixed(1);

    return {
      session_duration_ms: sessionDuration,
      buffer_size_bytes: this.buffer.length,
      buffer_usage_percent: bufferUsage,
      buffer_limit_bytes: this.maxBufferSize,
      history_lines: this.history.length,
      history_limit: this.maxHistoryLines,
      consecutive_errors: this.consecutiveErrors,
      error_limit: this.maxConsecutiveErrors,
      timeout_ms: this.maxQueryTimeout,
      session_healthy: this.consecutiveErrors < this.maxConsecutiveErrors
    };
  }

  /**
   * Get session status
   */
  getStatus() {
    const diag = this.getDiagnostics();
    const status = [];

    status.push(`Session Time: ${(diag.session_duration_ms / 1000).toFixed(1)}s`);
    status.push(`Buffer: ${(diag.buffer_size_bytes / 1024).toFixed(1)}KB / ${(diag.buffer_limit_bytes / 1024 / 1024).toFixed(0)}MB`);
    status.push(`History: ${diag.history_lines} lines`);
    status.push(`Errors: ${diag.consecutive_errors}/${diag.error_limit}`);

    if (!diag.session_healthy) {
      status.push('⚠️ SESSION UNSTABLE - Consider restarting');
    }

    return status.join(' | ');
  }
}

/**
 * REPL input validator
 */
export function validateREPLInput(input) {
  const issues = [];

  // Check for infinite loop patterns
  if (/^\s*while\s*\(true\)/.test(input)) {
    issues.push('Detected infinite while loop - this would hang the REPL');
  }

  if (/^\s*for\s*\(\s*;\s*;\s*\)/.test(input)) {
    issues.push('Detected infinite for loop - this would hang the REPL');
  }

  // Check for excessively nested structures
  const openBraces = (input.match(/\{/g) || []).length;
  const closeBraces = (input.match(/\}/g) || []).length;

  if (Math.abs(openBraces - closeBraces) > 10) {
    issues.push('Possible unmatched braces or excessive nesting');
  }

  // Check for very large strings
  const stringMatches = input.match(/"[^"]*"/g) || [];
  stringMatches.forEach((str, idx) => {
    if (str.length > 1024 * 100) { // 100KB string
      issues.push(`String ${idx + 1} is very large (${(str.length / 1024).toFixed(0)}KB)`);
    }
  });

  return {
    valid: issues.length === 0,
    issues,
    warning: issues.length > 0
      ? `⚠️ Potential issues detected:\n${issues.map(i => `  • ${i}`).join('\n')}`
      : null
  };
}

export default {
  REPLSession,
  validateREPLInput
};
