/**
 * @file Browser Executor
 * @module security/sandbox/browser-executor
 *
 * @description
 * Browser-based sandbox executor using Web Workers.
 * Provides sandboxed execution in browser environments with:
 * - Separate worker context
 * - Message-based communication
 * - WASM support
 * - Async/await support
 */

import { trace } from '@opentelemetry/api';

const tracer = trace.getTracer('browser-executor');

/**
 * Browser Executor (Web Workers)
 */
export class BrowserExecutor {
  /**
   * @param {Object} [config] - Executor configuration
   * @param {number} [config.timeout=5000] - Execution timeout in ms
   * @param {boolean} [config.enableWasm=true] - Enable WASM support
   * @param {boolean} [config.strictMode=true] - Enable strict mode
   */
  constructor(config = {}) {
    this.config = {
      timeout: config.timeout || 5000,
      enableWasm: config.enableWasm !== false,
      strictMode: config.strictMode !== false,
      ...config
    };

    /** @type {Map<string, Worker>} */
    this.workers = new Map();

    this.executionCount = 0;
    this.totalDuration = 0;
  }

  /**
   * Execute code in Web Worker
   * @param {string|Function} code - Code to execute
   * @param {Object} [context] - Execution context
   * @param {Object} [options] - Execution options
   * @returns {Promise<Object>} Execution result
   */
  async run(code, context = {}, options = {}) {
    return tracer.startActiveSpan('security.browser.execute', async (span) => {
      const startTime = Date.now();
      const executionId = `exec_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;

      try {
        span.setAttributes({
          'security.executor.type': 'browser',
          'security.execution.id': executionId,
          'security.timeout': options.timeout || this.config.timeout
        });

        // Convert function to string if needed
        const codeString = typeof code === 'function' ? code.toString() : code;

        // Wrap code in strict mode if enabled
        const wrappedCode = this.config.strictMode
          ? `"use strict";\n${codeString}`
          : codeString;

        // Create worker blob
        const workerCode = `
          self.onmessage = function(e) {
            const { code, context } = e.data;

            try {
              // Create safe environment
              const sandbox = {
                console: {
                  log: (...args) => self.postMessage({ type: 'log', args }),
                  error: (...args) => self.postMessage({ type: 'error', args }),
                  warn: (...args) => self.postMessage({ type: 'warn', args }),
                  info: (...args) => self.postMessage({ type: 'info', args })
                },
                Date: { now: () => Date.now() },
                Math: Math,
                JSON: JSON,
                ...context
              };

              // Execute code
              const func = new Function(...Object.keys(sandbox), code);
              const result = func(...Object.values(sandbox));

              self.postMessage({ type: 'result', success: true, result });
            } catch (error) {
              self.postMessage({
                type: 'result',
                success: false,
                error: error.message,
                stack: error.stack
              });
            }
          };
        `;

        const blob = new Blob([workerCode], { type: 'application/javascript' });
        const workerUrl = URL.createObjectURL(blob);

        // Create worker promise
        const result = await new Promise((resolve, reject) => {
          const timeout = setTimeout(() => {
            worker.terminate();
            this.workers.delete(executionId);
            URL.revokeObjectURL(workerUrl);
            reject(new Error(`Browser worker execution timeout after ${this.config.timeout}ms`));
          }, options.timeout || this.config.timeout);

          const worker = new Worker(workerUrl);
          this.workers.set(executionId, worker);

          worker.onmessage = (e) => {
            const { type, success, result, error, args } = e.data;

            if (type === 'log' || type === 'error' || type === 'warn' || type === 'info') {
              console[type]('[Worker]', ...args);
              return;
            }

            if (type === 'result') {
              clearTimeout(timeout);
              worker.terminate();
              this.workers.delete(executionId);
              URL.revokeObjectURL(workerUrl);

              if (success) {
                resolve({ success: true, result });
              } else {
                reject(new Error(error || 'Worker execution failed'));
              }
            }
          };

          worker.onerror = (error) => {
            clearTimeout(timeout);
            worker.terminate();
            this.workers.delete(executionId);
            URL.revokeObjectURL(workerUrl);
            reject(error);
          };

          // Send code to worker
          worker.postMessage({ code: wrappedCode, context });
        });

        const duration = Date.now() - startTime;
        this.executionCount++;
        this.totalDuration += duration;

        span.setAttributes({
          'security.execution.duration': duration,
          'security.execution.success': true
        });
        span.setStatus({ code: 1 }); // OK

        return {
          success: true,
          result: result.result,
          duration,
          executionId
        };

      } catch (error) {
        const duration = Date.now() - startTime;

        span.recordException(error);
        span.setAttributes({
          'security.execution.duration': duration,
          'security.execution.success': false,
          'security.error.message': error.message
        });
        span.setStatus({ code: 2, message: error.message });

        return {
          success: false,
          error: error.message,
          duration,
          executionId
        };
      } finally {
        span.end();
      }
    });
  }

  /**
   * Get executor statistics
   * @returns {Object}
   */
  getStats() {
    return {
      type: 'browser',
      config: this.config,
      executionCount: this.executionCount,
      averageDuration: this.executionCount > 0 ? this.totalDuration / this.executionCount : 0,
      activeWorkers: this.workers.size
    };
  }

  /**
   * Cleanup all workers
   */
  async cleanup() {
    for (const [executionId, worker] of this.workers.entries()) {
      try {
        worker.terminate();
      } catch (err) {
        // Ignore termination errors
      }
      this.workers.delete(executionId);
    }
  }
}
