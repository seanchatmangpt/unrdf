/**
 * @file Worker Thread Executor
 * @module security/sandbox/worker-executor
 *
 * @description
 * Fallback sandbox executor using Worker threads (Node.js 12+).
 * Provides process-level isolation with:
 * - Separate V8 context per worker
 * - Memory isolation (separate heap)
 * - Timeout controls
 * - Message-based communication
 * - OpenTelemetry instrumentation
 */

import { Worker } from 'worker_threads';
import { trace } from '@opentelemetry/api';
import { randomUUID } from 'crypto';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const tracer = trace.getTracer('worker-executor');
const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

/**
 * Worker Thread Executor
 */
export class WorkerExecutor {
  /**
   * @param {Object} [config] - Executor configuration
   * @param {number} [config.timeout=5000] - Execution timeout in ms
   * @param {number} [config.memoryLimit=128] - Memory limit in MB (soft limit)
   * @param {Array<string>} [config.allowedGlobals] - Allowed global variables
   * @param {boolean} [config.strictMode=true] - Enable strict mode
   */
  constructor(config = {}) {
    this.config = {
      timeout: config.timeout || 5000,
      memoryLimit: config.memoryLimit || 128,
      allowedGlobals: config.allowedGlobals || ['console', 'Date', 'Math', 'JSON'],
      strictMode: config.strictMode !== false,
      ...config
    };

    /** @type {Map<string, Worker>} */
    this.workers = new Map();

    this.executionCount = 0;
    this.totalDuration = 0;
  }

  /**
   * Execute code in worker thread
   * @param {string|Function} code - Code to execute
   * @param {Object} [context] - Execution context
   * @param {Object} [options] - Execution options
   * @returns {Promise<Object>} Execution result
   */
  async run(code, context = {}, options = {}) {
    return tracer.startActiveSpan('security.worker.execute', async (span) => {
      const startTime = Date.now();
      const executionId = randomUUID();

      try {
        span.setAttributes({
          'security.executor.type': 'worker',
          'security.execution.id': executionId,
          'security.timeout': options.timeout || this.config.timeout
        });

        // Convert function to string if needed
        const codeString = typeof code === 'function' ? code.toString() : code;

        // Wrap code in strict mode if enabled
        const wrappedCode = this.config.strictMode
          ? `"use strict";\n${codeString}`
          : codeString;

        // Create worker promise
        const result = await new Promise((resolve, reject) => {
          const timeout = setTimeout(() => {
            worker.terminate();
            this.workers.delete(executionId);
            reject(new Error(`Worker execution timeout after ${this.config.timeout}ms`));
          }, options.timeout || this.config.timeout);

          const worker = new Worker(
            join(__dirname, 'worker-executor-runtime.mjs'),
            {
              workerData: {
                code: wrappedCode,
                context,
                config: {
                  allowedGlobals: this.config.allowedGlobals,
                  strictMode: this.config.strictMode
                }
              },
              resourceLimits: {
                maxOldGenerationSizeMb: this.config.memoryLimit,
                maxYoungGenerationSizeMb: Math.floor(this.config.memoryLimit / 4)
              }
            }
          );

          this.workers.set(executionId, worker);

          worker.on('message', (message) => {
            clearTimeout(timeout);
            this.workers.delete(executionId);
            worker.terminate();

            if (message.success) {
              resolve(message);
            } else {
              reject(new Error(message.error || 'Worker execution failed'));
            }
          });

          worker.on('error', (error) => {
            clearTimeout(timeout);
            this.workers.delete(executionId);
            worker.terminate();
            reject(error);
          });

          worker.on('exit', (code) => {
            if (code !== 0 && !this.workers.has(executionId)) {
              // Already handled by message or error
              return;
            }
            clearTimeout(timeout);
            this.workers.delete(executionId);
            reject(new Error(`Worker exited with code ${code}`));
          });
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
          executionId,
          memoryUsed: result.memoryUsed || { used: 0 }
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

        // Categorize error
        let errorType = 'unknown';
        if (error.message?.includes('timeout')) {
          errorType = 'timeout';
        } else if (error.message?.includes('memory')) {
          errorType = 'memory_limit';
        }

        span.setAttribute('security.error.type', errorType);

        return {
          success: false,
          error: error.message,
          errorType,
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
      type: 'worker',
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
        await worker.terminate();
      } catch (err) {
        // Ignore termination errors
      }
      this.workers.delete(executionId);
    }
  }
}
