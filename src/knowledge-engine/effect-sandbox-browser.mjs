/**
 * @file Browser-compatible Effect Sandbox
 * @module effect-sandbox-browser
 * 
 * @description
 * Browser-compatible version of the effect sandbox that uses Web Workers
 * instead of Node.js worker threads for secure hook execution.
 */

import { randomUUID, Worker } from './browser-shims.mjs';
import { z } from 'zod';

/**
 * Schema for sandbox configuration
 */
const SandboxConfigSchema = z.object({
  type: z.enum(['worker', 'global']).default('worker'), // Only worker mode in browser
  timeout: z.number().int().positive().max(30000).default(5000), // Shorter timeout for browser
  memoryLimit: z.number().int().positive().max(100 * 1024 * 1024).default(10 * 1024 * 1024), // 10MB default
  allowedGlobals: z.array(z.string()).default(['console', 'Date', 'Math', 'JSON', 'Array', 'Object']),
  enableNetwork: z.boolean().default(false),
  enableFileSystem: z.boolean().default(false),
  strictMode: z.boolean().default(true)
});

/**
 * Schema for sandbox execution context
 */
const SandboxContextSchema = z.object({
  event: z.any(),
  store: z.any(),
  delta: z.any(),
  metadata: z.record(z.any()).optional()
});

/**
 * Schema for sandbox execution result
 */
const SandboxResultSchema = z.object({
  success: z.boolean(),
  result: z.any().optional(),
  error: z.string().optional(),
  duration: z.number().nonnegative()
});

/**
 * Browser-compatible Effect Sandbox
 */
export class EffectSandbox {
  /**
   *
   */
  constructor(config = {}) {
    this.config = SandboxConfigSchema.parse(config);
    this.workers = new Map();
    this.executionCount = 0;
    this.totalExecutions = 0;
    this.totalDuration = 0;
  }

  /**
   * Execute a hook effect in the sandbox
   * @param {Function} effect - The effect function to execute
   * @param {Object} context - Execution context
   * @param {Object} [options] - Execution options
   * @returns {Promise<Object>} Execution result
   */
  async executeEffect(effect, context, options = {}) {
    const executionId = randomUUID();
    const startTime = Date.now();

    try {
      // Validate context
      const validatedContext = SandboxContextSchema.parse(context);

      let result;
      switch (this.config.type) {
        case 'worker':
          result = await this._executeInWorker(effect, validatedContext, executionId, options);
          break;
        case 'global':
          // Not recommended for browser, but allowed for testing
          result = await this._executeInGlobal(effect, validatedContext, executionId, options);
          break;
        default:
          throw new Error(`Unsupported sandbox type: ${this.config.type}`);
      }

      const duration = Date.now() - startTime;
      this.totalExecutions++;
      this.totalDuration += duration;

      const validatedResult = SandboxResultSchema.parse({
        ...result,
        duration
      });

      return validatedResult;
    } catch (error) {
      const duration = Date.now() - startTime;
      this.totalExecutions++;

      return {
        success: false,
        error: error.message,
        duration
      };
    }
  }

  /**
   * Execute effect in Web Worker
   * @private
   */
  async _executeInWorker(effect, context, executionId, options) {
    const workerScript = this._createWorkerScript(effect);
    
    const worker = new Worker(workerScript, {
      name: `effect-sandbox-${executionId}`
    });

    return new Promise((resolve, reject) => {
      const timeout = setTimeout(() => {
        worker.terminate();
        reject(new Error(`Worker execution timeout after ${this.config.timeout}ms`));
      }, this.config.timeout);

      worker.onmessage = (event) => {
        clearTimeout(timeout);
        worker.terminate();
        
        if (event.data.error) {
          reject(new Error(event.data.error));
        } else {
          resolve(event.data);
        }
      };

      worker.onerror = (error) => {
        clearTimeout(timeout);
        worker.terminate();
        reject(new Error(`Worker error: ${error.message}`));
      };

      worker.postMessage({ context, config: this.config });
    });
  }

  /**
   * Execute effect in global scope (not recommended for production)
   * @private
   */
  async _executeInGlobal(effect, context, executionId, options) {
    return new Promise((resolve, reject) => {
      try {
        const result = effect(context, {
          emitEvent: (event) => console.log('Event:', event),
          log: (message) => console.log(message),
          assert: (condition, message) => {
            if (!condition) throw new Error(message || 'Assertion failed');
          }
        });

        if (result instanceof Promise) {
          result.then(resolve).catch(reject);
        } else {
          resolve({ success: true, result });
        }
      } catch (error) {
        resolve({ success: false, error: error.message });
      }
    });
  }

  /**
   * Create worker script from effect function
   * @private
   */
  _createWorkerScript(effect) {
    const allowedGlobals = this.config.allowedGlobals.join(', ');
    
    return `
      // Worker script for effect sandbox
      const effect = ${effect.toString()};
      
      // Sandbox globals
      ${this.config.allowedGlobals.map(global => 
        `const ${global} = self.${global};`
      ).join('\n')}
      
      // Safe console
      const console = {
        log: (...args) => self.postMessage({ type: 'console', level: 'log', args }),
        warn: (...args) => self.postMessage({ type: 'console', level: 'warn', args }),
        error: (...args) => self.postMessage({ type: 'console', level: 'error', args }),
        info: (...args) => self.postMessage({ type: 'console', level: 'info', args })
      };
      
      // Safe API implementations
      const emitEvent = (event) => {
        self.postMessage({ type: 'event', event });
      };
      
      const log = (message) => {
        self.postMessage({ type: 'log', message });
      };
      
      const assert = (condition, message) => {
        throw new Error(message || 'Assertion failed');
      };
      
      // Message handler
      self.onmessage = async (event) => {
        try {
          const { context, config } = event.data;
          
          const result = await effect(context, {
            emitEvent,
            log,
            assert
          });
          
          self.postMessage({ 
            success: true, 
            result,
            type: 'result'
          });
        } catch (error) {
          self.postMessage({ 
            success: false, 
            error: error.message,
            type: 'error'
          });
        }
      };
    `;
  }

  /**
   * Get sandbox statistics
   * @returns {Object} Statistics
   */
  getStats() {
    return {
      type: this.config.type,
      activeWorkers: this.workers.size,
      totalExecutions: this.totalExecutions,
      averageDuration: this.totalExecutions > 0 ? this.totalDuration / this.totalExecutions : 0,
      config: this.config
    };
  }

  /**
   * Clear sandbox state
   */
  clear() {
    // Terminate all workers
    for (const worker of this.workers.values()) {
      worker.terminate();
    }
    this.workers.clear();
    
    this.executionCount = 0;
    this.totalExecutions = 0;
    this.totalDuration = 0;
  }
}

/**
 * Create a new effect sandbox
 * @param {Object} [config] - Sandbox configuration
 * @returns {EffectSandbox} New sandbox instance
 */
export function createEffectSandbox(config = {}) {
  return new EffectSandbox(config);
}

export default EffectSandbox;
