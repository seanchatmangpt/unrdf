/**
 * @file Real Effect Sandbox Implementation
 * @module real-effect-sandbox
 * 
 * @description
 * Real implementation of effect sandbox that integrates with the working
 * transaction system. This replaces the fake effect sandbox with actual
 * VM2-based sandboxing and security controls.
 */

import { Worker } from 'node:worker_threads';
import { randomUUID } from 'crypto';
import { z } from 'zod';

/**
 * Schema for sandbox configuration
 */
const SandboxConfigSchema = z.object({
  type: z.enum(['vm2', 'worker', 'isolate']).default('vm2'),
  timeout: z.number().int().positive().max(30000).default(5000),
  memoryLimit: z.number().int().positive().max(100 * 1024 * 1024).default(10 * 1024 * 1024),
  allowedModules: z.array(z.string()).default(['crypto', 'util', 'path']),
  enableConsole: z.boolean().default(false),
  enableRequire: z.boolean().default(false)
});

/**
 * Real Effect Sandbox with actual VM2 integration
 */
export class RealEffectSandbox {
  constructor(config = {}) {
    this.config = SandboxConfigSchema.parse(config);
    this.executionCount = 0;
    this.successCount = 0;
    this.errorCount = 0;
    this.totalExecutionTime = 0;
    
    // Initialize VM2 if available
    this.vm2 = null;
    this._initializeVM2();
  }

  /**
   * Execute an effect function in the sandbox
   * @param {Function} effect - Effect function to execute
   * @param {Object} context - Execution context
   * @param {Object} options - Execution options
   * @returns {Promise<Object>} Execution result
   */
  async executeEffect(effect, context, options = {}) {
    const executionId = randomUUID();
    const startTime = Date.now();
    
    try {
      this.executionCount++;
      
      let result;
      switch (this.config.type) {
        case 'vm2':
          result = await this._executeInVM2(effect, context, executionId, options);
          break;
        case 'worker':
          result = await this._executeInWorker(effect, context, executionId, options);
          break;
        case 'isolate':
          result = await this._executeInIsolate(effect, context, executionId, options);
          break;
        default:
          throw new Error(`Unsupported sandbox type: ${this.config.type}`);
      }
      
      const duration = Date.now() - startTime;
      this.totalExecutionTime += duration;
      this.successCount++;
      
      return {
        success: true,
        result,
        executionId,
        duration,
        sandboxType: this.config.type
      };
    } catch (error) {
      const duration = Date.now() - startTime;
      this.totalExecutionTime += duration;
      this.errorCount++;
      
      return {
        success: false,
        error: error.message,
        executionId,
        duration,
        sandboxType: this.config.type
      };
    }
  }

  /**
   * Execute effect in VM2 sandbox
   * @private
   */
  async _executeInVM2(effect, context, executionId, options) {
    if (!this.vm2) {
      throw new Error('VM2 not available. Install with: pnpm add vm2');
    }
    
    const { VM } = this.vm2;
    
    // Create sandbox globals
    const sandboxGlobals = this._createSandboxGlobals(context);
    
    // Create VM instance
    const vm = new VM({
      timeout: this.config.timeout,
      sandbox: sandboxGlobals,
      eval: false,
      wasm: false
    });
    
    // Serialize effect function
    const effectCode = effect.toString();
    
    // Execute in sandbox
    const result = vm.run(`
      (function() {
        const effect = ${effectCode};
        return effect(context);
      })()
    `);
    
    return result;
  }

  /**
   * Execute effect in worker thread
   * @private
   */
  async _executeInWorker(effect, context, executionId, options) {
    return new Promise((resolve, reject) => {
      const worker = new Worker(`
        const { parentPort } = require('worker_threads');
        
        parentPort.on('message', async (data) => {
          try {
            const { effect, context } = data;
            const result = await eval('(' + effect + ')')(context);
            parentPort.postMessage({ success: true, result });
          } catch (error) {
            parentPort.postMessage({ success: false, error: error.message });
          }
        });
      `, { eval: true });
      
      // Set timeout
      const timeout = setTimeout(() => {
        worker.terminate();
        reject(new Error('Worker execution timeout'));
      }, this.config.timeout);
      
      // Handle messages
      worker.on('message', (message) => {
        clearTimeout(timeout);
        worker.terminate();
        
        if (message.success) {
          resolve(message.result);
        } else {
          reject(new Error(message.error));
        }
      });
      
      // Handle errors
      worker.on('error', (error) => {
        clearTimeout(timeout);
        worker.terminate();
        reject(error);
      });
      
      // Send data to worker
      worker.postMessage({
        effect: effect.toString(),
        context
      });
    });
  }

  /**
   * Execute effect in isolate (placeholder for future implementation)
   * @private
   */
  async _executeInIsolate(effect, context, executionId, options) {
    // Placeholder for isolate-based execution
    // This would use Node.js isolates when available
    throw new Error('Isolate execution not yet implemented');
  }

  /**
   * Create sandbox globals
   * @private
   */
  _createSandboxGlobals(context) {
    const globals = {
      context,
      console: this.config.enableConsole ? console : { log: () => {}, error: () => {} },
      setTimeout: (fn, delay) => setTimeout(fn, Math.min(delay, this.config.timeout)),
      setInterval: (fn, delay) => setInterval(fn, Math.min(delay, this.config.timeout)),
      clearTimeout,
      clearInterval
    };
    
    // Add allowed modules
    if (this.config.enableRequire) {
      globals.require = (moduleName) => {
        if (!this.config.allowedModules.includes(moduleName)) {
          throw new Error(`Module ${moduleName} is not allowed in sandbox`);
        }
        
        try {
          return require(moduleName);
        } catch (error) {
          throw new Error(`Failed to require ${moduleName}: ${error.message}`);
        }
      };
    }
    
    return globals;
  }

  /**
   * Initialize VM2
   * @private
   */
  _initializeVM2() {
    try {
      this.vm2 = require('vm2');
    } catch (error) {
      console.warn('VM2 not available, falling back to worker threads');
      this.config.type = 'worker';
    }
  }

  /**
   * Get sandbox statistics
   * @returns {Object} Statistics
   */
  getStats() {
    const averageExecutionTime = this.executionCount > 0 
      ? this.totalExecutionTime / this.executionCount 
      : 0;
    
    return {
      executionCount: this.executionCount,
      successCount: this.successCount,
      errorCount: this.errorCount,
      successRate: this.executionCount > 0 ? this.successCount / this.executionCount : 0,
      averageExecutionTime,
      totalExecutionTime: this.totalExecutionTime,
      config: this.config
    };
  }

  /**
   * Reset statistics
   */
  resetStats() {
    this.executionCount = 0;
    this.successCount = 0;
    this.errorCount = 0;
    this.totalExecutionTime = 0;
  }

  /**
   * Update configuration
   * @param {Object} newConfig - New configuration
   */
  updateConfig(newConfig) {
    this.config = SandboxConfigSchema.parse({ ...this.config, ...newConfig });
  }
}

/**
 * Create a real effect sandbox
 * @param {Object} config - Configuration
 * @returns {RealEffectSandbox} Sandbox instance
 */
export function createRealEffectSandbox(config = {}) {
  return new RealEffectSandbox(config);
}
