/**
 * @file Effect Sandbox for secure hook execution
 * @module effect-sandbox
 * 
 * @description
 * Provides secure sandboxing for hook effects using vm2 or worker threads
 * to prevent malicious code execution and system access.
 */

import { Worker, isMainThread, parentPort, workerData } from 'worker_threads';
import { join } from 'path';
import { randomUUID } from 'crypto';
import { z } from 'zod';

/**
 * Schema for sandbox configuration
 */
const SandboxConfigSchema = z.object({
  type: z.enum(['vm2', 'worker', 'isolate']).default('worker'),
  timeout: z.number().int().positive().max(300000).default(30000),
  memoryLimit: z.number().int().positive().max(1024 * 1024 * 1024).default(64 * 1024 * 1024), // 64MB
  cpuLimit: z.number().int().positive().max(100).default(50), // 50% CPU
  allowedModules: z.array(z.string()).default([]),
  allowedGlobals: z.array(z.string()).default(['console', 'Date', 'Math', 'JSON']),
  enableNetwork: z.boolean().default(false),
  enableFileSystem: z.boolean().default(false),
  enableProcess: z.boolean().default(false),
  strictMode: z.boolean().default(true)
});

/**
 * Schema for sandbox execution context
 */
const SandboxContextSchema = z.object({
  event: z.any(),
  store: z.any(),
  delta: z.any(),
  metadata: z.record(z.any()).optional(),
  allowedFunctions: z.array(z.string()).default(['emitEvent', 'log', 'assert'])
});

/**
 * Schema for sandbox execution result
 */
const SandboxResultSchema = z.object({
  success: z.boolean(),
  result: z.any().optional(),
  error: z.string().optional(),
  duration: z.number().nonnegative(),
  memoryUsed: z.number().nonnegative().optional(),
  cpuUsed: z.number().nonnegative().optional(),
  assertions: z.array(z.object({
    subject: z.string(),
    predicate: z.string(),
    object: z.string(),
    graph: z.string().optional()
  })).optional(),
  events: z.array(z.any()).optional()
});

/**
 * Extended sandbox configuration schema with worker pool settings
 */
const ExtendedSandboxConfigSchema = SandboxConfigSchema.extend({
  maxWorkerPoolSize: z.number().int().positive().max(200).default(50),
  workerTTL: z.number().int().positive().default(300000), // 5 minutes default TTL
  cleanupInterval: z.number().int().positive().default(60000) // 1 minute cleanup interval
});

/**
 * Effect Sandbox for secure hook execution
 */
export class EffectSandbox {
  /**
   * Create a new effect sandbox
   * @param {Object} [config] - Sandbox configuration
   */
  constructor(config = {}) {
    this.config = ExtendedSandboxConfigSchema.parse(config);
    this.workers = new Map();
    this.workerTimestamps = new Map(); // Track worker creation time for TTL
    this.executionCount = 0;
    this.totalExecutions = 0;
    this.totalDuration = 0;

    // Start periodic cleanup for stale workers
    this._startWorkerCleanup();
  }

  /**
   * Start periodic cleanup of stale workers
   * @private
   */
  _startWorkerCleanup() {
    this.cleanupTimer = setInterval(() => {
      this._cleanupStaleWorkers();
    }, this.config.cleanupInterval);

    // Don't let cleanup timer prevent process exit
    if (this.cleanupTimer.unref) {
      this.cleanupTimer.unref();
    }
  }

  /**
   * Cleanup workers that have exceeded TTL
   * @private
   */
  _cleanupStaleWorkers() {
    const now = Date.now();
    const ttl = this.config.workerTTL;

    for (const [executionId, timestamp] of this.workerTimestamps) {
      if (now - timestamp > ttl) {
        const worker = this.workers.get(executionId);
        if (worker) {
          console.warn(`[EffectSandbox] Force terminating stale worker ${executionId} (exceeded TTL of ${ttl}ms)`);
          worker.terminate();
          this.workers.delete(executionId);
        }
        this.workerTimestamps.delete(executionId);
      }
    }
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
      
      // Choose execution method based on config
      let result;
      switch (this.config.type) {
        case 'worker':
          result = await this._executeInWorker(effect, validatedContext, executionId, options);
          break;
        case 'vm2':
          result = await this._executeInVM2(effect, validatedContext, executionId, options);
          break;
        case 'isolate':
          result = await this._executeInIsolate(effect, validatedContext, executionId, options);
          break;
        default:
          throw new Error(`Unsupported sandbox type: ${this.config.type}`);
      }
      
      const duration = Date.now() - startTime;
      this._updateMetrics(duration);
      
      return {
        ...result,
        executionId,
        duration,
        success: true
      };
    } catch (error) {
      const duration = Date.now() - startTime;
      this._updateMetrics(duration);
      
      return {
        executionId,
        duration,
        success: false,
        error: error.message,
        result: null
      };
    }
  }

  /**
   * Execute effect in worker thread
   * @param {Function} effect - Effect function
   * @param {Object} context - Execution context
   * @param {string} executionId - Execution ID
   * @param {Object} options - Execution options
   * @returns {Promise<Object>} Execution result
   * @private
   */
  async _executeInWorker(effect, context, executionId, options) {
    // Check pool size limit before creating new worker
    if (this.workers.size >= this.config.maxWorkerPoolSize) {
      // Cleanup stale workers first
      this._cleanupStaleWorkers();

      // If still at limit, reject with clear error
      if (this.workers.size >= this.config.maxWorkerPoolSize) {
        throw new Error(`Worker pool exhausted: ${this.workers.size}/${this.config.maxWorkerPoolSize} workers active. Try again later.`);
      }
    }

    return new Promise((resolve, reject) => {
      const timeout = setTimeout(() => {
        // Force terminate worker on timeout
        const worker = this.workers.get(executionId);
        if (worker) {
          console.warn(`[EffectSandbox] Force terminating worker ${executionId} after timeout`);
          worker.terminate();
          this.workers.delete(executionId);
          this.workerTimestamps.delete(executionId);
        }
        reject(new Error(`Worker execution timeout after ${this.config.timeout}ms`));
      }, this.config.timeout);

      const worker = new Worker(join(__dirname, 'effect-sandbox-worker.mjs'), {
        workerData: {
          effect: effect.toString(),
          context,
          executionId,
          config: this.config,
          options
        }
      });

      worker.on('message', (result) => {
        clearTimeout(timeout);
        this.workers.delete(executionId);
        this.workerTimestamps.delete(executionId);
        resolve(result);
      });

      worker.on('error', (error) => {
        clearTimeout(timeout);
        this.workers.delete(executionId);
        this.workerTimestamps.delete(executionId);
        reject(error);
      });

      worker.on('exit', (code) => {
        if (code !== 0) {
          clearTimeout(timeout);
          this.workers.delete(executionId);
          this.workerTimestamps.delete(executionId);
          reject(new Error(`Worker exited with code ${code}`));
        }
      });

      this.workers.set(executionId, worker);
      this.workerTimestamps.set(executionId, Date.now());
    });
  }

  /**
   * Execute effect in VM2 (if available) - DEPRECATED
   * @param {Function} effect - Effect function
   * @param {Object} context - Execution context
   * @param {string} executionId - Execution ID
   * @param {Object} options - Execution options
   * @returns {Promise<Object>} Execution result
   * @private
   * @deprecated Use isolated-vm or worker executors instead
   */
  async _executeInVM2(effect, context, executionId, options) {
    try {
      // Use new executor pattern
      const { createExecutor } = await import('../security/sandbox/detector.mjs');
      const executor = await createExecutor('vm2', {
        timeout: this.config.timeout,
        strictMode: this.config.strictMode
      });

      // Create safe effect function
      const safeEffect = this._createSafeEffect(effect, context);

      const executionResult = await executor.run(safeEffect, context, options);

      if (!executionResult.success) {
        throw new Error(executionResult.error || 'VM2 execution failed');
      }

      return {
        result: executionResult.result,
        assertions: context.assertions || [],
        events: context.events || []
      };
    } catch (error) {
      if (error.message.includes('vm2')) {
        throw new Error('VM2 not available. Install with: pnpm add vm2');
      }
      throw error;
    }
  }

  /**
   * Execute effect in isolate (isolated-vm)
   * @param {Function} effect - Effect function
   * @param {Object} context - Execution context
   * @param {string} executionId - Execution ID
   * @param {Object} options - Execution options
   * @returns {Promise<Object>} Execution result
   * @private
   */
  async _executeInIsolate(effect, context, executionId, options) {
    try {
      // Use isolated-vm executor
      const { createExecutor } = await import('../security/sandbox/detector.mjs');
      const executor = await createExecutor('isolated-vm', {
        memoryLimit: this.config.memoryLimit / (1024 * 1024), // Convert bytes to MB
        timeout: this.config.timeout,
        enableAsync: true,
        enableThreatDetection: true,
        strictMode: this.config.strictMode
      });

      // Create safe effect function
      const safeEffect = this._createSafeEffect(effect, context);

      const executionResult = await executor.run(safeEffect, context, options);

      if (!executionResult.success) {
        throw new Error(executionResult.error || 'Isolate execution failed');
      }

      // Cleanup executor
      await executor.cleanup();

      return {
        result: executionResult.result,
        assertions: context.assertions || [],
        events: context.events || [],
        memoryUsed: executionResult.memoryUsed,
        codeHash: executionResult.codeHash
      };
    } catch (error) {
      throw new Error(`Isolate execution failed: ${error.message}`);
    }
  }

  /**
   * Create sandbox globals
   * @param {Object} context - Execution context
   * @returns {Object} Sandbox globals
   * @private
   */
  _createSandboxGlobals(context) {
    const globals = {};
    
    // Add allowed globals
    for (const globalName of this.config.allowedGlobals) {
      if (globalName in globalThis) {
        globals[globalName] = globalThis[globalName];
      }
    }
    
    // Add context-specific globals
    globals.event = context.event;
    globals.store = context.store;
    globals.delta = context.delta;
    globals.metadata = context.metadata || {};
    
    // Add safe functions
    globals.emitEvent = (eventData) => {
      if (!context.events) context.events = [];
      context.events.push(eventData);
    };
    
    globals.log = (message, level = 'info') => {
      console.log(`[Sandbox ${level.toUpperCase()}] ${message}`);
    };
    
    globals.assert = (subject, predicate, object, graph) => {
      if (!context.assertions) context.assertions = [];
      context.assertions.push({ subject, predicate, object, graph });
    };
    
    return globals;
  }

  /**
   * Create safe console
   * @returns {Object} Safe console object
   * @private
   */
  _createSafeConsole() {
    return {
      log: (message) => console.log(`[Sandbox] ${message}`),
      warn: (message) => console.warn(`[Sandbox] ${message}`),
      error: (message) => console.error(`[Sandbox] ${message}`),
      info: (message) => console.info(`[Sandbox] ${message}`)
    };
  }

  /**
   * Create safe require function
   * @returns {Function} Safe require function
   * @private
   */
  _createSafeRequire() {
    return (moduleName) => {
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

  /**
   * Create safe effect function
   * @param {Function} effect - Original effect function
   * @param {Object} context - Execution context
   * @returns {string} Safe effect code
   * @private
   */
  _createSafeEffect(effect, context) {
    // Convert function to string and wrap in safe context
    const effectCode = effect.toString();
    
    return `
      const context = ${JSON.stringify(context)};
      const effect = ${effectCode};
    `;
  }

  /**
   * Update execution metrics
   * @param {number} duration - Execution duration
   * @private
   */
  _updateMetrics(duration) {
    this.executionCount++;
    this.totalExecutions++;
    this.totalDuration += duration;
  }

  /**
   * Get sandbox statistics
   * @returns {Object} Statistics
   */
  getStats() {
    return {
      config: this.config,
      executionCount: this.executionCount,
      totalExecutions: this.totalExecutions,
      averageDuration: this.totalExecutions > 0 ? this.totalDuration / this.totalExecutions : 0,
      activeWorkers: this.workers.size
    };
  }

  /**
   * Terminate all workers
   */
  async terminate() {
    // Stop cleanup timer
    if (this.cleanupTimer) {
      clearInterval(this.cleanupTimer);
      this.cleanupTimer = null;
    }

    const terminationPromises = Array.from(this.workers.values()).map(worker => {
      return new Promise((resolve) => {
        worker.terminate();
        resolve();
      });
    });

    await Promise.all(terminationPromises);
    this.workers.clear();
    this.workerTimestamps.clear();
  }
}

/**
 * Create a sandboxed hook executor
 * @param {Function} hook - Hook function
 * @param {Object} [config] - Sandbox configuration
 * @returns {Function} Sandboxed hook function
 */
export function createSandboxedHook(hook, config = {}) {
  const sandbox = new EffectSandbox(config);
  
  return async (event, context) => {
    const sandboxContext = {
      event,
      store: context?.graph,
      delta: event?.payload?.delta,
      metadata: context?.metadata || {}
    };
    
    return sandbox.executeEffect(hook, sandboxContext);
  };
}

/**
 * Create an effect sandbox instance
 * @param {Object} [config] - Sandbox configuration
 * @returns {EffectSandbox} Sandbox instance
 */
export function createEffectSandbox(config = {}) {
  return new EffectSandbox(config);
}
