/**
 * @file Sandbox Restrictions for Hook Execution
 * @module sandbox-restrictions
 *
 * @description
 * Defines and enforces security restrictions for sandboxed hook execution.
 * Prevents privilege escalation and unauthorized system access.
 */

import { z } from 'zod';

/**
 * Schema for sandbox configuration
 */
const SandboxConfigSchema = z
  .object({
    allowFileSystem: z.boolean().default(false),
    allowNetwork: z.boolean().default(false),
    allowProcessAccess: z.boolean().default(false),
    allowEval: z.boolean().default(false),
    timeoutMs: z.number().default(5000),
    memoryLimitMB: z.number().default(50),
    maxIterations: z.number().default(100000),
  })
  .strict();

/**
 * Dangerous Node.js modules that should be blocked
 */
const BLOCKED_MODULES = new Set([
  'fs',
  'fs/promises',
  'child_process',
  'cluster',
  'crypto',
  'dgram',
  'dns',
  'http',
  'https',
  'http2',
  'inspector',
  'net',
  'os',
  'perf_hooks',
  'process',
  'repl',
  'tls',
  'tty',
  'v8',
  'vm',
  'worker_threads',
  'zlib',
]);

/**
 * Dangerous global objects/functions
 */
const BLOCKED_GLOBALS = new Set([
  'eval',
  'Function',
  'require',
  'import',
  'process',
  'global',
  '_dirname',
  '__filename',
  'Buffer',
  'clearImmediate',
  'setImmediate',
  'clearInterval',
  'clearTimeout',
  'setInterval',
  'setTimeout',
]);

/**
 * Sandbox Restrictions Manager
 */
export class SandboxRestrictions {
  /**
   * @param {Object} [config] - Sandbox configuration
   */
  constructor(config = {}) {
    this.config = SandboxConfigSchema.parse(config);
    this.iterationCount = 0;
    this.startTime = null;
  }

  /**
   * Create a restricted context for hook execution
   * @returns {Object} Restricted context object
   */
  createRestrictedContext() {
    const context = {
      // Allow safe Math operations
      Math: Math,

      // Allow safe JSON operations
      JSON: JSON,

      // Allow safe Date operations (read-only)
      Date: Date,

      // Allow safe String/Number/Boolean/Array operations
      String: String,
      Number: Number,
      Boolean: Boolean,
      Array: Array,
      Object: Object,

      // Logging (safe, write-only)
      console: {
        log: (...args) => console.log('[Sandboxed]', ...args),
        error: (...args) => console.error('[Sandboxed]', ...args),
        warn: (...args) => console.warn('[Sandboxed]', ...args),
      },

      // Block dangerous functions
      eval: undefined,
      Function: undefined,
      require: undefined,
      import: undefined,
      process: undefined,
      global: undefined,
      _dirname: undefined,
      __filename: undefined,
      Buffer: undefined,

      // Block timers (DoS prevention)
      setTimeout: undefined,
      setInterval: undefined,
      setImmediate: undefined,
      clearTimeout: undefined,
      clearInterval: undefined,
      clearImmediate: undefined,
    };

    // Freeze context to prevent modification
    return Object.freeze(context);
  }

  /**
   * Validate hook function code for dangerous patterns
   * @param {Function} hookFn - Hook function to validate
   * @returns {Object} Validation result { valid, violations }
   */
  validateHookCode(hookFn) {
    if (typeof hookFn !== 'function') {
      return {
        valid: false,
        violations: ['Hook must be a function'],
      };
    }

    const codeString = hookFn.toString();
    const violations = [];

    // Check for blocked module requires
    for (const moduleName of BLOCKED_MODULES) {
      if (codeString.includes(`require('${moduleName}')`)) {
        violations.push(`Blocked module access: ${moduleName}`);
      }
      if (codeString.includes(`require("${moduleName}")`)) {
        violations.push(`Blocked module access: ${moduleName}`);
      }
      if (codeString.includes(`from '${moduleName}'`)) {
        violations.push(`Blocked module import: ${moduleName}`);
      }
    }

    // Check for blocked globals
    for (const globalName of BLOCKED_GLOBALS) {
      // Use word boundaries to avoid false positives
      const pattern = new RegExp(`\\b${globalName}\\b`, 'g');
      if (pattern.test(codeString)) {
        violations.push(`Blocked global access: ${globalName}`);
      }
    }

    // Check for file system access patterns
    if (!this.config.allowFileSystem) {
      if (/\bfs\./g.test(codeString) || /readFileSync|writeFileSync/g.test(codeString)) {
        violations.push('File system access not allowed');
      }
    }

    // Check for network access patterns
    if (!this.config.allowNetwork) {
      if (/\bfetch\(|XMLHttpRequest|WebSocket/g.test(codeString)) {
        violations.push('Network access not allowed');
      }
    }

    // Check for process access
    if (!this.config.allowProcessAccess) {
      if (/\bprocess\./g.test(codeString)) {
        violations.push('Process access not allowed');
      }
    }

    // Check for eval usage
    if (!this.config.allowEval) {
      if (/\beval\(|new Function\(/g.test(codeString)) {
        violations.push('Dynamic code evaluation not allowed');
      }
    }

    return {
      valid: violations.length === 0,
      violations,
    };
  }

  /**
   * Execute a hook function with restrictions
   * @param {Function} hookFn - Hook function to execute
   * @param {Object} event - Event object
   * @returns {Promise<Object>} Execution result
   */
  async executeRestricted(hookFn, event) {
    // Validate code before execution
    const validation = this.validateHookCode(hookFn);
    if (!validation.valid) {
      return {
        success: false,
        error: `Security validation failed: ${validation.violations.join(', ')}`,
      };
    }

    this.startTime = Date.now();
    this.iterationCount = 0;

    try {
      // Create restricted context
      const restrictedContext = this.createRestrictedContext();

      // Execute with timeout
      const timeoutPromise = new Promise((_, reject) => {
        setTimeout(() => {
          reject(new Error(`Execution timeout after ${this.config.timeoutMs}ms`));
        }, this.config.timeoutMs);
      });

      // Wrap hook function to prevent context mutation
      const wrappedFn = async () => {
        try {
          // Prevent event mutation by freezing
          const frozenEvent = this._deepFreeze({ ...event });

          // Execute in restricted context
          const result = await hookFn.call(restrictedContext, frozenEvent);

          // Check iteration limit
          if (this.iterationCount > this.config.maxIterations) {
            throw new Error('Maximum iteration count exceeded');
          }

          return result;
        } catch (error) {
          // Block system access errors
          if (error.code === 'EACCES' || error.code === 'EPERM') {
            return {
              success: false,
              error: 'System access denied',
            };
          }
          throw error;
        }
      };

      const result = await Promise.race([wrappedFn(), timeoutPromise]);

      return result || { success: true };
    } catch (error) {
      if (error.message.includes('timeout')) {
        return {
          success: false,
          error: 'Execution timeout exceeded',
        };
      }

      if (error.message.includes('iteration')) {
        return {
          success: false,
          error: 'Maximum iteration count exceeded',
        };
      }

      return {
        success: false,
        error: error.message || 'Hook execution failed',
      };
    }
  }

  /**
   * Deep freeze an object to prevent mutation
   * @param {Object} obj - Object to freeze
   * @returns {Object} Frozen object
   * @private
   */
  _deepFreeze(obj) {
    if (obj === null || typeof obj !== 'object') {
      return obj;
    }

    Object.freeze(obj);

    Object.getOwnPropertyNames(obj).forEach(prop => {
      if (obj[prop] !== null && typeof obj[prop] === 'object') {
        this._deepFreeze(obj[prop]);
      }
    });

    return obj;
  }
}

/**
 * Create sandbox restrictions instance
 * @param {Object} [config] - Sandbox configuration
 * @returns {SandboxRestrictions} Restrictions instance
 */
export function createSandboxRestrictions(config = {}) {
  return new SandboxRestrictions(config);
}

/**
 * Default sandbox restrictions (strict mode)
 */
export const defaultSandboxRestrictions = new SandboxRestrictions();
