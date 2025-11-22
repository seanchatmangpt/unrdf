/**
 * @file VM2 Executor (Legacy - Deprecated)
 * @module security/sandbox/vm2-executor
 *
 * @description
 * Legacy sandbox executor using vm2 (DEPRECATED).
 *
 * ⚠️ SECURITY WARNING ⚠️
 * This executor is DEPRECATED and has known security vulnerabilities.
 * It should only be used for backward compatibility with existing code.
 * Migrate to isolated-vm or worker executors as soon as possible.
 *
 * Known issues:
 * - VM escape vulnerabilities
 * - Prototype pollution risks
 * - No memory isolation
 * - No CPU isolation
 *
 * @deprecated Use IsolatedVmExecutor or WorkerExecutor instead
 */

import { trace } from '@opentelemetry/api';

const tracer = trace.getTracer('vm2-executor');

// Show deprecation warning on first import
console.warn(
  '\n' +
    '⚠️  ========================================= ⚠️\n' +
    '⚠️  VM2 EXECUTOR DEPRECATION WARNING          ⚠️\n' +
    '⚠️  ========================================= ⚠️\n' +
    '\n' +
    'The vm2 package is deprecated and contains critical security vulnerabilities.\n' +
    'This executor should NOT be used in production environments.\n' +
    '\n' +
    'Migrate to:\n' +
    '  - IsolatedVmExecutor (recommended): Full V8 isolation\n' +
    '  - WorkerExecutor: Process-level isolation\n' +
    '\n' +
    'For more information:\n' +
    '  https://github.com/patriksimek/vm2/issues/533\n' +
    '\n' +
    'To suppress this warning, set UNRDF_SUPPRESS_VM2_WARNING=1\n' +
    '⚠️  ========================================= ⚠️\n'
);

/**
 * VM2 Executor (Deprecated)
 * @deprecated
 */
export class Vm2Executor {
  /**
   * @param {Object} [config] - Executor configuration
   * @param {number} [config.timeout=5000] - Execution timeout in ms
   * @param {Array<string>} [config.allowedModules] - Allowed modules
   * @param {boolean} [config.strictMode=true] - Enable strict mode
   */
  constructor(config = {}) {
    this.config = {
      timeout: config.timeout || 5000,
      allowedModules: config.allowedModules || [],
      strictMode: config.strictMode !== false,
      ...config,
    };

    this.executionCount = 0;
    this.totalDuration = 0;

    // Show warning on each instantiation unless suppressed
    if (!process.env.UNRDF_SUPPRESS_VM2_WARNING) {
      console.warn(
        '[DEPRECATION] Creating vm2 executor instance - consider migrating to isolated-vm'
      );
    }
  }

  /**
   * Execute code in vm2
   * @param {string|Function} code - Code to execute
   * @param {Object} [context] - Execution context
   * @param {Object} [options] - Execution options
   * @returns {Promise<Object>} Execution result
   */
  async run(code, context = {}, options = {}) {
    return tracer.startActiveSpan('security.vm2.execute', async span => {
      const startTime = Date.now();

      try {
        span.setAttributes({
          'security.executor.type': 'vm2',
          'security.executor.deprecated': true,
          'security.executor.security_level': 'low',
          'security.timeout': options.timeout || this.config.timeout,
        });

        // Dynamic import of vm2
        let VM;
        try {
          const vm2Module = await import('vm2');
          VM = vm2Module.VM;
        } catch (err) {
          throw new Error('vm2 not available - install with: pnpm add vm2');
        }

        // Convert function to string if needed
        const codeString = typeof code === 'function' ? code.toString() : code;

        // Create VM instance
        const vm = new VM({
          timeout: options.timeout || this.config.timeout,
          sandbox: {
            ...this._createSandboxGlobals(context),
            console: this._createSafeConsole(),
          },
          eval: false,
          wasm: false,
        });

        // Wrap code in strict mode if enabled
        const wrappedCode = this.config.strictMode
          ? `"use strict";\n(function() { ${codeString} })()`
          : `(function() { ${codeString} })()`;

        // Execute
        const result = vm.run(wrappedCode);

        const duration = Date.now() - startTime;
        this.executionCount++;
        this.totalDuration += duration;

        span.setAttributes({
          'security.execution.duration': duration,
          'security.execution.success': true,
        });
        span.setStatus({ code: 1 }); // OK

        return {
          success: true,
          result,
          duration,
          executorType: 'vm2',
          deprecated: true,
        };
      } catch (error) {
        const duration = Date.now() - startTime;

        span.recordException(error);
        span.setAttributes({
          'security.execution.duration': duration,
          'security.execution.success': false,
          'security.error.message': error.message,
        });
        span.setStatus({ code: 2, message: error.message });

        return {
          success: false,
          error: error.message,
          duration,
          executorType: 'vm2',
          deprecated: true,
        };
      } finally {
        span.end();
      }
    });
  }

  /**
   * Create sandbox globals
   * @param {Object} context
   * @returns {Object}
   * @private
   */
  _createSandboxGlobals(context) {
    return {
      Date: { now: () => Date.now() },
      Math,
      JSON,
      ...context,
    };
  }

  /**
   * Create safe console
   * @returns {Object}
   * @private
   */
  _createSafeConsole() {
    return {
      log: (...args) => console.log('[VM2]', ...args),
      error: (...args) => console.error('[VM2]', ...args),
      warn: (...args) => console.warn('[VM2]', ...args),
      info: (...args) => console.info('[VM2]', ...args),
    };
  }

  /**
   * Get executor statistics
   * @returns {Object}
   */
  getStats() {
    return {
      type: 'vm2',
      deprecated: true,
      config: this.config,
      executionCount: this.executionCount,
      averageDuration: this.executionCount > 0 ? this.totalDuration / this.executionCount : 0,
    };
  }

  /**
   * Cleanup (no-op for vm2)
   */
  async cleanup() {
    // vm2 doesn't require cleanup
  }
}
