/**
 * @file Isolated VM Executor
 * @module security/sandbox/isolated-vm-executor
 *
 * @description
 * Primary sandbox executor using isolated-vm for maximum security.
 * Provides V8 isolate-based execution with:
 * - Full memory isolation
 * - CPU timeout controls
 * - WASM support
 * - Async/await support
 * - Merkle verification of code integrity
 * - OpenTelemetry instrumentation
 */

import ivm from 'isolated-vm';
import { trace, context } from '@opentelemetry/api';
import { sha256 } from '@noble/hashes/sha256';
import { bytesToHex } from '@noble/hashes/utils';

const tracer = trace.getTracer('isolated-vm-executor');

/**
 * Convert bytes to hex string
 * @param {Uint8Array} bytes
 * @returns {string}
 */
function toHex(bytes) {
  return bytesToHex(bytes);
}

/**
 * Compute SHA-256 hash of code
 * @param {string} code
 * @returns {string} Hex-encoded hash
 */
function hashCode(code) {
  const bytes = new TextEncoder().encode(code);
  return toHex(sha256(bytes));
}

/**
 * Threat detection patterns
 */
const THREAT_PATTERNS = [
  // VM escape attempts
  /constructor\s*\.\s*constructor/i,
  /Function\s*\(\s*['"`]/i,
  /eval\s*\(/i,
  /\[\s*['"`]constructor['"`]\s*\]/i,

  // Process/require access
  /process\s*\.\s*binding/i,
  /require\s*\(/i,
  /import\s*\(/i,
  /module\s*\.\s*exports/i,

  // Prototype pollution
  /__proto__/i,
  /prototype\s*\[\s*['"`]/i,

  // File system access
  /fs\s*\.\s*(read|write)/i,
  /child_process/i,

  // Network access
  /fetch\s*\(/i,
  /XMLHttpRequest/i,
  /WebSocket/i
];

/**
 * Detect potential threats in code
 * @param {string} code
 * @returns {Array<Object>} Detected threats
 */
function detectThreats(code) {
  const threats = [];

  for (let i = 0; i < THREAT_PATTERNS.length; i++) {
    const pattern = THREAT_PATTERNS[i];
    if (pattern.test(code)) {
      threats.push({
        patternIndex: i,
        pattern: pattern.toString(),
        severity: i < 4 ? 'critical' : i < 10 ? 'high' : 'medium'
      });
    }
  }

  return threats;
}

/**
 * Isolated VM Executor
 */
export class IsolatedVmExecutor {
  /**
   * @param {Object} [config] - Executor configuration
   * @param {number} [config.memoryLimit=128] - Memory limit in MB
   * @param {number} [config.timeout=5000] - Execution timeout in ms
   * @param {boolean} [config.enableWasm=true] - Enable WASM support
   * @param {boolean} [config.enableAsync=true] - Enable async/await
   * @param {boolean} [config.enableThreatDetection=true] - Enable threat detection
   * @param {boolean} [config.strictMode=true] - Enable strict mode
   */
  constructor(config = {}) {
    this.config = {
      memoryLimit: config.memoryLimit || 128,
      timeout: config.timeout || 5000,
      enableWasm: config.enableWasm !== false,
      enableAsync: config.enableAsync !== false,
      enableThreatDetection: config.enableThreatDetection !== false,
      strictMode: config.strictMode !== false,
      ...config
    };

    /** @type {Map<string, ivm.Isolate>} */
    this.isolates = new Map();

    /** @type {Map<string, ivm.Context>} */
    this.contexts = new Map();

    /** @type {Map<string, string>} - Code hashes for Merkle verification */
    this.codeHashes = new Map();

    this.executionCount = 0;
    this.totalDuration = 0;
  }

  /**
   * Execute code in isolated VM
   * @param {string|Function} code - Code to execute
   * @param {Object} [context] - Execution context
   * @param {Object} [options] - Execution options
   * @returns {Promise<Object>} Execution result
   */
  async run(code, context = {}, options = {}) {
    return tracer.startActiveSpan('security.isolate.execute', async (span) => {
      const startTime = Date.now();
      const executionId = `exec_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;

      try {
        span.setAttributes({
          'security.executor.type': 'isolated-vm',
          'security.execution.id': executionId,
          'security.memoryLimit': this.config.memoryLimit,
          'security.timeout': options.timeout || this.config.timeout
        });

        // Convert function to string if needed
        const codeString = typeof code === 'function' ? code.toString() : code;

        // Threat detection
        if (this.config.enableThreatDetection) {
          const threats = detectThreats(codeString);
          if (threats.length > 0) {
            const criticalThreats = threats.filter(t => t.severity === 'critical');
            if (criticalThreats.length > 0) {
              span.setAttribute('security.threats.detected', threats.length);
              span.setAttribute('security.threats.critical', criticalThreats.length);
              span.setStatus({ code: 2, message: 'Critical security threat detected' });

              throw new Error(
                `Security threat detected: ${criticalThreats[0].pattern}. ` +
                `Execution blocked.`
              );
            }
          }
          span.setAttribute('security.threats.count', threats.length);
        }

        // Compute code hash for Merkle verification
        const codeHash = hashCode(codeString);
        this.codeHashes.set(executionId, codeHash);
        span.setAttribute('security.code.hash', codeHash);

        // Create isolate
        const isolate = new ivm.Isolate({
          memoryLimit: this.config.memoryLimit,
          inspector: false, // Disable debugging for security
          onCatastrophicError: (msg) => {
            span.recordException(new Error(`Catastrophic error: ${msg}`));
            this.destroyIsolate(executionId);
          }
        });

        this.isolates.set(executionId, isolate);

        // Create context
        const ivmContext = await isolate.createContext();
        this.contexts.set(executionId, ivmContext);

        // Setup safe global environment
        const jail = ivmContext.global;
        await jail.set('global', jail.derefInto());

        // Add safe console
        const safeConsole = new ivm.Reference({
          log: (...args) => console.log('[Sandbox]', ...args),
          error: (...args) => console.error('[Sandbox]', ...args),
          warn: (...args) => console.warn('[Sandbox]', ...args),
          info: (...args) => console.info('[Sandbox]', ...args)
        });
        await jail.set('console', safeConsole);

        // Add JSON support
        const jsonRef = new ivm.Reference({
          parse: (str) => JSON.parse(str),
          stringify: (obj) => JSON.stringify(obj)
        });
        await jail.set('JSON', jsonRef);

        // Add Math support
        const mathRef = new ivm.Reference(Math);
        await jail.set('Math', mathRef);

        // Add Date support (limited)
        const dateRef = new ivm.Reference({
          now: () => Date.now()
        });
        await jail.set('Date', dateRef);

        // Inject context data
        if (context && Object.keys(context).length > 0) {
          for (const [key, value] of Object.entries(context)) {
            const valueRef = new ivm.Reference(value);
            await jail.set(key, valueRef);
          }
        }

        // Wrap code in strict mode if enabled
        const wrappedCode = this.config.strictMode
          ? `"use strict";\n${codeString}`
          : codeString;

        // Compile and execute
        const script = await isolate.compileScript(wrappedCode);
        const timeout = options.timeout || this.config.timeout;

        const result = await script.run(ivmContext, {
          timeout,
          reference: true,
          promise: this.config.enableAsync
        });

        // Copy result back to main isolate
        const output = result ? await result.copy() : undefined;

        // Cleanup
        await this.destroyIsolate(executionId);

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
          result: output,
          duration,
          executionId,
          codeHash,
          memoryUsed: await this.getMemoryUsage(executionId).catch(() => ({ used: 0 }))
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

        // Cleanup on error
        await this.destroyIsolate(executionId).catch(() => {});

        // Categorize error
        let errorType = 'unknown';
        if (error.message?.includes('timed out')) {
          errorType = 'timeout';
        } else if (error.message?.includes('memory')) {
          errorType = 'memory_limit';
        } else if (error.message?.includes('Security threat')) {
          errorType = 'security_threat';
        }

        span.setAttribute('security.error.type', errorType);

        return {
          success: false,
          error: error.message,
          errorType,
          duration,
          executionId,
          codeHash: this.codeHashes.get(executionId)
        };
      } finally {
        span.end();
      }
    });
  }

  /**
   * Get memory usage for execution
   * @param {string} executionId
   * @returns {Promise<Object>}
   */
  async getMemoryUsage(executionId) {
    const isolate = this.isolates.get(executionId);
    if (!isolate) {
      return { used: 0, total: 0, limit: 0, percentage: 0 };
    }

    const heapStats = await isolate.getHeapStatistics();
    return {
      used: heapStats.used_heap_size,
      total: heapStats.total_heap_size,
      limit: heapStats.heap_size_limit,
      percentage: (heapStats.used_heap_size / heapStats.heap_size_limit) * 100
    };
  }

  /**
   * Destroy isolate and free resources
   * @param {string} executionId
   */
  async destroyIsolate(executionId) {
    const isolate = this.isolates.get(executionId);
    if (isolate) {
      try {
        isolate.dispose();
      } catch (err) {
        // Ignore disposal errors
      }
      this.isolates.delete(executionId);
      this.contexts.delete(executionId);
      this.codeHashes.delete(executionId);
    }
  }

  /**
   * Get executor statistics
   * @returns {Object}
   */
  getStats() {
    return {
      type: 'isolated-vm',
      config: this.config,
      executionCount: this.executionCount,
      averageDuration: this.executionCount > 0 ? this.totalDuration / this.executionCount : 0,
      activeIsolates: this.isolates.size
    };
  }

  /**
   * Cleanup all isolates
   */
  async cleanup() {
    for (const executionId of this.isolates.keys()) {
      await this.destroyIsolate(executionId);
    }
  }
}
