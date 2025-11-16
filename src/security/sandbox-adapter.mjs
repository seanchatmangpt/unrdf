/**
 * Sandbox adapter to abstract execution engine (isolated-vm preferred, vm2 deprecated).
 * Automatically detects and uses the best available executor.
 */
import { detectBestExecutor, createExecutor } from './sandbox/detector.mjs';

export class SandboxAdapter {
  /**
   * @param {Object} [options]
   * @param {string} [options.engine] - Force specific engine ('isolated-vm', 'worker', 'vm2', 'browser')
   * @param {number} [options.timeoutMs] - Execution timeout in milliseconds
   * @param {number} [options.memoryLimit] - Memory limit in MB
   * @param {Object} [options.sandbox] - Sandbox context
   * @param {boolean} [options.strictMode] - Enable strict mode
   */
  constructor(options = {}) {
    this.options = {
      timeoutMs: options.timeoutMs || 1000,
      memoryLimit: options.memoryLimit || 128,
      sandbox: options.sandbox || {},
      strictMode: options.strictMode !== false,
      ...options
    };

    this.engine = options.engine || null;
    this.executor = null;
    this.initialized = false;
  }

  /**
   * Initialize executor (lazy initialization)
   * @private
   */
  async _initialize() {
    if (this.initialized) return;

    // Detect best executor if not explicitly set
    if (!this.engine) {
      this.engine = await detectBestExecutor({
        preferIsolatedVm: true,
        allowVm2: process.env.UNRDF_ALLOW_VM2 === '1', // Only allow if explicitly enabled
        allowBrowser: true
      });
    }

    // Create executor instance
    this.executor = await createExecutor(this.engine, {
      timeout: this.options.timeoutMs,
      memoryLimit: this.options.memoryLimit,
      strictMode: this.options.strictMode
    });

    this.initialized = true;
  }

  /**
   * Execute untrusted code and return result.
   * @param {string} code
   * @returns {Promise<any>}
   */
  async run(code) {
    await this._initialize();

    const result = await this.executor.run(code, this.options.sandbox, {
      timeout: this.options.timeoutMs
    });

    if (!result.success) {
      throw new Error(result.error || 'Sandbox execution failed');
    }

    return result.result;
  }

  /**
   * Get executor type
   * @returns {string}
   */
  getEngine() {
    return this.engine;
  }

  /**
   * Get executor statistics
   * @returns {Object}
   */
  getStats() {
    if (!this.executor) {
      return { engine: this.engine, initialized: false };
    }
    return this.executor.getStats();
  }

  /**
   * Cleanup executor resources
   */
  async cleanup() {
    if (this.executor && this.executor.cleanup) {
      await this.executor.cleanup();
    }
  }
}

/**
 * Create sandbox adapter with auto-detection
 * @param {Object} [options]
 * @returns {SandboxAdapter}
 */
export function createSandboxAdapter(options = {}) {
  return new SandboxAdapter(options);
}


