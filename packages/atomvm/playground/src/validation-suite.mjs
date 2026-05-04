/**
 * Validation Suite Orchestrator
 *
 * **Big Bang 80/20**: Validates the 20% that proves 80% of system works:
 * - Process lifecycle (spawn, message, crash, restart)
 * - State machine integrity (all transitions valid)
 * - KGC-4D event integration (events reach 4D engine)
 * - Dual runtime validation (browser + Node.js both work)
 *
 * @module validation-suite
 */

import { ProcessValidator } from './process-validator.mjs';
import { SupervisionValidator } from './supervision-validator.mjs';
import { KGC4DValidator } from './kgc4d-validator.mjs';
import { RuntimeValidator } from './runtime-validator.mjs';

/**
 * Validation result
 *
 * @typedef {Object} ValidationResult
 * @property {string} name - Validation name
 * @property {'pending' | 'running' | 'pass' | 'fail'} status - Validation status
 * @property {string} [message] - Result message
 * @property {Error} [error] - Error if validation failed
 */

/**
 * Validation suite orchestrator
 *
 * **Poka-Yoke**: All validations must pass (system cannot fail)
 */
export class ValidationSuite {
  /**
   * @param {Object} options - Suite options
   * @param {Function} [options.log] - Logging function
   * @param {Function} [options.onResult] - Callback for validation results
   */
  constructor(options = {}) {
    this.log = options.log || console.log;
    this.onResult = options.onResult || (() => {});
    this.results = [];

    // Initialize validators
    this.processValidator = new ProcessValidator({ log: this.log });
    this.supervisionValidator = new SupervisionValidator({ log: this.log });
    this.kgc4dValidator = new KGC4DValidator({ log: this.log });
    this.runtimeValidator = new RuntimeValidator({ log: this.log });
  }

  /**
   * Run all validations
   *
   * **80/20 Core Validations**:
   * 1. Process lifecycle (spawn, message, crash, restart)
   * 2. State machine integrity (all transitions valid)
   * 3. KGC-4D event integration (events reach 4D engine)
   * 4. Dual runtime validation (browser + Node.js both work)
   *
   * @param {string} moduleName - Module name to validate
   * @returns {Promise<ValidationResult[]>} Validation results
   */
  async runAll(moduleName) {
    if (!moduleName || typeof moduleName !== 'string' || moduleName.trim().length === 0) {
      throw new Error('moduleName is required and must be a non-empty string');
    }

    this.results = [];
    this.log('Starting validation suite...');

    // 1. Process lifecycle validation
    await this.runValidation('Process Lifecycle', async () => {
      return await this.processValidator.validate(moduleName);
    });

    // 2. State machine integrity validation
    await this.runValidation('State Machine Integrity', async () => {
      return await this.runtimeValidator.validateStateMachine(moduleName);
    });

    // 3. KGC-4D event integration validation
    await this.runValidation('KGC-4D Event Integration', async () => {
      return await this.kgc4dValidator.validate(moduleName);
    });

    // 4. Dual runtime validation
    await this.runValidation('Dual Runtime (Browser + Node.js)', async () => {
      return await this.runtimeValidator.validateDualRuntime(moduleName);
    });

    // 5. Supervision validation
    await this.runValidation('Supervision Tree', async () => {
      return await this.supervisionValidator.validate(moduleName);
    });

    const allPassed = this.results.every(r => r.status === 'pass');
    this.log(`Validation suite complete: ${allPassed ? 'ALL PASSED' : 'SOME FAILED'}`);

    return this.results;
  }

  /**
   * Run a single validation
   *
   * @param {string} name - Validation name
   * @param {Function} validator - Validation function
   * @returns {Promise<void>}
   */
  async runValidation(name, validator) {
    const result = {
      name,
      status: 'running',
      message: 'Running...'
    };
    this.results.push(result);
    this.onResult(result);

    try {
      this.log(`Running validation: ${name}`);
      const validationResult = await validator();
      result.status = validationResult.passed ? 'pass' : 'fail';
      result.message = validationResult.message || (validationResult.passed ? 'Passed' : 'Failed');
      if (validationResult.error) {
        result.error = validationResult.error;
      }
    } catch (error) {
      result.status = 'fail';
      result.message = error.message || 'Validation failed';
      result.error = error;
      this.log(`Validation failed: ${name} - ${error.message}`);
    }

    this.onResult(result);
  }

  /**
   * Get validation results
   *
   * @returns {ValidationResult[]} Current results
   */
  getResults() {
    return this.results;
  }

  /**
   * Clear all results
   */
  clear() {
    this.results = [];
  }

  /**
   * POKA-YOKE: Check that module is built before validation
   * @param {string} moduleName - Module name
   * @private
   */
  async _checkModuleBuilt(moduleName) {
    const { existsSync } = await import('fs');
    const { join, resolve, dirname } = await import('path');
    const { fileURLToPath } = await import('url');

    // Determine if we're in browser or Node.js
    const isNode = typeof process !== 'undefined' && process.versions != null && process.versions.node != null;

    if (isNode) {
      const __filename = fileURLToPath(import.meta.url);
      const __dirname = dirname(__filename);
      const rootDir = resolve(__dirname, '../');
      const publicDir = join(rootDir, 'public');
      const avmFile = join(publicDir, `${moduleName}.avm`);

      if (!existsSync(avmFile)) {
        throw new Error(
          `Module not built: ${moduleName}.avm not found in ${publicDir}. ` +
          `Run: pnpm run build:erlang ${moduleName}`
        );
      }
    } else {
      // Browser: Check via fetch
      const avmPath = `/public/${moduleName}.avm`;
      try {
        const response = await fetch(avmPath, { method: 'HEAD' });
        if (!response.ok) {
          throw new Error(
            `Module not built: ${moduleName}.avm not found. ` +
            `Run: pnpm run build:erlang ${moduleName}`
          );
        }
      } catch (error) {
        throw new Error(
          `Module not built: ${moduleName}.avm not found. ` +
          `Run: pnpm run build:erlang ${moduleName}`
        );
      }
    }
  }
}

