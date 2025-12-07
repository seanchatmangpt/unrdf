/**
 * AtomVM Node.js Runtime
 *
 * Handles loading and execution of AtomVM in Node.js environment.
 * Uses AtomVM-node-<version>.js for execution.
 * 
 * **Poka-Yoke Design**: Uses state machine pattern to prevent invalid operations.
 * Invalid states are unrepresentable - operations can only be called in valid states.
 *
 * @module node-runtime
 */

import { createRequire } from 'module';
import { fileURLToPath } from 'url';
import { dirname, join, resolve } from 'path';
import { readFileSync } from 'fs';
import { trace } from '@opentelemetry/api';
import { startRoundtrip, endRoundtrip, getSLAStats, OPERATION_TYPES } from './roundtrip-sla.mjs';

// Get tracer lazily to ensure provider is registered first
function getTracer() {
  return trace.getTracer('atomvm-node-runtime');
}

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const require = createRequire(import.meta.url);

const ATOMVM_VERSION = 'v0.6.6';

/** @constant {number} Exit code indicating successful execution */
const SUCCESS_EXIT_CODE = 0;

/**
 * Runtime state machine states
 * 
 * **Poka-Yoke**: Enum prevents invalid states. Cannot be in multiple states simultaneously.
 * Valid transitions:
 * - Uninitialized => Loading => Ready
 * - Uninitialized => Loading => Error
 * - Ready => Executing => Ready
 * - Any => Destroyed (terminal state)
 * 
 * @typedef {'Uninitialized' | 'Loading' | 'Ready' | 'Executing' | 'Error' | 'Destroyed'} NodeRuntimeState
 */

/**
 * Validates that a string is non-empty
 * 
 * **Poka-Yoke**: Prevents empty/null/undefined strings from being used
 * 
 * @param {string} value - String to validate
 * @param {string} name - Name of parameter for error message
 * @returns {string} Validated non-empty string
 * @throws {Error} If value is empty, null, or undefined
 */
function validateNonEmptyString(value, name) {
  if (typeof value !== 'string' || value.trim().length === 0) {
    throw new Error(`${name} is required and must be a non-empty string`);
  }
  return value;
}

/**
 * AtomVM Node.js Runtime class
 * 
 * **Poka-Yoke Design**:
 * - State machine prevents invalid operations (cannot execute before load)
 * - Validation prevents invalid inputs (non-empty avmPath)
 * - Type guards ensure state consistency
 */
export class AtomVMNodeRuntime {
  /**
   * @param {Object} [options] - Runtime options
   * @param {Function} [options.log] - Logging function
   * @param {Function} [options.errorLog] - Error logging function
   */
  constructor(options = {}) {
    this.log = options.log || console.log;
    this.errorLog = options.errorLog || console.error;
    this.atomvmPath = null;
    
    // Poka-yoke: State machine prevents invalid operations
    /** @type {NodeRuntimeState} */
    this.state = 'Uninitialized';
  }

  /**
   * Type guard: Check if runtime is ready for operations
   * 
   * **Poka-Yoke**: Prevents operations in invalid states
   * 
   * @returns {boolean} True if runtime is ready
   */
  isReady() {
    return this.state === 'Ready' && this.atomvmPath !== null;
  }

  /**
   * Type guard: Check if runtime is loaded
   * 
   * **Poka-yoke**: Ensures state consistency
   * 
   * @returns {boolean} True if runtime is loaded
   */
  isLoaded() {
    return this.state === 'Ready' || this.state === 'Executing';
  }

  /**
   * Load AtomVM Node.js module
   * 
   * **Poka-Yoke**: State machine prevents multiple loads and loads after destroy
   *
   * @returns {Promise<void>}
   * @throws {Error} If state is invalid or file not found
   */
  async load() {
    return getTracer().startActiveSpan('atomvm.load_wasm', {
      attributes: {
        'runtime.type': 'node',
        'atomvm.version': ATOMVM_VERSION
      }
    }, async (span) => {
    // Poka-yoke: State machine prevents invalid operations
    if (this.state === 'Destroyed') {
      throw new Error('Cannot load AtomVM: Runtime has been destroyed');
    }
    if (this.state === 'Ready' || this.state === 'Executing') {
      this.log('AtomVM already loaded');
      return;
    }
    if (this.state === 'Loading') {
      throw new Error('AtomVM load already in progress');
    }

    // Transition to Loading state
    this.state = 'Loading';

    const publicDir = resolve(__dirname, '../public');
    const atomvmFile = join(publicDir, `AtomVM-node-${ATOMVM_VERSION}.js`);
    
    let stats;
    try {
      stats = require('fs').statSync(atomvmFile);
    } catch (error) {
      this.state = 'Error';
      span.setStatus({ code: 2, message: error.message });
      span.end();
      if (error.code === 'ENOENT') {
        throw new Error(`AtomVM file not found: ${atomvmFile}`);
      }
      throw new Error(`Failed to access AtomVM file ${atomvmFile}: ${error.message}`);
    }
    
    if (!stats.isFile()) {
      this.state = 'Error';
      span.setStatus({ code: 2, message: 'Path is not a file' });
      span.end();
      throw new Error(`AtomVM path is not a file: ${atomvmFile}`);
    }
    
    // Poka-yoke: State transition ensures consistency
    this.atomvmPath = atomvmFile;
    this.state = 'Ready';
    span.setAttribute('runtime.state', this.state);
    span.setStatus({ code: 1 }); // OK
    span.end();
    this.log(`Found AtomVM: AtomVM-node-${ATOMVM_VERSION}.js`);
    });
  }

  /**
   * Execute .avm file
   * 
   * **Poka-Yoke**: State machine and validation prevent invalid operations
   *
   * @param {string} avmPath - Path to .avm file to execute (required, non-empty)
   * @returns {Promise<any>} Execution result
   * @throws {Error} If runtime is not ready or avmPath is invalid
   */
  async execute(avmPath) {
    // Poka-yoke: State check prevents invalid operations
    if (!this.isReady()) {
      throw new Error(`Runtime not ready. Current state: ${this.state}. Call load() first.`);
    }

    // Poka-yoke: Validation prevents invalid inputs
    validateNonEmptyString(avmPath, 'avmPath');

    // Transition to Executing state
    this.state = 'Executing';

    return new Promise((resolve, reject) => {
      try {
        // Use Node.js child_process to execute AtomVM
        const { spawn } = require('child_process');
        
        this.log(`Executing: node ${this.atomvmPath} ${avmPath}`);

        let atomvm;
        try {
          atomvm = spawn('node', [this.atomvmPath, avmPath], {
            stdio: ['inherit', 'pipe', 'pipe']
          });
        } catch (error) {
          if (error.code === 'ENOENT') {
            throw new Error(`node command not found in PATH`);
          }
          throw error;
        }

        let stdout = '';
        let stderr = '';

        atomvm.stdout.on('data', (data) => {
          const text = data.toString();
          stdout += text;
          this.log(text.trim());
        });

        atomvm.stderr.on('data', (data) => {
          const text = data.toString();
          stderr += text;
          this.errorLog(text.trim());
        });

        atomvm.on('close', (code) => {
          if (code === SUCCESS_EXIT_CODE) {
            // Poka-yoke: State transition back to Ready after execution
            this.state = 'Ready';
            resolve({
              status: 'ok',
              exitCode: SUCCESS_EXIT_CODE,
              stdout,
              stderr
            });
          } else {
            // Poka-yoke: State transition to Error on failure
            this.state = 'Error';
            reject(new Error(`AtomVM exited with code ${code}\n${stderr}`));
          }
        });

        atomvm.on('error', (error) => {
          // Poka-yoke: State transition to Error on exception
          this.state = 'Error';
          reject(new Error(`Failed to execute AtomVM: ${error.message}`));
        });

      } catch (error) {
        // Poka-yoke: State transition to Error on exception
        this.state = 'Error';
        reject(error);
      }
    });
  }

  /**
   * Clean up resources
   * 
   * **Poka-Yoke**: Terminal state prevents further operations
   */
  destroy() {
    // Poka-yoke: Terminal state prevents further operations
    this.state = 'Destroyed';
    this.atomvmPath = null;
    this.log('Runtime destroyed');
  }
}

