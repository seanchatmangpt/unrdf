/**
 * AtomVM Runtime Manager
 *
 * Handles loading and execution of AtomVM WASM module.
 * Provides interface for running BEAM bytecode in the browser.
 *
 * **Poka-Yoke Design**: Uses state machine pattern to prevent invalid operations.
 * Invalid states are unrepresentable - operations can only be called in valid states.
 *
 * @module atomvm-runtime
 */

import { trace } from '@opentelemetry/api';
import { startRoundtrip, endRoundtrip, getSLAStats, OPERATION_TYPES } from './roundtrip-sla.mjs';

/* eslint-env browser */
// Get tracer lazily to ensure provider is registered first
function getTracer() {
  return trace.getTracer('atomvm-runtime');
}
const ATOMVM_VERSION = 'v0.6.6';

/** @constant {number} Delay before checking if Module is ready after script load */
const MODULE_LOAD_CHECK_DELAY_MS = 100;

/** @constant {number} Interval (ms) for polling Module initialization status */
const MODULE_INIT_CHECK_INTERVAL_MS = 50;

/** @constant {number} Maximum attempts to check Module initialization (10s total at 50ms intervals) */
const MODULE_INIT_MAX_ATTEMPTS = 200;

/** @constant {number} Delay (ms) before checking execution status */
const EXECUTION_STATUS_CHECK_DELAY_MS = 1000;

/** @constant {number} Maximum time (ms) to wait for execution to complete */
const EXECUTION_TIMEOUT_MS = 30000;

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
 * @typedef {'Uninitialized' | 'Loading' | 'Ready' | 'Executing' | 'Error' | 'Destroyed'} RuntimeState
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
 * AtomVM Runtime class
 *
 * **Poka-Yoke Design**:
 * - State machine prevents invalid operations (cannot execute before load)
 * - Validation prevents invalid inputs (non-empty moduleName)
 * - Type guards ensure state consistency
 */
export class AtomVMRuntime {
  /**
   * @param {Object} terminal - Terminal UI instance for logging
   * @param {string} moduleName - Name of module to execute (required, non-empty)
   */
  constructor(terminal, moduleName) {
    // Poka-yoke: Validate inputs at construction time
    validateNonEmptyString(moduleName, 'moduleName');

    this.terminal = terminal;
    this.atomvmModule = null;
    this.memory = null;
    this.moduleName = moduleName;

    // Poka-yoke: State machine prevents invalid operations
    /** @type {RuntimeState} */
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
    return this.state === 'Ready' && this.atomvmModule !== null;
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
   * Load AtomVM WASM module
   *
   * Loads the AtomVM.js script which contains the full Emscripten module.
   * The module will be available globally as `Module` after loading.
   *
   * **Poka-Yoke**: State machine prevents multiple loads and loads after destroy
   *
   * @returns {Promise<void>}
   * @throws {Error} If state is invalid or SharedArrayBuffer unavailable
   */
  async loadWASM() {
    // Poka-yoke: State machine prevents invalid operations
    if (this.state === 'Destroyed') {
      throw new Error('Cannot load WASM: Runtime has been destroyed');
    }
    if (this.state === 'Ready' || this.state === 'Executing') {
      this.terminal.log('WASM already loaded', 'info');
      return;
    }
    if (this.state === 'Loading') {
      throw new Error('WASM load already in progress');
    }

    // Transition to Loading state
    this.state = 'Loading';

    try {
      this.terminal.log('Checking SharedArrayBuffer support...', 'info');

      // Poka-yoke: Environment check prevents runtime errors
      if (typeof SharedArrayBuffer === 'undefined') {
        this.state = 'Error';
        const errorMsg =
          'SharedArrayBuffer not available. Cross-Origin-Isolation required. Page will reload automatically.';
        this.terminal.log(errorMsg, 'error');
        throw new Error(errorMsg);
      }

      this.terminal.log('SharedArrayBuffer confirmed available ✓', 'success');
      this.terminal.log('Loading AtomVM WASM module...', 'info');

      // Check if AtomVM.js is already loaded
      if (window.Module && window.Module.ready) {
        this.terminal.log('AtomVM module already loaded', 'info');
        this.atomvmModule = window.Module;
        // Poka-yoke: State transition ensures consistency
        this.state = 'Ready';
        return;
      }

      // Configure Module before loading (Emscripten pattern)
      // Create Module configuration object
      window.Module = window.Module || {};

      // Configure output capture before script loads
      const originalPrint =
        window.Module.print ||
        (text => {
          if (typeof console !== 'undefined' && console.log) {
            console.log(text);
          }
        });
      const originalPrintErr =
        window.Module.printErr ||
        (text => {
          if (typeof console !== 'undefined' && console.error) {
            console.error(text);
          }
        });

      window.Module.print = text => {
        originalPrint(text);
        this.terminal.log(String(text).trim(), 'info');
      };

      window.Module.printErr = text => {
        originalPrintErr(text);
        this.terminal.log(String(text).trim(), 'error');
      };

      // Prevent auto-run on initial load
      window.Module.noInitialRun = true;
      window.Module.arguments = [];

      // Load AtomVM.js script dynamically
      return new Promise((resolve, reject) => {
        // Check if script already exists
        const existingScript = document.querySelector(
          `script[src*="AtomVM-web-${ATOMVM_VERSION}"]`
        );
        if (existingScript) {
          if (window.Module && (window.Module.ready || window.Module.calledRun)) {
            this.atomvmModule = window.Module;
            // Poka-yoke: State transition ensures consistency
            this.state = 'Ready';
            this.terminal.log('AtomVM WASM module already loaded ✓', 'success');
            resolve();
            return;
          }
          // Wait for existing script to load
          existingScript.addEventListener('load', () => {
            setTimeout(() => {
              if (window.Module) {
                this.atomvmModule = window.Module;
                // Poka-yoke: State transition ensures consistency
                this.state = 'Ready';
                this.terminal.log('AtomVM WASM module loaded ✓', 'success');
                resolve();
              } else {
                this.state = 'Error';
                reject(new Error('AtomVM module not available after script load'));
              }
            }, MODULE_LOAD_CHECK_DELAY_MS);
          });
          return;
        }

        // Create script element
        const script = document.createElement('script');
        script.src = `/AtomVM-web-${ATOMVM_VERSION}.js`;
        script.async = true;

        script.onload = () => {
          // Wait for Module to be initialized
          let attempts = 0;

          const checkModule = setInterval(() => {
            attempts++;
            if (window.Module && (window.Module.ready || window.Module.calledRun)) {
              clearInterval(checkModule);
              this.atomvmModule = window.Module;
              // Poka-yoke: State transition ensures consistency
              this.state = 'Ready';
              span.setAttribute('runtime.state', this.state);
              span.setStatus({ code: 1 }); // OK
              span.end();
              this.terminal.log('AtomVM WASM module loaded ✓', 'success');
              resolve();
            } else if (attempts >= MODULE_INIT_MAX_ATTEMPTS) {
              clearInterval(checkModule);
              this.state = 'Error';
              const timeoutSeconds =
                (MODULE_INIT_MAX_ATTEMPTS * MODULE_INIT_CHECK_INTERVAL_MS) / 1000;
              reject(
                new Error(
                  `AtomVM module failed to initialize within ${timeoutSeconds}s timeout. Script loaded but Module.ready never became true.`
                )
              );
            }
          }, MODULE_INIT_CHECK_INTERVAL_MS);
        };

        script.onerror = error => {
          this.state = 'Error';
          span.setStatus({ code: 2, message: error?.message || 'Network error' });
          span.end();
          reject(
            new Error(
              `Failed to load AtomVM.js script from ${script.src}: ${error?.message || 'Network error or file not found'}`
            )
          );
        };

        document.head.appendChild(script);
      });
    } catch (error) {
      // Poka-yoke: State transition on error
      this.state = 'Error';
      this.terminal.log(`WASM load error: ${error.message}`, 'error');
      throw error;
    }
  }

  /**
   * Run example BEAM code
   *
   * Loads and executes a test .avm file using real AtomVM execution.
   *
   * **Poka-Yoke**: State machine prevents execution before load
   *
   * @returns {Promise<void>}
   * @throws {Error} If runtime is not ready
   */
  async runExample() {
    // Poka-yoke: State check prevents invalid operations
    if (!this.isReady()) {
      throw new Error(`Runtime not ready. Current state: ${this.state}. Call loadWASM() first.`);
    }

    return getTracer().startActiveSpan(
      'atomvm.run_example',
      {
        attributes: {
          'module.name': this.moduleName,
          'runtime.type': 'browser',
        },
      },
      async span => {
        try {
          this.terminal.log('', 'info');
          this.terminal.log('Loading example .avm file...', 'info');

          const avmPath = `/${this.moduleName}.avm`;

          let response;
          try {
            response = await fetch(avmPath, { method: 'HEAD' });
          } catch (error) {
            span.setStatus({ code: 2, message: error.message });
            throw new Error(`Failed to fetch .avm file ${avmPath}: ${error.message}`);
          }

          if (!response.ok) {
            const errorMsg = `Module not found: ${this.moduleName}.avm. Build it with: pnpm run build:erlang ${this.moduleName}`;
            span.setStatus({ code: 2, message: errorMsg });
            this.terminal.log(errorMsg, 'error');
            throw new Error(errorMsg);
          }

          this.terminal.log(`Executing ${avmPath}...`, 'info');
          this.terminal.log('', 'info');

          const result = await this.executeBeam(avmPath);
          span.setStatus({ code: 1 }); // OK
          return result;
        } catch (error) {
          this.terminal.log(`Execution error: ${error.message}`, 'error');
          span.setStatus({ code: 2, message: error.message });
          throw error;
        } finally {
          span.end();
        }
      }
    );
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
  async executeBeam(avmPath) {
    // Poka-yoke: State check prevents invalid operations
    if (!this.isReady()) {
      throw new Error(`Runtime not ready. Current state: ${this.state}. Call loadWASM() first.`);
    }

    // Poka-yoke: Validation prevents invalid inputs
    validateNonEmptyString(avmPath, 'avmPath');

    // Transition to Executing state
    const fromState = this.state;
    this.state = 'Executing';

    return getTracer().startActiveSpan(
      'atomvm.execute_beam',
      {
        attributes: {
          'module.name': this.moduleName,
          'avm.path': avmPath,
          'runtime.type': 'browser',
          from_state: fromState,
          to_state: this.state,
        },
      },
      span => {
        this.terminal.log(`Executing ${avmPath}...`, 'info');

        return new Promise((resolve, reject) => {
          try {
            // Configure Module to run with the .avm file
            this.atomvmModule.arguments = [avmPath];

            // Set up completion handler
            let exitHandlerCalled = false;
            const originalOnExit = this.atomvmModule.onExit;
            this.atomvmModule.onExit = code => {
              exitHandlerCalled = true;
              if (originalOnExit) originalOnExit(code);
              if (code === SUCCESS_EXIT_CODE) {
                // Poka-yoke: State transition back to Ready after execution
                this.state = 'Ready';
                span.setAttribute('final_state', this.state);
                span.setStatus({ code: 1 }); // OK
                span.end();
                this.terminal.log('', 'info');
                this.terminal.log('✓ Execution completed successfully', 'success');
                resolve({ status: 'ok', exitCode: SUCCESS_EXIT_CODE });
              } else {
                // Poka-yoke: State transition to Error on failure
                this.state = 'Error';
                span.setStatus({ code: 2, message: `Exit code ${code}` });
                span.end();
                this.terminal.log(`Execution exited with code ${code}`, 'error');
                reject(new Error(`AtomVM exited with code ${code}`));
              }
            };

            if (!this.atomvmModule.callMain && !this.atomvmModule.run) {
              throw new Error('No execution method available in AtomVM module (callMain or run)');
            }

            if (this.atomvmModule.callMain) {
              this.atomvmModule.callMain([avmPath]);
            } else {
              this.atomvmModule.run();
            }

            setTimeout(() => {
              if (!exitHandlerCalled) {
                // Execution still in progress, keep Executing state
                span.end();
                resolve({ status: 'running', message: 'Execution in progress' });
              }
            }, EXECUTION_STATUS_CHECK_DELAY_MS);

            setTimeout(() => {
              if (!exitHandlerCalled) {
                // Poka-yoke: State transition to Error on timeout
                this.state = 'Error';
                const timeoutSeconds = EXECUTION_TIMEOUT_MS / 1000;
                span.setStatus({ code: 2, message: `Timeout after ${timeoutSeconds}s` });
                span.end();
                reject(
                  new Error(`AtomVM execution did not complete within ${timeoutSeconds}s timeout`)
                );
              }
            }, EXECUTION_TIMEOUT_MS);
          } catch (error) {
            // Poka-yoke: State transition to Error on exception
            this.state = 'Error';
            span.setStatus({ code: 2, message: error.message });
            span.end();
            reject(error);
          }
        });
      }
    );
  }

  /**
   * Clean up resources
   *
   * **Poka-Yoke**: Terminal state prevents further operations
   */
  destroy() {
    // Poka-yoke: Terminal state prevents further operations
    this.state = 'Destroyed';
    this.atomvmModule = null;
    this.memory = null;
    this.terminal.log('Runtime destroyed', 'info');
  }
}
