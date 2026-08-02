/**
 * AtomVM Generic UNIX runtime for Node.js callers.
 *
 * This class executes the real AtomVM binary. It does not load a generated
 * JavaScript shim and it never invokes a shell.
 *
 * @module node-runtime
 */

import { spawn, spawnSync } from 'node:child_process';
import { accessSync, constants, statSync } from 'node:fs';
import { delimiter, isAbsolute, resolve } from 'node:path';
import { trace } from '@opentelemetry/api';

const SUCCESS_EXIT_CODE = 0;

function getTracer() {
  return trace.getTracer('atomvm-node-runtime');
}

function validateNonEmptyString(value, name) {
  if (typeof value !== 'string' || value.trim().length === 0) {
    throw new TypeError(`${name} is required and must be a non-empty string`);
  }
  return value;
}

function requireReadableFile(path, label) {
  const resolved = resolve(validateNonEmptyString(path, label));
  let info;
  try {
    info = statSync(resolved);
    accessSync(resolved, constants.R_OK);
  } catch (error) {
    throw new Error(`[ATOMVM_FILE_NOT_FOUND_REFUSED] ${label} is unavailable: ${resolved}`, { cause: error });
  }
  if (!info.isFile()) {
    throw new Error(`[ATOMVM_FILE_NOT_FOUND_REFUSED] ${label} is not a file: ${resolved}`);
  }
  return resolved;
}

function executableAt(path) {
  try {
    const info = statSync(path);
    accessSync(path, constants.X_OK);
    return info.isFile();
  } catch {
    return false;
  }
}

function resolveExecutable(candidate) {
  const requested = validateNonEmptyString(candidate, 'atomvmBinary');
  if (isAbsolute(requested) || requested.includes('/') || requested.includes('\\')) {
    const resolved = resolve(requested);
    if (executableAt(resolved)) return resolved;
    throw new Error(`[ATOMVM_BINARY_NOT_FOUND_REFUSED] AtomVM binary is unavailable or not executable: ${resolved}`);
  }

  for (const directory of (process.env.PATH ?? '').split(delimiter).filter(Boolean)) {
    const resolved = resolve(directory, requested);
    if (executableAt(resolved)) return resolved;
  }
  throw new Error(`[ATOMVM_BINARY_NOT_FOUND_REFUSED] AtomVM binary was not found in PATH: ${requested}`);
}

/**
 * @typedef {'Uninitialized' | 'Loading' | 'Ready' | 'Executing' | 'Error' | 'Destroyed'} NodeRuntimeState
 */

export class AtomVMNodeRuntime {
  constructor(options = {}) {
    this.log = options.log ?? console.log;
    this.errorLog = options.errorLog ?? console.error;
    this.requestedBinary = options.atomvmBinary ?? process.env.ATOMVM_BIN ?? 'AtomVM';
    this.libraryPaths = Object.freeze([...(options.libraryPaths ?? [])]);
    this.atomvmPath = null;
    this.runtimeVersion = null;
    /** @type {NodeRuntimeState} */
    this.state = 'Uninitialized';
  }

  isReady() {
    return this.state === 'Ready' && this.atomvmPath !== null;
  }

  isLoaded() {
    return this.state === 'Ready' || this.state === 'Executing';
  }

  async load() {
    return getTracer().startActiveSpan('atomvm.load_native', async span => {
      try {
        if (this.state === 'Destroyed') {
          throw new Error('Cannot load AtomVM: runtime has been destroyed');
        }
        if (this.state === 'Ready' || this.state === 'Executing') return;
        if (this.state === 'Loading') throw new Error('AtomVM load already in progress');

        this.state = 'Loading';
        this.atomvmPath = resolveExecutable(this.requestedBinary);
        const version = spawnSync(this.atomvmPath, ['-v'], {
          encoding: 'utf8',
          shell: false,
        });
        if (version.error || version.status !== SUCCESS_EXIT_CODE) {
          throw new Error(
            `[ATOMVM_VERSION_PROBE_REFUSED] Unable to execute ${this.atomvmPath} -v: ` +
            `${version.error?.message ?? version.stderr ?? `exit ${version.status}`}`,
          );
        }

        this.runtimeVersion = `${version.stdout ?? ''}${version.stderr ?? ''}`.trim();
        this.state = 'Ready';
        span.setAttributes({
          'runtime.type': 'generic-unix',
          'runtime.path': this.atomvmPath,
          'atomvm.version': this.runtimeVersion,
          'runtime.state': this.state,
        });
        span.setStatus({ code: 1 });
        this.log(`Found AtomVM ${this.runtimeVersion} at ${this.atomvmPath}`);
      } catch (error) {
        this.state = 'Error';
        span.recordException(error);
        span.setStatus({ code: 2, message: error.message });
        throw error;
      } finally {
        span.end();
      }
    });
  }

  async execute(avmPath) {
    if (!this.isReady()) {
      throw new Error(`Runtime not ready. Current state: ${this.state}. Call load() first.`);
    }

    const applicationPath = requireReadableFile(avmPath, 'avmPath');
    const libraries = this.libraryPaths.map(path => requireReadableFile(path, 'libraryPath'));
    this.state = 'Executing';

    return new Promise((resolvePromise, reject) => {
      const args = [applicationPath, ...libraries];
      this.log(`Executing: ${this.atomvmPath} ${args.join(' ')}`);
      const child = spawn(this.atomvmPath, args, {
        stdio: ['ignore', 'pipe', 'pipe'],
        shell: false,
      });

      let stdout = '';
      let stderr = '';
      child.stdout.on('data', data => {
        const text = data.toString();
        stdout += text;
        this.log(text.trim());
      });
      child.stderr.on('data', data => {
        const text = data.toString();
        stderr += text;
        this.errorLog(text.trim());
      });

      child.on('error', error => {
        this.state = 'Error';
        reject(new Error(`[ATOMVM_EXECUTION_BLOCKED] Failed to execute AtomVM: ${error.message}`, { cause: error }));
      });
      child.on('close', (exitCode, signal) => {
        if (exitCode !== SUCCESS_EXIT_CODE) {
          this.state = 'Error';
          reject(new Error(
            `[ATOMVM_EXIT_BLOCKED] AtomVM exited with code ${exitCode}` +
            `${signal ? ` (${signal})` : ''}\n${stderr}`,
          ));
          return;
        }

        this.state = 'Ready';
        resolvePromise(Object.freeze({
          status: 'ok',
          runtime: 'AtomVM',
          runtimeVersion: this.runtimeVersion,
          binary: this.atomvmPath,
          exitCode,
          stdout,
          stderr,
        }));
      });
    });
  }

  async executeBeam(avmPath) {
    return this.execute(avmPath);
  }

  async runExample(moduleName) {
    validateNonEmptyString(moduleName, 'moduleName');
    return this.execute(resolve(new URL('../public/', import.meta.url).pathname, `${moduleName}.avm`));
  }

  destroy() {
    this.state = 'Destroyed';
    this.atomvmPath = null;
    this.runtimeVersion = null;
    this.log('Runtime destroyed');
  }
}
