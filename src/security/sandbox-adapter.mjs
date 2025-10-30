/**
 * Sandbox adapter to abstract execution engine (vm2 today, isolated-vm later).
 */
import { VM } from 'vm2';

export class SandboxAdapter {
  /**
   * @param {Object} [options]
   */
  constructor(options = {}) {
    this.engine = 'vm2';
    this.vm = new VM({
      timeout: options.timeoutMs || 1000,
      sandbox: options.sandbox || {},
      eval: false,
      wasm: false,
    });
  }

  /**
   * Execute untrusted code and return result.
   * @param {string} code
   * @returns {any}
   */
  run(code) {
    return this.vm.run(code);
  }
}

export function createSandboxAdapter(options = {}) {
  // Future: switch on process.env.UNRDF_SANDBOX_ENGINE === 'isolated-vm'
  return new SandboxAdapter(options);
}


