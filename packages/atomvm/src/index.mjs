/**
 * AtomVM Browser Runtime Entry Point
 *
 * Initializes the service worker for Cross-Origin-Isolation and loads AtomVM.
 * Requires coi-serviceworker to enable SharedArrayBuffer support.
 *
 * @module @unrdf/atomvm
 */

import { registerServiceWorker, checkCrossOriginIsolation } from './service-worker-manager.mjs';
import { AtomVMRuntime } from './atomvm-runtime.mjs';
import { TerminalUI } from './terminal-ui.mjs';

/**
 * Main application state
 */
class App {
  constructor() {
    this.terminal = new TerminalUI();
    this.runtime = null;
    this.isReady = false;
  }

  /**
   * Initialize the application
   */
  async init() {
    this.terminal.log('Starting AtomVM browser initialization...', 'info');

    try {
      // Step 1: Register service worker for COI
      this.terminal.log('Registering coi-serviceworker...', 'info');
      const swRegistered = await registerServiceWorker();

      if (!swRegistered) {
        throw new Error('Service worker registration failed');
      }

      this.updateSWStatus('registered ✓');
      this.terminal.log('Service worker registered successfully', 'success');

      // Step 2: Check Cross-Origin-Isolation
      const isIsolated = checkCrossOriginIsolation();

      if (isIsolated) {
        this.terminal.log('Cross-Origin-Isolation enabled ✓', 'success');
        this.terminal.log('SharedArrayBuffer available ✓', 'success');
      } else {
        this.terminal.log('Waiting for page reload to enable COI...', 'info');
        this.terminal.log('Page will reload automatically', 'info');
        return; // Will reload via service worker
      }

      // Step 3: Initialize AtomVM runtime
      this.terminal.log('Initializing AtomVM runtime...', 'info');
      this.runtime = new AtomVMRuntime(this.terminal);

      // Step 4: Update UI
      this.isReady = true;
      this.updateStatus('ready', '✅ AtomVM ready! Service worker active and runtime initialized.');
      this.enableControls();

      this.terminal.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━', 'success');
      this.terminal.log('🚀 AtomVM is ready to execute BEAM code!', 'success');

    } catch (error) {
      this.terminal.log(`Initialization failed: ${error.message}`, 'error');
      this.updateStatus('error', `❌ Error: ${error.message}`);
      console.error('Initialization error:', error);
    }
  }

  /**
   * Update status display
   * @param {string} type - Status type (loading, ready, error)
   * @param {string} message - Status message
   */
  updateStatus(type, message) {
    const statusEl = document.getElementById('status');
    if (statusEl) {
      statusEl.className = `status ${type}`;
      statusEl.textContent = message;
    }
  }

  /**
   * Update service worker status display
   * @param {string} status - Service worker status
   */
  updateSWStatus(status) {
    const swStatusEl = document.getElementById('swStatus');
    if (swStatusEl) {
      swStatusEl.textContent = status;
    }
  }

  /**
   * Enable control buttons
   */
  enableControls() {
    const initBtn = document.getElementById('initBtn');
    const runExampleBtn = document.getElementById('runExampleBtn');

    if (initBtn) {
      initBtn.disabled = false;
      initBtn.onclick = () => this.handleInit();
    }

    if (runExampleBtn) {
      runExampleBtn.disabled = false;
      runExampleBtn.onclick = () => this.handleRunExample();
    }

    const clearBtn = document.getElementById('clearBtn');
    if (clearBtn) {
      clearBtn.onclick = () => this.terminal.clear();
    }
  }

  /**
   * Handle AtomVM initialization button
   */
  async handleInit() {
    if (!this.runtime) {
      this.terminal.log('Runtime not available', 'error');
      return;
    }

    try {
      this.terminal.log('Initializing AtomVM WASM module...', 'info');
      await this.runtime.loadWASM();
      this.terminal.log('AtomVM WASM module loaded successfully', 'success');
    } catch (error) {
      this.terminal.log(`WASM load failed: ${error.message}`, 'error');
    }
  }

  /**
   * Handle run example button
   */
  async handleRunExample() {
    if (!this.runtime) {
      this.terminal.log('Runtime not available', 'error');
      return;
    }

    try {
      this.terminal.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━', 'info');
      this.terminal.log('Running example BEAM code...', 'info');
      await this.runtime.runExample();
    } catch (error) {
      this.terminal.log(`Example execution failed: ${error.message}`, 'error');
    }
  }
}

// Initialize app when DOM is ready
if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', () => {
    const app = new App();
    app.init();
  });
} else {
  const app = new App();
  app.init();
}

export { App };
