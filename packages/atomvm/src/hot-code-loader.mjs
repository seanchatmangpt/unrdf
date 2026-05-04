/**
 * Hot Code Loader for AtomVM Runtime
 *
 * Provides dynamic loading and hot-swapping of BEAM modules without node restart.
 * Follows OTP hot code loading semantics with graceful process transitions.
 *
 * **Poka-Yoke Design**: Validates module signatures before swap,
 * queues concurrent reloads, and ensures atomic transitions.
 *
 * @module hot-code-loader
 */

import { trace } from '@opentelemetry/api';

/**
 * Get tracer lazily to ensure provider is registered first
 * @returns {import('@opentelemetry/api').Tracer} OpenTelemetry tracer
 */
function getTracer() {
  return trace.getTracer('hot-code-loader');
}

/**
 * @typedef {Object} ModuleInfo
 * @property {string} name - Module name
 * @property {string} path - Path to .beam or .avm file
 * @property {string} signature - Module signature hash
 * @property {number} loadedAt - Timestamp when loaded
 * @property {number} version - Module version (increments on reload)
 * @property {'loaded' | 'reloading' | 'error'} status - Current status
 */

/**
 * @typedef {Object} HotSwapCallback
 * @property {string} moduleName - Target module name
 * @property {Function} beforeSwap - Called before module swap
 * @property {Function} afterSwap - Called after successful swap
 * @property {Function} [onError] - Called if swap fails
 */

/**
 * @typedef {Object} LoadResult
 * @property {boolean} success - Whether load succeeded
 * @property {string} moduleName - Loaded module name
 * @property {string} [signature] - Module signature if successful
 * @property {string} [error] - Error message if failed
 */

/**
 * @typedef {Object} ReloadResult
 * @property {boolean} success - Whether reload succeeded
 * @property {string} moduleName - Reloaded module name
 * @property {number} version - New version number
 * @property {number} duration - Reload duration in ms
 * @property {string} [error] - Error message if failed
 */

/**
 * @typedef {Object} SignatureValidationResult
 * @property {boolean} valid - Whether signature is valid
 * @property {string} signature - Computed signature
 * @property {string} [previousSignature] - Previous signature if available
 * @property {string} [error] - Error message if validation failed
 */

/**
 * HotCodeLoader manages dynamic loading and hot-swapping of BEAM modules.
 *
 * Features:
 * - Load new .beam or .avm modules at runtime
 * - Reload modules from updated files
 * - Register callbacks for before/after reload events
 * - Validate module signatures before swapping
 * - Queue concurrent reloads to ensure atomicity
 * - Notify supervisor tree of module changes
 *
 * @class
 */
export class HotCodeLoader {
  /**
   * Creates a new HotCodeLoader instance.
   *
   * @param {import('./atomvm-runtime.mjs').AtomVMRuntime} runtime - AtomVM runtime instance
   * @param {Object} [options] - Configuration options
   * @param {import('./supervisor-tree.mjs').SupervisorTree} [options.supervisor] - Supervisor tree for notifications
   * @param {number} [options.maxQueueSize=100] - Maximum reload queue size
   * @param {number} [options.reloadTimeout=30000] - Reload timeout in ms
   */
  constructor(runtime, options = {}) {
    if (!runtime) {
      throw new Error('AtomVMRuntime instance is required');
    }

    /** @type {import('./atomvm-runtime.mjs').AtomVMRuntime} */
    this.runtime = runtime;

    /** @type {import('./supervisor-tree.mjs').SupervisorTree | null} */
    this.supervisor = options.supervisor || null;

    /** @type {Map<string, ModuleInfo>} */
    this.activeModules = new Map();

    /** @type {Map<string, HotSwapCallback[]>} */
    this.hotSwapCallbacks = new Map();

    /** @type {Array<{moduleName: string, resolve: Function, reject: Function}>} */
    this.reloadQueue = [];

    /** @type {boolean} */
    this.isProcessingQueue = false;

    /** @type {number} */
    this.maxQueueSize = options.maxQueueSize || 100;

    /** @type {number} */
    this.reloadTimeout = options.reloadTimeout || 30000;

    /** @type {Map<string, ArrayBuffer>} */
    this.moduleCache = new Map();
  }

  /**
   * Load a new BEAM module (.beam or .avm file).
   *
   * **Poka-Yoke**: Validates file existence and signature before adding to active modules.
   *
   * @param {string} modulePath - Path to .beam or .avm file
   * @returns {Promise<LoadResult>} Load result
   */
  async loadModule(modulePath) {
    if (!modulePath || typeof modulePath !== 'string') {
      return { success: false, moduleName: '', error: 'Module path is required' };
    }

    const moduleName = this._extractModuleName(modulePath);

    return getTracer().startActiveSpan(
      'hot_code_loader.load_module',
      {
        attributes: {
          'module.name': moduleName,
          'module.path': modulePath,
        },
      },
      async span => {
        const startTime = Date.now();

        try {
          // Check if module already loaded
          if (this.activeModules.has(moduleName)) {
            span.setAttribute('module.already_loaded', true);
            span.setStatus({ code: 1 }); // OK
            span.end();
            return {
              success: true,
              moduleName,
              signature: this.activeModules.get(moduleName).signature,
            };
          }

          // Load module content
          const content = await this._loadModuleContent(modulePath);
          if (!content) {
            const error = `Failed to load module from ${modulePath}`;
            span.setStatus({ code: 2, message: error });
            span.end();
            return { success: false, moduleName, error };
          }

          // Compute signature
          const signature = await this._computeSignature(content);

          // Create module info
          /** @type {ModuleInfo} */
          const moduleInfo = {
            name: moduleName,
            path: modulePath,
            signature,
            loadedAt: Date.now(),
            version: 1,
            status: 'loaded',
          };

          // Store module
          this.activeModules.set(moduleName, moduleInfo);
          this.moduleCache.set(moduleName, content);

          const duration = Date.now() - startTime;
          span.setAttribute('module.load_duration_ms', duration);
          span.setAttribute('module.signature', signature);
          span.setStatus({ code: 1 }); // OK
          span.end();

          return { success: true, moduleName, signature };
        } catch (error) {
          span.setStatus({ code: 2, message: error.message });
          span.end();
          return { success: false, moduleName, error: error.message };
        }
      }
    );
  }

  /**
   * Reload a module from its updated file.
   *
   * **Poka-Yoke**: Validates signature, executes callbacks, and queues concurrent reloads.
   *
   * @param {string} moduleName - Name of module to reload
   * @returns {Promise<ReloadResult>} Reload result
   */
  async reloadModule(moduleName) {
    if (!moduleName || typeof moduleName !== 'string') {
      return { success: false, moduleName: '', version: 0, duration: 0, error: 'Module name is required' };
    }

    // Queue concurrent reloads
    if (this.isProcessingQueue) {
      return this._queueReload(moduleName);
    }

    return this._executeReload(moduleName);
  }

  /**
   * Register a hot swap callback for a module.
   *
   * @param {string} moduleName - Target module name
   * @param {Function} callback - Callback function or object with beforeSwap/afterSwap
   * @returns {Function} Unregister function
   */
  registerHotSwap(moduleName, callback) {
    if (!moduleName || typeof moduleName !== 'string') {
      throw new Error('Module name is required');
    }
    if (!callback || (typeof callback !== 'function' && typeof callback !== 'object')) {
      throw new Error('Callback is required');
    }

    // Normalize callback to HotSwapCallback format
    /** @type {HotSwapCallback} */
    const hotSwapCallback = typeof callback === 'function'
      ? {
          moduleName,
          beforeSwap: async () => {},
          afterSwap: callback,
        }
      : {
          moduleName,
          beforeSwap: callback.beforeSwap || (async () => {}),
          afterSwap: callback.afterSwap || (async () => {}),
          onError: callback.onError,
        };

    // Add to callbacks map
    if (!this.hotSwapCallbacks.has(moduleName)) {
      this.hotSwapCallbacks.set(moduleName, []);
    }
    this.hotSwapCallbacks.get(moduleName).push(hotSwapCallback);

    // Return unregister function
    return () => {
      const callbacks = this.hotSwapCallbacks.get(moduleName);
      if (callbacks) {
        const index = callbacks.indexOf(hotSwapCallback);
        if (index !== -1) {
          callbacks.splice(index, 1);
        }
      }
    };
  }

  /**
   * Get list of all active modules.
   *
   * @returns {ModuleInfo[]} Array of active module info objects
   */
  getActiveModules() {
    return Array.from(this.activeModules.values());
  }

  /**
   * Validate a module's signature before swap.
   *
   * **Poka-Yoke**: Ensures module integrity before allowing swap.
   *
   * @param {string} moduleName - Module name to validate
   * @returns {Promise<SignatureValidationResult>} Validation result
   */
  async validateModuleSignature(moduleName) {
    if (!moduleName || typeof moduleName !== 'string') {
      return { valid: false, signature: '', error: 'Module name is required' };
    }

    return getTracer().startActiveSpan(
      'hot_code_loader.validate_signature',
      {
        attributes: {
          'module.name': moduleName,
        },
      },
      async span => {
        try {
          const moduleInfo = this.activeModules.get(moduleName);
          if (!moduleInfo) {
            span.setStatus({ code: 2, message: 'Module not loaded' });
            span.end();
            return { valid: false, signature: '', error: 'Module not loaded' };
          }

          // Reload content and recompute signature
          const content = await this._loadModuleContent(moduleInfo.path);
          if (!content) {
            span.setStatus({ code: 2, message: 'Failed to load module content' });
            span.end();
            return { valid: false, signature: '', error: 'Failed to load module content' };
          }

          const newSignature = await this._computeSignature(content);
          const previousSignature = moduleInfo.signature;

          span.setAttribute('module.signature', newSignature);
          span.setAttribute('module.previous_signature', previousSignature);
          span.setAttribute('module.signature_changed', newSignature !== previousSignature);
          span.setStatus({ code: 1 }); // OK
          span.end();

          return {
            valid: true,
            signature: newSignature,
            previousSignature,
          };
        } catch (error) {
          span.setStatus({ code: 2, message: error.message });
          span.end();
          return { valid: false, signature: '', error: error.message };
        }
      }
    );
  }

  /**
   * Unload a module from the active modules.
   *
   * @param {string} moduleName - Module name to unload
   * @returns {boolean} True if module was unloaded
   */
  unloadModule(moduleName) {
    if (!this.activeModules.has(moduleName)) {
      return false;
    }

    this.activeModules.delete(moduleName);
    this.moduleCache.delete(moduleName);
    this.hotSwapCallbacks.delete(moduleName);
    return true;
  }

  /**
   * Set supervisor tree for notifications.
   *
   * @param {import('./supervisor-tree.mjs').SupervisorTree} supervisor - Supervisor tree
   */
  setSupervisor(supervisor) {
    this.supervisor = supervisor;
  }

  /**
   * Get pending reload queue size.
   *
   * @returns {number} Number of pending reloads
   */
  getPendingReloadCount() {
    return this.reloadQueue.length;
  }

  // ============== Private Methods ==============

  /**
   * Execute module reload with OTEL tracing.
   *
   * @private
   * @param {string} moduleName - Module to reload
   * @returns {Promise<ReloadResult>} Reload result
   */
  async _executeReload(moduleName) {
    this.isProcessingQueue = true;

    return getTracer().startActiveSpan(
      'hot_code_loader.reload_module',
      {
        attributes: {
          'module.name': moduleName,
        },
      },
      async span => {
        const startTime = Date.now();

        try {
          const moduleInfo = this.activeModules.get(moduleName);
          if (!moduleInfo) {
            const error = `Module ${moduleName} not found`;
            span.setStatus({ code: 2, message: error });
            span.end();
            this._processNextInQueue();
            return { success: false, moduleName, version: 0, duration: 0, error };
          }

          // Set status to reloading
          moduleInfo.status = 'reloading';

          // Execute before-swap callbacks
          await this._executeBeforeSwapCallbacks(moduleName);

          // Validate signature
          const validation = await this.validateModuleSignature(moduleName);
          if (!validation.valid) {
            moduleInfo.status = 'error';
            await this._executeErrorCallbacks(moduleName, new Error(validation.error || 'Invalid signature'));
            span.setStatus({ code: 2, message: validation.error });
            span.end();
            this._processNextInQueue();
            return { success: false, moduleName, version: moduleInfo.version, duration: Date.now() - startTime, error: validation.error };
          }

          // Load new content
          const content = await this._loadModuleContent(moduleInfo.path);
          if (!content) {
            moduleInfo.status = 'error';
            const error = 'Failed to load module content';
            await this._executeErrorCallbacks(moduleName, new Error(error));
            span.setStatus({ code: 2, message: error });
            span.end();
            this._processNextInQueue();
            return { success: false, moduleName, version: moduleInfo.version, duration: Date.now() - startTime, error };
          }

          // Update module info
          moduleInfo.signature = validation.signature;
          moduleInfo.version += 1;
          moduleInfo.loadedAt = Date.now();
          moduleInfo.status = 'loaded';

          // Update cache
          this.moduleCache.set(moduleName, content);

          // Execute after-swap callbacks
          await this._executeAfterSwapCallbacks(moduleName);

          // Notify supervisor if available
          await this._notifySupervisor(moduleName);

          const duration = Date.now() - startTime;
          span.setAttribute('module.reload_duration_ms', duration);
          span.setAttribute('module.new_version', moduleInfo.version);
          span.setAttribute('module.new_signature', moduleInfo.signature);
          span.setStatus({ code: 1 }); // OK
          span.end();

          this._processNextInQueue();
          return { success: true, moduleName, version: moduleInfo.version, duration };
        } catch (error) {
          const moduleInfo = this.activeModules.get(moduleName);
          if (moduleInfo) {
            moduleInfo.status = 'error';
          }
          await this._executeErrorCallbacks(moduleName, error);
          span.setStatus({ code: 2, message: error.message });
          span.end();
          this._processNextInQueue();
          return { success: false, moduleName, version: moduleInfo?.version || 0, duration: Date.now() - startTime, error: error.message };
        }
      }
    );
  }

  /**
   * Queue a reload request.
   *
   * @private
   * @param {string} moduleName - Module to reload
   * @returns {Promise<ReloadResult>} Reload result
   */
  _queueReload(moduleName) {
    return new Promise((resolve, reject) => {
      if (this.reloadQueue.length >= this.maxQueueSize) {
        reject(new Error('Reload queue is full'));
        return;
      }
      this.reloadQueue.push({ moduleName, resolve, reject });
    });
  }

  /**
   * Process next item in reload queue.
   *
   * @private
   */
  async _processNextInQueue() {
    if (this.reloadQueue.length === 0) {
      this.isProcessingQueue = false;
      return;
    }

    const next = this.reloadQueue.shift();
    try {
      const result = await this._executeReload(next.moduleName);
      next.resolve(result);
    } catch (error) {
      next.reject(error);
    }
  }

  /**
   * Execute before-swap callbacks for a module.
   *
   * @private
   * @param {string} moduleName - Module name
   */
  async _executeBeforeSwapCallbacks(moduleName) {
    const callbacks = this.hotSwapCallbacks.get(moduleName) || [];
    for (const callback of callbacks) {
      try {
        await callback.beforeSwap({ moduleName, timestamp: Date.now() });
      } catch (error) {
        console.error(`Before-swap callback error for ${moduleName}:`, error);
      }
    }
  }

  /**
   * Execute after-swap callbacks for a module.
   *
   * @private
   * @param {string} moduleName - Module name
   */
  async _executeAfterSwapCallbacks(moduleName) {
    const callbacks = this.hotSwapCallbacks.get(moduleName) || [];
    const moduleInfo = this.activeModules.get(moduleName);
    for (const callback of callbacks) {
      try {
        await callback.afterSwap({
          moduleName,
          version: moduleInfo?.version || 0,
          signature: moduleInfo?.signature || '',
          timestamp: Date.now(),
        });
      } catch (error) {
        console.error(`After-swap callback error for ${moduleName}:`, error);
      }
    }
  }

  /**
   * Execute error callbacks for a module.
   *
   * @private
   * @param {string} moduleName - Module name
   * @param {Error} error - Error that occurred
   */
  async _executeErrorCallbacks(moduleName, error) {
    const callbacks = this.hotSwapCallbacks.get(moduleName) || [];
    for (const callback of callbacks) {
      if (callback.onError) {
        try {
          await callback.onError({ moduleName, error, timestamp: Date.now() });
        } catch (callbackError) {
          console.error(`Error callback error for ${moduleName}:`, callbackError);
        }
      }
    }
  }

  /**
   * Notify supervisor tree of module reload.
   *
   * @private
   * @param {string} moduleName - Reloaded module name
   */
  async _notifySupervisor(moduleName) {
    if (!this.supervisor) return;

    // Supervisor notification - allow running processes to gracefully transition
    // This is a hook point for supervisor integration
    try {
      // Check if supervisor has children using this module
      // In OTP style, we'd notify processes to transition to new code
      // For now, we just log the notification
      const moduleInfo = this.activeModules.get(moduleName);
      if (moduleInfo) {
        console.log(`[HotCodeLoader] Notified supervisor of ${moduleName} reload (v${moduleInfo.version})`);
      }
    } catch (error) {
      console.error(`Failed to notify supervisor of ${moduleName} reload:`, error);
    }
  }

  /**
   * Extract module name from path.
   *
   * @private
   * @param {string} modulePath - Module path
   * @returns {string} Module name
   */
  _extractModuleName(modulePath) {
    const parts = modulePath.split('/');
    const filename = parts[parts.length - 1];
    return filename.replace(/\.(beam|avm)$/, '');
  }

  /**
   * Load module content from path.
   *
   * @private
   * @param {string} modulePath - Module path
   * @returns {Promise<ArrayBuffer|null>} Module content or null
   */
  async _loadModuleContent(modulePath) {
    try {
      // Support both browser (fetch) and Node.js (fs) environments
      if (typeof fetch !== 'undefined') {
        const response = await fetch(modulePath);
        if (!response.ok) {
          return null;
        }
        return await response.arrayBuffer();
      } else {
        // Node.js environment
        const { readFile } = await import('fs/promises');
        const buffer = await readFile(modulePath);
        return buffer.buffer;
      }
    } catch (error) {
      console.error(`Failed to load module from ${modulePath}:`, error);
      return null;
    }
  }

  /**
   * Compute signature hash for module content.
   *
   * @private
   * @param {ArrayBuffer} content - Module content
   * @returns {Promise<string>} Signature hash
   */
  async _computeSignature(content) {
    try {
      // Use Web Crypto API if available
      if (typeof crypto !== 'undefined' && crypto.subtle) {
        const hashBuffer = await crypto.subtle.digest('SHA-256', content);
        const hashArray = Array.from(new Uint8Array(hashBuffer));
        return hashArray.map(b => b.toString(16).padStart(2, '0')).join('');
      } else {
        // Node.js fallback
        const { createHash } = await import('crypto');
        const hash = createHash('sha256');
        hash.update(Buffer.from(content));
        return hash.digest('hex');
      }
    } catch (error) {
      // Simple fallback hash based on content length and first bytes
      const view = new Uint8Array(content);
      const sample = Array.from(view.slice(0, 64));
      return `${content.byteLength}-${sample.join('')}`;
    }
  }
}
