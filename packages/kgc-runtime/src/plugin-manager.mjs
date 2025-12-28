/**
 * @file Plugin Manager - Lifecycle management for KGC plugins
 * @module @unrdf/kgc-runtime/plugin-manager
 * @description Manages plugin lifecycle with state machine: REGISTERED → LOADED → EXECUTING → UNLOADED
 */

import { z } from 'zod';
import { PluginManifestSchema, PluginStateSchema } from './schemas.mjs';

/**
 * Plugin lifecycle states
 */
export const PLUGIN_STATES = {
  REGISTERED: 'registered',
  LOADED: 'loaded',
  EXECUTING: 'executing',
  UNLOADED: 'unloaded',
  FAILED: 'failed',
};

/**
 * Valid state transitions for plugin lifecycle
 */
const VALID_TRANSITIONS = {
  [PLUGIN_STATES.REGISTERED]: [PLUGIN_STATES.LOADED, PLUGIN_STATES.FAILED],
  [PLUGIN_STATES.LOADED]: [PLUGIN_STATES.EXECUTING, PLUGIN_STATES.UNLOADED, PLUGIN_STATES.FAILED],
  [PLUGIN_STATES.EXECUTING]: [PLUGIN_STATES.LOADED, PLUGIN_STATES.UNLOADED, PLUGIN_STATES.FAILED],
  [PLUGIN_STATES.UNLOADED]: [PLUGIN_STATES.LOADED],
  [PLUGIN_STATES.FAILED]: [PLUGIN_STATES.UNLOADED],
};

/**
 * Plugin Manager - Manages plugin lifecycle with deterministic state machine
 *
 * @example
 * import { PluginManager } from '@unrdf/kgc-runtime/plugin-manager';
 * const manager = new PluginManager();
 * await manager.registerPlugin({ name: 'my-plugin', version: '1.0.0', ... });
 * await manager.loadPlugin('my-plugin');
 * await manager.activatePlugin('my-plugin');
 */
export class PluginManager {
  /**
   * Create new plugin manager
   * @param {Object} options - Configuration options
   * @param {Object} options.isolation - Isolation settings
   * @param {boolean} options.parallelLoading - Enable parallel loading (default: true)
   */
  constructor(options = {}) {
    /** @type {Map<string, Object>} */
    this.plugins = new Map();

    /** @type {Map<string, string>} Plugin state map */
    this.states = new Map();

    /** @type {Array<Object>} Audit log for all operations */
    this.auditLog = [];

    /** @type {number} State transition counter */
    this.transitionCount = 0;

    this.options = {
      parallelLoading: options.parallelLoading ?? true,
      isolation: options.isolation || {},
    };
  }

  /**
   * Register a plugin
   *
   * @param {Object} manifest - Plugin manifest
   * @returns {Promise<string>} Plugin ID
   * @throws {Error} If manifest is invalid
   *
   * @example
   * const id = await manager.registerPlugin({
   *   name: 'custom-receipt',
   *   version: '1.0.0',
   *   description: 'Custom receipt types',
   *   entryPoint: './plugin.mjs',
   *   capabilities: ['custom-receipt'],
   *   api_version: '5.0.1'
   * });
   */
  async registerPlugin(manifest) {
    // Validate manifest
    const validated = PluginManifestSchema.parse(manifest);

    const pluginId = `${validated.name}@${validated.version}`;

    // Check if already registered
    if (this.plugins.has(pluginId)) {
      throw new Error(`Plugin already registered: ${pluginId}`);
    }

    // Store plugin
    this.plugins.set(pluginId, {
      ...validated,
      id: pluginId,
      registeredAt: Date.now(),
      loadedAt: null,
      instance: null,
    });

    // Set initial state
    await this._transitionState(pluginId, PLUGIN_STATES.REGISTERED);

    // Log registration
    await this._auditLog('register', pluginId, { manifest: validated });

    return pluginId;
  }

  /**
   * Load a plugin (initialize but don't activate)
   *
   * @param {string} pluginId - Plugin ID (name@version)
   * @returns {Promise<void>}
   * @throws {Error} If plugin not found or load fails
   *
   * @example
   * await manager.loadPlugin('custom-receipt@1.0.0');
   */
  async loadPlugin(pluginId) {
    const plugin = this.plugins.get(pluginId);

    if (!plugin) {
      throw new Error(`Plugin not found: ${pluginId}`);
    }

    // Verify state allows loading
    const currentState = this.states.get(pluginId);
    if (!this._canTransition(currentState, PLUGIN_STATES.LOADED)) {
      throw new Error(`Cannot load plugin in state: ${currentState}`);
    }

    try {
      // Transition to loaded state
      await this._transitionState(pluginId, PLUGIN_STATES.LOADED);

      // Update metadata
      plugin.loadedAt = Date.now();

      // Log load
      await this._auditLog('load', pluginId, { success: true });
    } catch (error) {
      await this._transitionState(pluginId, PLUGIN_STATES.FAILED);
      await this._auditLog('load', pluginId, { success: false, error: error.message });
      throw error;
    }
  }

  /**
   * Activate a plugin (start execution)
   *
   * @param {string} pluginId - Plugin ID
   * @returns {Promise<void>}
   * @throws {Error} If plugin not loaded or activation fails
   *
   * @example
   * await manager.activatePlugin('custom-receipt@1.0.0');
   */
  async activatePlugin(pluginId) {
    const plugin = this.plugins.get(pluginId);

    if (!plugin) {
      throw new Error(`Plugin not found: ${pluginId}`);
    }

    const currentState = this.states.get(pluginId);
    if (!this._canTransition(currentState, PLUGIN_STATES.EXECUTING)) {
      throw new Error(`Cannot activate plugin in state: ${currentState}`);
    }

    try {
      // Transition to executing state
      await this._transitionState(pluginId, PLUGIN_STATES.EXECUTING);

      // Log activation
      await this._auditLog('activate', pluginId, { success: true });
    } catch (error) {
      await this._transitionState(pluginId, PLUGIN_STATES.FAILED);
      await this._auditLog('activate', pluginId, { success: false, error: error.message });
      throw error;
    }
  }

  /**
   * Deactivate a plugin (stop execution but keep loaded)
   *
   * @param {string} pluginId - Plugin ID
   * @returns {Promise<void>}
   *
   * @example
   * await manager.deactivatePlugin('custom-receipt@1.0.0');
   */
  async deactivatePlugin(pluginId) {
    const plugin = this.plugins.get(pluginId);

    if (!plugin) {
      throw new Error(`Plugin not found: ${pluginId}`);
    }

    const currentState = this.states.get(pluginId);
    if (currentState !== PLUGIN_STATES.EXECUTING) {
      throw new Error(`Plugin not executing: ${currentState}`);
    }

    // Transition back to loaded
    await this._transitionState(pluginId, PLUGIN_STATES.LOADED);

    // Log deactivation
    await this._auditLog('deactivate', pluginId, { success: true });
  }

  /**
   * Unload a plugin (cleanup and remove from memory)
   *
   * @param {string} pluginId - Plugin ID
   * @returns {Promise<void>}
   *
   * @example
   * await manager.unloadPlugin('custom-receipt@1.0.0');
   */
  async unloadPlugin(pluginId) {
    const plugin = this.plugins.get(pluginId);

    if (!plugin) {
      throw new Error(`Plugin not found: ${pluginId}`);
    }

    const currentState = this.states.get(pluginId);
    if (!this._canTransition(currentState, PLUGIN_STATES.UNLOADED)) {
      throw new Error(`Cannot unload plugin in state: ${currentState}`);
    }

    // Transition to unloaded
    await this._transitionState(pluginId, PLUGIN_STATES.UNLOADED);

    // Cleanup
    plugin.instance = null;
    plugin.loadedAt = null;

    // Log unload
    await this._auditLog('unload', pluginId, { success: true });
  }

  /**
   * Uninstall a plugin (complete removal)
   *
   * @param {string} pluginId - Plugin ID
   * @returns {Promise<void>}
   *
   * @example
   * await manager.uninstallPlugin('custom-receipt@1.0.0');
   */
  async uninstallPlugin(pluginId) {
    const plugin = this.plugins.get(pluginId);

    if (!plugin) {
      throw new Error(`Plugin not found: ${pluginId}`);
    }

    // Must be unloaded first
    const currentState = this.states.get(pluginId);
    if (currentState !== PLUGIN_STATES.UNLOADED && currentState !== PLUGIN_STATES.REGISTERED) {
      throw new Error(`Must unload plugin before uninstall: ${currentState}`);
    }

    // Remove from registry
    this.plugins.delete(pluginId);
    this.states.delete(pluginId);

    // Log uninstall
    await this._auditLog('uninstall', pluginId, { success: true });
  }

  /**
   * Get plugin state
   *
   * @param {string} pluginId - Plugin ID
   * @returns {string|null} Current state or null if not found
   */
  getPluginState(pluginId) {
    return this.states.get(pluginId) || null;
  }

  /**
   * Get plugin manifest
   *
   * @param {string} pluginId - Plugin ID
   * @returns {Object|null} Plugin manifest or null if not found
   */
  getPlugin(pluginId) {
    const plugin = this.plugins.get(pluginId);
    if (!plugin) return null;

    return {
      ...plugin,
      state: this.states.get(pluginId),
    };
  }

  /**
   * List all plugins
   *
   * @returns {Array<Object>} Array of plugin manifests with states
   */
  listPlugins() {
    return Array.from(this.plugins.entries()).map(([id, plugin]) => ({
      ...plugin,
      state: this.states.get(id),
    }));
  }

  /**
   * List plugins by state
   *
   * @param {string} state - Plugin state
   * @returns {Array<Object>} Plugins in specified state
   */
  listPluginsByState(state) {
    return this.listPlugins().filter(p => p.state === state);
  }

  /**
   * Get audit log
   *
   * @param {Object} filters - Optional filters
   * @param {string} filters.pluginId - Filter by plugin ID
   * @param {string} filters.action - Filter by action
   * @returns {Array<Object>} Filtered audit log entries
   */
  getAuditLog(filters = {}) {
    let log = this.auditLog;

    if (filters.pluginId) {
      log = log.filter(entry => entry.pluginId === filters.pluginId);
    }

    if (filters.action) {
      log = log.filter(entry => entry.action === filters.action);
    }

    return log;
  }

  /**
   * Get transition count
   *
   * @returns {number} Total number of state transitions
   */
  getTransitionCount() {
    return this.transitionCount;
  }

  /**
   * Load multiple plugins in parallel
   *
   * @param {string[]} pluginIds - Array of plugin IDs
   * @returns {Promise<Object>} Results { success: [], failed: [] }
   */
  async loadPluginsParallel(pluginIds) {
    if (!this.options.parallelLoading) {
      throw new Error('Parallel loading not enabled');
    }

    const results = await Promise.allSettled(
      pluginIds.map(id => this.loadPlugin(id))
    );

    const success = [];
    const failed = [];

    results.forEach((result, index) => {
      if (result.status === 'fulfilled') {
        success.push(pluginIds[index]);
      } else {
        failed.push({
          pluginId: pluginIds[index],
          error: result.reason?.message || 'Unknown error',
        });
      }
    });

    return { success, failed };
  }

  // ===== Private Methods =====

  /**
   * Transition plugin to new state
   * @private
   */
  async _transitionState(pluginId, newState) {
    const currentState = this.states.get(pluginId);

    // Validate transition
    if (currentState && !this._canTransition(currentState, newState)) {
      throw new Error(`Invalid transition: ${currentState} -> ${newState}`);
    }

    // Update state
    this.states.set(pluginId, newState);
    this.transitionCount++;
  }

  /**
   * Check if state transition is valid
   * @private
   */
  _canTransition(fromState, toState) {
    if (!fromState) return true; // Initial registration
    return VALID_TRANSITIONS[fromState]?.includes(toState) || false;
  }

  /**
   * Add entry to audit log
   * @private
   */
  async _auditLog(action, pluginId, details) {
    const entry = {
      timestamp: Date.now(),
      action,
      pluginId,
      details,
    };

    this.auditLog.push(entry);
  }
}

/**
 * Create a new plugin manager instance
 *
 * @param {Object} options - Configuration options
 * @returns {PluginManager} New plugin manager
 */
export function createPluginManager(options = {}) {
  return new PluginManager(options);
}
