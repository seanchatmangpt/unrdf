/**
 * @file Plugin Isolation - Capability-based security for plugins
 * @module @unrdf/kgc-runtime/plugin-isolation
 * @description Provides sandboxing and capability whitelist for plugin execution
 */

import { z } from 'zod';

/**
 * Default capability whitelist
 * Plugins can only access these capabilities unless explicitly granted more
 */
const DEFAULT_WHITELIST = [
  'receipt:generate',
  'receipt:validate',
  'schema:validate',
  'tool:register',
  'bounds:check',
];

/**
 * Blocked capabilities (never allowed)
 */
const BLOCKED_CAPABILITIES = [
  'filesystem:write',
  'filesystem:delete',
  'network:http',
  'network:socket',
  'process:spawn',
  'process:exit',
  'eval:code',
];

/**
 * Capability schema
 */
const CapabilitySchema = z.string().regex(
  /^[a-z_-]+:[a-z_-]+$/,
  'Capability must be in format "category:action"'
);

/**
 * Plugin sandbox configuration schema
 */
const SandboxConfigSchema = z.object({
  /** Whitelisted capabilities */
  whitelist: z.array(CapabilitySchema).default([...DEFAULT_WHITELIST]),

  /** Additional capabilities to grant */
  additionalCapabilities: z.array(CapabilitySchema).optional(),

  /** Whether to enforce strict mode (reject any non-whitelisted) */
  strictMode: z.boolean().default(true),

  /** Maximum memory usage (bytes) */
  maxMemory: z.number().int().positive().optional(),

  /** Maximum execution time (milliseconds) */
  maxExecutionTime: z.number().int().positive().default(30000),

  /** Whether to allow custom receipt types */
  allowCustomReceipts: z.boolean().default(false),
});

/**
 * Plugin Isolation Manager - Enforces capability-based security
 *
 * @example
 * import { PluginIsolation } from '@unrdf/kgc-runtime/plugin-isolation';
 * const isolation = new PluginIsolation({
 *   whitelist: ['receipt:generate', 'schema:validate'],
 *   strictMode: true
 * });
 * const allowed = isolation.checkCapability('receipt:generate'); // true
 * const denied = isolation.checkCapability('filesystem:write'); // false
 */
export class PluginIsolation {
  /**
   * Create new plugin isolation manager
   * @param {Object} config - Sandbox configuration
   */
  constructor(config = {}) {
    this.config = SandboxConfigSchema.parse(config);

    // Build complete whitelist
    this.whitelist = new Set([
      ...this.config.whitelist,
      ...(this.config.additionalCapabilities || []),
    ]);

    // Blocked capabilities always rejected
    this.blocklist = new Set(BLOCKED_CAPABILITIES);

    /** @type {Array<Object>} Access log */
    this.accessLog = [];

    /** @type {Map<string, number>} Capability usage counter */
    this.usageStats = new Map();
  }

  /**
   * Check if a capability is allowed
   *
   * @param {string} capability - Capability to check (format: "category:action")
   * @returns {boolean} True if allowed
   *
   * @example
   * const allowed = isolation.checkCapability('receipt:generate');
   * console.log(allowed); // true or false
   */
  checkCapability(capability) {
    // Validate format
    try {
      CapabilitySchema.parse(capability);
    } catch {
      return false;
    }

    // Always deny blocked capabilities
    if (this.blocklist.has(capability)) {
      this._logAccess(capability, false, 'blocked');
      return false;
    }

    // Check whitelist
    const allowed = this.whitelist.has(capability);

    // In strict mode, deny if not explicitly whitelisted
    if (this.config.strictMode && !allowed) {
      this._logAccess(capability, false, 'not_whitelisted');
      return false;
    }

    // Log successful access
    if (allowed) {
      this._logAccess(capability, true, 'allowed');
    }

    return allowed;
  }

  /**
   * Request a capability (throws if denied)
   *
   * @param {string} capability - Capability to request
   * @throws {Error} If capability is denied
   *
   * @example
   * isolation.requestCapability('receipt:generate'); // OK
   * isolation.requestCapability('filesystem:write'); // throws Error
   */
  requestCapability(capability) {
    if (!this.checkCapability(capability)) {
      throw new Error(`Capability denied: ${capability}`);
    }
  }

  /**
   * Grant additional capability
   *
   * @param {string} capability - Capability to grant
   * @throws {Error} If capability is blocked
   *
   * @example
   * isolation.grantCapability('custom:action');
   */
  grantCapability(capability) {
    CapabilitySchema.parse(capability);

    if (this.blocklist.has(capability)) {
      throw new Error(`Cannot grant blocked capability: ${capability}`);
    }

    this.whitelist.add(capability);
    this._logAccess(capability, true, 'granted');
  }

  /**
   * Revoke a capability
   *
   * @param {string} capability - Capability to revoke
   *
   * @example
   * isolation.revokeCapability('custom:action');
   */
  revokeCapability(capability) {
    this.whitelist.delete(capability);
    this._logAccess(capability, false, 'revoked');
  }

  /**
   * Get whitelisted capabilities
   *
   * @returns {string[]} Array of whitelisted capabilities
   */
  getWhitelist() {
    return Array.from(this.whitelist);
  }

  /**
   * Get blocked capabilities
   *
   * @returns {string[]} Array of blocked capabilities
   */
  getBlocklist() {
    return Array.from(this.blocklist);
  }

  /**
   * Get access log
   *
   * @param {Object} filters - Optional filters
   * @param {string} filters.capability - Filter by capability
   * @param {boolean} filters.allowed - Filter by allowed status
   * @returns {Array<Object>} Filtered access log
   */
  getAccessLog(filters = {}) {
    let log = this.accessLog;

    if (filters.capability) {
      log = log.filter(entry => entry.capability === filters.capability);
    }

    if (filters.allowed !== undefined) {
      log = log.filter(entry => entry.allowed === filters.allowed);
    }

    return log;
  }

  /**
   * Get usage statistics
   *
   * @returns {Object} Usage stats by capability
   */
  getUsageStats() {
    return Object.fromEntries(this.usageStats);
  }

  /**
   * Create isolated execution context
   *
   * @param {Function} fn - Function to execute
   * @param {Array<string>} requiredCapabilities - Required capabilities
   * @returns {Promise<any>} Execution result
   * @throws {Error} If any required capability is denied
   *
   * @example
   * const result = await isolation.executeIsolated(
   *   async () => { return { data: 42 }; },
   *   ['receipt:generate']
   * );
   */
  async executeIsolated(fn, requiredCapabilities = []) {
    // Check all required capabilities upfront
    for (const capability of requiredCapabilities) {
      this.requestCapability(capability);
    }

    const startTime = Date.now();

    try {
      // Execute with timeout
      const result = await this._executeWithTimeout(fn, this.config.maxExecutionTime);

      const duration = Date.now() - startTime;

      // Log execution
      this._logExecution(requiredCapabilities, true, duration);

      return result;
    } catch (error) {
      const duration = Date.now() - startTime;
      this._logExecution(requiredCapabilities, false, duration, error.message);
      throw error;
    }
  }

  /**
   * Validate plugin manifest capabilities
   *
   * @param {Array<string>} requestedCapabilities - Capabilities from plugin manifest
   * @returns {Object} Validation result { allowed: [], denied: [], blocked: [] }
   *
   * @example
   * const validation = isolation.validatePluginCapabilities([
   *   'receipt:generate',
   *   'filesystem:write'
   * ]);
   * console.log(validation.denied); // ['filesystem:write']
   */
  validatePluginCapabilities(requestedCapabilities) {
    const allowed = [];
    const denied = [];
    const blocked = [];

    for (const capability of requestedCapabilities) {
      if (this.blocklist.has(capability)) {
        blocked.push(capability);
      } else if (this.whitelist.has(capability)) {
        allowed.push(capability);
      } else {
        denied.push(capability);
      }
    }

    return { allowed, denied, blocked };
  }

  // ===== Private Methods =====

  /**
   * Log capability access
   * @private
   */
  _logAccess(capability, allowed, reason) {
    this.accessLog.push({
      timestamp: Date.now(),
      capability,
      allowed,
      reason,
    });

    // Update usage stats
    if (allowed) {
      const count = this.usageStats.get(capability) || 0;
      this.usageStats.set(capability, count + 1);
    }
  }

  /**
   * Log execution
   * @private
   */
  _logExecution(capabilities, success, duration, error = null) {
    this.accessLog.push({
      timestamp: Date.now(),
      type: 'execution',
      capabilities,
      success,
      duration,
      error,
    });
  }

  /**
   * Execute function with timeout
   * @private
   */
  async _executeWithTimeout(fn, timeout) {
    return Promise.race([
      fn(),
      new Promise((_, reject) =>
        setTimeout(() => reject(new Error('Execution timeout')), timeout)
      ),
    ]);
  }
}

/**
 * Create a new plugin isolation manager
 *
 * @param {Object} config - Sandbox configuration
 * @returns {PluginIsolation} New isolation manager
 */
export function createPluginIsolation(config = {}) {
  return new PluginIsolation(config);
}

/**
 * Create a public API proxy that only exposes whitelisted methods
 *
 * @param {Object} api - Full API object
 * @param {Array<string>} whitelist - Whitelisted method names
 * @returns {Object} Proxy object with only whitelisted methods
 *
 * @example
 * const publicAPI = createPublicAPI(fullAPI, ['generate', 'validate']);
 */
export function createPublicAPI(api, whitelist) {
  const proxy = {};

  for (const method of whitelist) {
    if (typeof api[method] === 'function') {
      proxy[method] = api[method].bind(api);
    } else if (api[method] !== undefined) {
      proxy[method] = api[method];
    }
  }

  return Object.freeze(proxy);
}
