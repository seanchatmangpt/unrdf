/**
 * Configuration manager domain model
 * @module domain/models/config
 */

import { ConfigSchema, OutputFormatSchema, ShellTypeSchema } from '../types.mjs';

/**
 * @typedef {import('../types.mjs').ConfigData} ConfigData
 */

/**
 * Default configuration values
 * @type {ConfigData}
 */
const DEFAULT_CONFIG = {
  outputFormat: 'json',
  verbose: false,
  color: true,
  timeout: 30000,
  maxResults: 100,
};

/**
 * Configuration manager class
 * @class Config
 */
export class Config {
  /**
   * Create a new Config instance
   * @param {Partial<ConfigData>} [initial] - Initial config values
   */
  constructor(initial = {}) {
    // Merge with defaults and validate
    const merged = { ...DEFAULT_CONFIG, ...initial };
    const result = ConfigSchema.safeParse(merged);
    if (!result.success) {
      throw new Error(`Invalid configuration: ${result.error.message}`);
    }

    /** @type {ConfigData} */
    this._data = result.data;

    /** @type {Set<(key: string, value: unknown, oldValue: unknown) => void>} */
    this._listeners = new Set();
  }

  /**
   * Get a config value
   * @param {keyof ConfigData} key - Config key
   * @returns {unknown}
   */
  get(key) {
    return this._data[key];
  }

  /**
   * Set a config value
   * @param {keyof ConfigData} key - Config key
   * @param {unknown} value - Config value
   * @returns {Config} this instance for chaining
   * @throws {Error} If validation fails
   */
  set(key, value) {
    // Validate the specific key
    const testData = { ...this._data, [key]: value };
    const result = ConfigSchema.safeParse(testData);
    if (!result.success) {
      throw new Error(`Invalid value for '${key}': ${result.error.message}`);
    }

    const oldValue = this._data[key];
    this._data = result.data;

    // Notify listeners
    for (const listener of this._listeners) {
      listener(key, value, oldValue);
    }

    return this;
  }

  /**
   * Check if a config key exists
   * @param {string} key - Config key
   * @returns {boolean}
   */
  has(key) {
    return key in this._data;
  }

  /**
   * Delete a config value (reset to default)
   * @param {keyof ConfigData} key - Config key
   * @returns {boolean} true if key was reset
   */
  delete(key) {
    if (key in DEFAULT_CONFIG) {
      const oldValue = this._data[key];
      this._data[key] = DEFAULT_CONFIG[key];
      for (const listener of this._listeners) {
        listener(key, DEFAULT_CONFIG[key], oldValue);
      }
      return true;
    }
    if (key === 'customSettings' && this._data.customSettings) {
      const oldValue = this._data.customSettings;
      delete this._data.customSettings;
      for (const listener of this._listeners) {
        listener(key, undefined, oldValue);
      }
      return true;
    }
    return false;
  }

  /**
   * Get all config keys
   * @returns {string[]}
   */
  keys() {
    return Object.keys(this._data);
  }

  /**
   * Get all config entries
   * @returns {Array<[string, unknown]>}
   */
  entries() {
    return Object.entries(this._data);
  }

  /**
   * Get output format
   * @returns {import('../types.mjs').OutputFormat}
   */
  getOutputFormat() {
    return /** @type {import('../types.mjs').OutputFormat} */ (this._data.outputFormat);
  }

  /**
   * Set output format
   * @param {string} format - Output format
   * @returns {Config} this instance for chaining
   */
  setOutputFormat(format) {
    const result = OutputFormatSchema.safeParse(format);
    if (!result.success) {
      throw new Error(`Invalid output format: ${format}`);
    }
    return this.set('outputFormat', result.data);
  }

  /**
   * Get shell type
   * @returns {import('../types.mjs').ShellType|undefined}
   */
  getShell() {
    return /** @type {import('../types.mjs').ShellType|undefined} */ (this._data.shell);
  }

  /**
   * Set shell type
   * @param {string} shell - Shell type
   * @returns {Config} this instance for chaining
   */
  setShell(shell) {
    const result = ShellTypeSchema.safeParse(shell);
    if (!result.success) {
      throw new Error(`Invalid shell type: ${shell}`);
    }
    return this.set('shell', result.data);
  }

  /**
   * Check if verbose mode is enabled
   * @returns {boolean}
   */
  isVerbose() {
    return Boolean(this._data.verbose);
  }

  /**
   * Set verbose mode
   * @param {boolean} enabled - Enable or disable
   * @returns {Config} this instance for chaining
   */
  setVerbose(enabled) {
    return this.set('verbose', Boolean(enabled));
  }

  /**
   * Check if color output is enabled
   * @returns {boolean}
   */
  isColorEnabled() {
    return Boolean(this._data.color);
  }

  /**
   * Set color output
   * @param {boolean} enabled - Enable or disable
   * @returns {Config} this instance for chaining
   */
  setColor(enabled) {
    return this.set('color', Boolean(enabled));
  }

  /**
   * Get timeout value in ms
   * @returns {number}
   */
  getTimeout() {
    return Number(this._data.timeout);
  }

  /**
   * Set timeout value
   * @param {number} ms - Timeout in milliseconds
   * @returns {Config} this instance for chaining
   */
  setTimeout(ms) {
    return this.set('timeout', ms);
  }

  /**
   * Get max results limit
   * @returns {number}
   */
  getMaxResults() {
    return Number(this._data.maxResults);
  }

  /**
   * Set max results limit
   * @param {number} limit - Maximum results
   * @returns {Config} this instance for chaining
   */
  setMaxResults(limit) {
    return this.set('maxResults', limit);
  }

  /**
   * Get custom setting
   * @param {string} key - Setting key
   * @returns {unknown}
   */
  getCustom(key) {
    return this._data.customSettings?.[key];
  }

  /**
   * Set custom setting
   * @param {string} key - Setting key
   * @param {unknown} value - Setting value
   * @returns {Config} this instance for chaining
   */
  setCustom(key, value) {
    const customSettings = { ...this._data.customSettings, [key]: value };
    return this.set('customSettings', customSettings);
  }

  /**
   * Add a change listener
   * @param {(key: string, value: unknown, oldValue: unknown) => void} listener - Listener function
   * @returns {() => void} Unsubscribe function
   */
  onChange(listener) {
    this._listeners.add(listener);
    return () => this._listeners.delete(listener);
  }

  /**
   * Reset all values to defaults
   * @returns {Config} this instance for chaining
   */
  reset() {
    const oldData = { ...this._data };
    this._data = { ...DEFAULT_CONFIG };
    for (const listener of this._listeners) {
      for (const key of Object.keys(oldData)) {
        if (oldData[key] !== this._data[key]) {
          listener(key, this._data[key], oldData[key]);
        }
      }
    }
    return this;
  }

  /**
   * Merge config values
   * @param {Partial<ConfigData>} values - Values to merge
   * @returns {Config} this instance for chaining
   */
  merge(values) {
    const merged = { ...this._data, ...values };
    const result = ConfigSchema.safeParse(merged);
    if (!result.success) {
      throw new Error(`Invalid configuration values: ${result.error.message}`);
    }
    const oldData = { ...this._data };
    this._data = result.data;
    for (const listener of this._listeners) {
      for (const key of Object.keys(values)) {
        if (oldData[key] !== this._data[key]) {
          listener(key, this._data[key], oldData[key]);
        }
      }
    }
    return this;
  }

  /**
   * Convert to plain object
   * @returns {ConfigData}
   */
  toJSON() {
    return { ...this._data };
  }

  /**
   * Validate the current config
   * @returns {{success: boolean, data?: ConfigData, error?: import('zod').ZodError}}
   */
  validate() {
    return ConfigSchema.safeParse(this._data);
  }

  /**
   * Create from JSON object
   * @param {Object} json - JSON object
   * @returns {Config}
   * @throws {Error} If validation fails
   */
  static fromJSON(json) {
    const result = ConfigSchema.safeParse(json);
    if (!result.success) {
      throw new Error(`Invalid config data: ${result.error.message}`);
    }
    return new Config(result.data);
  }

  /**
   * Create with default values
   * @returns {Config}
   */
  static create() {
    return new Config();
  }

  /**
   * Get default configuration
   * @returns {ConfigData}
   */
  static getDefaults() {
    return { ...DEFAULT_CONFIG };
  }
}
