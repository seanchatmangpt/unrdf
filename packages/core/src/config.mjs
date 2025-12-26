/**
 * @file Configuration management for UNRDF
 * @module @unrdf/core/config
 *
 * Provides type-safe configuration with:
 * - Environment variable parsing
 * - Zod validation
 * - Sensible defaults
 * - Configuration precedence: env > explicit > defaults
 */

import { z } from 'zod';

/**
 * Core configuration schema
 */
export const ConfigSchema = z.object({
  // Store configuration
  store: z.object({
    capacity: z.number().int().min(100).default(10000),
    validate: z.boolean().default(true),
  }).default({}),

  // Query configuration
  query: z.object({
    timeout: z.number().int().min(0).default(5000),
    maxResults: z.number().int().min(1).default(10000),
  }).default({}),

  // Environment
  env: z.enum(['development', 'production', 'test']).default('production'),

  // Logging
  logging: z.object({
    level: z.enum(['debug', 'info', 'warn', 'error']).default('info'),
    enabled: z.boolean().default(true),
  }).default({}),
});

/**
 * @typedef {z.infer<typeof ConfigSchema>} Config
 */

/**
 * Default configuration
 */
export const DEFAULT_CONFIG = {
  store: {
    capacity: 10000,
    validate: true,
  },
  query: {
    timeout: 5000,
    maxResults: 10000,
  },
  env: 'production',
  logging: {
    level: 'info',
    enabled: true,
  },
};

/**
 * Parse environment variables into config
 *
 * Environment variable mapping:
 * - UNRDF_STORE_CAPACITY → store.capacity
 * - UNRDF_QUERY_TIMEOUT → query.timeout
 * - UNRDF_ENV → env
 * - UNRDF_LOG_LEVEL → logging.level
 *
 * @param {Object} env - Environment object (default: process.env)
 * @returns {Partial<Config>} Parsed configuration
 * @example
 * const config = parseEnv(process.env);
 */
export function parseEnv(env = {}) {
  const config = {};

  // Store configuration
  if (env.UNRDF_STORE_CAPACITY) {
    config.store = config.store || {};
    config.store.capacity = Number.parseInt(env.UNRDF_STORE_CAPACITY, 10);
  }

  if (env.UNRDF_STORE_VALIDATE !== undefined) {
    config.store = config.store || {};
    config.store.validate = env.UNRDF_STORE_VALIDATE === 'true';
  }

  // Query configuration
  if (env.UNRDF_QUERY_TIMEOUT) {
    config.query = config.query || {};
    config.query.timeout = Number.parseInt(env.UNRDF_QUERY_TIMEOUT, 10);
  }

  if (env.UNRDF_QUERY_MAX_RESULTS) {
    config.query = config.query || {};
    config.query.maxResults = Number.parseInt(env.UNRDF_QUERY_MAX_RESULTS, 10);
  }

  // Environment
  if (env.UNRDF_ENV) {
    config.env = env.UNRDF_ENV;
  } else if (env.NODE_ENV) {
    config.env = env.NODE_ENV;
  }

  // Logging
  if (env.UNRDF_LOG_LEVEL) {
    config.logging = config.logging || {};
    config.logging.level = env.UNRDF_LOG_LEVEL;
  }

  if (env.UNRDF_LOG_ENABLED !== undefined) {
    config.logging = config.logging || {};
    config.logging.enabled = env.UNRDF_LOG_ENABLED === 'true';
  }

  return config;
}

/**
 * Merge configurations with precedence
 *
 * Precedence order (highest to lowest):
 * 1. Explicit config
 * 2. Environment variables
 * 3. Defaults
 *
 * @param {...Partial<Config>} configs - Configurations to merge
 * @returns {Config} Merged configuration
 * @example
 * const config = mergeConfig(
 *   DEFAULT_CONFIG,
 *   parseEnv(process.env),
 *   { query: { timeout: 10000 } }
 * );
 */
export function mergeConfig(...configs) {
  const merged = {};

  for (const config of configs) {
    for (const key in config) {
      if (typeof config[key] === 'object' && !Array.isArray(config[key])) {
        merged[key] = { ...merged[key], ...config[key] };
      } else {
        merged[key] = config[key];
      }
    }
  }

  return merged;
}

/**
 * Create validated configuration
 *
 * Merges defaults, environment, and explicit config, then validates.
 *
 * @param {Partial<Config>} [explicit={}] - Explicit configuration
 * @param {Object} [env=process.env] - Environment variables
 * @returns {Config} Validated configuration
 * @throws {Error} If configuration is invalid
 * @example
 * const config = createConfig({
 *   query: { timeout: 10000 }
 * });
 *
 * @example
 * const config = createConfig({}, process.env);
 */
export function createConfig(explicit = {}, env) {
  // Use process.env if in Node.js environment
  const envObject = env ?? (typeof process !== 'undefined' ? process.env : {});

  // Merge: defaults < env < explicit
  const merged = mergeConfig(
    DEFAULT_CONFIG,
    parseEnv(envObject),
    explicit
  );

  // Validate
  try {
    return ConfigSchema.parse(merged);
  } catch (error) {
    throw new Error(`Invalid configuration: ${error.message}`);
  }
}

/**
 * Get configuration value by path
 *
 * @param {Config} config - Configuration object
 * @param {string} path - Dot-separated path (e.g., 'query.timeout')
 * @returns {*} Configuration value
 * @example
 * const timeout = getConfigValue(config, 'query.timeout');
 */
export function getConfigValue(config, path) {
  const parts = path.split('.');
  let value = config;

  for (const part of parts) {
    value = value?.[part];
    if (value === undefined) {
      return undefined;
    }
  }

  return value;
}

/**
 * Validate partial configuration
 *
 * Useful for validating user input before merging.
 *
 * @param {*} config - Configuration to validate
 * @returns {boolean} True if valid
 * @example
 * if (validateConfig(userConfig)) {
 *   const final = createConfig(userConfig);
 * }
 */
export function validateConfig(config) {
  try {
    ConfigSchema.partial().parse(config);
    return true;
  } catch {
    return false;
  }
}

/**
 * Global configuration instance
 * @type {Config}
 */
let globalConfig = null;

/**
 * Get global configuration
 *
 * Creates default config if not initialized.
 *
 * @returns {Config} Global configuration
 * @example
 * const config = getGlobalConfig();
 * console.log(config.query.timeout);
 */
export function getGlobalConfig() {
  if (!globalConfig) {
    globalConfig = createConfig();
  }
  return globalConfig;
}

/**
 * Set global configuration
 *
 * @param {Partial<Config>} config - Configuration to set
 * @returns {Config} New global configuration
 * @example
 * setGlobalConfig({ query: { timeout: 10000 } });
 */
export function setGlobalConfig(config) {
  globalConfig = createConfig(config);
  return globalConfig;
}

/**
 * Reset global configuration to defaults
 *
 * @returns {Config} Default configuration
 * @example
 * resetGlobalConfig();
 */
export function resetGlobalConfig() {
  globalConfig = createConfig();
  return globalConfig;
}
