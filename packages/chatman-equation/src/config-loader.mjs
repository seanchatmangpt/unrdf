/**
 * @file config-loader.mjs
 * @module @unrdf/chatman-equation/config-loader
 * @description TOML configuration loader with validation
 */

import { readFileSync } from 'fs';
import TOML from '@iarna/toml';
import { z } from 'zod';

/**
 * Base config schema
 */
const BaseConfigSchema = z.object({
  template: z.string().optional(),
  output: z.string().optional(),
  title: z.string(),
  description: z.string().optional(),
  version: z.string().optional(),
  updated: z.string().optional(),
}).passthrough(); // Allow additional fields

/**
 * Load and parse TOML configuration file
 * @param {string} configPath - Path to TOML config file
 * @returns {Object} Parsed configuration object
 * @throws {Error} If file not found or parsing fails
 * @example
 * const config = loadConfig('./config/equation.toml');
 */
export function loadConfig(configPath) {
  try {
    const content = readFileSync(configPath, 'utf-8');
    const parsed = TOML.parse(content);

    // Validate base structure
    const validated = BaseConfigSchema.parse(parsed);

    // Add metadata
    validated._configPath = configPath;
    validated._loadedAt = new Date().toISOString();

    return validated;
  } catch (error) {
    if (error.code === 'ENOENT') {
      throw new Error(`Config file not found: ${configPath}`);
    }
    throw new Error(`Failed to load config from ${configPath}: ${error.message}`);
  }
}

/**
 * Load multiple config files
 * @param {Array<string>} configPaths - Array of config file paths
 * @returns {Array<Object>} Array of parsed configurations
 * @example
 * const configs = loadConfigs(['./a.toml', './b.toml']);
 */
export function loadConfigs(configPaths) {
  return configPaths.map((path) => {
    try {
      return loadConfig(path);
    } catch (error) {
      return {
        _error: error.message,
        _path: path,
      };
    }
  });
}

/**
 * Validate config against custom schema
 * @param {Object} config - Configuration object
 * @param {z.ZodSchema} schema - Zod schema for validation
 * @returns {Object} Validated configuration
 * @throws {z.ZodError} If validation fails
 * @example
 * const schema = z.object({ equations: z.array(z.object(...)) });
 * const validated = validateConfig(config, schema);
 */
export function validateConfig(config, schema) {
  return schema.parse(config);
}

/**
 * Merge multiple configs with deep merge
 * @param {...Object} configs - Configuration objects to merge
 * @returns {Object} Merged configuration
 * @example
 * const merged = mergeConfigs(baseConfig, overrideConfig);
 */
export function mergeConfigs(...configs) {
  return configs.reduce((acc, config) => {
    return deepMerge(acc, config);
  }, {});
}

/**
 * Deep merge utility
 * @private
 * @param {Object} target - Target object
 * @param {Object} source - Source object
 * @returns {Object} Merged object
 */
function deepMerge(target, source) {
  const result = { ...target };

  for (const key in source) {
    if (Object.prototype.hasOwnProperty.call(source, key)) {
      if (isObject(source[key]) && isObject(target[key])) {
        result[key] = deepMerge(target[key], source[key]);
      } else {
        result[key] = source[key];
      }
    }
  }

  return result;
}

/**
 * Check if value is an object
 * @private
 * @param {*} value - Value to check
 * @returns {boolean} True if object
 */
function isObject(value) {
  return value !== null && typeof value === 'object' && !Array.isArray(value);
}

export default loadConfig;
