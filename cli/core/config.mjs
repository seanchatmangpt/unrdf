/**
 * @file CLI Configuration Loader
 * @module cli-v2/core/config
 *
 * @description
 * Hierarchical configuration loading from multiple sources.
 */

import { readFile, access } from 'node:fs/promises';
import { resolve, join } from 'node:path';
import { homedir } from 'node:os';
import { z } from 'zod';

/**
 * Configuration schema
 */
const ConfigSchema = z.object({
  baseIRI: z.string().url().default('http://example.org/'),
  sidecar: z.object({
    endpoint: z.string().url().default('http://localhost:50051'),
    timeout: z.number().default(30000),
    retries: z.number().default(3)
  }).default({}),
  store: z.object({
    type: z.enum(['memory', 'file', 'oxigraph']).default('memory'),
    path: z.string().optional()
  }).default({}),
  telemetry: z.object({
    enabled: z.boolean().default(true),
    endpoint: z.string().optional()
  }).default({}),
  output: z.object({
    format: z.enum(['json', 'yaml', 'table', 'tree']).default('table'),
    color: z.boolean().default(true)
  }).default({}),
  plugins: z.object({
    enabled: z.boolean().default(true),
    dirs: z.array(z.string()).optional()
  }).default({})
});

/**
 * Load configuration from multiple sources in hierarchical order:
 * 1. Command-line flags
 * 2. Environment variables
 * 3. Current directory config file
 * 4. User home directory config file
 * 5. System-wide config file
 * 6. Defaults
 *
 * @param {Object} overrides - Command-line overrides
 * @returns {Promise<Object>} Merged configuration
 */
export async function loadConfig(overrides = {}) {
  const configs = [];

  // Load from system config
  const systemConfigPath = '/etc/unrdf/config.json';
  configs.push(await loadConfigFile(systemConfigPath));

  // Load from user home config
  const homeConfigPath = join(homedir(), '.unrdf/config.json');
  configs.push(await loadConfigFile(homeConfigPath));

  // Load from current directory
  const localConfigPath = resolve(process.cwd(), 'unrdf.config.json');
  configs.push(await loadConfigFile(localConfigPath));

  // Load from current directory (mjs variant)
  const localConfigMjsPath = resolve(process.cwd(), 'unrdf.config.mjs');
  configs.push(await loadConfigModuleFile(localConfigMjsPath));

  // Load from environment variables
  configs.push(loadEnvConfig());

  // Add overrides
  configs.push(overrides);

  // Merge all configs
  const merged = mergeConfigs(configs);

  // Validate and apply defaults
  return ConfigSchema.parse(merged);
}

/**
 * Load configuration from a JSON file
 * @param {string} path - Config file path
 * @returns {Promise<Object>} Configuration object
 */
async function loadConfigFile(path) {
  try {
    await access(path);
    const content = await readFile(path, 'utf-8');
    return JSON.parse(content);
  } catch {
    return {};
  }
}

/**
 * Load configuration from an ES module file
 * @param {string} path - Config file path
 * @returns {Promise<Object>} Configuration object
 */
async function loadConfigModuleFile(path) {
  try {
    await access(path);
    const module = await import(path);
    return module.default || {};
  } catch {
    return {};
  }
}

/**
 * Load configuration from environment variables
 * @returns {Object} Configuration from env vars
 */
function loadEnvConfig() {
  const config = {};

  if (process.env.UNRDF_BASE_IRI) {
    config.baseIRI = process.env.UNRDF_BASE_IRI;
  }

  if (process.env.UNRDF_SIDECAR_ENDPOINT) {
    config.sidecar = config.sidecar || {};
    config.sidecar.endpoint = process.env.UNRDF_SIDECAR_ENDPOINT;
  }

  if (process.env.UNRDF_OUTPUT_FORMAT) {
    config.output = config.output || {};
    config.output.format = process.env.UNRDF_OUTPUT_FORMAT;
  }

  if (process.env.UNRDF_TELEMETRY_ENABLED) {
    config.telemetry = config.telemetry || {};
    config.telemetry.enabled = process.env.UNRDF_TELEMETRY_ENABLED === 'true';
  }

  return config;
}

/**
 * Deep merge multiple configuration objects
 * @param {Array<Object>} configs - Array of config objects
 * @returns {Object} Merged configuration
 */
function mergeConfigs(configs) {
  const result = {};

  for (const config of configs) {
    deepMerge(result, config);
  }

  return result;
}

/**
 * Deep merge two objects
 * @param {Object} target - Target object
 * @param {Object} source - Source object
 */
function deepMerge(target, source) {
  for (const key in source) {
    if (source[key] && typeof source[key] === 'object' && !Array.isArray(source[key])) {
      target[key] = target[key] || {};
      deepMerge(target[key], source[key]);
    } else {
      target[key] = source[key];
    }
  }
}
