/**
 * @fileoverview Config Middleware
 *
 * @description
 * Loads CLI configuration from files, applies CLI overrides,
 * validates config with Zod, and caches loaded configurations.
 * Supports multiple config file formats (JSON, YAML, JS).
 *
 * @module cli/middleware/config
 * @version 1.0.0
 * @license MIT
 */

import { readFileSync, existsSync } from 'node:fs';
import { resolve, join, extname, dirname } from 'node:path';
import { z } from 'zod';
import YAML from 'yaml';

/**
 * @typedef {Object} CLIConfig
 * @property {Object} logging - Logging configuration
 * @property {Object} output - Output configuration
 * @property {Object} validation - Validation configuration
 * @property {Object} middleware - Middleware configuration
 * @property {Object} templates - Template paths
 * @property {Object} knowledgeGraph - Knowledge graph configuration
 */

/**
 * Config file search paths in priority order
 * @type {Array<string>}
 */
const CONFIG_SEARCH_PATHS = [
  '.playgroundrc',
  '.playgroundrc.json',
  '.playgroundrc.yaml',
  '.playgroundrc.yml',
  'playground.config.mjs',
  'playground.config.js',
  'playground.config.json'
];

/**
 * Config cache
 * @type {Map<string, {config: Object, timestamp: number}>}
 */
const configCache = new Map();

/**
 * Cache TTL in milliseconds (5 minutes)
 */
const CACHE_TTL = 5 * 60 * 1000;

/**
 * Default configuration
 * @type {CLIConfig}
 */
export const DEFAULT_CONFIG = {
  logging: {
    level: 'info',
    format: 'pretty',
    structured: false
  },
  output: {
    format: 'table',
    color: true,
    directory: './output'
  },
  validation: {
    strict: false,
    abortOnError: true
  },
  middleware: {
    abortOnError: true,
    enabled: ['config', 'validation', 'logging', 'profiling']
  },
  templates: {
    papers: './templates/papers',
    thesis: './templates/thesis',
    custom: './templates/custom'
  },
  knowledgeGraph: {
    endpoint: null,
    defaultGraph: null,
    prefixes: {}
  }
};

/**
 * Zod schema for configuration validation
 */
export const ConfigSchema = z.object({
  logging: z.object({
    level: z.enum(['debug', 'info', 'warn', 'error']).default('info'),
    format: z.enum(['pretty', 'json', 'jsonld']).default('pretty'),
    structured: z.boolean().default(false)
  }).default({}),

  output: z.object({
    format: z.enum(['json', 'json-pretty', 'yaml', 'table', 'latex', 'csv']).default('table'),
    color: z.boolean().default(true),
    directory: z.string().default('./output')
  }).default({}),

  validation: z.object({
    strict: z.boolean().default(false),
    abortOnError: z.boolean().default(true),
    schema: z.any().optional(),
    preconditions: z.array(z.object({
      type: z.string(),
      args: z.string(),
      label: z.string().optional()
    })).default([]),
    custom: z.array(z.object({
      name: z.string(),
      args: z.string()
    })).default([])
  }).default({}),

  middleware: z.object({
    abortOnError: z.boolean().default(true),
    enabled: z.array(z.string()).default(['config', 'validation', 'logging', 'profiling'])
  }).default({}),

  templates: z.object({
    papers: z.string().default('./templates/papers'),
    thesis: z.string().default('./templates/thesis'),
    custom: z.string().default('./templates/custom')
  }).default({}),

  knowledgeGraph: z.object({
    endpoint: z.string().nullable().default(null),
    defaultGraph: z.string().nullable().default(null),
    prefixes: z.record(z.string()).default({})
  }).default({})
}).strict().default({});

/**
 * Load configuration from a file
 * @param {string} filePath - Path to config file
 * @returns {Object} Loaded configuration
 */
export function loadConfigFile(filePath) {
  const resolved = resolve(filePath);

  if (!existsSync(resolved)) {
    throw new Error(`Config file not found: ${filePath}`);
  }

  const ext = extname(resolved).toLowerCase();
  const content = readFileSync(resolved, 'utf-8');

  switch (ext) {
    case '.json':
      return JSON.parse(content);

    case '.yaml':
    case '.yml':
      return YAML.parse(content);

    case '.mjs':
    case '.js':
      // Note: Dynamic import would need to be async
      // For now, only support JSON/YAML in sync context
      throw new Error('JavaScript config files require async loading. Use .json or .yaml instead.');

    default:
      // Try to parse as JSON, then YAML
      try {
        return JSON.parse(content);
      } catch {
        return YAML.parse(content);
      }
  }
}

/**
 * Async load config file (supports JS/MJS)
 * @param {string} filePath - Path to config file
 * @returns {Promise<Object>} Loaded configuration
 */
export async function loadConfigFileAsync(filePath) {
  const resolved = resolve(filePath);

  if (!existsSync(resolved)) {
    throw new Error(`Config file not found: ${filePath}`);
  }

  const ext = extname(resolved).toLowerCase();

  if (ext === '.mjs' || ext === '.js') {
    const module = await import(resolved);
    return module.default || module;
  }

  return loadConfigFile(filePath);
}

/**
 * Find config file in search paths
 * @param {string} [startDir] - Directory to start searching from
 * @returns {string|null} Path to config file or null
 */
export function findConfigFile(startDir = process.cwd()) {
  let currentDir = resolve(startDir);
  const root = dirname(currentDir);

  // Search up the directory tree
  while (currentDir !== root) {
    for (const configName of CONFIG_SEARCH_PATHS) {
      const configPath = join(currentDir, configName);
      if (existsSync(configPath)) {
        return configPath;
      }
    }
    currentDir = dirname(currentDir);
  }

  // Check home directory
  const homeDir = process.env.HOME || process.env.USERPROFILE;
  if (homeDir) {
    for (const configName of CONFIG_SEARCH_PATHS) {
      const configPath = join(homeDir, configName);
      if (existsSync(configPath)) {
        return configPath;
      }
    }
  }

  return null;
}

/**
 * Get cached config or load from file
 * @param {string} filePath - Path to config file
 * @returns {Object} Configuration
 */
function getCachedConfig(filePath) {
  const resolved = resolve(filePath);
  const cached = configCache.get(resolved);

  if (cached && (Date.now() - cached.timestamp) < CACHE_TTL) {
    return cached.config;
  }

  const config = loadConfigFile(filePath);
  configCache.set(resolved, { config, timestamp: Date.now() });
  return config;
}

/**
 * Clear config cache
 */
export function clearConfigCache() {
  configCache.clear();
}

/**
 * Merge configurations with deep merge
 * @param {Object} base - Base configuration
 * @param {Object} override - Override configuration
 * @returns {Object} Merged configuration
 */
export function mergeConfigs(base, override) {
  const result = { ...base };

  for (const [key, value] of Object.entries(override)) {
    if (value === undefined) continue;

    if (
      typeof value === 'object' &&
      value !== null &&
      !Array.isArray(value) &&
      typeof result[key] === 'object' &&
      result[key] !== null
    ) {
      result[key] = mergeConfigs(result[key], value);
    } else {
      result[key] = value;
    }
  }

  return result;
}

/**
 * Apply CLI argument overrides to config
 * @param {Object} config - Base configuration
 * @param {Object} args - CLI arguments
 * @returns {Object} Configuration with overrides
 */
export function applyCliOverrides(config, args) {
  const overrides = {};

  // Map common CLI args to config paths
  if (args.format) {
    overrides.output = { ...config.output, format: args.format };
  }

  if (args.quiet) {
    overrides.logging = { ...config.logging, level: 'error' };
  }

  if (args.verbose) {
    overrides.logging = { ...config.logging, level: 'debug' };
  }

  if (args.output) {
    overrides.output = { ...config.output, directory: dirname(args.output) };
  }

  if (args.strict !== undefined) {
    overrides.validation = { ...config.validation, strict: args.strict };
  }

  // Handle dot-notation config overrides (e.g., --config.logging.level=debug)
  for (const [key, value] of Object.entries(args)) {
    if (key.startsWith('config.')) {
      const path = key.replace('config.', '').split('.');
      let target = overrides;

      for (let i = 0; i < path.length - 1; i++) {
        target[path[i]] = target[path[i]] || {};
        target = target[path[i]];
      }

      target[path[path.length - 1]] = value;
    }
  }

  return mergeConfigs(config, overrides);
}

/**
 * Config middleware handler
 * @param {Object} context - Middleware context
 * @returns {Promise<Object>} Modified context
 */
export async function configMiddleware(context) {
  let config = { ...DEFAULT_CONFIG };

  // Try to load config from specified path
  if (context.args.config || context.options.config) {
    const configPath = context.args.config || context.options.config;
    try {
      const loadedConfig = getCachedConfig(configPath);
      config = mergeConfigs(config, loadedConfig);
    } catch (err) {
      if (!context.quiet) {
        console.warn(`Warning: Could not load config from ${configPath}: ${err.message}`);
      }
    }
  } else {
    // Try to find config file
    const foundConfig = findConfigFile();
    if (foundConfig) {
      try {
        const loadedConfig = getCachedConfig(foundConfig);
        config = mergeConfigs(config, loadedConfig);

        if (context.verbose) {
          console.log(`Using config from: ${foundConfig}`);
        }
      } catch (err) {
        if (!context.quiet) {
          console.warn(`Warning: Could not load config from ${foundConfig}: ${err.message}`);
        }
      }
    }
  }

  // Apply CLI overrides
  config = applyCliOverrides(config, context.args);

  // Validate config with Zod
  const validationResult = ConfigSchema.safeParse(config);

  if (!validationResult.success) {
    const errors = validationResult.error.errors
      .map(e => `  - ${e.path.join('.')}: ${e.message}`)
      .join('\n');

    if (!context.quiet) {
      console.warn(`Config validation warnings:\n${errors}`);
    }

    // Use partially valid config with defaults filled in
    config = ConfigSchema.parse({});
  } else {
    config = validationResult.data;
  }

  // Store config in context
  context.config = config;

  // Add config utilities to context
  context.getConfig = (path, defaultValue) => {
    if (!path) return config;

    return path.split('.').reduce((obj, key) => {
      return obj?.[key];
    }, config) ?? defaultValue;
  };

  context.setConfig = (path, value) => {
    const parts = path.split('.');
    let target = config;

    for (let i = 0; i < parts.length - 1; i++) {
      target[parts[i]] = target[parts[i]] || {};
      target = target[parts[i]];
    }

    target[parts[parts.length - 1]] = value;
  };

  return context;
}

export default configMiddleware;
