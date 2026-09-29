/**
 * @file Config Parser
 * @module cli/commands/sync/config-parser
 * @description TOML configuration file parser for `unrdf.toml`
 */
import { readFile } from 'fs/promises';
import { existsSync } from 'fs';
import { resolve, dirname, isAbsolute, parse } from 'path';
import TOML from '@iarna/toml';
import { SyncConfigSchema, detectRDFFormat } from './schemas.mjs';

/**
 * Configuration parse error
 */
export class ConfigParseError extends Error {
  /**
   * @param {string} message - Error message
   * @param {Object} [options] - Error options
   * @param {string} [options.path] - Config file path
   * @param {number} [options.line] - Line number
   * @param {number} [options.column] - Column number
   * @param {string} [options.cause] - Underlying cause
   */
  constructor(message, options = {}) {
    super(message);
    this.name = 'ConfigParseError';
    this.path = options.path;
    this.line = options.line;
    this.column = options.column;
    this.cause = options.cause;
  }

  /**
   * Format error for display
   * @returns {string} Formatted error message
   */
  format() {
    let msg = `Configuration Error: ${this.message}`;
    if (this.path) msg += `\n  File: ${this.path}`;
    if (this.line != null) msg += ` at line ${this.line}`;
    if (this.column != null) msg += `, column ${this.column}`;
    if (this.cause) msg += `\n  Cause: ${this.cause}`;
    return msg;
  }
}

/**
 * Configuration validation error
 */
export class ConfigValidationError extends Error {
  /**
   * @param {string} message - Error message
   * @param {Array} errors - Validation errors
   */
  constructor(message, errors = []) {
    super(message);
    this.name = 'ConfigValidationError';
    this.errors = errors;
  }

  /**
   * Format error for display
   * @returns {string} Formatted error message
   */
  format() {
    let msg = 'Configuration Validation Failed:\n';
    for (const err of this.errors) {
      const path = err.path?.join('.') || 'unknown';
      msg += `  - ${path}: ${err.message}`;
      if (err.received !== undefined) msg += `\n      Received: "${err.received}"`;
      if (err.expected !== undefined) msg += `\n      Expected: ${err.expected}`;
      msg += '\n';
    }
    return msg;
  }
}

/**
 * Parse `unrdf.toml` configuration file
 * @param {string} configPath - Path to config file
 * @param {Object} [options] - Parse options
 * @param {boolean} [options.resolvePaths=true] - Whether to resolve relative paths
 * @returns {Promise<Object>} Parsed and validated config
 */
export async function parseConfig(configPath, options = {}) {
  const { resolvePaths = true } = options;
  const absolutePath = resolve(configPath);

  if (!existsSync(absolutePath)) {
    throw new ConfigParseError('Configuration file not found: ' + absolutePath, {
      path: absolutePath,
    });
  }

  let content;
  try {
    content = await readFile(absolutePath, 'utf-8');
  } catch (err) {
    throw new ConfigParseError('Failed to read configuration file', {
      path: absolutePath,
      cause: err.message,
    });
  }

  let parsed;
  try {
    parsed = TOML.parse(content);
  } catch (err) {
    throw new ConfigParseError('Invalid TOML syntax', {
      path: absolutePath,
      line: err.line,
      column: err.column,
      cause: err.message,
    });
  }

  const substituted = substituteEnvVars(parsed);
  const baseDir = dirname(absolutePath);
  const resolved = resolvePaths ? resolveConfigPaths(substituted, baseDir) : substituted;

  const result = SyncConfigSchema.safeParse(resolved);
  if (!result.success) {
    const zodErrors = result.error?.issues ?? result.error?.errors ?? [];
    const errors = zodErrors.map(e => ({
      path: e.path,
      message: e.message,
      code: e.code,
      received: e.received,
      expected: e.expected,
    }));
    throw new ConfigValidationError('Configuration validation failed', errors);
  }

  findUnknownKeysRecursive(resolved, KNOWN_CONFIG_KEYS, []);
  return result.data;
}

/**
 * Known top-level and nested config keys for typo detection.
 * Each key maps to known child keys (or null if leaf/object without strict children).
 */
const KNOWN_CONFIG_KEYS = {
  project: { name: null, version: null, description: null, author: null, license: null },
  ontology: { source: null, format: null, base_iri: null, prefixes: null, follow_imports: null, additional: null },
  generation: {
    output_dir: null, templates_dir: null, ontology_dir: null, rules: null,
    require_audit_trail: null, parallel: null, incremental: null, overwrite: null,
    backup_before_overwrite: null, backup_suffix: null,
  },
  sync: { enabled: null, on_change: null, conflict_mode: null },
  rdf: { base_uri: null, default_prefix: null },
  templates: null, // array — skip recursive check
};

/**
 * Recursively detect unknown keys in parsed config and emit warnings.
 * @param {Object} obj - Current config object to inspect
 * @param {Object} knownKeys - Map of known key names to their children
 * @param {string[]} path - Current key path for error messages
 */
function findUnknownKeysRecursive(obj, knownKeys, path) {
  if (!obj || typeof obj !== 'object' || Array.isArray(obj)) return;
  for (const key of Object.keys(obj)) {
    const currentPath = [...path, key];
    if (!(key in knownKeys)) {
      process.emitWarning(
        `Unknown configuration key: "${currentPath.join('.')}" — possible typo.`,
        { code: 'UNRDF_UNKNOWN_CONFIG_KEY' }
      );
      continue;
    }
    const children = knownKeys[key];
    if (children && typeof obj[key] === 'object' && obj[key] !== null && !Array.isArray(obj[key])) {
      findUnknownKeysRecursive(obj[key], children, currentPath);
    }
  }
}

/**
 * Resolve relative paths in configuration
 * @param {Object} config - Configuration object
 * @param {string} baseDir - Base directory for relative paths
 * @returns {Object} Configuration with resolved paths
 */
export function resolveConfigPaths(config, baseDir) {
  const resolved = JSON.parse(JSON.stringify(config));

  if (resolved.ontology?.source) {
    resolved.ontology.source = resolve(baseDir, resolved.ontology.source);
    if (!config.ontology?.format) {
      resolved.ontology.format = detectRDFFormat(resolved.ontology.source);
    }
  }

  if (resolved.generation?.output_dir) {
    // Preserve trailing slash for absolute paths, resolve normalizes it away
    const hadTrailingSlash = resolved.generation.output_dir.endsWith('/');
    resolved.generation.output_dir = resolve(baseDir, resolved.generation.output_dir);
    if (hadTrailingSlash && isAbsolute(config.generation.output_dir)) {
      resolved.generation.output_dir += '/';
    }
  }

  if (resolved.generation?.templates_dir) {
    resolved.generation.templates_dir = resolve(baseDir, resolved.generation.templates_dir);
  }

  if (resolved.generation?.ontology_dir) {
    resolved.generation.ontology_dir = resolve(baseDir, resolved.generation.ontology_dir);
  }

  if (resolved.generation?.rules) {
    resolved.generation.rules = resolved.generation.rules.map(rule => ({
      ...rule,
      template: resolve(baseDir, rule.template),
    }));
  }

  if (resolved.templates) {
    resolved.templates = resolved.templates.map(t => {
      const resolvedTemplate = {
        name: t.name,
        source: resolve(baseDir, t.source),
        output: t.output ? resolve(baseDir, t.output) : t.output,
        resolvedSource: resolve(baseDir, t.source),
        resolvedOutput: t.output ? resolve(baseDir, t.output) : t.output,
      };
      // Preserve any additional properties from original template config
      for (const key of Object.keys(t)) {
        if (!(key in resolvedTemplate)) {
          resolvedTemplate[key] = t[key];
        }
      }
      return resolvedTemplate;
    });
  }

  return resolved;
}

/**
 * Substitute environment variables in configuration
 * @param {*} config - Configuration value
 * @returns {*} Configuration with substituted values
 */
export function substituteEnvVars(config) {
  if (typeof config === 'string') {
    let result = config;
    // Handle ${VAR:-default}
    result = result.replace(/\$\{([^}:-]+):-([^}]*)\}/g, (match, name, def) => {
      const val = process.env[name];
      return val !== undefined ? val : def;
    });
    // Handle ${VAR} - replace with empty string if undefined
    result = result.replace(/\$\{([^}]+)\}/g, (match, name) => {
      const val = process.env[name];
      return val !== undefined ? val : '';
    });
    // Handle $VAR - replace with empty string if undefined
    result = result.replace(/\$([A-Z_][A-Z0-9_]*)/gi, (match, name) => {
      const val = process.env[name];
      return val !== undefined ? val : '';
    });
    return result;
  }
  if (Array.isArray(config)) return config.map(substituteEnvVars);
  if (config && typeof config === 'object') {
    const result = {};
    for (const [k, v] of Object.entries(config)) result[k] = substituteEnvVars(v);
    return result;
  }
  return config;
}

/**
 * Validate configuration against schema
 * @param {Object} config - Configuration to validate
 * @returns {{ success: boolean, data?: Object, errors?: Array }} Validation result
 */
export function validateConfig(config) {
  const result = SyncConfigSchema.safeParse(config);
  if (result.success) {
    return { success: true, data: result.data };
  }
  // Zod uses 'issues' array on ZodError, fallback to 'errors' for compatibility
  const zodErrors = result.error?.issues ?? result.error?.errors ?? [];
  return {
    success: false,
    errors: zodErrors.map(e => ({
      path: e.path,
      message: e.message,
      code: e.code,
      received: e.received,
      expected: e.expected,
    })),
  };
}

/**
 * Create default configuration
 * @param {Object} [overrides] - Configuration overrides
 * @returns {Object} Default configuration merged with overrides
 */
export function createDefaultConfig(overrides = {}) {
  const defaults = {
    project: { name: 'untitled', version: '1.0.0' },
    generation: {
      output_dir: 'lib',
      incremental: true,
      overwrite: false,
      rules: [],
    },
    sync: {
      enabled: true,
      on_change: 'manual',
      conflict_mode: 'warn',
    },
  };

  return deepMerge(defaults, overrides);
}

function deepMerge(target, source) {
  const result = { ...target };
  for (const key of Object.keys(source)) {
    if (source[key] && typeof source[key] === 'object' && !Array.isArray(source[key])) {
      result[key] = deepMerge(result[key] || {}, source[key]);
    } else {
      result[key] = source[key];
    }
  }
  return result;
}

/**
 * Serialize configuration to TOML format
 * @param {Object} config - Configuration to serialize
 * @returns {string} TOML string
 */
export function serializeConfig(config) {
  const validation = validateConfig(config);
  if (!validation.success) {
    throw new ConfigValidationError('Cannot serialize invalid config', validation.errors);
  }

  return objectToToml(config);
}

function objectToToml(obj, prefix = '') {
  let result = '';
  const sections = [];
  const values = [];

  for (const [key, value] of Object.entries(obj)) {
    if (value && typeof value === 'object' && !Array.isArray(value)) {
      sections.push([key, value]);
    } else {
      values.push([key, value]);
    }
  }

  // Write values first
  for (const [key, value] of values) {
    result += `${key} = ${toTomlValue(value)}\n`;
  }

  // Write sections
  for (const [key, value] of sections) {
    const sectionName = prefix ? `${prefix}.${key}` : key;
    result += `\n[${sectionName}]\n`;
    result += objectToToml(value, sectionName);
  }

  return result;
}

function toTomlValue(value) {
  if (typeof value === 'string') return `"${value.replace(/"/g, '\\"')}"`;
  if (typeof value === 'boolean') return value ? 'true' : 'false';
  if (typeof value === 'number') return String(value);
  if (Array.isArray(value)) return `[${value.map(toTomlValue).join(', ')}]`;
  return String(value);
}

/**
 * Find configuration file in directory or parent directories
 * @param {string} dir - Directory to search (will search upward)
 * @returns {Promise<string|null>} Path to config file or null
 */
export async function findConfigFile(dir) {
  const candidates = ['unrdf.toml'];

  // Start from the given directory and search upward
  let currentDir = resolve(dir);
  const root = parse(currentDir).root;

  while (currentDir !== root && currentDir !== resolve(root, '..')) {
    for (const name of candidates) {
      const path = resolve(currentDir, name);
      if (existsSync(path)) return path;
    }

    // Move to parent directory
    const parentDir = resolve(currentDir, '..');
    if (parentDir === currentDir) break; // Reached root
    currentDir = parentDir;
  }

  return null;
}

export default {
  parseConfig,
  resolveConfigPaths,
  substituteEnvVars,
  validateConfig,
  createDefaultConfig,
  serializeConfig,
  findConfigFile,
  ConfigParseError,
  ConfigValidationError,
};
