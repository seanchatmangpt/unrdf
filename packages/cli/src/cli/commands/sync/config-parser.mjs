/**
 * @file Config Parser
 * @module cli/commands/sync/config-parser
 * @description TOML configuration file parser for ggen.toml
 */
import { readFile } from 'fs/promises';
import { existsSync } from 'fs';
import { resolve, dirname, isAbsolute } from 'path';
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
 * Parse ggen.toml configuration file
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
    parsed = parseSimpleToml(content);
  } catch (err) {
    throw new ConfigParseError('Invalid TOML syntax', {
      path: absolutePath,
      cause: err.message,
    });
  }

  const substituted = substituteEnvVars(parsed);
  const baseDir = dirname(absolutePath);
  const resolved = resolvePaths ? resolveConfigPaths(substituted, baseDir) : substituted;

  const result = SyncConfigSchema.safeParse(resolved);
  if (!result.success) {
    const errors = result.error.errors.map(e => ({
      path: e.path,
      message: e.message,
      code: e.code,
      received: e.received,
      expected: e.expected,
    }));
    throw new ConfigValidationError('Configuration validation failed', errors);
  }

  return result.data;
}

function parseSimpleToml(content) {
  const result = {};
  let currentSection = result;
  let currentArraySection = null;
  let inMultilineString = false;
  let multilineKey = '';
  let multilineValue = '';

  const lines = content.split('\n');
  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    const trimmed = line.trim();

    // Handle multiline strings
    if (inMultilineString) {
      if (trimmed.endsWith('"""')) {
        multilineValue += '\n' + line.slice(0, line.lastIndexOf('"""'));
        currentSection[multilineKey] = multilineValue.trim();
        inMultilineString = false;
        multilineKey = '';
        multilineValue = '';
      } else {
        multilineValue += '\n' + line;
      }
      continue;
    }

    if (!trimmed || trimmed.startsWith('#')) continue;

    // Array table [[section.name]]
    const arrayMatch = trimmed.match(/^\[\[([^\]]+)\]\]$/);
    if (arrayMatch) {
      const parts = arrayMatch[1].split('.');
      let parent = result;
      for (let j = 0; j < parts.length - 1; j++) {
        if (!parent[parts[j]]) parent[parts[j]] = {};
        parent = parent[parts[j]];
      }
      const lastPart = parts[parts.length - 1];
      if (!parent[lastPart]) parent[lastPart] = [];
      const newItem = {};
      parent[lastPart].push(newItem);
      currentSection = newItem;
      currentArraySection = parent[lastPart];
      continue;
    }

    // Regular section [section.name]
    const sectionMatch = trimmed.match(/^\[([^\]]+)\]$/);
    if (sectionMatch) {
      const parts = sectionMatch[1].split('.');
      currentSection = result;
      for (const part of parts) {
        if (!currentSection[part]) currentSection[part] = {};
        currentSection = currentSection[part];
      }
      currentArraySection = null;
      continue;
    }

    // Key-value pair
    const kvMatch = trimmed.match(/^([^=]+)\s*=\s*(.*)$/);
    if (kvMatch) {
      const key = kvMatch[1].trim();
      let value = kvMatch[2].trim();

      // Start of multiline string """
      if (value === '"""' || value.startsWith('"""')) {
        if (value === '"""') {
          inMultilineString = true;
          multilineKey = key;
          multilineValue = '';
        } else if (value.endsWith('"""') && value.length > 6) {
          // Single line triple-quoted
          currentSection[key] = value.slice(3, -3);
        } else {
          // Start of multiline
          inMultilineString = true;
          multilineKey = key;
          multilineValue = value.slice(3);
        }
        continue;
      }

      // Regular value parsing
      if (value.startsWith('"') && value.endsWith('"')) {
        value = value.slice(1, -1).replace(/\\n/g, '\n').replace(/\\"/g, '"');
      } else if (value === 'true') {
        value = true;
      } else if (value === 'false') {
        value = false;
      } else if (/^\d+$/.test(value)) {
        value = parseInt(value, 10);
      } else if (/^\d+\.\d+$/.test(value)) {
        value = parseFloat(value);
      } else if (value.startsWith('[') && value.endsWith(']')) {
        try { value = JSON.parse(value.replace(/'/g, '"')); } catch (e) { /* keep as string */ }
      }

      currentSection[key] = value;
    }
  }

  return result;
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
    resolved.templates = resolved.templates.map(t => ({
      ...t,
      source: resolve(baseDir, t.source),
      output: t.output ? resolve(baseDir, t.output) : t.output,
    }));
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
 * Find configuration file in directory
 * @param {string} dir - Directory to search
 * @returns {Promise<string|null>} Path to config file or null
 */
export async function findConfigFile(dir) {
  const candidates = ['ggen.toml', '.ggen.toml', 'unrdf.toml', '.unrdf.toml'];
  for (const name of candidates) {
    const path = resolve(dir, name);
    if (existsSync(path)) return path;
  }
  return null;
}

export default { parseConfig, resolveConfigPaths, substituteEnvVars, validateConfig, createDefaultConfig, serializeConfig, findConfigFile, ConfigParseError, ConfigValidationError };
