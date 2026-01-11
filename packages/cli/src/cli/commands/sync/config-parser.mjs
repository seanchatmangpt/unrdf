/**
 * @file Config Parser
 * @module cli/commands/sync/config-parser
 * @description TOML configuration file parser for ggen.toml
 */
import { readFile } from 'fs/promises';
import { existsSync } from 'fs';
import { resolve, dirname } from 'path';
import { SyncConfigSchema, detectRDFFormat } from './schemas.mjs';

/**
 * Parse ggen.toml configuration file
 * @param {string} configPath - Path to config file
 * @returns {Promise<Object>} Parsed and validated config
 */
export async function parseConfig(configPath) {
  const absolutePath = resolve(configPath);
  if (!existsSync(absolutePath)) {
    throw new Error('Configuration file not found: ' + absolutePath);
  }

  const content = await readFile(absolutePath, 'utf-8');
  const parsed = parseSimpleToml(content);
  const substituted = substituteEnvVars(parsed);
  const baseDir = dirname(absolutePath);
  const resolved = resolveConfigPaths(substituted, baseDir);
  
  const result = SyncConfigSchema.safeParse(resolved);
  if (!result.success) {
    const errors = result.error.errors.map(e => e.path.join('.') + ': ' + e.message);
    throw new Error('Configuration validation failed:\n  ' + errors.join('\n  '));
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

export function resolveConfigPaths(config, baseDir) {
  const resolved = JSON.parse(JSON.stringify(config));
  
  if (resolved.ontology?.source) {
    resolved.ontology.source = resolve(baseDir, resolved.ontology.source);
    if (!config.ontology?.format) {
      resolved.ontology.format = detectRDFFormat(resolved.ontology.source);
    }
  }
  
  if (resolved.generation?.output_dir) {
    resolved.generation.output_dir = resolve(baseDir, resolved.generation.output_dir);
  }
  
  if (resolved.generation?.rules) {
    resolved.generation.rules = resolved.generation.rules.map(rule => ({
      ...rule,
      template: resolve(baseDir, rule.template),
    }));
  }
  
  return resolved;
}

export function substituteEnvVars(config) {
  if (typeof config === 'string') {
    let result = config;
    // Handle ${VAR:-default}
    result = result.replace(/\$\{([^}:-]+):-([^}]*)\}/g, (_, name, def) => process.env[name] ?? def);
    // Handle ${VAR}
    result = result.replace(/\$\{([^}]+)\}/g, (_, name) => process.env[name] ?? '');
    // Handle $VAR
    result = result.replace(/\$([A-Z_][A-Z0-9_]*)/gi, (_, name) => process.env[name] ?? '');
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

export default { parseConfig, resolveConfigPaths, substituteEnvVars };
