/**
 * @fileoverview Config command - Manage CLI configuration
 *
 * @description
 * CLI commands for getting, setting, listing, resetting,
 * editing, and validating configuration.
 *
 * @module cli/commands/config
 * @version 2.0.0
 * @license MIT
 */

import { defineCommand } from 'citty';
import { z } from 'zod';

// =============================================================================
// Constants
// =============================================================================

/**
 * Default configuration values
 * @type {Object.<string, string>}
 */
const DEFAULT_CONFIG = {
  'author.name': 'Unknown Author',
  'author.email': '',
  'author.affiliation': '',
  'output.format': 'latex',
  'output.directory': './output',
  'cli.verbose': 'false',
  'cli.color': 'true',
  'cli.quiet': 'false',
  'templates.directory': './templates',
  'templates.extension': '.tex.njk',
  'ontology.path': './ontologies/papers-thesis.ttl',
  'ontology.prefixes.pt': 'http://papers-thesis.org/ontology#',
  'ontology.prefixes.ex': 'http://papers-thesis.org/examples#',
  'editor.command': '$EDITOR',
  'editor.args': ''
};

/**
 * Configuration schema for validation
 */
const ConfigSchema = z.object({
  'author.name': z.string(),
  'author.email': z.string().email().or(z.literal('')),
  'author.affiliation': z.string(),
  'output.format': z.enum(['latex', 'json', 'yaml', 'markdown']),
  'output.directory': z.string(),
  'cli.verbose': z.enum(['true', 'false']),
  'cli.color': z.enum(['true', 'false']),
  'cli.quiet': z.enum(['true', 'false']),
  'templates.directory': z.string(),
  'templates.extension': z.string(),
  'ontology.path': z.string(),
  'ontology.prefixes.pt': z.string().url(),
  'ontology.prefixes.ex': z.string().url(),
  'editor.command': z.string(),
  'editor.args': z.string()
}).partial();

// In-memory config store (would be persisted in production)
let currentConfig = { ...DEFAULT_CONFIG };

// =============================================================================
// Zod Schemas
// =============================================================================

/**
 * Zod schema for config key validation
 */
const ConfigKeySchema = z.string().regex(
  /^[a-z]+(\.[a-z]+)*$/,
  'Config key must be in format: category.key (e.g., author.name)'
);

// =============================================================================
// Helper Functions
// =============================================================================

/**
 * Print progress indicator
 * @param {string} message - Progress message
 * @param {string} type - Message type
 */
function printProgress(message, type = 'info') {
  const icons = {
    info: '\x1b[34mi\x1b[0m',
    success: '\x1b[32m+\x1b[0m',
    warning: '\x1b[33m!\x1b[0m',
    error: '\x1b[31mx\x1b[0m',
    progress: '\x1b[36m~\x1b[0m'
  };
  console.log(`${icons[type] || icons.info} ${message}`);
}

/**
 * Group config by category
 * @param {Object} config - Configuration object
 * @returns {Object} Grouped configuration
 */
function groupByCategory(config) {
  const categories = {};
  for (const [key, value] of Object.entries(config)) {
    const [category] = key.split('.');
    if (!categories[category]) {
      categories[category] = [];
    }
    categories[category].push({ key, value });
  }
  return categories;
}

/**
 * Simple YAML formatter
 * @param {Object} obj - Object to format
 * @param {number} indent - Current indentation level
 * @returns {string} YAML-formatted string
 */
function toSimpleYaml(obj, indent = 0) {
  const spaces = '  '.repeat(indent);
  let result = '';

  for (const [key, value] of Object.entries(obj)) {
    if (typeof value === 'object' && value !== null && !Array.isArray(value)) {
      result += `${spaces}${key}:\n${toSimpleYaml(value, indent + 1)}`;
    } else if (Array.isArray(value)) {
      result += `${spaces}${key}:\n`;
      for (const item of value) {
        result += `${spaces}  - ${item}\n`;
      }
    } else {
      result += `${spaces}${key}: ${value}\n`;
    }
  }

  return result;
}

// =============================================================================
// Get Command
// =============================================================================

/**
 * Get configuration value subcommand
 */
const getCommand = defineCommand({
  meta: {
    name: 'get',
    description: 'Get configuration value'
  },
  args: {
    key: {
      type: 'positional',
      description: 'Configuration key',
      required: true
    },
    format: {
      type: 'string',
      alias: 'f',
      description: 'Output format (value, json)',
      default: 'value'
    }
  },
  async run({ args }) {
    try {
      const key = ConfigKeySchema.parse(args.key);
      const value = currentConfig[key];

      if (value === undefined) {
        printProgress(`Configuration key not found: ${key}`, 'error');
        console.error('Run "playground config list" to see available keys');
        process.exit(1);
      }

      if (args.format === 'json') {
        console.log(JSON.stringify({ [key]: value }, null, 2));
      } else {
        console.log(value);
      }

      return { key, value };

    } catch (error) {
      if (error instanceof z.ZodError) {
        printProgress('Invalid configuration key format', 'error');
        console.error('Expected format: category.key (e.g., author.name)');
      } else {
        printProgress(`Error getting configuration: ${error.message}`, 'error');
      }
      process.exit(1);
    }
  }
});

// =============================================================================
// Set Command
// =============================================================================

/**
 * Set configuration value subcommand
 */
const setCommand = defineCommand({
  meta: {
    name: 'set',
    description: 'Set configuration value'
  },
  args: {
    key: {
      type: 'positional',
      description: 'Configuration key (e.g., author.name)',
      required: true
    },
    value: {
      type: 'positional',
      description: 'Configuration value',
      required: true
    }
  },
  async run({ args }) {
    try {
      const key = ConfigKeySchema.parse(args.key);
      const value = args.value;

      // Validate key exists in schema
      if (!(key in DEFAULT_CONFIG)) {
        printProgress(`Warning: '${key}' is not a recognized configuration key`, 'warning');
        console.log('Run "playground config list" to see available keys');
      }

      const oldValue = currentConfig[key];
      currentConfig[key] = value;

      printProgress(`Configuration updated: ${key}`, 'success');
      if (oldValue !== undefined && oldValue !== value) {
        console.log(`  Previous: ${oldValue}`);
      }
      console.log(`  Current:  ${value}`);

      return { key, value, oldValue };

    } catch (error) {
      if (error instanceof z.ZodError) {
        printProgress('Invalid configuration key format', 'error');
        console.error('Expected format: category.key (e.g., author.name)');
      } else {
        printProgress(`Error setting configuration: ${error.message}`, 'error');
      }
      process.exit(1);
    }
  }
});

// =============================================================================
// List Command
// =============================================================================

/**
 * List all configuration subcommand
 */
const listCommand = defineCommand({
  meta: {
    name: 'list',
    description: 'List all configuration'
  },
  args: {
    format: {
      type: 'string',
      alias: 'f',
      description: 'Output format (table, json, yaml)',
      default: 'table'
    },
    defaults: {
      type: 'boolean',
      description: 'Show default values',
      default: false
    },
    category: {
      type: 'string',
      alias: 'c',
      description: 'Filter by category (author, output, cli, templates, ontology, editor)'
    }
  },
  async run({ args }) {
    let config = args.defaults ? DEFAULT_CONFIG : currentConfig;

    // Filter by category if specified
    if (args.category) {
      const filtered = {};
      for (const [key, value] of Object.entries(config)) {
        if (key.startsWith(args.category + '.')) {
          filtered[key] = value;
        }
      }
      if (Object.keys(filtered).length === 0) {
        printProgress(`No configuration found for category: ${args.category}`, 'warning');
        console.log('Available categories: author, output, cli, templates, ontology, editor');
        return;
      }
      config = filtered;
    }

    if (args.format === 'json') {
      console.log(JSON.stringify(config, null, 2));
      return config;
    }

    if (args.format === 'yaml') {
      console.log(toSimpleYaml(config));
      return config;
    }

    // Table format
    console.log('Configuration:\n');

    const categories = groupByCategory(config);

    for (const [category, items] of Object.entries(categories)) {
      console.log(`  \x1b[36m[${category}]\x1b[0m`);
      for (const { key, value } of items) {
        const displayValue = value === '' ? '\x1b[90m(not set)\x1b[0m' : value;
        console.log(`    ${key.padEnd(30)} ${displayValue}`);
      }
      console.log('');
    }

    if (args.defaults) {
      console.log('\x1b[90m(showing default values)\x1b[0m');
    }

    return config;
  }
});

// =============================================================================
// Reset Command
// =============================================================================

/**
 * Reset configuration to defaults subcommand
 */
const resetCommand = defineCommand({
  meta: {
    name: 'reset',
    description: 'Reset configuration to defaults'
  },
  args: {
    confirm: {
      type: 'boolean',
      alias: 'y',
      description: 'Confirm reset without prompt',
      default: false
    },
    key: {
      type: 'string',
      alias: 'k',
      description: 'Reset specific key only'
    }
  },
  async run({ args }) {
    if (args.key) {
      // Reset specific key
      try {
        const key = ConfigKeySchema.parse(args.key);

        if (!(key in DEFAULT_CONFIG)) {
          printProgress(`Unknown configuration key: ${key}`, 'error');
          process.exit(1);
        }

        const oldValue = currentConfig[key];
        currentConfig[key] = DEFAULT_CONFIG[key];

        printProgress(`Reset ${key} to default`, 'success');
        console.log(`  Previous: ${oldValue}`);
        console.log(`  Default:  ${DEFAULT_CONFIG[key]}`);

        return { key, oldValue, newValue: DEFAULT_CONFIG[key] };

      } catch (error) {
        printProgress('Invalid configuration key format', 'error');
        process.exit(1);
      }
    }

    // Reset all
    if (!args.confirm) {
      console.log('This will reset all configuration to defaults.');
      console.log('Run with --confirm or -y to proceed.');
      console.log('\nCurrent non-default values:');

      let hasChanges = false;
      for (const [key, defaultValue] of Object.entries(DEFAULT_CONFIG)) {
        if (currentConfig[key] !== defaultValue) {
          console.log(`  ${key}: ${currentConfig[key]} -> ${defaultValue}`);
          hasChanges = true;
        }
      }

      if (!hasChanges) {
        console.log('  (all values are already at defaults)');
      }

      return;
    }

    const oldConfig = { ...currentConfig };
    currentConfig = { ...DEFAULT_CONFIG };

    printProgress('Configuration reset to defaults', 'success');
    console.log('Run "playground config list" to see current configuration');

    return { oldConfig, newConfig: currentConfig };
  }
});

// =============================================================================
// Edit Command
// =============================================================================

/**
 * Edit configuration in $EDITOR subcommand
 */
const editCommand = defineCommand({
  meta: {
    name: 'edit',
    description: 'Edit configuration in $EDITOR'
  },
  args: {
    format: {
      type: 'string',
      alias: 'f',
      description: 'Export format for editing (json, yaml)',
      default: 'yaml'
    }
  },
  async run({ args }) {
    const editor = process.env.EDITOR || process.env.VISUAL || 'vi';

    printProgress(`Opening configuration in ${editor}...`, 'progress');
    console.log('\nConfiguration would be exported and opened in editor.');
    console.log(`Editor: ${editor}`);
    console.log(`Format: ${args.format}`);
    console.log('\nIn production, this would:');
    console.log('  1. Export config to temp file');
    console.log('  2. Open in $EDITOR');
    console.log('  3. Parse and validate on save');
    console.log('  4. Apply changes');

    // Show current config that would be edited
    console.log('\nCurrent configuration:');
    if (args.format === 'json') {
      console.log(JSON.stringify(currentConfig, null, 2));
    } else {
      console.log(toSimpleYaml(currentConfig));
    }

    return { editor, format: args.format };
  }
});

// =============================================================================
// Validate Command
// =============================================================================

/**
 * Validate configuration subcommand
 */
const validateCommand = defineCommand({
  meta: {
    name: 'validate',
    description: 'Validate current configuration'
  },
  args: {
    strict: {
      type: 'boolean',
      description: 'Enable strict validation',
      default: false
    },
    format: {
      type: 'string',
      alias: 'f',
      description: 'Output format (table, json)',
      default: 'table'
    }
  },
  async run({ args }) {
    printProgress('Validating configuration...', 'progress');

    const errors = [];
    const warnings = [];

    // Check required fields
    if (!currentConfig['author.name'] || currentConfig['author.name'] === 'Unknown Author') {
      warnings.push('author.name is not set');
    }

    if (!currentConfig['author.email']) {
      warnings.push('author.email is not set');
    } else if (args.strict) {
      try {
        z.string().email().parse(currentConfig['author.email']);
      } catch {
        errors.push('author.email is not a valid email');
      }
    }

    // Check directories exist (would check filesystem in production)
    const outputDir = currentConfig['output.directory'];
    const templatesDir = currentConfig['templates.directory'];

    if (args.strict) {
      if (!outputDir.startsWith('./') && !outputDir.startsWith('/')) {
        warnings.push(`output.directory should be absolute or relative path: ${outputDir}`);
      }
    }

    // Check boolean values
    const boolFields = ['cli.verbose', 'cli.color', 'cli.quiet'];
    for (const field of boolFields) {
      if (!['true', 'false'].includes(currentConfig[field])) {
        errors.push(`${field} must be 'true' or 'false'`);
      }
    }

    // Check URL fields
    const urlFields = ['ontology.prefixes.pt', 'ontology.prefixes.ex'];
    for (const field of urlFields) {
      try {
        new URL(currentConfig[field]);
      } catch {
        errors.push(`${field} must be a valid URL`);
      }
    }

    const result = {
      valid: errors.length === 0,
      errors,
      warnings,
      checkedKeys: Object.keys(currentConfig).length,
      validatedAt: new Date().toISOString()
    };

    if (args.format === 'json') {
      console.log(JSON.stringify(result, null, 2));
      return result;
    }

    if (result.valid) {
      printProgress('Configuration is valid!', 'success');
    } else {
      printProgress('Configuration has errors:', 'error');
      errors.forEach(err => console.log(`  - ${err}`));
    }

    if (warnings.length > 0) {
      console.log('\nWarnings:');
      warnings.forEach(warn => printProgress(warn, 'warning'));
    }

    console.log(`\nValidated ${result.checkedKeys} configuration keys`);

    return result;
  }
});

// =============================================================================
// Main Config Command Export
// =============================================================================

/**
 * Config command with subcommands
 * @type {import('citty').CommandDef}
 */
export const configCommand = defineCommand({
  meta: {
    name: 'config',
    description: 'Manage CLI configuration'
  },
  subCommands: {
    get: getCommand,
    set: setCommand,
    list: listCommand,
    reset: resetCommand,
    edit: editCommand,
    validate: validateCommand
  }
});
