/**
 * @fileoverview Config command - Manage CLI configuration
 *
 * @description
 * CLI commands for getting, setting, listing, and resetting configuration.
 *
 * @module cli/commands/config
 * @version 1.0.0
 * @license MIT
 */

import { defineCommand } from 'citty';
import { z } from 'zod';

// Default configuration
const DEFAULT_CONFIG = {
  'author.name': 'Unknown Author',
  'author.email': '',
  'author.affiliation': '',
  'output.format': 'latex',
  'output.directory': './output',
  'cli.verbose': 'false',
  'cli.color': 'true',
  'templates.directory': './templates',
  'ontology.path': './ontologies/papers-thesis.ttl',
};

// In-memory config store (would be persisted in real implementation)
let currentConfig = { ...DEFAULT_CONFIG };

/**
 * Zod schema for config key validation
 */
const ConfigKeySchema = z
  .string()
  .regex(/^[a-z]+(\.[a-z]+)*$/, 'Config key must be in format: category.key (e.g., author.name)');

/**
 * Config command with subcommands for set, get, list, reset
 * @type {import('citty').CommandDef}
 */
export const configCommand = defineCommand({
  meta: {
    name: 'config',
    description: 'Manage CLI configuration',
  },
  subCommands: {
    /**
     * Set configuration value
     */
    set: defineCommand({
      meta: {
        name: 'set',
        description: 'Set configuration value',
      },
      args: {
        key: {
          type: 'positional',
          description: 'Configuration key (e.g., author.name)',
          required: true,
        },
        value: {
          type: 'positional',
          description: 'Configuration value',
          required: true,
        },
      },
      async run({ args }) {
        try {
          const key = ConfigKeySchema.parse(args.key);
          const value = args.value;

          // Validate key exists in schema
          if (!(key in DEFAULT_CONFIG)) {
            console.warn(`Warning: '${key}' is not a recognized configuration key`);
            console.warn('Run "playground config list" to see available keys');
          }

          // TODO: Integration layer - persist to config file

          currentConfig[key] = value;
          console.log(`Configuration updated: ${key} = ${value}`);
        } catch (error) {
          if (error instanceof z.ZodError) {
            console.error('Invalid configuration key format');
            console.error('Expected format: category.key (e.g., author.name)');
          } else {
            console.error('Error setting configuration:', error.message);
          }
          process.exit(1);
        }
      },
    }),

    /**
     * Get configuration value
     */
    get: defineCommand({
      meta: {
        name: 'get',
        description: 'Get configuration value',
      },
      args: {
        key: {
          type: 'positional',
          description: 'Configuration key',
          required: true,
        },
        format: {
          type: 'string',
          alias: 'f',
          description: 'Output format (value, json)',
          default: 'value',
        },
      },
      async run({ args }) {
        try {
          const key = ConfigKeySchema.parse(args.key);

          // TODO: Integration layer - load from config file

          const value = currentConfig[key];

          if (value === undefined) {
            console.error(`Configuration key not found: ${key}`);
            console.error('Run "playground config list" to see available keys');
            process.exit(1);
          }

          if (args.format === 'json') {
            console.log(JSON.stringify({ [key]: value }, null, 2));
          } else {
            console.log(value);
          }
        } catch (error) {
          if (error instanceof z.ZodError) {
            console.error('Invalid configuration key format');
          } else {
            console.error('Error getting configuration:', error.message);
          }
          process.exit(1);
        }
      },
    }),

    /**
     * List all configuration
     */
    list: defineCommand({
      meta: {
        name: 'list',
        description: 'List all configuration',
      },
      args: {
        format: {
          type: 'string',
          alias: 'f',
          description: 'Output format (table, json, yaml)',
          default: 'table',
        },
        defaults: {
          type: 'boolean',
          description: 'Show default values',
          default: false,
        },
      },
      async run({ args }) {
        // TODO: Integration layer - load from config file

        const config = args.defaults ? DEFAULT_CONFIG : currentConfig;

        if (args.format === 'json') {
          console.log(JSON.stringify(config, null, 2));
          return;
        }

        console.log('Configuration:\n');

        // Group by category
        const categories = {};
        for (const [key, value] of Object.entries(config)) {
          const [category] = key.split('.');
          if (!categories[category]) {
            categories[category] = [];
          }
          categories[category].push({ key, value });
        }

        for (const [category, items] of Object.entries(categories)) {
          console.log(`  [${category}]`);
          for (const { key, value } of items) {
            const displayValue = value === '' ? '(not set)' : value;
            console.log(`    ${key.padEnd(25)} ${displayValue}`);
          }
          console.log('');
        }

        if (args.defaults) {
          console.log('(showing default values)');
        }
      },
    }),

    /**
     * Reset configuration to defaults
     */
    reset: defineCommand({
      meta: {
        name: 'reset',
        description: 'Reset to defaults',
      },
      args: {
        confirm: {
          type: 'boolean',
          alias: 'y',
          description: 'Confirm reset without prompt',
          default: false,
        },
        key: {
          type: 'string',
          alias: 'k',
          description: 'Reset specific key only',
        },
      },
      async run({ args }) {
        if (args.key) {
          // Reset specific key
          try {
            const key = ConfigKeySchema.parse(args.key);

            if (!(key in DEFAULT_CONFIG)) {
              console.error(`Unknown configuration key: ${key}`);
              process.exit(1);
            }

            currentConfig[key] = DEFAULT_CONFIG[key];
            console.log(`Reset ${key} to default: ${DEFAULT_CONFIG[key]}`);
          } catch (error) {
            console.error('Invalid configuration key format');
            process.exit(1);
          }
          return;
        }

        // Reset all
        if (!args.confirm) {
          console.log('This will reset all configuration to defaults.');
          console.log('Run with --confirm to proceed.');
          return;
        }

        // TODO: Integration layer - clear config file

        currentConfig = { ...DEFAULT_CONFIG };
        console.log('Configuration reset to defaults.');
        console.log('Run "playground config list" to see current configuration.');
      },
    }),
  },
});
