/**
 * @file Sidecar Config Command
 * @module cli-v2/commands/sidecar/config
 *
 * @description
 * Manage KGC sidecar configuration including contexts, endpoints, and connection settings.
 * Supports get, set, list, and use-context operations.
 */

import { defineCommand } from 'citty';
import { formatOutput } from '../../formatters/index.mjs';
import { createSidecarConfig } from '../../../sidecar/config.mjs';
import { writeFileSync, existsSync, mkdirSync } from 'fs';
import { homedir } from 'os';
import { join, dirname } from 'path';

/**
 * Get configuration value by key path
 * @param {Object} config - Configuration object
 * @param {string} key - Dot-notation key path (e.g., 'endpoint.address')
 * @returns {*} Configuration value
 */
function getConfigValue(config, key) {
  const keys = key.split('.');
  let value = config.getContext();

  for (const k of keys) {
    if (value && typeof value === 'object' && k in value) {
      value = value[k];
    } else {
      return undefined;
    }
  }

  return value;
}

/**
 * Set configuration value by key path
 * @param {Object} config - Configuration object
 * @param {string} key - Dot-notation key path
 * @param {string} value - Value to set
 */
function setConfigValue(config, key, value) {
  const keys = key.split('.');
  const context = config.getContext();

  let current = context;
  for (let i = 0; i < keys.length - 1; i++) {
    const k = keys[i];
    if (!(k in current)) {
      current[k] = {};
    }
    current = current[k];
  }

  const lastKey = keys[keys.length - 1];

  // Try to parse value as JSON/number/boolean
  let parsedValue = value;
  try {
    parsedValue = JSON.parse(value);
  } catch {
    // Keep as string if not valid JSON
  }

  current[lastKey] = parsedValue;
}

/**
 * Save configuration to file
 * @param {Object} config - Configuration manager
 * @param {string} [path] - Config file path
 */
function saveConfig(config, path) {
  const configPath = path || join(homedir(), '.kgc', 'config.json');
  const configDir = dirname(configPath);

  // Ensure config directory exists
  if (!existsSync(configDir)) {
    mkdirSync(configDir, { recursive: true });
  }

  // Write configuration
  const configJson = JSON.stringify(config.toJSON(), null, 2);
  writeFileSync(configPath, configJson, 'utf-8');

  return configPath;
}

/**
 * Sidecar config command
 */
export const configCommand = defineCommand({
  meta: {
    name: 'config',
    description: 'Manage sidecar configuration'
  },
  subCommands: {
    /**
     * Get configuration value
     */
    get: defineCommand({
      meta: {
        name: 'get',
        description: 'Get configuration value'
      },
      args: {
        key: {
          type: 'positional',
          description: 'Configuration key (dot notation, e.g., endpoint.address)',
          required: false
        },
        output: {
          type: 'string',
          description: 'Output format (json, yaml, table)',
          default: 'json'
        }
      },
      async run(ctx) {
        try {
          const config = createSidecarConfig();

          if (ctx.args.key) {
            // Get specific key
            const value = getConfigValue(config, ctx.args.key);

            if (value === undefined) {
              console.error(`Configuration key not found: ${ctx.args.key}`);
              process.exit(1);
            }

            const result = { [ctx.args.key]: value };
            console.log(formatOutput(result, ctx.args.output));
          } else {
            // Get entire configuration
            const fullConfig = config.toJSON();
            console.log(formatOutput(fullConfig, ctx.args.output));
          }
        } catch (error) {
          console.error(`Failed to get configuration: ${error.message}`);
          process.exit(1);
        }
      }
    }),

    /**
     * Set configuration value
     */
    set: defineCommand({
      meta: {
        name: 'set',
        description: 'Set configuration value'
      },
      args: {
        key: {
          type: 'positional',
          description: 'Configuration key (dot notation)',
          required: true
        },
        value: {
          type: 'positional',
          description: 'Configuration value',
          required: true
        },
        file: {
          type: 'string',
          description: 'Config file path',
          alias: 'f'
        }
      },
      async run(ctx) {
        try {
          const config = createSidecarConfig();

          // Set value
          setConfigValue(config, ctx.args.key, ctx.args.value);

          // Save configuration
          const configPath = saveConfig(config, ctx.args.file);

          console.log(`✅ Set ${ctx.args.key} = ${ctx.args.value}`);
          console.log(`💾 Configuration saved to: ${configPath}`);
        } catch (error) {
          console.error(`Failed to set configuration: ${error.message}`);
          process.exit(1);
        }
      }
    }),

    /**
     * List all contexts
     */
    list: defineCommand({
      meta: {
        name: 'list',
        description: 'List all configuration contexts'
      },
      args: {
        output: {
          type: 'string',
          description: 'Output format (json, yaml, table)',
          default: 'table'
        }
      },
      async run(ctx) {
        try {
          const config = createSidecarConfig();
          const fullConfig = config.toJSON();

          const contexts = fullConfig.contexts.map(context => ({
            name: context.name,
            current: context.name === fullConfig.currentContext ? '✓' : '',
            address: context.endpoint.address,
            port: context.endpoint.port,
            tls_enabled: context.endpoint.tls?.enabled || false,
            namespace: context.namespace || '-'
          }));

          const output = formatOutput(contexts, ctx.args.output, {
            columns: ['name', 'current', 'address', 'port', 'tls_enabled', 'namespace'],
            headers: ['NAME', 'CURRENT', 'ADDRESS', 'PORT', 'TLS', 'NAMESPACE']
          });

          console.log(output);
        } catch (error) {
          console.error(`Failed to list contexts: ${error.message}`);
          process.exit(1);
        }
      }
    }),

    /**
     * Switch to a different context
     */
    'use-context': defineCommand({
      meta: {
        name: 'use-context',
        description: 'Switch to a different context'
      },
      args: {
        context: {
          type: 'positional',
          description: 'Context name',
          required: true
        },
        file: {
          type: 'string',
          description: 'Config file path',
          alias: 'f'
        }
      },
      async run(ctx) {
        try {
          const config = createSidecarConfig();

          // Switch context
          config.useContext(ctx.args.context);

          // Save configuration
          const configPath = saveConfig(config, ctx.args.file);

          console.log(`✅ Switched to context: ${ctx.args.context}`);
          console.log(`📍 Address: ${config.getAddress()}`);
          console.log(`💾 Configuration saved to: ${configPath}`);
        } catch (error) {
          console.error(`Failed to switch context: ${error.message}`);
          process.exit(1);
        }
      }
    }),

    /**
     * Show current context
     */
    current: defineCommand({
      meta: {
        name: 'current',
        description: 'Show current context'
      },
      args: {
        output: {
          type: 'string',
          description: 'Output format (json, yaml, table)',
          default: 'table'
        }
      },
      async run(ctx) {
        try {
          const config = createSidecarConfig();
          const currentContext = config.getContext();

          const contextInfo = {
            name: currentContext.name,
            address: config.getAddress(),
            timeout: currentContext.timeout,
            max_retries: currentContext.maxRetries,
            namespace: currentContext.namespace || '-',
            circuit_breaker: currentContext.circuitBreaker?.enabled ? 'enabled' : 'disabled'
          };

          const output = formatOutput(contextInfo, ctx.args.output);
          console.log(output);
        } catch (error) {
          console.error(`Failed to get current context: ${error.message}`);
          process.exit(1);
        }
      }
    })
  }
});

export default configCommand;
