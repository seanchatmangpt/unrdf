/**
 * @file UNRDF Daemon Configuration Loader
 * @module @unrdf/daemon/config
 * @description Loads and validates configuration from unrdf.toml and environment variables
 */

import { readFileSync } from 'fs';
import { resolve } from 'path';
import { z } from 'zod';

/**
 * Groq configuration schema
 */
const GroqConfigSchema = z.object({
  apiKey: z.string().optional(),
  baseURL: z.string().url().optional(),
  model: z.string().default('openai/gpt-oss-20b'),
  reasoningFormat: z.enum(['parsed', 'raw', 'hidden']).optional(),
  reasoningEffort: z.enum(['low', 'medium', 'high', 'none', 'default']).optional(),
  structuredOutputs: z.boolean().default(true),
  strictJsonSchema: z.boolean().default(true),
  parallelToolCalls: z.boolean().default(true),
  serviceTier: z.enum(['on_demand', 'performance', 'flex', 'auto']).default('on_demand'),
  timeout: z.number().default(30000),
});

/**
 * Daemon configuration schema
 */
const DaemonConfigSchema = z.object({
  port: z.number().default(3000),
  securityMiddleware: z.boolean().default(true),
  rateLimiting: z.boolean().default(true),
  requestTimeout: z.number().default(30000),
});

/**
 * MCP configuration schema
 */
const MCPConfigSchema = z.object({
  transport: z.enum(['stdio', 'sse']).default('stdio'),
  ssePort: z.number().default(3001),
  autoStart: z.boolean().default(true),
});

/**
 * Hooks configuration schema
 */
const HooksConfigSchema = z.object({
  enabled: z.boolean().default(true),
  config: z.string().optional(),
  receiptChaining: z.boolean().default(true),
});

/**
 * Observability configuration schema
 */
const ObservabilityConfigSchema = z.object({
  enabled: z.boolean().default(true),
  otelEndpoint: z.string().url().optional(),
  consoleLogging: z.boolean().default(true),
  logLevel: z.enum(['debug', 'info', 'warn', 'error']).default('info'),
});

/**
 * Root configuration schema
 */
export const DaemonFullConfigSchema = z.object({
  project: z.object({
    name: z.string(),
    version: z.string(),
    description: z.string().optional(),
  }).optional(),
  groq: GroqConfigSchema.optional(),
  daemon: DaemonConfigSchema.optional(),
  mcp: MCPConfigSchema.optional(),
  hooks: HooksConfigSchema.optional(),
  observability: ObservabilityConfigSchema.optional(),
});

/**
 * @typedef {z.infer<typeof DaemonFullConfigSchema>} DaemonFullConfig
 */

/**
 * Load configuration from TOML file (simple TOML parser for basic configs)
 * @param {string} content - TOML content
 * @returns {Record<string, any>} Parsed configuration object
 */
function parseToml(content) {
  const config = {};
  let currentSection = null;
  let currentSectionName = '';

  for (const line of content.split('\n')) {
    const trimmed = line.trim();

    // Skip empty lines and comments
    if (!trimmed || trimmed.startsWith('#')) {
      continue;
    }

    // Section header: [section]
    if (trimmed.startsWith('[') && trimmed.endsWith(']')) {
      currentSectionName = trimmed.slice(1, -1);
      currentSection = config[currentSectionName] || {};
      config[currentSectionName] = currentSection;
      continue;
    }

    // Key-value pair: key = value
    const match = trimmed.match(/^([^=]+)=(.+)$/);
    if (match && currentSection) {
      const key = match[1].trim();
      let value = match[2].trim();

      // Parse value type
      if (value === 'true') value = true;
      else if (value === 'false') value = false;
      else if (!isNaN(Number(value)) && value !== '') value = Number(value);
      else if (value.startsWith('"') && value.endsWith('"')) value = value.slice(1, -1);
      else if (value.startsWith("'") && value.endsWith("'")) value = value.slice(1, -1);

      currentSection[key] = value;
    }
  }

  return config;
}

/**
 * Load configuration from unrdf.toml in project root
 * @param {string} [configPath] - Custom path to unrdf.toml
 * @returns {Partial<DaemonFullConfig>} Partial configuration object
 */
export function loadConfigFromToml(configPath) {
  const path = configPath || resolve(process.cwd(), 'unrdf.toml');

  try {
    const content = readFileSync(path, 'utf-8');
    const config = parseToml(content);
    return DaemonFullConfigSchema.parse(config);
  } catch (err) {
    console.warn(`Could not load config from ${path}:`, err instanceof Error ? err.message : String(err));
    return {};
  }
}

/**
 * Load configuration from environment variables
 * @returns {Partial<DaemonFullConfig>} Configuration from environment
 */
export function loadConfigFromEnv() {
  return {
    groq: {
      apiKey: process.env.GROQ_API_KEY,
      model: process.env.GROQ_MODEL || 'openai/gpt-oss-20b',
      serviceTier: process.env.GROQ_SERVICE_TIER || 'on_demand',
    },
    daemon: {
      port: process.env.DAEMON_PORT ? Number(process.env.DAEMON_PORT) : 3000,
    },
    mcp: {
      transport: process.env.MCP_TRANSPORT || 'stdio',
      autoStart: process.env.MCP_AUTO_START !== 'false',
    },
    observability: {
      logLevel: process.env.LOG_LEVEL || 'info',
    },
  };
}

/**
 * Merge multiple configurations (env overrides toml)
 * @param {...Partial<DaemonFullConfig>} configs - Configuration objects to merge
 * @returns {DaemonFullConfig} Merged configuration
 */
export function mergeConfigs(...configs) {
  const merged = {};

  for (const config of configs) {
    for (const [section, values] of Object.entries(config || {})) {
      if (typeof values === 'object' && values !== null) {
        merged[section] = { ...(merged[section] || {}), ...values };
      } else if (values !== undefined) {
        merged[section] = values;
      }
    }
  }

  return DaemonFullConfigSchema.parse(merged);
}

/**
 * Load and merge all configuration sources
 * @param {string} [configPath] - Custom path to unrdf.toml
 * @returns {DaemonFullConfig} Merged configuration from all sources
 */
export function loadConfig(configPath) {
  const tomlConfig = loadConfigFromToml(configPath);
  const envConfig = loadConfigFromEnv();
  return mergeConfigs(tomlConfig, envConfig);
}

/**
 * Get config section with defaults
 * @param {DaemonFullConfig} config - Configuration object
 * @param {string} section - Section name to retrieve
 * @returns {*} Configuration section
 */
export function getConfigSection(config, section) {
  return config[section] || {};
}
