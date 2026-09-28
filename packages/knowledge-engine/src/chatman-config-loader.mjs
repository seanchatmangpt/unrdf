/**
 * @file Chatman Configuration Loader - TOML Config Parser
 * @module knowledge-engine/chatman-config-loader
 *
 * @description
 * Loads Chatman Equation configurations from TOML files.
 * Supports market dynamics, organizational dynamics, strategic dynamics,
 * and disruption arithmetic rule configurations.
 */

import { readFile } from 'fs/promises';
import { existsSync } from 'fs';
import { resolve } from 'path';
import { parse as parseToml } from '@iarna/toml';
import { z } from 'zod';

/**
 * Chatman config schema
 */
export const ChatmanConfigSchema = z.object({
  chatman: z
    .object({
      version: z.string().default('1.0.0'),
      observable_ratio: z.number().min(0).max(1).default(0.05),
      closure_threshold: z.number().min(0).max(1).default(0.95),
    })
    .optional(),
  market_dynamics: z
    .array(
      z.object({
        id: z.string(),
        name: z.string(),
        category: z.string(),
        pattern: z.string(),
        dark_field_multiplier: z.number().positive().default(19),
        confidence: z.number().min(0).max(1).default(0.8),
      })
    )
    .optional(),
  organizational_dynamics: z
    .array(
      z.object({
        id: z.string(),
        name: z.string(),
        category: z.string(),
        pattern: z.string(),
        dark_field_multiplier: z.number().positive().default(19),
        confidence: z.number().min(0).max(1).default(0.8),
      })
    )
    .optional(),
  strategic_dynamics: z
    .array(
      z.object({
        id: z.string(),
        name: z.string(),
        category: z.string(),
        pattern: z.string(),
        dark_field_multiplier: z.number().positive().default(19),
        confidence: z.number().min(0).max(1).default(0.8),
      })
    )
    .optional(),
  disruption_arithmetic: z
    .array(
      z.object({
        id: z.string(),
        name: z.string(),
        category: z.string(),
        pattern: z.string(),
        dark_field_multiplier: z.number().positive().default(19),
        confidence: z.number().min(0).max(1).default(0.8),
      })
    )
    .optional(),
});

/**
 * Configuration load error
 */
export class ConfigLoadError extends Error {
  /**
   * @param {string} message - Error message
   * @param {Object} [options] - Error options
   */
  constructor(message, options = {}) {
    super(message);
    this.name = 'ConfigLoadError';
    this.path = options.path;
    this.cause = options.cause;
  }
}

/**
 * Chatman configuration loader
 */
export class ChatmanConfigLoader {
  /**
   * Create a new config loader
   * @param {Object} [options] - Loader options
   * @param {Function} [options.tracer] - OTEL tracer function
   */
  constructor(options = {}) {
    this.tracer = options.tracer;
    this.cache = new Map();
  }

  /**
   * Load configuration from TOML file
   * @param {string} configPath - Path to config file
   * @param {Object} [options] - Load options
   * @param {boolean} [options.useCache=true] - Use cached config
   * @returns {Promise<Object>} Parsed configuration
   */
  async load(configPath, options = {}) {
    const { useCache = true } = options;
    const span = this.tracer?.startSpan?.('chatman.config.load');

    try {
      const absolutePath = resolve(configPath);

      // Check cache
      if (useCache && this.cache.has(absolutePath)) {
        span?.addEvent?.('config_cache_hit', { path: absolutePath });
        return this.cache.get(absolutePath);
      }

      span?.setAttribute?.('config.path', absolutePath);

      // Verify file exists
      if (!existsSync(absolutePath)) {
        throw new ConfigLoadError(`Configuration file not found: ${absolutePath}`, {
          path: absolutePath,
        });
      }

      // Read and parse TOML
      const content = await readFile(absolutePath, 'utf-8');
      const parsed = parseToml(content);

      // Validate with Zod
      const validated = ChatmanConfigSchema.parse(parsed);

      // Normalize field names (convert snake_case to camelCase for internal use)
      const normalized = this._normalizeConfig(validated);

      // Cache the result
      if (useCache) {
        this.cache.set(absolutePath, normalized);
      }

      span?.addEvent?.('config_loaded', {
        'config.version': normalized.chatman?.version || '1.0.0',
        'config.market_rules': normalized.marketDynamics?.length || 0,
        'config.org_rules': normalized.organizationalDynamics?.length || 0,
        'config.strategic_rules': normalized.strategicDynamics?.length || 0,
        'config.disruption_rules': normalized.disruptionArithmetic?.length || 0,
      });

      return normalized;
    } catch (error) {
      span?.recordException?.(error);
      span?.setStatus?.({ code: 2, message: error.message });

      if (error instanceof ConfigLoadError) {
        throw error;
      }

      if (error.name === 'ZodError') {
        throw new ConfigLoadError('Configuration validation failed', {
          cause: error.message,
        });
      }

      throw new ConfigLoadError('Failed to load configuration', {
        cause: error.message,
      });
    } finally {
      span?.end?.();
    }
  }

  /**
   * Normalize configuration field names
   * @param {Object} config - Parsed config
   * @returns {Object} Normalized config
   * @private
   */
  _normalizeConfig(config) {
    const normalized = {};

    if (config.chatman) {
      normalized.chatman = {
        version: config.chatman.version,
        observableRatio: config.chatman.observable_ratio,
        closureThreshold: config.chatman.closure_threshold,
      };
    }

    if (config.market_dynamics) {
      normalized.marketDynamics = config.market_dynamics.map(rule => ({
        id: rule.id,
        name: rule.name,
        category: rule.category,
        pattern: rule.pattern,
        darkFieldMultiplier: rule.dark_field_multiplier,
        confidence: rule.confidence,
      }));
    }

    if (config.organizational_dynamics) {
      normalized.organizationalDynamics = config.organizational_dynamics.map(rule => ({
        id: rule.id,
        name: rule.name,
        category: rule.category,
        pattern: rule.pattern,
        darkFieldMultiplier: rule.dark_field_multiplier,
        confidence: rule.confidence,
      }));
    }

    if (config.strategic_dynamics) {
      normalized.strategicDynamics = config.strategic_dynamics.map(rule => ({
        id: rule.id,
        name: rule.name,
        category: rule.category,
        pattern: rule.pattern,
        darkFieldMultiplier: rule.dark_field_multiplier,
        confidence: rule.confidence,
      }));
    }

    if (config.disruption_arithmetic) {
      normalized.disruptionArithmetic = config.disruption_arithmetic.map(rule => ({
        id: rule.id,
        name: rule.name,
        category: rule.category,
        pattern: rule.pattern,
        darkFieldMultiplier: rule.dark_field_multiplier,
        confidence: rule.confidence,
      }));
    }

    return normalized;
  }

  /**
   * Clear configuration cache
   */
  clearCache() {
    this.cache.clear();
  }

  /**
   * Get cached configuration
   * @param {string} configPath - Config path
   * @returns {Object|undefined} Cached config or undefined
   */
  getCached(configPath) {
    return this.cache.get(resolve(configPath));
  }
}

/**
 * Create a Chatman config loader instance
 * @param {Object} [options] - Loader options
 * @returns {ChatmanConfigLoader} Loader instance
 */
export function createChatmanConfigLoader(options = {}) {
  return new ChatmanConfigLoader(options);
}

/**
 * Load Chatman configuration from TOML file
 * @param {string} configPath - Path to config file
 * @param {Object} [options] - Load options
 * @returns {Promise<Object>} Parsed configuration
 */
export async function loadChatmanConfig(configPath, options = {}) {
  const loader = new ChatmanConfigLoader(options);
  return loader.load(configPath, options);
}
