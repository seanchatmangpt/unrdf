#!/usr/bin/env node
/**
 * @fileoverview Configuration parsing and validation for KGC Probe
 *
 * Centralizes config logic with Zod validation for type safety.
 *
 * Design principles:
 * - Fail fast: Invalid config = immediate error
 * - Defaults: Sensible defaults for all options
 * - Composable: Can be used by CLI, API, or tests
 * - Validated: Zod schemas ensure type correctness
 */

import { z } from 'zod';
import { resolve, normalize } from 'node:path';

/**
 * Probe configuration schema
 */
export const ProbeConfigSchema = z.object({
  // Output directory for probe results
  outputDir: z.string().default('./kgc-output'),

  // Allowed filesystem roots (sandboxing)
  allowedRoots: z.array(z.string()).default([process.cwd()]),

  // Allowed network hosts (for network probes)
  allowedHosts: z.array(z.string()).default([]),

  // Time budget per agent (ms)
  budgetMs: z.number().int().positive().default(5000),

  // Benchmark sample count
  samples: z.number().int().positive().default(10),

  // Enforce timeouts
  enforceTimeouts: z.boolean().default(true),

  // Maximum timeout limit (ms)
  maxTimeoutMs: z.number().int().positive().default(50000),

  // Probe version
  probeVersion: z.string().default('1.0.0')
});

/**
 * Validated probe configuration
 *
 * @typedef {z.infer<typeof ProbeConfigSchema>} ProbeConfig
 */

/**
 * Parse and validate CLI options into ProbeConfig
 *
 * @param {Object} options - Raw CLI options
 * @param {string} options.out - Output directory
 * @param {string[]} options.root - Allowed root paths
 * @param {string[]} [options.netAllow] - Allowed network hosts
 * @param {number} options.budgetMs - Time budget (ms)
 * @param {number} options.samples - Benchmark samples
 * @returns {ProbeConfig}
 */
export function parseConfig(options) {
  const config = {
    outputDir: options.out,
    allowedRoots: (options.root || [process.cwd()]).map(r => resolve(normalize(r))),
    allowedHosts: options.netAllow || [],
    budgetMs: options.budgetMs,
    samples: options.samples,
    enforceTimeouts: true,
    maxTimeoutMs: (options.budgetMs || 5000) * 10,
    probeVersion: '1.0.0'
  };

  return ProbeConfigSchema.parse(config);
}

/**
 * Validate ProbeConfig
 *
 * @param {unknown} config - Config to validate
 * @returns {ProbeConfig}
 * @throws {z.ZodError} if validation fails
 */
export function validateConfig(config) {
  return ProbeConfigSchema.parse(config);
}

/**
 * Create default ProbeConfig
 *
 * @returns {ProbeConfig}
 */
export function createDefaultConfig() {
  return ProbeConfigSchema.parse({});
}

/**
 * Merge configs (right overrides left)
 *
 * @param {Partial<ProbeConfig>} base - Base config
 * @param {Partial<ProbeConfig>} override - Override config
 * @returns {ProbeConfig}
 */
export function mergeConfigs(base, override) {
  return ProbeConfigSchema.parse({ ...base, ...override });
}

export default {
  ProbeConfigSchema,
  parseConfig,
  validateConfig,
  createDefaultConfig,
  mergeConfigs
};
