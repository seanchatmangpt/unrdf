/**
 * @file CLI Configuration Loader
 * @module cli/utils/config-loader
 *
 * @description
 * Handles loading and validation of UNRDF configuration from
 * unrdf.config.mjs and environment variables.
 */

import { access } from 'node:fs/promises';
import { resolve } from 'node:path';
import { z } from 'zod';

/**
 * Configuration schema
 */
export const ConfigSchema = z.object({
  baseIRI: z.string().url().default('http://example.org/'),
  prefixes: z.record(z.string(), z.string().url()).default({
    ex: 'http://example.org/',
    foaf: 'http://xmlns.com/foaf/0.1/',
    schema: 'https://schema.org/',
    rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
    rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
    owl: 'http://www.w3.org/2002/07/owl#',
  }),
  validation: z
    .object({
      strict: z.boolean().default(true),
      validateOnLoad: z.boolean().default(true),
    })
    .default({
      strict: true,
      validateOnLoad: true,
    }),
});

/**
 * Load configuration from unrdf.config.mjs or environment
 * @returns {Promise<Object>} Validated configuration
 */
export async function loadConfig() {
  const configPath = resolve(process.cwd(), 'unrdf.config.mjs');

  let config = {};

  // Try to load from file
  try {
    await access(configPath);
    const configModule = await import(configPath);
    config = configModule.default || {};
  } catch {
    // File doesn't exist, use defaults
  }

  // Merge with environment variables
  if (process.env.UNRDF_BASE_IRI) {
    config.baseIRI = process.env.UNRDF_BASE_IRI;
  }

  if (process.env.UNRDF_PREFIXES) {
    try {
      config.prefixes = JSON.parse(process.env.UNRDF_PREFIXES);
    } catch {
      // Invalid JSON, ignore
    }
  }

  // Validate and apply defaults
  return ConfigSchema.parse(config);
}

/**
 * Create default configuration object
 * @returns {Object} Default configuration
 */
export function createDefaultConfig() {
  return ConfigSchema.parse({});
}
