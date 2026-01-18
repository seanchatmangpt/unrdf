/**
 * @file Zod Validation Schemas for Tera Engine
 * @module @unrdf/tera-engine/schemas
 * @description Runtime validation schemas for all public APIs
 */

import { z } from 'zod';

/**
 * Template context schema - data passed to templates
 */
export const TemplateContextSchema = z.record(z.string(), z.any()).describe('Template context data');

/**
 * Template options schema
 */
export const TemplateOptionsSchema = z.object({
  autoescape: z.boolean().default(true).describe('Enable HTML auto-escaping'),
  strictVariables: z.boolean().default(false).describe('Throw on undefined variables'),
  trimBlocks: z.boolean().default(false).describe('Trim newlines after blocks'),
  lstripBlocks: z.boolean().default(false).describe('Strip leading whitespace before blocks'),
}).strict();

/**
 * Filter function schema
 */
export const FilterFunctionSchema = z.function().describe('Template filter function');

/**
 * Filter registry schema
 */
export const FilterRegistrySchema = z.record(z.string(), FilterFunctionSchema);

/**
 * TOML loader options schema
 */
export const TomlLoaderOptionsSchema = z.object({
  path: z.string().optional().describe('Path to TOML file'),
  content: z.string().optional().describe('TOML content string'),
}).strict().refine(
  data => data.path || data.content,
  'Either path or content must be provided'
);

/**
 * RDF loader options schema
 */
export const RdfLoaderOptionsSchema = z.object({
  path: z.string().optional().describe('Path to RDF file'),
  content: z.string().optional().describe('RDF content string'),
  format: z.enum(['turtle', 'ntriples', 'nquads', 'jsonld', 'rdfxml']).default('turtle'),
  baseIRI: z.string().optional().describe('Base IRI for relative URIs'),
}).strict().refine(
  data => data.path || data.content,
  'Either path or content must be provided'
);

/**
 * SPARQL query options schema
 */
export const SparqlQueryOptionsSchema = z.object({
  query: z.string().min(1).describe('SPARQL query string'),
  format: z.enum(['json', 'simple', 'bindings']).default('simple'),
}).strict();

/**
 * Render options schema
 */
export const RenderOptionsSchema = z.object({
  template: z.string().min(1).describe('Template string'),
  context: TemplateContextSchema.describe('Data context'),
  options: TemplateOptionsSchema.optional().describe('Template options'),
}).strict();

/**
 * Template engine configuration schema
 */
export const EngineConfigSchema = z.object({
  templateDir: z.string().optional().describe('Base directory for templates'),
  autoReload: z.boolean().default(false).describe('Auto-reload templates on change'),
  cache: z.boolean().default(true).describe('Enable template caching'),
  filters: FilterRegistrySchema.optional().describe('Custom filters'),
  options: TemplateOptionsSchema.optional().describe('Default template options'),
}).strict();

/**
 * Validates template context
 * @param {unknown} context - Context to validate
 * @returns {Record<string, any>} Validated context
 */
export function validateContext(context) {
  return TemplateContextSchema.parse(context);
}

/**
 * Validates template options
 * @param {unknown} options - Options to validate
 * @returns {Object} Validated options
 */
export function validateOptions(options) {
  return TemplateOptionsSchema.parse(options);
}

/**
 * Validates engine configuration
 * @param {unknown} config - Configuration to validate
 * @returns {Object} Validated configuration
 */
export function validateEngineConfig(config) {
  return EngineConfigSchema.parse(config);
}

/**
 * Validates render options
 * @param {unknown} options - Render options to validate
 * @returns {Object} Validated render options
 */
export function validateRenderOptions(options) {
  return RenderOptionsSchema.parse(options);
}

/**
 * Validates TOML loader options
 * @param {unknown} options - TOML loader options
 * @returns {Object} Validated options
 */
export function validateTomlOptions(options) {
  return TomlLoaderOptionsSchema.parse(options);
}

/**
 * Validates RDF loader options
 * @param {unknown} options - RDF loader options
 * @returns {Object} Validated options
 */
export function validateRdfOptions(options) {
  return RdfLoaderOptionsSchema.parse(options);
}

/**
 * Validates SPARQL query options
 * @param {unknown} options - SPARQL query options
 * @returns {Object} Validated options
 */
export function validateSparqlOptions(options) {
  return SparqlQueryOptionsSchema.parse(options);
}
