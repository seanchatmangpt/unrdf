/**
 * @file Sync Command Schemas
 * @module cli/commands/sync/schemas
 * @description Zod validation schemas for ggen.toml configuration
 */
import { z } from 'zod';

/**
 * RDF format enum
 */
export const RDFFormatSchema = z.enum([
  'turtle',
  'ntriples',
  'nquads',
  'jsonld',
  'rdfxml',
  'trig',
]);

/**
 * Schema for project configuration
 */
export const ProjectConfigSchema = z.object({
  name: z.string().min(1),
  version: z.string().optional(),
  description: z.string().optional(),
  author: z.string().optional(),
  license: z.string().optional(),
});

/**
 * Schema for ontology source configuration
 */
export const OntologyConfigSchema = z.object({
  source: z.string().min(1),
  format: RDFFormatSchema.default('turtle'),
  base_iri: z.string().url().optional(),
  prefixes: z.record(z.string()).optional(),
  follow_imports: z.boolean().default(false),
});

/**
 * Schema for a single generation rule
 */
export const GenerationRuleSchema = z.object({
  name: z.string().min(1),
  description: z.string().optional(),
  query: z.string().min(1),
  template: z.string().min(1),
  output_file: z.string().min(1),
  enabled: z.boolean().default(true),
  mode: z.enum(['overwrite', 'append', 'skip_existing']).default('overwrite'),
  depends_on: z.array(z.string()).optional(),
});

/**
 * Schema for generation configuration
 */
export const GenerationConfigSchema = z.object({
  output_dir: z.string().default('lib'),
  templates_dir: z.string().optional(),
  ontology_dir: z.string().optional(),
  rules: z.array(GenerationRuleSchema).default([]),
  require_audit_trail: z.boolean().default(false),
  parallel: z.boolean().default(false),
  incremental: z.boolean().default(true),
  overwrite: z.boolean().default(false),
});

/**
 * Schema for sync configuration
 */
export const SyncConfigSchema2 = z.object({
  enabled: z.boolean().default(true),
  on_change: z.enum(['manual', 'auto', 'watch']).default('manual'),
  conflict_mode: z.enum(['warn', 'error', 'overwrite', 'skip']).default('warn'),
});

/**
 * Schema for RDF configuration
 */
export const RDFConfigSchema = z.object({
  base_uri: z.string().url().optional(),
  default_prefix: z.string().optional(),
});

/**
 * Template configuration schema
 */
export const TemplateConfigSchema = z.object({
  name: z.string().min(1),
  source: z.string().min(1),
  output: z.string().optional(),
});

/**
 * Main configuration schema for ggen.toml
 */
export const SyncConfigSchema = z.object({
  project: ProjectConfigSchema.optional(),
  ontology: OntologyConfigSchema.optional(),
  generation: GenerationConfigSchema.default({}),
  sync: SyncConfigSchema2.default({}),
  rdf: RDFConfigSchema.optional(),
  templates: z.array(TemplateConfigSchema).optional(),
});

/**
 * CLI arguments schema
 */
export const SyncArgsSchema = z.object({
  config: z.string().default('ggen.toml'),
  dryRun: z.boolean().default(false),
  verbose: z.boolean().default(false),
  force: z.boolean().default(false),
  rule: z.string().optional(),
  output: z.enum(['text', 'json']).default('text'),
  timeout: z.number().positive().default(30000),
});

/**
 * Detect RDF format from file extension
 * @param {string} filePath - File path
 * @returns {string} RDF format
 */
export function detectRDFFormat(filePath) {
  const ext = filePath.toLowerCase().split('.').pop();
  const formats = {
    ttl: 'turtle', turtle: 'turtle',
    nt: 'ntriples', ntriples: 'ntriples',
    nq: 'nquads', jsonld: 'jsonld', json: 'jsonld',
    rdf: 'rdfxml', xml: 'rdfxml', owl: 'rdfxml',
    trig: 'trig',
  };
  return formats[ext] || 'turtle';
}

export default {
  SyncConfigSchema, SyncArgsSchema, ProjectConfigSchema,
  OntologyConfigSchema, GenerationRuleSchema, GenerationConfigSchema,
  RDFFormatSchema, detectRDFFormat, SyncConfigSchema2, RDFConfigSchema,
  TemplateConfigSchema,
};
