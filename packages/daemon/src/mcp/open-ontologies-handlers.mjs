/**
 * @file Open-Ontologies MCP Tool Handlers
 * @description MCP tool handler functions for open-ontologies CLI commands
 */

import { z } from 'zod';
import {
  runOntoCommand,
  validateOntoInstallation,
  ensureDataDir,
  createOntoResponse,
  createOntoErrorResponse,
} from './open-ontologies-helpers.mjs';

// ============================================================================
// Zod Schemas for Tool Inputs
// ============================================================================

const FileSchema = z.object({
  file: z.string().describe('Path to RDF file'),
  format: z.enum(['ttl', 'nt', 'jsonld', 'rdfxml', 'nq']).optional().describe('RDF format'),
});

const QuerySchema = z.object({
  query: z.string().describe('SPARQL query string'),
  format: z.enum(['json', 'ttl', 'nt', 'csv']).optional().describe('Output format'),
});

const LoadSchema = z.object({
  file: z.string().describe('Path to ontology file to load'),
  format: z.enum(['ttl', 'nt', 'jsonld', 'rdfxml']).optional().describe('Input format'),
});

const MarketplaceSchema = z.object({
  search: z.string().optional().describe('Search term for marketplace'),
  install: z.string().optional().describe('Ontology name to install'),
  list: z.boolean().optional().describe('List all available ontologies'),
});

const ReasonSchema = z.object({
  engine: z.enum(['rdfs', 'owl-rl', 'owl-dl']).optional().describe('Reasoning engine'),
  limit: z.number().optional().describe('Max inferred triples'),
});

const ShaclSchema = z.object({
  data: z.string().describe('Data file to validate'),
  shapes: z.string().describe('SHACL shapes file'),
});

const SaveSchema = z.object({
  file: z.string().describe('Output file path'),
  format: z.enum(['ttl', 'nt', 'jsonld', 'rdfxml', 'nq']).optional().describe('Output format'),
});

const ClearSchema = z.object({
  confirm: z.boolean().optional().describe('Confirm clear operation'),
});

const ConvertSchema = z.object({
  input: z.string().describe('Input file path'),
  output: z.string().describe('Output file path'),
  outputFormat: z.enum(['ttl', 'nt', 'jsonld', 'rdfxml', 'nq']).describe('Output format'),
  inputFormat: z.enum(['ttl', 'nt', 'jsonld', 'rdfxml', 'nq']).optional().describe('Input format (auto-detect if omitted)'),
});

const AlignSchema = z.object({
  source: z.string().describe('Source ontology file'),
  target: z.string().describe('Target ontology file'),
  threshold: z.number().min(0).max(1).optional().describe('Similarity threshold (0-1)'),
});

const DriftSchema = z.object({
  old: z.string().describe('Old version ontology file'),
  new: z.string().describe('New version ontology file'),
  detailed: z.boolean().optional().describe('Show detailed diff'),
});

const PlanSchema = z.object({
  changes: z.string().describe('Changes file (Turtle with diff triples)'),
  dryRun: z.boolean().optional().describe('Dry run without applying'),
});

const ApplySchema = z.object({
  plan: z.string().describe('Plan JSON file'),
  backup: z.boolean().optional().describe('Create backup before applying'),
});

const VersionSchema = z.object({
  tag: z.string().optional().describe('Version tag to save'),
  restore: z.string().optional().describe('Version tag to restore'),
  list: z.boolean().optional().describe('List all versions'),
});

// ============================================================================
// Tool Handlers
// ============================================================================

/**
 * Validate RDF/OWL syntax
 */
export async function onto_validate(args) {
  try {
    await validateOntoInstallation();

    const { file, format } = FileSchema.parse(args);
    const cliArgs = ['validate', file];
    if (format) cliArgs.push('--format', format);

    const result = await runOntoCommand(cliArgs, { timeoutMs: 10000 });
    return createOntoResponse(result, 'onto_validate');
  } catch (error) {
    return createOntoErrorResponse(error, 'onto_validate');
  }
}

/**
 * Show ontology statistics
 */
export async function onto_stats(args) {
  try {
    await validateOntoInstallation();

    const parsed = z.object({
      verbose: z.boolean().optional(),
    }).parse(args);

    const cliArgs = ['stats'];
    if (parsed.verbose) cliArgs.push('--verbose');

    const result = await runOntoCommand(cliArgs);
    return createOntoResponse(result, 'onto_stats');
  } catch (error) {
    return createOntoErrorResponse(error, 'onto_stats');
  }
}

/**
 * Execute SPARQL query
 */
export async function onto_query(args) {
  try {
    await validateOntoInstallation();

    const { query, format } = QuerySchema.parse(args);
    const cliArgs = ['query', query];
    if (format) cliArgs.push('--format', format);

    const result = await runOntoCommand(cliArgs, { timeoutMs: 15000 });
    return createOntoResponse(result, 'onto_query');
  } catch (error) {
    return createOntoErrorResponse(error, 'onto_query');
  }
}

/**
 * Load ontology into store
 */
export async function onto_load(args) {
  try {
    await validateOntoInstallation();
    await ensureDataDir();

    const { file, format } = LoadSchema.parse(args);
    const cliArgs = ['load', file];
    if (format) cliArgs.push('--format', format);

    const result = await runOntoCommand(cliArgs, { timeoutMs: 10000 });
    return createOntoResponse(result, 'onto_load');
  } catch (error) {
    return createOntoErrorResponse(error, 'onto_load');
  }
}

/**
 * Browse/install marketplace ontologies
 */
export async function onto_marketplace(args) {
  try {
    await validateOntoInstallation();
    await ensureDataDir();

    const { search, install, list } = MarketplaceSchema.parse(args);
    const cliArgs = ['marketplace'];

    if (search) cliArgs.push('--search', search);
    if (install) cliArgs.push('--install', install);
    if (list) cliArgs.push('--list');

    const result = await runOntoCommand(cliArgs, { timeoutMs: 30000 });
    return createOntoResponse(result, 'onto_marketplace');
  } catch (error) {
    return createOntoErrorResponse(error, 'onto_marketplace');
  }
}

// ============================================================================
// Phase 2: Advanced Features
// ============================================================================

/**
 * Perform RDFS/OWL reasoning
 */
export async function onto_reason(args) {
  try {
    await validateOntoInstallation();

    const { engine, limit } = ReasonSchema.parse(args);
    const cliArgs = ['reason'];

    if (engine) cliArgs.push('--engine', engine);
    if (limit) cliArgs.push('--limit', limit.toString());

    const result = await runOntoCommand(cliArgs, { timeoutMs: 30000 });
    return createOntoResponse(result, 'onto_reason');
  } catch (error) {
    return createOntoErrorResponse(error, 'onto_reason');
  }
}

/**
 * Validate with SHACL shapes
 */
export async function onto_shacl(args) {
  try {
    await validateOntoInstallation();

    const { data, shapes } = ShaclSchema.parse(args);
    const cliArgs = ['shacl', data, shapes];

    const result = await runOntoCommand(cliArgs, { timeoutMs: 15000 });
    return createOntoResponse(result, 'onto_shacl');
  } catch (error) {
    return createOntoErrorResponse(error, 'onto_shacl');
  }
}

/**
 * Save ontology store to file
 */
export async function onto_save(args) {
  try {
    await validateOntoInstallation();

    const { file, format } = SaveSchema.parse(args);
    const cliArgs = ['save', file];
    if (format) cliArgs.push('--format', format);

    const result = await runOntoCommand(cliArgs, { timeoutMs: 10000 });
    return createOntoResponse(result, 'onto_save');
  } catch (error) {
    return createOntoErrorResponse(error, 'onto_save');
  }
}

/**
 * Clear ontology store
 */
export async function onto_clear(args) {
  try {
    await validateOntoInstallation();

    const { confirm } = ClearSchema.parse(args);
    if (!confirm) {
      return createOntoResponse(
        { error: 'Set confirm: true to clear store' },
        'onto_clear'
      );
    }

    const result = await runOntoCommand(['clear']);
    return createOntoResponse(result, 'onto_clear');
  } catch (error) {
    return createOntoErrorResponse(error, 'onto_clear');
  }
}

/**
 * Convert RDF file between formats
 */
export async function onto_convert(args) {
  try {
    await validateOntoInstallation();

    const { input, output, outputFormat, inputFormat } = ConvertSchema.parse(args);
    const cliArgs = ['convert', input, output, '--output-format', outputFormat];
    if (inputFormat) cliArgs.push('--input-format', inputFormat);

    const result = await runOntoCommand(cliArgs, { timeoutMs: 10000 });
    return createOntoResponse(result, 'onto_convert');
  } catch (error) {
    return createOntoErrorResponse(error, 'onto_convert');
  }
}

// ============================================================================
// Phase 3: Expert Features
// ============================================================================

/**
 * Detect ontology alignment candidates
 */
export async function onto_align(args) {
  try {
    await validateOntoInstallation();

    const { source, target, threshold } = AlignSchema.parse(args);
    const cliArgs = ['align', source, target];
    if (threshold) cliArgs.push('--threshold', threshold.toString());

    const result = await runOntoCommand(cliArgs, { timeoutMs: 30000 });
    return createOntoResponse(result, 'onto_align');
  } catch (error) {
    return createOntoErrorResponse(error, 'onto_align');
  }
}

/**
 * Detect version drift
 */
export async function onto_drift(args) {
  try {
    await validateOntoInstallation();

    const { old, new: newVer, detailed } = DriftSchema.parse(args);
    const cliArgs = ['drift', old, newVer];
    if (detailed) cliArgs.push('--detailed');

    const result = await runOntoCommand(cliArgs, { timeoutMs: 15000 });
    return createOntoResponse(result, 'onto_drift');
  } catch (error) {
    return createOntoErrorResponse(error, 'onto_drift');
  }
}

/**
 * Plan ontology changes
 */
export async function onto_plan(args) {
  try {
    await validateOntoInstallation();

    const { changes, dryRun } = PlanSchema.parse(args);
    const cliArgs = ['plan', changes];
    if (dryRun) cliArgs.push('--dry-run');

    const result = await runOntoCommand(cliArgs, { timeoutMs: 10000 });
    return createOntoResponse(result, 'onto_plan');
  } catch (error) {
    return createOntoErrorResponse(error, 'onto_plan');
  }
}

/**
 * Apply planned changes
 */
export async function onto_apply(args) {
  try {
    await validateOntoInstallation();
    await ensureDataDir();

    const { plan, backup } = ApplySchema.parse(args);
    const cliArgs = ['apply', plan];
    if (backup) cliArgs.push('--backup');

    const result = await runOntoCommand(cliArgs, { timeoutMs: 15000 });
    return createOntoResponse(result, 'onto_apply');
  } catch (error) {
    return createOntoErrorResponse(error, 'onto_apply');
  }
}

/**
 * Manage ontology versions
 */
export async function onto_version(args) {
  try {
    await validateOntoInstallation();
    await ensureDataDir();

    const { tag, restore, list } = VersionSchema.parse(args);
    const cliArgs = ['version'];

    if (tag) cliArgs.push('--tag', tag);
    if (restore) cliArgs.push('--restore', restore);
    if (list) cliArgs.push('--list');

    const result = await runOntoCommand(cliArgs, { timeoutMs: 10000 });
    return createOntoResponse(result, 'onto_version');
  } catch (error) {
    return createOntoErrorResponse(error, 'onto_version');
  }
}
