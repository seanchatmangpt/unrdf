/**
 * @file MCP Tool Handlers
 * @module @unrdf/daemon/mcp/handlers
 * @description Auto-generated handlers that delegate to the unrdf CLI.
 * @generated 2026-04-03 17:40:10 from cli-commands.ttl
 *
 * DO NOT EDIT — regenerate with: unrdf sync --rule mcp-handlers
 *
 * Each handler maps MCP tool arguments to unrdf CLI flags and returns
 * stdout/stderr as MCP content. Boolean args become bare flags (--flag),
 * all others become --flag value pairs.
 */

import { spawn } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import { dirname, resolve } from 'node:path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// Resolve unrdf CLI entry point relative to this file
// handlers.mjs lives at packages/daemon/src/mcp/handlers.mjs
// main.mjs lives at packages/cli/src/cli/main.mjs
const CLI_MAIN = resolve(__dirname, '../../../cli/src/cli/main.mjs');

/**
 * Spawn the unrdf CLI and return MCP-formatted content.
 * @param {string[]} cliArgs - Arguments to pass after `node CLI_MAIN`
 * @returns {Promise<{content: Array<{type: string, text: string}>, isError: boolean}>}
 */
async function executeCli(cliArgs) {
  return new Promise((res) => {
    let stdout = '';
    let stderr = '';

    const proc = spawn(process.execPath, [CLI_MAIN, ...cliArgs], {
      env: process.env,
      stdio: ['ignore', 'pipe', 'pipe'],
    });

    proc.stdout.on('data', (chunk) => { stdout += chunk; });
    proc.stderr.on('data', (chunk) => { stderr += chunk; });

    proc.on('close', (code) => {
      const text = stdout || stderr || '(no output)';
      res({
        content: [{ type: 'text', text }],
        isError: code !== 0,
      });
    });

    proc.on('error', (err) => {
      res({
        content: [{ type: 'text', text: `Failed to spawn unrdf: ${err.message}` }],
        isError: true,
      });
    });
  });
}


// ─── Generated Handlers ────────────────────────────────────────────────────────

/**
 * Add a prefix mapping to a context (CLI: unrdf context add)
 */
export async function context_add(args = {}) {
  const cliArgs = 'context add'.split(' ');
  if (args['file'] !== undefined) cliArgs.push('--file', String(args['file']));
  if (args['namespace'] !== undefined) cliArgs.push('--namespace', String(args['namespace']));
  if (args['prefix'] !== undefined) cliArgs.push('--prefix', String(args['prefix']));
  return executeCli(cliArgs);
}

/**
 * Create a new JSON-LD context (CLI: unrdf context create)
 */
export async function context_create(args = {}) {
  const cliArgs = 'context create'.split(' ');
  if (args['name'] !== undefined) cliArgs.push('--name', String(args['name']));
  if (args['output'] !== undefined) cliArgs.push('--output', String(args['output']));
  return executeCli(cliArgs);
}

/**
 * List all prefix mappings in a context (CLI: unrdf context list)
 */
export async function context_list(args = {}) {
  const cliArgs = 'context list'.split(' ');
  if (args['file'] !== undefined) cliArgs.push('--file', String(args['file']));
  if (args['format'] !== undefined) cliArgs.push('--format', String(args['format']));
  return executeCli(cliArgs);
}

/**
 * Remove a prefix mapping from a context (CLI: unrdf context remove)
 */
export async function context_remove(args = {}) {
  const cliArgs = 'context remove'.split(' ');
  if (args['file'] !== undefined) cliArgs.push('--file', String(args['file']));
  if (args['prefix'] !== undefined) cliArgs.push('--prefix', String(args['prefix']));
  return executeCli(cliArgs);
}

/**
 * Convert RDF between formats (CLI: unrdf convert)
 */
export async function convert(args = {}) {
  const cliArgs = 'convert'.split(' ');
  if (args['from'] !== undefined) cliArgs.push('--from', String(args['from']));
  if (args['input'] !== undefined) cliArgs.push('--input', String(args['input']));
  if (args['output'] !== undefined) cliArgs.push('--output', String(args['output']));
  if (args['to'] !== undefined) cliArgs.push('--to', String(args['to']));
  return executeCli(cliArgs);
}

/**
 * Convert RDF to JSON-LD format (CLI: unrdf to-json)
 */
export async function convert_to_json(args = {}) {
  const cliArgs = 'to-json'.split(' ');
  if (args['input'] !== undefined) cliArgs.push('--input', String(args['input']));
  if (args['output'] !== undefined) cliArgs.push('--output', String(args['output']));
  return executeCli(cliArgs);
}

/**
 * Convert RDF to N-Triples format (CLI: unrdf to-ntriples)
 */
export async function convert_to_ntriples(args = {}) {
  const cliArgs = 'to-ntriples'.split(' ');
  if (args['input'] !== undefined) cliArgs.push('--input', String(args['input']));
  if (args['output'] !== undefined) cliArgs.push('--output', String(args['output']));
  return executeCli(cliArgs);
}

/**
 * Convert RDF to Turtle format (CLI: unrdf to-turtle)
 */
export async function convert_to_turtle(args = {}) {
  const cliArgs = 'to-turtle'.split(' ');
  if (args['input'] !== undefined) cliArgs.push('--input', String(args['input']));
  if (args['output'] !== undefined) cliArgs.push('--output', String(args['output']));
  return executeCli(cliArgs);
}

/**
 * Manage daemon cluster (CLI: unrdf daemon cluster)
 */
export async function daemon_cluster(args = {}) {
  const cliArgs = 'daemon cluster'.split(' ');
  if (args['include-metrics']) cliArgs.push('--include-metrics');
  if (args['json']) cliArgs.push('--json');
  return executeCli(cliArgs);
}

/**
 * View daemon configuration (CLI: unrdf daemon config)
 */
export async function daemon_config(args = {}) {
  const cliArgs = 'daemon config'.split(' ');
  if (args['json']) cliArgs.push('--json');
  return executeCli(cliArgs);
}

/**
 * List all running daemon instances (CLI: unrdf daemon list)
 */
export async function daemon_list(args = {}) {
  const cliArgs = 'daemon list'.split(' ');
  if (args['include-metadata']) cliArgs.push('--include-metadata');
  if (args['json']) cliArgs.push('--json');
  return executeCli(cliArgs);
}

/**
 * View daemon logs (CLI: unrdf daemon logs)
 */
export async function daemon_logs(args = {}) {
  const cliArgs = 'daemon logs'.split(' ');
  if (args['filter'] !== undefined) cliArgs.push('--filter', String(args['filter']));
  if (args['follow']) cliArgs.push('--follow');
  if (args['json']) cliArgs.push('--json');
  if (args['max-lines'] !== undefined) cliArgs.push('--max-lines', String(args['max-lines']));
  return executeCli(cliArgs);
}

/**
 * Run an operation on the daemon (CLI: unrdf daemon run)
 */
export async function daemon_run(args = {}) {
  const cliArgs = 'daemon run'.split(' ');
  if (args['json']) cliArgs.push('--json');
  if (args['operation'] !== undefined) cliArgs.push('--operation', String(args['operation']));
  if (args['payload'] !== undefined) cliArgs.push('--payload', String(args['payload']));
  if (args['timeout'] !== undefined) cliArgs.push('--timeout', String(args['timeout']));
  return executeCli(cliArgs);
}

/**
 * Schedule an operation on the daemon (CLI: unrdf daemon schedule)
 */
export async function daemon_schedule(args = {}) {
  const cliArgs = 'daemon schedule'.split(' ');
  if (args['json']) cliArgs.push('--json');
  if (args['operation'] !== undefined) cliArgs.push('--operation', String(args['operation']));
  if (args['payload'] !== undefined) cliArgs.push('--payload', String(args['payload']));
  if (args['trigger'] !== undefined) cliArgs.push('--trigger', String(args['trigger']));
  return executeCli(cliArgs);
}

/**
 * Check daemon status and health (CLI: unrdf daemon status)
 */
export async function daemon_status(args = {}) {
  const cliArgs = 'daemon status'.split(' ');
  if (args['include-metrics']) cliArgs.push('--include-metrics');
  if (args['json']) cliArgs.push('--json');
  return executeCli(cliArgs);
}

/**
 * Create a new RDF graph (CLI: unrdf graph create)
 */
export async function graph_create(args = {}) {
  const cliArgs = 'graph create'.split(' ');
  if (args['file'] !== undefined) cliArgs.push('--file', String(args['file']));
  if (args['name'] !== undefined) cliArgs.push('--name', String(args['name']));
  return executeCli(cliArgs);
}

/**
 * Dump an RDF graph to a file (CLI: unrdf graph dump)
 */
export async function graph_dump(args = {}) {
  const cliArgs = 'graph dump'.split(' ');
  if (args['file'] !== undefined) cliArgs.push('--file', String(args['file']));
  if (args['format'] !== undefined) cliArgs.push('--format', String(args['format']));
  if (args['output'] !== undefined) cliArgs.push('--output', String(args['output']));
  return executeCli(cliArgs);
}

/**
 * Load RDF data into a graph from a file (CLI: unrdf graph load)
 */
export async function graph_load(args = {}) {
  const cliArgs = 'graph load'.split(' ');
  if (args['file'] !== undefined) cliArgs.push('--file', String(args['file']));
  if (args['format'] !== undefined) cliArgs.push('--format', String(args['format']));
  if (args['graph'] !== undefined) cliArgs.push('--graph', String(args['graph']));
  return executeCli(cliArgs);
}

/**
 * Execute SPARQL query on a graph file (CLI: unrdf graph query)
 */
export async function graph_query(args = {}) {
  const cliArgs = 'graph query'.split(' ');
  if (args['file'] !== undefined) cliArgs.push('--file', String(args['file']));
  if (args['format'] !== undefined) cliArgs.push('--format', String(args['format']));
  if (args['query'] !== undefined) cliArgs.push('--query', String(args['query']));
  return executeCli(cliArgs);
}

/**
 * Get statistics about an RDF graph (CLI: unrdf graph stats)
 */
export async function graph_stats(args = {}) {
  const cliArgs = 'graph stats'.split(' ');
  if (args['file'] !== undefined) cliArgs.push('--file', String(args['file']));
  return executeCli(cliArgs);
}

/**
 * Define RDF hooks configuration (CLI: unrdf hooks define)
 */
export async function hooks_define(args = {}) {
  const cliArgs = 'hooks define'.split(' ');
  if (args['config'] !== undefined) cliArgs.push('--config', String(args['config']));
  if (args['output'] !== undefined) cliArgs.push('--output', String(args['output']));
  if (args['validate']) cliArgs.push('--validate');
  return executeCli(cliArgs);
}

/**
 * Evaluate a hook condition against a store (CLI: unrdf hooks evaluate-condition)
 */
export async function hooks_evaluate_condition(args = {}) {
  const cliArgs = 'hooks evaluate-condition'.split(' ');
  if (args['condition'] !== undefined) cliArgs.push('--condition', String(args['condition']));
  if (args['config'] !== undefined) cliArgs.push('--config', String(args['config']));
  if (args['store'] !== undefined) cliArgs.push('--store', String(args['store']));
  return executeCli(cliArgs);
}

/**
 * Execute registered RDF hooks on a store (CLI: unrdf hooks execute)
 */
export async function hooks_execute(args = {}) {
  const cliArgs = 'hooks execute'.split(' ');
  if (args['config'] !== undefined) cliArgs.push('--config', String(args['config']));
  if (args['output'] !== undefined) cliArgs.push('--output', String(args['output']));
  if (args['show-receipts']) cliArgs.push('--show-receipts');
  if (args['store'] !== undefined) cliArgs.push('--store', String(args['store']));
  return executeCli(cliArgs);
}

/**
 * List all available hook conditions (CLI: unrdf hooks list-conditions)
 */
export async function hooks_list_conditions(args = {}) {
  const cliArgs = 'hooks list-conditions'.split(' ');
  return executeCli(cliArgs);
}

/**
 * View execution receipts from hook runs (CLI: unrdf hooks receipts)
 */
export async function hooks_receipts(args = {}) {
  const cliArgs = 'hooks receipts'.split(' ');
  if (args['file'] !== undefined) cliArgs.push('--file', String(args['file']));
  if (args['format'] !== undefined) cliArgs.push('--format', String(args['format']));
  if (args['verify']) cliArgs.push('--verify');
  return executeCli(cliArgs);
}

/**
 * Execute SPARQL query on a data file (CLI: unrdf query)
 */
export async function query(args = {}) {
  const cliArgs = 'query'.split(' ');
  if (args['file'] !== undefined) cliArgs.push('--file', String(args['file']));
  if (args['format'] !== undefined) cliArgs.push('--format', String(args['format']));
  if (args['query'] !== undefined) cliArgs.push('--query', String(args['query']));
  return executeCli(cliArgs);
}

/**
 * Execute SPARQL query from a file (CLI: unrdf query-file)
 */
export async function query_file(args = {}) {
  const cliArgs = 'query-file'.split(' ');
  if (args['data'] !== undefined) cliArgs.push('--data', String(args['data']));
  if (args['format'] !== undefined) cliArgs.push('--format', String(args['format']));
  if (args['query'] !== undefined) cliArgs.push('--query', String(args['query']));
  return executeCli(cliArgs);
}

/**
 * Synchronize and generate code from RDF ontology (CLI: unrdf sync)
 */
export async function sync(args = {}) {
  const cliArgs = 'sync'.split(' ');
  if (args['config'] !== undefined) cliArgs.push('--config', String(args['config']));
  if (args['dry-run']) cliArgs.push('--dry-run');
  if (args['force']) cliArgs.push('--force');
  if (args['output'] !== undefined) cliArgs.push('--output', String(args['output']));
  if (args['rule'] !== undefined) cliArgs.push('--rule', String(args['rule']));
  if (args['verbose']) cliArgs.push('--verbose');
  return executeCli(cliArgs);
}

/**
 * Extract data from RDF using template patterns (CLI: unrdf template extract)
 */
export async function template_extract(args = {}) {
  const cliArgs = 'template extract'.split(' ');
  if (args['file'] !== undefined) cliArgs.push('--file', String(args['file']));
  if (args['format'] !== undefined) cliArgs.push('--format', String(args['format']));
  if (args['subject'] !== undefined) cliArgs.push('--subject', String(args['subject']));
  return executeCli(cliArgs);
}

/**
 * Generate files from a Nunjucks template (CLI: unrdf template generate)
 */
export async function template_generate(args = {}) {
  const cliArgs = 'template generate'.split(' ');
  if (args['batch']) cliArgs.push('--batch');
  if (args['classUri'] !== undefined) cliArgs.push('--classUri', String(args['classUri']));
  if (args['dryRun']) cliArgs.push('--dryRun');
  if (args['file'] !== undefined) cliArgs.push('--file', String(args['file']));
  if (args['force']) cliArgs.push('--force');
  if (args['outputDir'] !== undefined) cliArgs.push('--outputDir', String(args['outputDir']));
  if (args['sparql'] !== undefined) cliArgs.push('--sparql', String(args['sparql']));
  if (args['subject'] !== undefined) cliArgs.push('--subject', String(args['subject']));
  if (args['template'] !== undefined) cliArgs.push('--template', String(args['template']));
  return executeCli(cliArgs);
}

/**
 * List available templates (CLI: unrdf template list)
 */
export async function template_list(args = {}) {
  const cliArgs = 'template list'.split(' ');
  if (args['dir'] !== undefined) cliArgs.push('--dir', String(args['dir']));
  if (args['format'] !== undefined) cliArgs.push('--format', String(args['format']));
  return executeCli(cliArgs);
}

/**
 * Query template variables and context (CLI: unrdf template query)
 */
export async function template_query(args = {}) {
  const cliArgs = 'template query'.split(' ');
  if (args['file'] !== undefined) cliArgs.push('--file', String(args['file']));
  if (args['format'] !== undefined) cliArgs.push('--format', String(args['format']));
  if (args['sparql'] !== undefined) cliArgs.push('--sparql', String(args['sparql']));
  return executeCli(cliArgs);
}

