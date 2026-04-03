/**
 * @file MCP Server Implementation
 * @module @unrdf/daemon/mcp
 * @description Model Context Protocol server for UNRDF Daemon
 * @generated 2026-04-03 15:01:06 from cli-commands.ttl
 *
 * DO NOT EDIT — regenerate with: unrdf sync --rule mcp-index
 */

import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js';
import { z } from 'zod';
import { mcpResources } from './resources.mjs';
import { mcpPrompts } from './prompts.mjs';
import { cliRegistry } from './cli-registry.mjs';
import * as handlers from './handlers.mjs';


// ─── Tool Input Schemas ────────────────────────────────────────────────────────

const schema_context_add = z.object({
  file: z.string(),
  namespace: z.string(),
  prefix: z.string(),
}).strict();

const schema_context_create = z.object({
  name: z.string(),
  output: z.string().optional(),
}).strict();

const schema_context_list = z.object({
  file: z.string(),
  format: z.string().optional().default("table"),
}).strict();

const schema_context_remove = z.object({
  file: z.string(),
  prefix: z.string(),
}).strict();

const schema_convert = z.object({
  from: z.string().optional(),
  input: z.string(),
  output: z.string(),
  to: z.string().optional(),
}).strict();

const schema_convert_to_json = z.object({
  input: z.string(),
  output: z.string().optional(),
}).strict();

const schema_convert_to_ntriples = z.object({
  input: z.string(),
  output: z.string().optional(),
}).strict();

const schema_convert_to_turtle = z.object({
  input: z.string(),
  output: z.string().optional(),
}).strict();

const schema_daemon_cluster = z.object({
  include-metrics: z.boolean().optional(),
  json: z.boolean().optional(),
}).strict();

const schema_daemon_config = z.object({
  json: z.boolean().optional(),
}).strict();

const schema_daemon_list = z.object({
  include-metadata: z.boolean().optional(),
  json: z.boolean().optional(),
}).strict();

const schema_daemon_logs = z.object({
  filter: z.string().optional(),
  follow: z.boolean().optional(),
  json: z.boolean().optional(),
  max-lines: z.number().optional(),
}).strict();

const schema_daemon_run = z.object({
  json: z.boolean().optional(),
  operation: z.string(),
  payload: z.string().optional(),
  timeout: z.number().optional(),
}).strict();

const schema_daemon_schedule = z.object({
  json: z.boolean().optional(),
  operation: z.string(),
  payload: z.string().optional(),
  trigger: z.string(),
}).strict();

const schema_daemon_status = z.object({
  include-metrics: z.boolean().optional(),
  json: z.boolean().optional(),
}).strict();

const schema_graph_create = z.object({
  file: z.string().optional().default("<name>.nq"),
  name: z.string(),
}).strict();

const schema_graph_dump = z.object({
  file: z.string(),
  format: z.enum(["jsonld", "ntriples", "turtle"]).optional().default("turtle"),
  output: z.string(),
}).strict();

const schema_graph_load = z.object({
  file: z.string(),
  format: z.string().optional(),
  graph: z.string().optional(),
}).strict();

const schema_graph_query = z.object({
  file: z.string(),
  format: z.enum(["json", "table", "turtle"]).optional().default("json"),
  query: z.string(),
}).strict();

const schema_graph_stats = z.object({
  file: z.string(),
}).strict();

const schema_hooks_define = z.object({
  config: z.string(),
  output: z.string().optional(),
  validate: z.boolean().optional(),
}).strict();

const schema_hooks_evaluate_condition = z.object({
  condition: z.string(),
  config: z.string(),
  store: z.string(),
}).strict();

const schema_hooks_execute = z.object({
  config: z.string(),
  output: z.string().optional(),
  show-receipts: z.boolean().optional(),
  store: z.string(),
}).strict();

const schema_hooks_list_conditions = z.object({
}).strict();

const schema_hooks_receipts = z.object({
  file: z.string(),
  format: z.string().optional().default("table"),
  verify: z.boolean().optional(),
}).strict();

const schema_query = z.object({
  file: z.string(),
  format: z.string().optional().default("json"),
  query: z.string(),
}).strict();

const schema_query_file = z.object({
  data: z.string(),
  format: z.string().optional(),
  query: z.string(),
}).strict();

const schema_sync = z.object({
  config: z.string().optional(),
  dry-run: z.boolean().optional(),
  force: z.boolean().optional(),
  output: z.string().optional().default("text"),
  rule: z.string().optional(),
  verbose: z.boolean().optional(),
}).strict();

const schema_template_extract = z.object({
  file: z.string(),
  format: z.string().optional().default("json"),
  subject: z.string().optional(),
}).strict();

const schema_template_generate = z.object({
  batch: z.boolean().optional(),
  classUri: z.string().optional(),
  dryRun: z.boolean().optional(),
  file: z.string().optional(),
  force: z.boolean().optional(),
  outputDir: z.string().optional(),
  sparql: z.string().optional(),
  subject: z.string().optional(),
  template: z.string(),
}).strict();

const schema_template_list = z.object({
  dir: z.string().optional(),
  format: z.string().optional().default("table"),
}).strict();

const schema_template_query = z.object({
  file: z.string(),
  format: z.string().optional().default("json"),
  sparql: z.string().optional(),
}).strict();


// ─── Server Factory ────────────────────────────────────────────────────────────

/**
 * Create and configure the MCP server
 * @returns {McpServer} Configured MCP server instance
 */
export function createMCPServer() {
  const server = new McpServer({
    name: 'unrdf-daemon-mcp',
    version: '26.4.3',
  });

  server.tool(
    'context_add',
    'Add a prefix mapping to a context',
    schema_context_add,
    async (args) => {
      const fn = handlers['context_add'];
      if (!fn) throw new Error('Handler not implemented: context_add');
      return fn(args);
    }
  );

  server.tool(
    'context_create',
    'Create a new JSON-LD context',
    schema_context_create,
    async (args) => {
      const fn = handlers['context_create'];
      if (!fn) throw new Error('Handler not implemented: context_create');
      return fn(args);
    }
  );

  server.tool(
    'context_list',
    'List all prefix mappings in a context',
    schema_context_list,
    async (args) => {
      const fn = handlers['context_list'];
      if (!fn) throw new Error('Handler not implemented: context_list');
      return fn(args);
    }
  );

  server.tool(
    'context_remove',
    'Remove a prefix mapping from a context',
    schema_context_remove,
    async (args) => {
      const fn = handlers['context_remove'];
      if (!fn) throw new Error('Handler not implemented: context_remove');
      return fn(args);
    }
  );

  server.tool(
    'convert',
    'Convert RDF between formats',
    schema_convert,
    async (args) => {
      const fn = handlers['convert'];
      if (!fn) throw new Error('Handler not implemented: convert');
      return fn(args);
    }
  );

  server.tool(
    'convert_to_json',
    'Convert RDF to JSON-LD format',
    schema_convert_to_json,
    async (args) => {
      const fn = handlers['convert_to_json'];
      if (!fn) throw new Error('Handler not implemented: convert_to_json');
      return fn(args);
    }
  );

  server.tool(
    'convert_to_ntriples',
    'Convert RDF to N-Triples format',
    schema_convert_to_ntriples,
    async (args) => {
      const fn = handlers['convert_to_ntriples'];
      if (!fn) throw new Error('Handler not implemented: convert_to_ntriples');
      return fn(args);
    }
  );

  server.tool(
    'convert_to_turtle',
    'Convert RDF to Turtle format',
    schema_convert_to_turtle,
    async (args) => {
      const fn = handlers['convert_to_turtle'];
      if (!fn) throw new Error('Handler not implemented: convert_to_turtle');
      return fn(args);
    }
  );

  server.tool(
    'daemon_cluster',
    'Manage daemon cluster',
    schema_daemon_cluster,
    async (args) => {
      const fn = handlers['daemon_cluster'];
      if (!fn) throw new Error('Handler not implemented: daemon_cluster');
      return fn(args);
    }
  );

  server.tool(
    'daemon_config',
    'View daemon configuration',
    schema_daemon_config,
    async (args) => {
      const fn = handlers['daemon_config'];
      if (!fn) throw new Error('Handler not implemented: daemon_config');
      return fn(args);
    }
  );

  server.tool(
    'daemon_list',
    'List all running daemon instances',
    schema_daemon_list,
    async (args) => {
      const fn = handlers['daemon_list'];
      if (!fn) throw new Error('Handler not implemented: daemon_list');
      return fn(args);
    }
  );

  server.tool(
    'daemon_logs',
    'View daemon logs',
    schema_daemon_logs,
    async (args) => {
      const fn = handlers['daemon_logs'];
      if (!fn) throw new Error('Handler not implemented: daemon_logs');
      return fn(args);
    }
  );

  server.tool(
    'daemon_run',
    'Run an operation on the daemon',
    schema_daemon_run,
    async (args) => {
      const fn = handlers['daemon_run'];
      if (!fn) throw new Error('Handler not implemented: daemon_run');
      return fn(args);
    }
  );

  server.tool(
    'daemon_schedule',
    'Schedule an operation on the daemon',
    schema_daemon_schedule,
    async (args) => {
      const fn = handlers['daemon_schedule'];
      if (!fn) throw new Error('Handler not implemented: daemon_schedule');
      return fn(args);
    }
  );

  server.tool(
    'daemon_status',
    'Check daemon status and health',
    schema_daemon_status,
    async (args) => {
      const fn = handlers['daemon_status'];
      if (!fn) throw new Error('Handler not implemented: daemon_status');
      return fn(args);
    }
  );

  server.tool(
    'graph_create',
    'Create a new RDF graph',
    schema_graph_create,
    async (args) => {
      const fn = handlers['graph_create'];
      if (!fn) throw new Error('Handler not implemented: graph_create');
      return fn(args);
    }
  );

  server.tool(
    'graph_dump',
    'Dump an RDF graph to a file',
    schema_graph_dump,
    async (args) => {
      const fn = handlers['graph_dump'];
      if (!fn) throw new Error('Handler not implemented: graph_dump');
      return fn(args);
    }
  );

  server.tool(
    'graph_load',
    'Load RDF data into a graph from a file',
    schema_graph_load,
    async (args) => {
      const fn = handlers['graph_load'];
      if (!fn) throw new Error('Handler not implemented: graph_load');
      return fn(args);
    }
  );

  server.tool(
    'graph_query',
    'Execute SPARQL query on a graph file',
    schema_graph_query,
    async (args) => {
      const fn = handlers['graph_query'];
      if (!fn) throw new Error('Handler not implemented: graph_query');
      return fn(args);
    }
  );

  server.tool(
    'graph_stats',
    'Get statistics about an RDF graph',
    schema_graph_stats,
    async (args) => {
      const fn = handlers['graph_stats'];
      if (!fn) throw new Error('Handler not implemented: graph_stats');
      return fn(args);
    }
  );

  server.tool(
    'hooks_define',
    'Define RDF hooks configuration',
    schema_hooks_define,
    async (args) => {
      const fn = handlers['hooks_define'];
      if (!fn) throw new Error('Handler not implemented: hooks_define');
      return fn(args);
    }
  );

  server.tool(
    'hooks_evaluate_condition',
    'Evaluate a hook condition against a store',
    schema_hooks_evaluate_condition,
    async (args) => {
      const fn = handlers['hooks_evaluate_condition'];
      if (!fn) throw new Error('Handler not implemented: hooks_evaluate_condition');
      return fn(args);
    }
  );

  server.tool(
    'hooks_execute',
    'Execute registered RDF hooks on a store',
    schema_hooks_execute,
    async (args) => {
      const fn = handlers['hooks_execute'];
      if (!fn) throw new Error('Handler not implemented: hooks_execute');
      return fn(args);
    }
  );

  server.tool(
    'hooks_list_conditions',
    'List all available hook conditions',
    schema_hooks_list_conditions,
    async (args) => {
      const fn = handlers['hooks_list_conditions'];
      if (!fn) throw new Error('Handler not implemented: hooks_list_conditions');
      return fn(args);
    }
  );

  server.tool(
    'hooks_receipts',
    'View execution receipts from hook runs',
    schema_hooks_receipts,
    async (args) => {
      const fn = handlers['hooks_receipts'];
      if (!fn) throw new Error('Handler not implemented: hooks_receipts');
      return fn(args);
    }
  );

  server.tool(
    'query',
    'Execute SPARQL query on a data file',
    schema_query,
    async (args) => {
      const fn = handlers['query'];
      if (!fn) throw new Error('Handler not implemented: query');
      return fn(args);
    }
  );

  server.tool(
    'query_file',
    'Execute SPARQL query from a file',
    schema_query_file,
    async (args) => {
      const fn = handlers['query_file'];
      if (!fn) throw new Error('Handler not implemented: query_file');
      return fn(args);
    }
  );

  server.tool(
    'sync',
    'Synchronize and generate code from RDF ontology',
    schema_sync,
    async (args) => {
      const fn = handlers['sync'];
      if (!fn) throw new Error('Handler not implemented: sync');
      return fn(args);
    }
  );

  server.tool(
    'template_extract',
    'Extract data from RDF using template patterns',
    schema_template_extract,
    async (args) => {
      const fn = handlers['template_extract'];
      if (!fn) throw new Error('Handler not implemented: template_extract');
      return fn(args);
    }
  );

  server.tool(
    'template_generate',
    'Generate files from a Nunjucks template',
    schema_template_generate,
    async (args) => {
      const fn = handlers['template_generate'];
      if (!fn) throw new Error('Handler not implemented: template_generate');
      return fn(args);
    }
  );

  server.tool(
    'template_list',
    'List available templates',
    schema_template_list,
    async (args) => {
      const fn = handlers['template_list'];
      if (!fn) throw new Error('Handler not implemented: template_list');
      return fn(args);
    }
  );

  server.tool(
    'template_query',
    'Query template variables and context',
    schema_template_query,
    async (args) => {
      const fn = handlers['template_query'];
      if (!fn) throw new Error('Handler not implemented: template_query');
      return fn(args);
    }
  );

  return server;
}

// ─── Transport Helpers ─────────────────────────────────────────────────────────

/**
 * Start the MCP server with stdio transport
 * @returns {Promise<void>}
 */
export async function startMCPServer() {
  const server = createMCPServer();
  const transport = new StdioServerTransport();
  await server.connect(transport);
  console.error('MCP server connected to stdio');
}

/**
 * Start the MCP server with SSE transport (for HTTP)
 * @param {number} port Port to listen on
 * @returns {Promise<http.Server>}
 */
export async function startMCPServerSSE(port = 8765) {
  const server = createMCPServer();

  // For SSE transport, we need to handle HTTP differently
  console.error(`SSE transport requested on port ${port}`);
  console.error('Note: SSE transport requires HTTP server integration');

  // For now, fall back to stdio
  const transport = new StdioServerTransport();
  await server.connect(transport);
}

export { mcpResources, mcpPrompts };
