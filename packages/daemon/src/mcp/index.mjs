/**
 * @file MCP Server Implementation
 * @module @unrdf/daemon/mcp
 * @description Model Context Protocol server for UNRDF Daemon
 * @generated 2026-04-03 21:03:03 from cli-commands.ttl
 *
 * DO NOT EDIT — regenerate with: unrdf sync --rule mcp-index
 */

import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js';
import { z } from 'zod';
import { registerResources } from './resources.mjs';
import { registerPrompts } from './prompts.mjs';
import * as handlers from './handlers.mjs';
import { withMcpSpan } from './otel-instrumentation.mjs';


/**
 * Create and configure the MCP server
 * @returns {McpServer} Configured MCP server instance
 */
export function createMCPServer() {
  const server = new McpServer({
    name: 'unrdf-daemon-mcp',
    version: '26.4.3',
  });

  // Register resources and prompts
  registerResources(server);
  registerPrompts(server);

  server.registerTool(
    'context_add',
    {
      description: 'Add prefix to context',
      inputSchema: {
        "file": z.string().describe('Context file'),
        "namespace": z.string().describe('Namespace IRI'),
        "prefix": z.string().describe('Prefix name'),
      },
    },
    async (args) => {
      const fn = handlers['context_add'];
      if (!fn) throw new Error('Handler not implemented: context_add');
      return withMcpSpan('context_add', fn)(args);
    }
  );

  server.registerTool(
    'context_create',
    {
      description: 'Create a new JSON-LD context',
      inputSchema: {
        "name": z.string().describe('Context name'),
        "output": z.string().describe('Output file path').optional(),
      },
    },
    async (args) => {
      const fn = handlers['context_create'];
      if (!fn) throw new Error('Handler not implemented: context_create');
      return withMcpSpan('context_create', fn)(args);
    }
  );

  server.registerTool(
    'context_list',
    {
      description: 'List context prefixes',
      inputSchema: {
        "file": z.string().describe('Context file'),
        "format": z.string().describe('Output format (table, json)').default('table').optional(),
      },
    },
    async (args) => {
      const fn = handlers['context_list'];
      if (!fn) throw new Error('Handler not implemented: context_list');
      return withMcpSpan('context_list', fn)(args);
    }
  );

  server.registerTool(
    'context_remove',
    {
      description: 'Remove prefix from context',
      inputSchema: {
        "file": z.string().describe('Context file'),
        "prefix": z.string().describe('Prefix name to remove'),
      },
    },
    async (args) => {
      const fn = handlers['context_remove'];
      if (!fn) throw new Error('Handler not implemented: context_remove');
      return withMcpSpan('context_remove', fn)(args);
    }
  );

  server.registerTool(
    'convert',
    {
      description: 'Convert RDF between formats',
      inputSchema: {
        "from": z.string().describe('Input format (turtle, ntriples, nquads) - auto-detected if not specified').optional(),
        "input": z.string().describe('Input RDF file'),
        "output": z.string().describe('Output file'),
        "to": z.string().describe('Output format (turtle, ntriples, nquads) - auto-detected if not specified').optional(),
      },
    },
    async (args) => {
      const fn = handlers['convert'];
      if (!fn) throw new Error('Handler not implemented: convert');
      return withMcpSpan('convert', fn)(args);
    }
  );

  server.registerTool(
    'daemon_cluster',
    {
      description: 'Show Raft cluster status and members',
      inputSchema: {
        "include-metrics": z.boolean().describe('Include detailed member metrics').optional(),
        "json": z.boolean().describe('Output as JSON').optional(),
      },
    },
    async (args) => {
      const fn = handlers['daemon_cluster'];
      if (!fn) throw new Error('Handler not implemented: daemon_cluster');
      return withMcpSpan('daemon_cluster', fn)(args);
    }
  );

  server.registerTool(
    'daemon_config',
    {
      description: 'Display current daemon configuration',
      inputSchema: {
        "json": z.boolean().describe('Output as JSON').optional(),
      },
    },
    async (args) => {
      const fn = handlers['daemon_config'];
      if (!fn) throw new Error('Handler not implemented: daemon_config');
      return withMcpSpan('daemon_config', fn)(args);
    }
  );

  server.registerTool(
    'daemon_list',
    {
      description: 'List all configured operations',
      inputSchema: {
        "include-metadata": z.boolean().describe('Include metadata in output').optional(),
        "json": z.boolean().describe('Output as JSON').optional(),
      },
    },
    async (args) => {
      const fn = handlers['daemon_list'];
      if (!fn) throw new Error('Handler not implemented: daemon_list');
      return withMcpSpan('daemon_list', fn)(args);
    }
  );

  server.registerTool(
    'daemon_logs',
    {
      description: 'View operation logs with filtering',
      inputSchema: {
        "filter": z.string().describe('Filter logs by pattern').optional(),
        "follow": z.boolean().describe('Follow log output (stream mode)').optional(),
        "json": z.boolean().describe('Output as JSON').optional(),
        "max-lines": z.number().describe('Maximum lines to display').optional(),
      },
    },
    async (args) => {
      const fn = handlers['daemon_logs'];
      if (!fn) throw new Error('Handler not implemented: daemon_logs');
      return withMcpSpan('daemon_logs', fn)(args);
    }
  );

  server.registerTool(
    'daemon_run',
    {
      description: 'Execute operation immediately',
      inputSchema: {
        "json": z.boolean().describe('Output as JSON').optional(),
        "operation": z.string().describe('Operation ID to execute'),
        "payload": z.string().describe('Operation payload (JSON string)').optional(),
        "timeout": z.number().describe('Execution timeout in milliseconds').optional(),
      },
    },
    async (args) => {
      const fn = handlers['daemon_run'];
      if (!fn) throw new Error('Handler not implemented: daemon_run');
      return withMcpSpan('daemon_run', fn)(args);
    }
  );

  server.registerTool(
    'daemon_schedule',
    {
      description: 'Add scheduled trigger to operation',
      inputSchema: {
        "json": z.boolean().describe('Output as JSON').optional(),
        "operation": z.string().describe('Operation ID to schedule'),
        "payload": z.string().describe('Trigger payload (JSON string)').optional(),
        "trigger": z.string().describe('Trigger type (cron, interval, reactive, event)'),
      },
    },
    async (args) => {
      const fn = handlers['daemon_schedule'];
      if (!fn) throw new Error('Handler not implemented: daemon_schedule');
      return withMcpSpan('daemon_schedule', fn)(args);
    }
  );

  server.registerTool(
    'daemon_status',
    {
      description: 'Show daemon health and metrics',
      inputSchema: {
        "include-metrics": z.boolean().describe('Include detailed metrics').optional(),
        "json": z.boolean().describe('Output as JSON').optional(),
      },
    },
    async (args) => {
      const fn = handlers['daemon_status'];
      if (!fn) throw new Error('Handler not implemented: daemon_status');
      return withMcpSpan('daemon_status', fn)(args);
    }
  );

  server.registerTool(
    'graph_create',
    {
      description: 'Create a new RDF graph',
      inputSchema: {
        "file": z.string().describe('Output file path (default: <name>.nq)').optional(),
        "name": z.string().describe('Graph name'),
      },
    },
    async (args) => {
      const fn = handlers['graph_create'];
      if (!fn) throw new Error('Handler not implemented: graph_create');
      return withMcpSpan('graph_create', fn)(args);
    }
  );

  server.registerTool(
    'graph_dump',
    {
      description: 'Export graph data to file',
      inputSchema: {
        "file": z.string().describe('Source graph file'),
        "format": z.string().describe('Output format (turtle, ntriples, nquads)').default('turtle').optional(),
        "output": z.string().describe('Output file'),
      },
    },
    async (args) => {
      const fn = handlers['graph_dump'];
      if (!fn) throw new Error('Handler not implemented: graph_dump');
      return withMcpSpan('graph_dump', fn)(args);
    }
  );

  server.registerTool(
    'graph_load',
    {
      description: 'Load RDF data into a graph',
      inputSchema: {
        "file": z.string().describe('RDF file to load (Turtle, N-Triples, N-Quads)'),
        "format": z.string().describe('RDF format (turtle, ntriples, nquads) - auto-detected if not specified').optional(),
        "graph": z.string().describe('Target graph name').optional(),
      },
    },
    async (args) => {
      const fn = handlers['graph_load'];
      if (!fn) throw new Error('Handler not implemented: graph_load');
      return withMcpSpan('graph_load', fn)(args);
    }
  );

  server.registerTool(
    'graph_query',
    {
      description: 'Execute SPARQL query on graph',
      inputSchema: {
        "file": z.string().describe('Graph file to query'),
        "format": z.string().describe('Output format (table, json, turtle)').default('table').optional(),
        "query": z.string().describe('SPARQL query string'),
      },
    },
    async (args) => {
      const fn = handlers['graph_query'];
      if (!fn) throw new Error('Handler not implemented: graph_query');
      return withMcpSpan('graph_query', fn)(args);
    }
  );

  server.registerTool(
    'graph_stats',
    {
      description: 'Show graph statistics',
      inputSchema: {
        "file": z.string().describe('Graph file to analyze'),
      },
    },
    async (args) => {
      const fn = handlers['graph_stats'];
      if (!fn) throw new Error('Handler not implemented: graph_stats');
      return withMcpSpan('graph_stats', fn)(args);
    }
  );

  server.registerTool(
    'hooks_define',
    {
      description: 'Define hooks from config file',
      inputSchema: {
        "config": z.string().describe('Hooks config file (JSON)'),
        "output": z.string().describe('Output file for hook metadata (JSON)').optional(),
        "validate": z.boolean().describe('Validate without executing').default(false).optional(),
      },
    },
    async (args) => {
      const fn = handlers['hooks_define'];
      if (!fn) throw new Error('Handler not implemented: hooks_define');
      return withMcpSpan('hooks_define', fn)(args);
    }
  );

  server.registerTool(
    'hooks_evaluate_condition',
    {
      description: 'Evaluate a single condition against a store',
      inputSchema: {
        "condition": z.string().describe('Condition type (sparql-ask, sparql-select, shacl, delta, threshold, count, window, n3, datalog)'),
        "config": z.string().describe('Condition config as JSON'),
        "store": z.string().describe('Store file (NQ/Turtle/N-Triples)'),
      },
    },
    async (args) => {
      const fn = handlers['hooks_evaluate_condition'];
      if (!fn) throw new Error('Handler not implemented: hooks_evaluate_condition');
      return withMcpSpan('hooks_evaluate_condition', fn)(args);
    }
  );

  server.registerTool(
    'hooks_execute',
    {
      description: 'Execute hooks against an RDF store',
      inputSchema: {
        "config": z.string().describe('Hooks config file (JSON)'),
        "output": z.string().describe('Output file for results (JSON)').optional(),
        "show-receipts": z.boolean().describe('Show receipt chain with hashes').default(false).optional(),
        "store": z.string().describe('Store file (NQ/Turtle/N-Triples)'),
      },
    },
    async (args) => {
      const fn = handlers['hooks_execute'];
      if (!fn) throw new Error('Handler not implemented: hooks_execute');
      return withMcpSpan('hooks_execute', fn)(args);
    }
  );

  server.registerTool(
    'hooks_list_conditions',
    {
      description: 'List available condition kinds',
      inputSchema: {
      },
    },
    async (args) => {
      const fn = handlers['hooks_list_conditions'];
      if (!fn) throw new Error('Handler not implemented: hooks_list_conditions');
      return withMcpSpan('hooks_list_conditions', fn)(args);
    }
  );

  server.registerTool(
    'hooks_receipts',
    {
      description: 'Display receipt chain from hook execution results',
      inputSchema: {
        "file": z.string().describe('Execution result file (JSON)'),
        "format": z.string().describe('Output format (json, table)').default('table').optional(),
        "verify": z.boolean().describe('Verify receipt chain integrity').default(false).optional(),
      },
    },
    async (args) => {
      const fn = handlers['hooks_receipts'];
      if (!fn) throw new Error('Handler not implemented: hooks_receipts');
      return withMcpSpan('hooks_receipts', fn)(args);
    }
  );

  server.registerTool(
    'mcp_inspect',
    {
      description: 'List all exposed tools, resources, and prompts',
      inputSchema: {
        "json": z.boolean().describe('Output as JSON').optional(),
      },
    },
    async (args) => {
      const fn = handlers['mcp_inspect'];
      if (!fn) throw new Error('Handler not implemented: mcp_inspect');
      return withMcpSpan('mcp_inspect', fn)(args);
    }
  );

  server.registerTool(
    'mcp_start',
    {
      description: 'Start the MCP server',
      inputSchema: {
        "port": z.number().describe('Port for SSE transport (when transport=sse)').default(3001).optional(),
        "transport": z.string().describe('Transport type: stdio or sse').default('stdio').optional(),
      },
    },
    async (args) => {
      const fn = handlers['mcp_start'];
      if (!fn) throw new Error('Handler not implemented: mcp_start');
      return withMcpSpan('mcp_start', fn)(args);
    }
  );

  server.registerTool(
    'mcp_status',
    {
      description: 'Show if MCP server is running',
      inputSchema: {
        "json": z.boolean().describe('Output as JSON').optional(),
      },
    },
    async (args) => {
      const fn = handlers['mcp_status'];
      if (!fn) throw new Error('Handler not implemented: mcp_status');
      return withMcpSpan('mcp_status', fn)(args);
    }
  );

  server.registerTool(
    'mcp_stop',
    {
      description: 'Stop the running MCP server',
      inputSchema: {
        "json": z.boolean().describe('Output as JSON').optional(),
      },
    },
    async (args) => {
      const fn = handlers['mcp_stop'];
      if (!fn) throw new Error('Handler not implemented: mcp_stop');
      return withMcpSpan('mcp_stop', fn)(args);
    }
  );

  server.registerTool(
    'query',
    {
      description: 'Execute SPARQL query',
      inputSchema: {
        "file": z.string().describe('RDF data file'),
        "format": z.string().describe('Output format (table, json, csv)').default('table').optional(),
        "query": z.string().describe('SPARQL query string'),
      },
    },
    async (args) => {
      const fn = handlers['query'];
      if (!fn) throw new Error('Handler not implemented: query');
      return withMcpSpan('query', fn)(args);
    }
  );

  server.registerTool(
    'query_file',
    {
      description: 'Execute SPARQL query from file',
      inputSchema: {
        "data": z.string().describe('RDF data file'),
        "format": z.string().describe('Output format (table, json, csv)').default('table').optional(),
        "query": z.string().describe('SPARQL query file (.sparql)'),
      },
    },
    async (args) => {
      const fn = handlers['query_file'];
      if (!fn) throw new Error('Handler not implemented: query_file');
      return withMcpSpan('query_file', fn)(args);
    }
  );

  server.registerTool(
    'sync',
    {
      description: 'Generate synchronized code artifacts from RDF ontology',
      inputSchema: {
        "config": z.string().describe('Path to `.unrdf.toml` configuration file').default('.unrdf.toml').optional(),
        "dry-run": z.boolean().describe('Preview changes without writing files').default(false).optional(),
        "force": z.boolean().describe('Overwrite existing files without prompting').default(false).optional(),
        "output": z.string().describe('Output format: text or json').default('text').optional(),
        "rule": z.string().describe('Run only the specified rule by name').optional(),
        "verbose": z.boolean().describe('Enable verbose output').default(false).optional(),
        "watch": z.boolean().describe('Watch ontology and template files for changes').default(false).optional(),
      },
    },
    async (args) => {
      const fn = handlers['sync'];
      if (!fn) throw new Error('Handler not implemented: sync');
      return withMcpSpan('sync', fn)(args);
    }
  );

  server.registerTool(
    'template_extract',
    {
      description: 'Extract properties for a subject as JSON (template debugging)',
      inputSchema: {
        "file": z.string().describe('RDF data file'),
        "format": z.string().describe('Output format (json|yaml|table)').default('json').optional(),
        "subject": z.string().describe('Subject URI').optional(),
      },
    },
    async (args) => {
      const fn = handlers['template_extract'];
      if (!fn) throw new Error('Handler not implemented: template_extract');
      return withMcpSpan('template_extract', fn)(args);
    }
  );

  server.registerTool(
    'template_generate',
    {
      description: 'Generate files from RDF + Nunjucks template (`--template`). RDF path can be positional or `rdf:` in template frontmatter.',
      inputSchema: {
        "batch": z.boolean().describe('One output per instance of `--class-uri`; SPARQL must use ?subject').default(false).optional(),
        "classUri": z.string().describe('RDFS/OWL class IRI for batch mode (with `--batch`)').optional(),
        "dryRun": z.boolean().describe('Print paths without writing').default(false).optional(),
        "file": z.string().describe('RDF file (Turtle, N-Triples, N-Quads, …). Optional if template sets `rdf:` in frontmatter.').optional(),
        "force": z.boolean().describe('Overwrite existing files').default(false).optional(),
        "outputDir": z.string().describe('Output directory for generated files').default('./generated').optional(),
        "sparql": z.string().describe('SPARQL SELECT (overrides frontmatter `sparql:`)').optional(),
        "subject": z.string().describe('Focus subject URI; replaces ?subject in SPARQL when present').optional(),
        "template": z.string().describe('Template .njk path'),
      },
    },
    async (args) => {
      const fn = handlers['template_generate'];
      if (!fn) throw new Error('Handler not implemented: template_generate');
      return withMcpSpan('template_generate', fn)(args);
    }
  );

  server.registerTool(
    'template_list',
    {
      description: 'List discovered .njk templates (default: bundled sync templates)',
      inputSchema: {
      },
    },
    async (args) => {
      const fn = handlers['template_list'];
      if (!fn) throw new Error('Handler not implemented: template_list');
      return withMcpSpan('template_list', fn)(args);
    }
  );

  server.registerTool(
    'template_query',
    {
      description: 'Run SPARQL SELECT on an RDF file and print template-style context. For CONSTRUCT/ASK/DESCRIBE use `unrdf query`.',
      inputSchema: {
        "file": z.string().describe('RDF data file'),
        "format": z.string().describe('Output format (table|json)').default('table').optional(),
        "predicate": z.string().describe('Find all values of predicate').optional(),
        "sparql": z.string().describe('SPARQL SELECT query').optional(),
        "subject": z.string().describe('Find all predicates for subject').optional(),
      },
    },
    async (args) => {
      const fn = handlers['template_query'];
      if (!fn) throw new Error('Handler not implemented: template_query');
      return withMcpSpan('template_query', fn)(args);
    }
  );

  server.registerTool(
    'to_json',
    {
      description: 'Convert RDF to JSON representation',
      inputSchema: {
        "input": z.string().describe('Input RDF file'),
        "output": z.string().describe('Output JSON file').optional(),
      },
    },
    async (args) => {
      const fn = handlers['to_json'];
      if (!fn) throw new Error('Handler not implemented: to_json');
      return withMcpSpan('to_json', fn)(args);
    }
  );

  server.registerTool(
    'to_ntriples',
    {
      description: 'Convert RDF to N-Triples format',
      inputSchema: {
        "input": z.string().describe('Input RDF file'),
        "output": z.string().describe('Output N-Triples file').optional(),
      },
    },
    async (args) => {
      const fn = handlers['to_ntriples'];
      if (!fn) throw new Error('Handler not implemented: to_ntriples');
      return withMcpSpan('to_ntriples', fn)(args);
    }
  );

  server.registerTool(
    'to_turtle',
    {
      description: 'Convert RDF to Turtle format',
      inputSchema: {
        "input": z.string().describe('Input RDF file'),
        "output": z.string().describe('Output Turtle file').optional(),
      },
    },
    async (args) => {
      const fn = handlers['to_turtle'];
      if (!fn) throw new Error('Handler not implemented: to_turtle');
      return withMcpSpan('to_turtle', fn)(args);
    }
  );

  return server;
}

/**
 * Get MCP server running status
 * @returns {Promise<{running: boolean, transport: string, pid: number, uptime: string}>}
 */
export async function getMCPServerStatus() {
  return {
    running: true,
    transport: 'stdio',
    pid: process.pid,
    uptime: `${Math.floor(process.uptime())}s`,
  };
}

/**
 * Inspect MCP server capabilities without starting it
 * @returns {Promise<{tools: Array, resources: Array, prompts: Array}>}
 */
export async function inspectMCPServer() {
  const { mcpResources } = await import('./resources.mjs');
  const { mcpPrompts } = await import('./prompts.mjs');
  return {
    tools: [
      { name: 'context_add', description: 'Add prefix to context' },
      { name: 'context_create', description: 'Create a new JSON-LD context' },
      { name: 'context_list', description: 'List context prefixes' },
      { name: 'context_remove', description: 'Remove prefix from context' },
      { name: 'convert', description: 'Convert RDF between formats' },
      { name: 'daemon_cluster', description: 'Show Raft cluster status and members' },
      { name: 'daemon_config', description: 'Display current daemon configuration' },
      { name: 'daemon_list', description: 'List all configured operations' },
      { name: 'daemon_logs', description: 'View operation logs with filtering' },
      { name: 'daemon_run', description: 'Execute operation immediately' },
      { name: 'daemon_schedule', description: 'Add scheduled trigger to operation' },
      { name: 'daemon_status', description: 'Show daemon health and metrics' },
      { name: 'graph_create', description: 'Create a new RDF graph' },
      { name: 'graph_dump', description: 'Export graph data to file' },
      { name: 'graph_load', description: 'Load RDF data into a graph' },
      { name: 'graph_query', description: 'Execute SPARQL query on graph' },
      { name: 'graph_stats', description: 'Show graph statistics' },
      { name: 'hooks_define', description: 'Define hooks from config file' },
      { name: 'hooks_evaluate_condition', description: 'Evaluate a single condition against a store' },
      { name: 'hooks_execute', description: 'Execute hooks against an RDF store' },
      { name: 'hooks_list_conditions', description: 'List available condition kinds' },
      { name: 'hooks_receipts', description: 'Display receipt chain from hook execution results' },
      { name: 'mcp_inspect', description: 'List all exposed tools, resources, and prompts' },
      { name: 'mcp_start', description: 'Start the MCP server' },
      { name: 'mcp_status', description: 'Show if MCP server is running' },
      { name: 'mcp_stop', description: 'Stop the running MCP server' },
      { name: 'query', description: 'Execute SPARQL query' },
      { name: 'query_file', description: 'Execute SPARQL query from file' },
      { name: 'sync', description: 'Generate synchronized code artifacts from RDF ontology' },
      { name: 'template_extract', description: 'Extract properties for a subject as JSON (template debugging)' },
      { name: 'template_generate', description: 'Generate files from RDF + Nunjucks template (`--template`). RDF path can be positional or `rdf:` in template frontmatter.' },
      { name: 'template_list', description: 'List discovered .njk templates (default: bundled sync templates)' },
      { name: 'template_query', description: 'Run SPARQL SELECT on an RDF file and print template-style context. For CONSTRUCT/ASK/DESCRIBE use `unrdf query`.' },
      { name: 'to_json', description: 'Convert RDF to JSON representation' },
      { name: 'to_ntriples', description: 'Convert RDF to N-Triples format' },
      { name: 'to_turtle', description: 'Convert RDF to Turtle format' },
    ],
    resources: mcpResources,
    prompts: mcpPrompts,
  };
}

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
 * @returns {Promise<void>}
 * @throws {Error} SSE transport is not yet implemented
 */
export async function startMCPServerSSE(port = 8765) {
  throw new Error(
    'SSE transport is not yet implemented. ' +
    'Use startMCPServer() for stdio transport instead. ' +
    'To implement SSE, you must provide an HTTP server integration.'
  );
}
