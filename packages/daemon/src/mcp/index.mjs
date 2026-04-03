/**
 * @file MCP Server Implementation
 * @module @unrdf/daemon/mcp
 * @description Model Context Protocol server for UNRDF Daemon
 * @generated 2026-04-03 15:14:12 from cli-commands.ttl
 *
 * DO NOT EDIT — regenerate with: unrdf sync --rule mcp-index
 */

import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js';
import { mcpResources } from './resources.mjs';
import { mcpPrompts } from './prompts.mjs';
import * as handlers from './handlers.mjs';


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
    async (args) => {
      const fn = handlers['context_add'];
      if (!fn) throw new Error('Handler not implemented: context_add');
      return fn(args);
    }
  );

  server.tool(
    'context_create',
    'Create a new JSON-LD context',
    async (args) => {
      const fn = handlers['context_create'];
      if (!fn) throw new Error('Handler not implemented: context_create');
      return fn(args);
    }
  );

  server.tool(
    'context_list',
    'List all prefix mappings in a context',
    async (args) => {
      const fn = handlers['context_list'];
      if (!fn) throw new Error('Handler not implemented: context_list');
      return fn(args);
    }
  );

  server.tool(
    'context_remove',
    'Remove a prefix mapping from a context',
    async (args) => {
      const fn = handlers['context_remove'];
      if (!fn) throw new Error('Handler not implemented: context_remove');
      return fn(args);
    }
  );

  server.tool(
    'convert',
    'Convert RDF between formats',
    async (args) => {
      const fn = handlers['convert'];
      if (!fn) throw new Error('Handler not implemented: convert');
      return fn(args);
    }
  );

  server.tool(
    'convert_to_json',
    'Convert RDF to JSON-LD format',
    async (args) => {
      const fn = handlers['convert_to_json'];
      if (!fn) throw new Error('Handler not implemented: convert_to_json');
      return fn(args);
    }
  );

  server.tool(
    'convert_to_ntriples',
    'Convert RDF to N-Triples format',
    async (args) => {
      const fn = handlers['convert_to_ntriples'];
      if (!fn) throw new Error('Handler not implemented: convert_to_ntriples');
      return fn(args);
    }
  );

  server.tool(
    'convert_to_turtle',
    'Convert RDF to Turtle format',
    async (args) => {
      const fn = handlers['convert_to_turtle'];
      if (!fn) throw new Error('Handler not implemented: convert_to_turtle');
      return fn(args);
    }
  );

  server.tool(
    'daemon_cluster',
    'Manage daemon cluster',
    async (args) => {
      const fn = handlers['daemon_cluster'];
      if (!fn) throw new Error('Handler not implemented: daemon_cluster');
      return fn(args);
    }
  );

  server.tool(
    'daemon_config',
    'View daemon configuration',
    async (args) => {
      const fn = handlers['daemon_config'];
      if (!fn) throw new Error('Handler not implemented: daemon_config');
      return fn(args);
    }
  );

  server.tool(
    'daemon_list',
    'List all running daemon instances',
    async (args) => {
      const fn = handlers['daemon_list'];
      if (!fn) throw new Error('Handler not implemented: daemon_list');
      return fn(args);
    }
  );

  server.tool(
    'daemon_logs',
    'View daemon logs',
    async (args) => {
      const fn = handlers['daemon_logs'];
      if (!fn) throw new Error('Handler not implemented: daemon_logs');
      return fn(args);
    }
  );

  server.tool(
    'daemon_run',
    'Run an operation on the daemon',
    async (args) => {
      const fn = handlers['daemon_run'];
      if (!fn) throw new Error('Handler not implemented: daemon_run');
      return fn(args);
    }
  );

  server.tool(
    'daemon_schedule',
    'Schedule an operation on the daemon',
    async (args) => {
      const fn = handlers['daemon_schedule'];
      if (!fn) throw new Error('Handler not implemented: daemon_schedule');
      return fn(args);
    }
  );

  server.tool(
    'daemon_status',
    'Check daemon status and health',
    async (args) => {
      const fn = handlers['daemon_status'];
      if (!fn) throw new Error('Handler not implemented: daemon_status');
      return fn(args);
    }
  );

  server.tool(
    'graph_create',
    'Create a new RDF graph',
    async (args) => {
      const fn = handlers['graph_create'];
      if (!fn) throw new Error('Handler not implemented: graph_create');
      return fn(args);
    }
  );

  server.tool(
    'graph_dump',
    'Dump an RDF graph to a file',
    async (args) => {
      const fn = handlers['graph_dump'];
      if (!fn) throw new Error('Handler not implemented: graph_dump');
      return fn(args);
    }
  );

  server.tool(
    'graph_load',
    'Load RDF data into a graph from a file',
    async (args) => {
      const fn = handlers['graph_load'];
      if (!fn) throw new Error('Handler not implemented: graph_load');
      return fn(args);
    }
  );

  server.tool(
    'graph_query',
    'Execute SPARQL query on a graph file',
    async (args) => {
      const fn = handlers['graph_query'];
      if (!fn) throw new Error('Handler not implemented: graph_query');
      return fn(args);
    }
  );

  server.tool(
    'graph_stats',
    'Get statistics about an RDF graph',
    async (args) => {
      const fn = handlers['graph_stats'];
      if (!fn) throw new Error('Handler not implemented: graph_stats');
      return fn(args);
    }
  );

  server.tool(
    'hooks_define',
    'Define RDF hooks configuration',
    async (args) => {
      const fn = handlers['hooks_define'];
      if (!fn) throw new Error('Handler not implemented: hooks_define');
      return fn(args);
    }
  );

  server.tool(
    'hooks_evaluate_condition',
    'Evaluate a hook condition against a store',
    async (args) => {
      const fn = handlers['hooks_evaluate_condition'];
      if (!fn) throw new Error('Handler not implemented: hooks_evaluate_condition');
      return fn(args);
    }
  );

  server.tool(
    'hooks_execute',
    'Execute registered RDF hooks on a store',
    async (args) => {
      const fn = handlers['hooks_execute'];
      if (!fn) throw new Error('Handler not implemented: hooks_execute');
      return fn(args);
    }
  );

  server.tool(
    'hooks_list_conditions',
    'List all available hook conditions',
    async (args) => {
      const fn = handlers['hooks_list_conditions'];
      if (!fn) throw new Error('Handler not implemented: hooks_list_conditions');
      return fn(args);
    }
  );

  server.tool(
    'hooks_receipts',
    'View execution receipts from hook runs',
    async (args) => {
      const fn = handlers['hooks_receipts'];
      if (!fn) throw new Error('Handler not implemented: hooks_receipts');
      return fn(args);
    }
  );

  server.tool(
    'query',
    'Execute SPARQL query on a data file',
    async (args) => {
      const fn = handlers['query'];
      if (!fn) throw new Error('Handler not implemented: query');
      return fn(args);
    }
  );

  server.tool(
    'query_file',
    'Execute SPARQL query from a file',
    async (args) => {
      const fn = handlers['query_file'];
      if (!fn) throw new Error('Handler not implemented: query_file');
      return fn(args);
    }
  );

  server.tool(
    'sync',
    'Synchronize and generate code from RDF ontology',
    async (args) => {
      const fn = handlers['sync'];
      if (!fn) throw new Error('Handler not implemented: sync');
      return fn(args);
    }
  );

  server.tool(
    'template_extract',
    'Extract data from RDF using template patterns',
    async (args) => {
      const fn = handlers['template_extract'];
      if (!fn) throw new Error('Handler not implemented: template_extract');
      return fn(args);
    }
  );

  server.tool(
    'template_generate',
    'Generate files from a Nunjucks template',
    async (args) => {
      const fn = handlers['template_generate'];
      if (!fn) throw new Error('Handler not implemented: template_generate');
      return fn(args);
    }
  );

  server.tool(
    'template_list',
    'List available templates',
    async (args) => {
      const fn = handlers['template_list'];
      if (!fn) throw new Error('Handler not implemented: template_list');
      return fn(args);
    }
  );

  server.tool(
    'template_query',
    'Query template variables and context',
    async (args) => {
      const fn = handlers['template_query'];
      if (!fn) throw new Error('Handler not implemented: template_query');
      return fn(args);
    }
  );

  return server;
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
 */
export async function startMCPServerSSE(port = 8765) {
  const server = createMCPServer();
  console.error(`SSE transport requested on port ${port}`);
  console.error('Note: SSE transport requires HTTP server integration');
  const transport = new StdioServerTransport();
  await server.connect(transport);
}

export { mcpResources, mcpPrompts };
