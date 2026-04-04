/**
 * @file MCP Server Implementation
 * @module @unrdf/daemon/mcp
 * @description Model Context Protocol server for UNRDF Daemon
 * @generated 2026-04-03 17:40:10 from cli-commands.ttl
 *
 * DO NOT EDIT — regenerate with: unrdf sync --rule mcp-index
 */

import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js';
import { registerResources } from './resources.mjs';
import { registerPrompts } from './prompts.mjs';
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

  // Register resources and prompts
  registerResources(server);
  registerPrompts(server);

  server.registerTool(
    'context_add',
    {
      description: 'Add a prefix mapping to a context',
      inputSchema: {
        type: 'object',
        properties: {
          "file": {
            type: 'string',
            description: 'Context file',
          },
          "namespace": {
            type: 'string',
            description: 'Namespace IRI',
          },
          "prefix": {
            type: 'string',
            description: 'Prefix name',
          },
        },
        required: ["file", "namespace", "prefix"],
      },
    },
    async (args) => {
      const fn = handlers['context_add'];
      if (!fn) throw new Error('Handler not implemented: context_add');
      return fn(args);
    }
  );

  server.registerTool(
    'context_create',
    {
      description: 'Create a new JSON-LD context',
      inputSchema: {
        type: 'object',
        properties: {
          "name": {
            type: 'string',
            description: 'Context name',
          },
          "output": {
            type: 'string',
            description: 'Output file path',
          },
        },
        required: ["name"],
      },
    },
    async (args) => {
      const fn = handlers['context_create'];
      if (!fn) throw new Error('Handler not implemented: context_create');
      return fn(args);
    }
  );

  server.registerTool(
    'context_list',
    {
      description: 'List all prefix mappings in a context',
      inputSchema: {
        type: 'object',
        properties: {
          "file": {
            type: 'string',
            description: 'Context file',
          },
          "format": {
            type: 'string',
            description: 'Output format',
            default: 'table',
          },
        },
        required: ["file"],
      },
    },
    async (args) => {
      const fn = handlers['context_list'];
      if (!fn) throw new Error('Handler not implemented: context_list');
      return fn(args);
    }
  );

  server.registerTool(
    'context_remove',
    {
      description: 'Remove a prefix mapping from a context',
      inputSchema: {
        type: 'object',
        properties: {
          "file": {
            type: 'string',
            description: 'Context file',
          },
          "prefix": {
            type: 'string',
            description: 'Prefix to remove',
          },
        },
        required: ["file", "prefix"],
      },
    },
    async (args) => {
      const fn = handlers['context_remove'];
      if (!fn) throw new Error('Handler not implemented: context_remove');
      return fn(args);
    }
  );

  server.registerTool(
    'convert',
    {
      description: 'Convert RDF between formats',
      inputSchema: {
        type: 'object',
        properties: {
          "from": {
            type: 'string',
            description: 'Source format',
          },
          "input": {
            type: 'string',
            description: 'Input file',
          },
          "output": {
            type: 'string',
            description: 'Output file',
          },
          "to": {
            type: 'string',
            description: 'Target format',
          },
        },
        required: ["input", "output"],
      },
    },
    async (args) => {
      const fn = handlers['convert'];
      if (!fn) throw new Error('Handler not implemented: convert');
      return fn(args);
    }
  );

  server.registerTool(
    'convert_to_json',
    {
      description: 'Convert RDF to JSON-LD format',
      inputSchema: {
        type: 'object',
        properties: {
          "input": {
            type: 'string',
            description: 'Input file',
          },
          "output": {
            type: 'string',
            description: 'Output file path',
          },
        },
        required: ["input"],
      },
    },
    async (args) => {
      const fn = handlers['convert_to_json'];
      if (!fn) throw new Error('Handler not implemented: convert_to_json');
      return fn(args);
    }
  );

  server.registerTool(
    'convert_to_ntriples',
    {
      description: 'Convert RDF to N-Triples format',
      inputSchema: {
        type: 'object',
        properties: {
          "input": {
            type: 'string',
            description: 'Input file',
          },
          "output": {
            type: 'string',
            description: 'Output file path',
          },
        },
        required: ["input"],
      },
    },
    async (args) => {
      const fn = handlers['convert_to_ntriples'];
      if (!fn) throw new Error('Handler not implemented: convert_to_ntriples');
      return fn(args);
    }
  );

  server.registerTool(
    'convert_to_turtle',
    {
      description: 'Convert RDF to Turtle format',
      inputSchema: {
        type: 'object',
        properties: {
          "input": {
            type: 'string',
            description: 'Input file',
          },
          "output": {
            type: 'string',
            description: 'Output file path',
          },
        },
        required: ["input"],
      },
    },
    async (args) => {
      const fn = handlers['convert_to_turtle'];
      if (!fn) throw new Error('Handler not implemented: convert_to_turtle');
      return fn(args);
    }
  );

  server.registerTool(
    'daemon_cluster',
    {
      description: 'Manage daemon cluster',
      inputSchema: {
        type: 'object',
        properties: {
          "include-metrics": {
            type: 'boolean',
            description: 'Include metrics',
          },
          "json": {
            type: 'boolean',
            description: 'Output as JSON',
          },
        },
        required: [],
      },
    },
    async (args) => {
      const fn = handlers['daemon_cluster'];
      if (!fn) throw new Error('Handler not implemented: daemon_cluster');
      return fn(args);
    }
  );

  server.registerTool(
    'daemon_config',
    {
      description: 'View daemon configuration',
      inputSchema: {
        type: 'object',
        properties: {
          "json": {
            type: 'boolean',
            description: 'Output as JSON',
          },
        },
        required: [],
      },
    },
    async (args) => {
      const fn = handlers['daemon_config'];
      if (!fn) throw new Error('Handler not implemented: daemon_config');
      return fn(args);
    }
  );

  server.registerTool(
    'daemon_list',
    {
      description: 'List all running daemon instances',
      inputSchema: {
        type: 'object',
        properties: {
          "include-metadata": {
            type: 'boolean',
            description: 'Include metadata',
          },
          "json": {
            type: 'boolean',
            description: 'Output as JSON',
          },
        },
        required: [],
      },
    },
    async (args) => {
      const fn = handlers['daemon_list'];
      if (!fn) throw new Error('Handler not implemented: daemon_list');
      return fn(args);
    }
  );

  server.registerTool(
    'daemon_logs',
    {
      description: 'View daemon logs',
      inputSchema: {
        type: 'object',
        properties: {
          "filter": {
            type: 'string',
            description: 'Log filter expression',
          },
          "follow": {
            type: 'boolean',
            description: 'Follow log output',
          },
          "json": {
            type: 'boolean',
            description: 'Output as JSON',
          },
          "max-lines": {
            type: 'number',
            description: 'Maximum lines to display',
          },
        },
        required: [],
      },
    },
    async (args) => {
      const fn = handlers['daemon_logs'];
      if (!fn) throw new Error('Handler not implemented: daemon_logs');
      return fn(args);
    }
  );

  server.registerTool(
    'daemon_run',
    {
      description: 'Run an operation on the daemon',
      inputSchema: {
        type: 'object',
        properties: {
          "json": {
            type: 'boolean',
            description: 'Output as JSON',
          },
          "operation": {
            type: 'string',
            description: 'Operation name',
          },
          "payload": {
            type: 'string',
            description: 'Operation payload',
          },
          "timeout": {
            type: 'number',
            description: 'Operation timeout in seconds',
          },
        },
        required: ["operation"],
      },
    },
    async (args) => {
      const fn = handlers['daemon_run'];
      if (!fn) throw new Error('Handler not implemented: daemon_run');
      return fn(args);
    }
  );

  server.registerTool(
    'daemon_schedule',
    {
      description: 'Schedule an operation on the daemon',
      inputSchema: {
        type: 'object',
        properties: {
          "json": {
            type: 'boolean',
            description: 'Output as JSON',
          },
          "operation": {
            type: 'string',
            description: 'Operation name',
          },
          "payload": {
            type: 'string',
            description: 'Operation payload',
          },
          "trigger": {
            type: 'string',
            description: 'Trigger cron expression',
          },
        },
        required: ["operation", "trigger"],
      },
    },
    async (args) => {
      const fn = handlers['daemon_schedule'];
      if (!fn) throw new Error('Handler not implemented: daemon_schedule');
      return fn(args);
    }
  );

  server.registerTool(
    'daemon_status',
    {
      description: 'Check daemon status and health',
      inputSchema: {
        type: 'object',
        properties: {
          "include-metrics": {
            type: 'boolean',
            description: 'Include performance metrics',
          },
          "json": {
            type: 'boolean',
            description: 'Output as JSON',
          },
        },
        required: [],
      },
    },
    async (args) => {
      const fn = handlers['daemon_status'];
      if (!fn) throw new Error('Handler not implemented: daemon_status');
      return fn(args);
    }
  );

  server.registerTool(
    'graph_create',
    {
      description: 'Create a new RDF graph',
      inputSchema: {
        type: 'object',
        properties: {
          "file": {
            type: 'string',
            description: 'Output file path',
            default: '<name>.nq',
          },
          "name": {
            type: 'string',
            description: 'Graph name',
          },
        },
        required: ["name"],
      },
    },
    async (args) => {
      const fn = handlers['graph_create'];
      if (!fn) throw new Error('Handler not implemented: graph_create');
      return fn(args);
    }
  );

  server.registerTool(
    'graph_dump',
    {
      description: 'Dump an RDF graph to a file',
      inputSchema: {
        type: 'object',
        properties: {
          "file": {
            type: 'string',
            description: 'Graph file to dump',
          },
          "format": {
            type: 'string',
            description: 'Output format',
            enum: ["jsonld", "ntriples", "turtle"],
            default: 'turtle',
          },
          "output": {
            type: 'string',
            description: 'Output file path',
          },
        },
        required: ["file", "output"],
      },
    },
    async (args) => {
      const fn = handlers['graph_dump'];
      if (!fn) throw new Error('Handler not implemented: graph_dump');
      return fn(args);
    }
  );

  server.registerTool(
    'graph_load',
    {
      description: 'Load RDF data into a graph from a file',
      inputSchema: {
        type: 'object',
        properties: {
          "file": {
            type: 'string',
            description: 'RDF file to load',
          },
          "format": {
            type: 'string',
            description: 'RDF format (turtle, ntriples, jsonld, trig)',
          },
          "graph": {
            type: 'string',
            description: 'Target graph IRI',
          },
        },
        required: ["file"],
      },
    },
    async (args) => {
      const fn = handlers['graph_load'];
      if (!fn) throw new Error('Handler not implemented: graph_load');
      return fn(args);
    }
  );

  server.registerTool(
    'graph_query',
    {
      description: 'Execute SPARQL query on a graph file',
      inputSchema: {
        type: 'object',
        properties: {
          "file": {
            type: 'string',
            description: 'Graph file to query',
          },
          "format": {
            type: 'string',
            description: 'Output format',
            enum: ["json", "table", "turtle"],
            default: 'json',
          },
          "query": {
            type: 'string',
            description: 'SPARQL query string',
          },
        },
        required: ["file", "query"],
      },
    },
    async (args) => {
      const fn = handlers['graph_query'];
      if (!fn) throw new Error('Handler not implemented: graph_query');
      return fn(args);
    }
  );

  server.registerTool(
    'graph_stats',
    {
      description: 'Get statistics about an RDF graph',
      inputSchema: {
        type: 'object',
        properties: {
          "file": {
            type: 'string',
            description: 'Graph file',
          },
        },
        required: ["file"],
      },
    },
    async (args) => {
      const fn = handlers['graph_stats'];
      if (!fn) throw new Error('Handler not implemented: graph_stats');
      return fn(args);
    }
  );

  server.registerTool(
    'hooks_define',
    {
      description: 'Define RDF hooks configuration',
      inputSchema: {
        type: 'object',
        properties: {
          "config": {
            type: 'string',
            description: 'Configuration file to create',
          },
          "output": {
            type: 'string',
            description: 'Output file path',
          },
          "validate": {
            type: 'boolean',
            description: 'Validate configuration',
          },
        },
        required: ["config"],
      },
    },
    async (args) => {
      const fn = handlers['hooks_define'];
      if (!fn) throw new Error('Handler not implemented: hooks_define');
      return fn(args);
    }
  );

  server.registerTool(
    'hooks_evaluate_condition',
    {
      description: 'Evaluate a hook condition against a store',
      inputSchema: {
        type: 'object',
        properties: {
          "condition": {
            type: 'string',
            description: 'Condition expression',
          },
          "config": {
            type: 'string',
            description: 'Configuration file',
          },
          "store": {
            type: 'string',
            description: 'Store file',
          },
        },
        required: ["condition", "config", "store"],
      },
    },
    async (args) => {
      const fn = handlers['hooks_evaluate_condition'];
      if (!fn) throw new Error('Handler not implemented: hooks_evaluate_condition');
      return fn(args);
    }
  );

  server.registerTool(
    'hooks_execute',
    {
      description: 'Execute registered RDF hooks on a store',
      inputSchema: {
        type: 'object',
        properties: {
          "config": {
            type: 'string',
            description: 'Hooks configuration file',
          },
          "output": {
            type: 'string',
            description: 'Output file path',
          },
          "show-receipts": {
            type: 'boolean',
            description: 'Display execution receipts',
          },
          "store": {
            type: 'string',
            description: 'Store file or identifier',
          },
        },
        required: ["config", "store"],
      },
    },
    async (args) => {
      const fn = handlers['hooks_execute'];
      if (!fn) throw new Error('Handler not implemented: hooks_execute');
      return fn(args);
    }
  );

  server.registerTool(
    'hooks_list_conditions',
    {
      description: 'List all available hook conditions',
      inputSchema: {
        type: 'object',
        properties: {
        },
        required: [],
      },
    },
    async (args) => {
      const fn = handlers['hooks_list_conditions'];
      if (!fn) throw new Error('Handler not implemented: hooks_list_conditions');
      return fn(args);
    }
  );

  server.registerTool(
    'hooks_receipts',
    {
      description: 'View execution receipts from hook runs',
      inputSchema: {
        type: 'object',
        properties: {
          "file": {
            type: 'string',
            description: 'Receipts file',
          },
          "format": {
            type: 'string',
            description: 'Output format',
            default: 'table',
          },
          "verify": {
            type: 'boolean',
            description: 'Verify receipt signatures',
          },
        },
        required: ["file"],
      },
    },
    async (args) => {
      const fn = handlers['hooks_receipts'];
      if (!fn) throw new Error('Handler not implemented: hooks_receipts');
      return fn(args);
    }
  );

  server.registerTool(
    'query',
    {
      description: 'Execute SPARQL query on a data file',
      inputSchema: {
        type: 'object',
        properties: {
          "file": {
            type: 'string',
            description: 'Data file',
          },
          "format": {
            type: 'string',
            description: 'Output format',
            default: 'json',
          },
          "query": {
            type: 'string',
            description: 'SPARQL query',
          },
        },
        required: ["file", "query"],
      },
    },
    async (args) => {
      const fn = handlers['query'];
      if (!fn) throw new Error('Handler not implemented: query');
      return fn(args);
    }
  );

  server.registerTool(
    'query_file',
    {
      description: 'Execute SPARQL query from a file',
      inputSchema: {
        type: 'object',
        properties: {
          "data": {
            type: 'string',
            description: 'Data file',
          },
          "format": {
            type: 'string',
            description: 'Output format',
          },
          "query": {
            type: 'string',
            description: 'Query file',
          },
        },
        required: ["data", "query"],
      },
    },
    async (args) => {
      const fn = handlers['query_file'];
      if (!fn) throw new Error('Handler not implemented: query_file');
      return fn(args);
    }
  );

  server.registerTool(
    'sync',
    {
      description: 'Synchronize and generate code from RDF ontology',
      inputSchema: {
        type: 'object',
        properties: {
          "config": {
            type: 'string',
            description: 'Path to .unrdf.toml config',
          },
          "dry-run": {
            type: 'boolean',
            description: 'Preview without writing files',
          },
          "force": {
            type: 'boolean',
            description: 'Force regeneration',
          },
          "output": {
            type: 'string',
            description: 'Output format (text or json)',
            default: 'text',
          },
          "rule": {
            type: 'string',
            description: 'Run only specified rule',
          },
          "verbose": {
            type: 'boolean',
            description: 'Verbose output',
          },
        },
        required: [],
      },
    },
    async (args) => {
      const fn = handlers['sync'];
      if (!fn) throw new Error('Handler not implemented: sync');
      return fn(args);
    }
  );

  server.registerTool(
    'template_extract',
    {
      description: 'Extract data from RDF using template patterns',
      inputSchema: {
        type: 'object',
        properties: {
          "file": {
            type: 'string',
            description: 'RDF file',
          },
          "format": {
            type: 'string',
            description: 'Output format',
            default: 'json',
          },
          "subject": {
            type: 'string',
            description: 'Subject IRI filter',
          },
        },
        required: ["file"],
      },
    },
    async (args) => {
      const fn = handlers['template_extract'];
      if (!fn) throw new Error('Handler not implemented: template_extract');
      return fn(args);
    }
  );

  server.registerTool(
    'template_generate',
    {
      description: 'Generate files from a Nunjucks template',
      inputSchema: {
        type: 'object',
        properties: {
          "batch": {
            type: 'boolean',
            description: 'Batch mode',
          },
          "classUri": {
            type: 'string',
            description: 'Class URI filter',
          },
          "dryRun": {
            type: 'boolean',
            description: 'Preview without writing',
          },
          "file": {
            type: 'string',
            description: 'Data file (RDF)',
          },
          "force": {
            type: 'boolean',
            description: 'Force overwrite',
          },
          "outputDir": {
            type: 'string',
            description: 'Output directory',
          },
          "sparql": {
            type: 'string',
            description: 'SPARQL query',
          },
          "subject": {
            type: 'string',
            description: 'Subject filter',
          },
          "template": {
            type: 'string',
            description: 'Template file path',
          },
        },
        required: ["template"],
      },
    },
    async (args) => {
      const fn = handlers['template_generate'];
      if (!fn) throw new Error('Handler not implemented: template_generate');
      return fn(args);
    }
  );

  server.registerTool(
    'template_list',
    {
      description: 'List available templates',
      inputSchema: {
        type: 'object',
        properties: {
          "dir": {
            type: 'string',
            description: 'Template directory',
          },
          "format": {
            type: 'string',
            description: 'Output format',
            default: 'table',
          },
        },
        required: [],
      },
    },
    async (args) => {
      const fn = handlers['template_list'];
      if (!fn) throw new Error('Handler not implemented: template_list');
      return fn(args);
    }
  );

  server.registerTool(
    'template_query',
    {
      description: 'Query template variables and context',
      inputSchema: {
        type: 'object',
        properties: {
          "file": {
            type: 'string',
            description: 'Template file',
          },
          "format": {
            type: 'string',
            description: 'Output format',
            default: 'json',
          },
          "sparql": {
            type: 'string',
            description: 'SPARQL query',
          },
        },
        required: ["file"],
      },
    },
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
 * @throws {Error} SSE transport is not yet implemented
 */
export async function startMCPServerSSE(port = 8765) {
  throw new Error(
    'SSE transport is not yet implemented. ' +
    'Use startMCPServer() for stdio transport instead. ' +
    'To implement SSE, you must provide an HTTP server integration.'
  );
}
