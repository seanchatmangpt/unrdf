/**
 * @file UNRDF MCP Server — CLI commands as MCP tools
 * @module @unrdf/daemon/mcp
 *
 * Implements MCP (Model Context Protocol) using the official SDK.
 * Each unrdf CLI command is exposed as an MCP tool.
 * SDK handles all JSON-RPC 2.0 framing, protocol negotiation, and transport.
 */

import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js';
import { SSEServerTransport } from '@modelcontextprotocol/sdk/server/sse.js';
import { spawn } from 'node:child_process';
import { createServer } from 'node:http';
import { z } from 'zod';

// ---------------------------------------------------------------------------
// Tool definitions with Zod schemas
// ---------------------------------------------------------------------------

const TOOLS = [
  {
    name: 'unrdf_query',
    description: 'Execute a SPARQL query (SELECT/ASK/CONSTRUCT) against an RDF file. Returns JSON results for SELECT, boolean for ASK, triples for CONSTRUCT.',
    schema: z.object({
      file: z.string().describe('RDF data file (Turtle, N-Triples, N-Quads, …)'),
      query: z.string().describe('SPARQL query string'),
      format: z.enum(['table', 'json', 'csv']).default('json').describe('Output format'),
    }).required(['file', 'query']),
    args: ({ file, query, format = 'json' }) => ['query', '--file', file, '--query', query, '--format', format],
  },
  {
    name: 'unrdf_convert',
    description: 'Convert an RDF file between formats (Turtle ↔ N-Triples ↔ N-Quads ↔ JSON-LD).',
    schema: z.object({
      input: z.string().describe('Input RDF file path'),
      output: z.string().describe('Output file path'),
      from: z.string().optional().describe('Input format (auto-detected if omitted)'),
      to: z.string().optional().describe('Output format (auto-detected if omitted)'),
    }).required(['input', 'output']),
    args: ({ input, output, from: f, to }) => {
      const a = ['convert', '--input', input, '--output', output];
      if (f) a.push('--from', f);
      if (to) a.push('--to', to);
      return a;
    },
  },
  {
    name: 'unrdf_to_turtle',
    description: 'Convert any RDF format to Turtle.',
    schema: z.object({
      input: z.string().describe('Input RDF file path'),
      output: z.string().optional().describe('Output file path (prints to stdout if omitted)'),
    }).required(['input']),
    args: ({ input, output }) => {
      const a = ['to-turtle', '--input', input];
      if (output) a.push('--output', output);
      return a;
    },
  },
  {
    name: 'unrdf_to_ntriples',
    description: 'Convert any RDF format to N-Triples.',
    schema: z.object({
      input: z.string().describe('Input RDF file path'),
      output: z.string().optional().describe('Output file path (prints to stdout if omitted)'),
    }).required(['input']),
    args: ({ input, output }) => {
      const a = ['to-ntriples', '--input', input];
      if (output) a.push('--output', output);
      return a;
    },
  },
  {
    name: 'unrdf_graph_stats',
    description: 'Show statistics for an RDF graph file: total quads, unique subjects/predicates/objects, named graphs.',
    schema: z.object({
      file: z.string().describe('RDF graph file to analyze'),
    }).required(['file']),
    args: ({ file }) => ['graph', 'stats', '--file', file],
  },
  {
    name: 'unrdf_graph_query',
    description: 'Execute a SPARQL query directly against an RDF graph file.',
    schema: z.object({
      file: z.string().describe('RDF graph file'),
      query: z.string().describe('SPARQL query string'),
      format: z.enum(['table', 'json']).default('json').describe('Output format'),
    }).required(['file', 'query']),
    args: ({ file, query, format = 'json' }) => ['graph', 'query', '--file', file, '--query', query, '--format', format],
  },
  {
    name: 'unrdf_graph_dump',
    description: 'Export an RDF graph file to another format or file.',
    schema: z.object({
      file: z.string().describe('Source graph file'),
      output: z.string().describe('Output file path'),
      format: z.enum(['nquads', 'trig']).default('nquads').describe('Output format'),
    }).required(['file', 'output']),
    args: ({ file, output, format = 'nquads' }) => ['graph', 'dump', '--file', file, '--output', output, '--format', format],
  },
  {
    name: 'unrdf_hooks_execute',
    description: 'Execute knowledge hooks against an RDF store using a JSON hooks config.',
    schema: z.object({
      store: z.string().describe('RDF store file (Turtle/N-Quads)'),
      config: z.string().describe('Hooks config JSON file path'),
      output: z.string().optional().describe('Output file for results (JSON)'),
      showReceipts: z.boolean().optional().describe('Show BLAKE3 receipt chain'),
    }).required(['store', 'config']),
    args: ({ store, config, output, showReceipts }) => {
      const a = ['hooks', 'execute', '--store', store, '--config', config];
      if (output) a.push('--output', output);
      if (showReceipts) a.push('--show-receipts');
      return a;
    },
  },
  {
    name: 'unrdf_hooks_define',
    description: 'Validate hook definitions from a JSON config file against the KnowledgeHookSchema.',
    schema: z.object({
      config: z.string().describe('Hooks config JSON file path'),
      validate: z.boolean().optional().describe('Validate only, do not execute'),
    }).required(['config']),
    args: ({ config, validate }) => {
      const a = ['hooks', 'define', '--config', config];
      if (validate) a.push('--validate');
      return a;
    },
  },
  {
    name: 'unrdf_hooks_list_conditions',
    description: 'List all available hook condition kinds with descriptions and examples.',
    schema: z.object({}),
    args: () => ['hooks', 'list-conditions'],
  },
  {
    name: 'unrdf_hooks_receipts',
    description: 'Display and optionally verify the BLAKE3 receipt chain from hook execution results.',
    schema: z.object({
      file: z.string().describe('Execution result JSON file'),
      format: z.enum(['table', 'json']).default('table').describe('Output format'),
      verify: z.boolean().optional().describe('Verify chain integrity'),
    }).required(['file']),
    args: ({ file, format = 'table', verify }) => {
      const a = ['hooks', 'receipts', '--file', file, '--format', format];
      if (verify) a.push('--verify');
      return a;
    },
  },
  {
    name: 'unrdf_sync',
    description: 'Generate synchronized code artifacts (Zod schemas, JSDoc types, OpenAPI specs) from an RDF ontology using .unrdf.toml config.',
    schema: z.object({
      config: z.string().optional().describe('Path to .unrdf.toml config file (default: .unrdf.toml)'),
      dryRun: z.boolean().optional().describe('Preview changes without writing files'),
      rule: z.string().optional().describe('Run only the specified rule by name'),
      output: z.enum(['text', 'json']).optional().describe('Output format'),
    }),
    args: ({ config, dryRun, rule, output }) => {
      const a = ['sync'];
      if (config) a.push('--config', config);
      if (dryRun) a.push('--dry-run');
      if (rule) a.push('--rule', rule);
      if (output) a.push('--output', output);
      return a;
    },
  },
  {
    name: 'unrdf_template_generate',
    description: 'Generate a file from an RDF data source and a Nunjucks template. Template must have a SPARQL SELECT query in frontmatter.',
    schema: z.object({
      file: z.string().describe('RDF data file (Turtle, N-Triples, …)'),
      template: z.string().describe('Nunjucks template (.njk) file path'),
    }).required(['file', 'template']),
    args: ({ file, template }) => ['template', 'generate', '--file', file, '--template', template],
  },
  {
    name: 'unrdf_template_list',
    description: 'List all discovered Nunjucks templates (bundled sync templates: Zod, JSDoc, OpenAPI).',
    schema: z.object({}),
    args: () => ['template', 'list'],
  },
  {
    name: 'unrdf_template_query',
    description: 'Run a SPARQL SELECT query against an RDF file and print the results as template context (JSON). Use for debugging templates.',
    schema: z.object({
      file: z.string().describe('RDF data file'),
      query: z.string().optional().describe('SPARQL SELECT query'),
    }).required(['file']),
    args: ({ file, query }) => {
      const a = ['template', 'query', '--file', file];
      if (query) a.push('--sparql', query);
      return a;
    },
  },
];

// ---------------------------------------------------------------------------
// CLI executor — spawns `unrdf <args>` and captures output
// ---------------------------------------------------------------------------

async function runCLI(args) {
  return new Promise((resolve, reject) => {
    const proc = spawn('unrdf', args, { env: process.env });
    let stdout = '';
    let stderr = '';
    proc.stdout.on('data', d => { stdout += d.toString(); });
    proc.stderr.on('data', d => { stderr += d.toString(); });
    proc.on('close', code => {
      if (code === 0) {
        resolve(stdout.trim());
      } else {
        reject(new Error(stderr.trim() || stdout.trim() || `exit code ${code}`));
      }
    });
    proc.on('error', reject);
  });
}

// ---------------------------------------------------------------------------
// Server state (module-level singleton)
// ---------------------------------------------------------------------------

let _server = null;
let _transport = null;
let _httpServer = null;
let _startTime = null;

// ---------------------------------------------------------------------------
// Public API (used by `unrdf mcp start/status/inspect/stop`)
// ---------------------------------------------------------------------------

export async function startMCPServer({ transport = 'stdio', port = 3001 } = {}) {
  // Create SDK server
  const server = new McpServer({
    name: 'unrdf',
    version: '26.4.3',
  });

  // Register all tools with the SDK
  for (const tool of TOOLS) {
    server.tool(
      tool.name,
      tool.description,
      tool.schema,
      async (args) => {
        try {
          const cliArgs = tool.args(args);
          const output = await runCLI(cliArgs);
          return {
            content: [{ type: 'text', text: output }],
          };
        } catch (err) {
          return {
            content: [{ type: 'text', text: err.message }],
            isError: true,
          };
        }
      }
    );
  }

  // Connect transport
  if (transport === 'stdio') {
    _transport = new StdioServerTransport();
    await server.connect(_transport);
    _server = { type: 'stdio', pid: process.pid };
    _startTime = Date.now();
    return _server;
  }

  if (transport === 'sse') {
    _httpServer = createServer();
    _transport = new SSEServerTransport('/sse', _httpServer);
    await server.connect(_transport);
    _httpServer.listen(port);
    _server = { type: 'sse', port, pid: process.pid };
    _startTime = Date.now();
    return _server;
  }

  throw new Error(`Unknown transport: ${transport}`);
}

export async function getMCPServerStatus() {
  if (!_server) return { running: false };
  const uptime = Math.round((Date.now() - _startTime) / 1000);
  return {
    running: true,
    type: _server.type,
    port: _server.port,
    pid: _server.pid,
    uptime: `${uptime}s`,
  };
}

export async function inspectMCPServer() {
  return {
    tools: TOOLS.map(t => ({ name: t.name, description: t.description })),
    resources: [],
    prompts: [],
  };
}

export async function stopMCPServer() {
  if (!_server) return { stopped: false };

  try {
    if (_transport) {
      await _transport.close();
    }
    if (_httpServer) {
      _httpServer.close();
    }
  } catch (err) {
    // Transport may not support close, or may already be closed
  }

  _server = null;
  _transport = null;
  _httpServer = null;
  _startTime = null;
  return { stopped: true };
}
