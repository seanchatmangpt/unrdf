/**
 * @file MCP Tool Definitions
 * @module @unrdf/daemon/mcp/tools-generated
 * @description Auto-generated tool JSON schema definitions
 * @generated 2026-04-03 21:03:03 from cli-commands.ttl
 *
 * DO NOT EDIT — regenerate with: unrdf sync --rule mcp-tool-defs
 */


/**
 * All MCP tool definitions with JSON Schema input schemas
 * @type {Array<{name: string, description: string, inputSchema: object}>}
 */
export const mcpGeneratedTools = [
  {
    name: 'context_add',
    description: 'Add prefix to context',
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
  {
    name: 'context_create',
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
  {
    name: 'context_list',
    description: 'List context prefixes',
    inputSchema: {
      type: 'object',
      properties: {
        "file": {
          type: 'string',
          description: 'Context file',
        },
        "format": {
          type: 'string',
          description: 'Output format (table, json)',
          default: 'table',
        },
      },
      required: ["file"],
    },
  },
  {
    name: 'context_remove',
    description: 'Remove prefix from context',
    inputSchema: {
      type: 'object',
      properties: {
        "file": {
          type: 'string',
          description: 'Context file',
        },
        "prefix": {
          type: 'string',
          description: 'Prefix name to remove',
        },
      },
      required: ["file", "prefix"],
    },
  },
  {
    name: 'convert',
    description: 'Convert RDF between formats',
    inputSchema: {
      type: 'object',
      properties: {
        "from": {
          type: 'string',
          description: 'Input format (turtle, ntriples, nquads) - auto-detected if not specified',
        },
        "input": {
          type: 'string',
          description: 'Input RDF file',
        },
        "output": {
          type: 'string',
          description: 'Output file',
        },
        "to": {
          type: 'string',
          description: 'Output format (turtle, ntriples, nquads) - auto-detected if not specified',
        },
      },
      required: ["input", "output"],
    },
  },
  {
    name: 'daemon_cluster',
    description: 'Show Raft cluster status and members',
    inputSchema: {
      type: 'object',
      properties: {
        "include-metrics": {
          type: 'boolean',
          description: 'Include detailed member metrics',
        },
        "json": {
          type: 'boolean',
          description: 'Output as JSON',
        },
      },
      required: [],
    },
  },
  {
    name: 'daemon_config',
    description: 'Display current daemon configuration',
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
  {
    name: 'daemon_list',
    description: 'List all configured operations',
    inputSchema: {
      type: 'object',
      properties: {
        "include-metadata": {
          type: 'boolean',
          description: 'Include metadata in output',
        },
        "json": {
          type: 'boolean',
          description: 'Output as JSON',
        },
      },
      required: [],
    },
  },
  {
    name: 'daemon_logs',
    description: 'View operation logs with filtering',
    inputSchema: {
      type: 'object',
      properties: {
        "filter": {
          type: 'string',
          description: 'Filter logs by pattern',
        },
        "follow": {
          type: 'boolean',
          description: 'Follow log output (stream mode)',
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
  {
    name: 'daemon_run',
    description: 'Execute operation immediately',
    inputSchema: {
      type: 'object',
      properties: {
        "json": {
          type: 'boolean',
          description: 'Output as JSON',
        },
        "operation": {
          type: 'string',
          description: 'Operation ID to execute',
        },
        "payload": {
          type: 'string',
          description: 'Operation payload (JSON string)',
        },
        "timeout": {
          type: 'number',
          description: 'Execution timeout in milliseconds',
        },
      },
      required: ["operation"],
    },
  },
  {
    name: 'daemon_schedule',
    description: 'Add scheduled trigger to operation',
    inputSchema: {
      type: 'object',
      properties: {
        "json": {
          type: 'boolean',
          description: 'Output as JSON',
        },
        "operation": {
          type: 'string',
          description: 'Operation ID to schedule',
        },
        "payload": {
          type: 'string',
          description: 'Trigger payload (JSON string)',
        },
        "trigger": {
          type: 'string',
          description: 'Trigger type (cron, interval, reactive, event)',
        },
      },
      required: ["operation", "trigger"],
    },
  },
  {
    name: 'daemon_status',
    description: 'Show daemon health and metrics',
    inputSchema: {
      type: 'object',
      properties: {
        "include-metrics": {
          type: 'boolean',
          description: 'Include detailed metrics',
        },
        "json": {
          type: 'boolean',
          description: 'Output as JSON',
        },
      },
      required: [],
    },
  },
  {
    name: 'graph_create',
    description: 'Create a new RDF graph',
    inputSchema: {
      type: 'object',
      properties: {
        "file": {
          type: 'string',
          description: 'Output file path (default: <name>.nq)',
        },
        "name": {
          type: 'string',
          description: 'Graph name',
        },
      },
      required: ["name"],
    },
  },
  {
    name: 'graph_dump',
    description: 'Export graph data to file',
    inputSchema: {
      type: 'object',
      properties: {
        "file": {
          type: 'string',
          description: 'Source graph file',
        },
        "format": {
          type: 'string',
          description: 'Output format (turtle, ntriples, nquads)',
          default: 'turtle',
        },
        "output": {
          type: 'string',
          description: 'Output file',
        },
      },
      required: ["file", "output"],
    },
  },
  {
    name: 'graph_load',
    description: 'Load RDF data into a graph',
    inputSchema: {
      type: 'object',
      properties: {
        "file": {
          type: 'string',
          description: 'RDF file to load (Turtle, N-Triples, N-Quads)',
        },
        "format": {
          type: 'string',
          description: 'RDF format (turtle, ntriples, nquads) - auto-detected if not specified',
        },
        "graph": {
          type: 'string',
          description: 'Target graph name',
        },
      },
      required: ["file"],
    },
  },
  {
    name: 'graph_query',
    description: 'Execute SPARQL query on graph',
    inputSchema: {
      type: 'object',
      properties: {
        "file": {
          type: 'string',
          description: 'Graph file to query',
        },
        "format": {
          type: 'string',
          description: 'Output format (table, json, turtle)',
          default: 'table',
        },
        "query": {
          type: 'string',
          description: 'SPARQL query string',
        },
      },
      required: ["file", "query"],
    },
  },
  {
    name: 'graph_stats',
    description: 'Show graph statistics',
    inputSchema: {
      type: 'object',
      properties: {
        "file": {
          type: 'string',
          description: 'Graph file to analyze',
        },
      },
      required: ["file"],
    },
  },
  {
    name: 'hooks_define',
    description: 'Define hooks from config file',
    inputSchema: {
      type: 'object',
      properties: {
        "config": {
          type: 'string',
          description: 'Hooks config file (JSON)',
        },
        "output": {
          type: 'string',
          description: 'Output file for hook metadata (JSON)',
        },
        "validate": {
          type: 'boolean',
          description: 'Validate without executing',
          default: 'false',
        },
      },
      required: ["config"],
    },
  },
  {
    name: 'hooks_evaluate_condition',
    description: 'Evaluate a single condition against a store',
    inputSchema: {
      type: 'object',
      properties: {
        "condition": {
          type: 'string',
          description: 'Condition type (sparql-ask, sparql-select, shacl, delta, threshold, count, window, n3, datalog)',
        },
        "config": {
          type: 'string',
          description: 'Condition config as JSON',
        },
        "store": {
          type: 'string',
          description: 'Store file (NQ/Turtle/N-Triples)',
        },
      },
      required: ["condition", "config", "store"],
    },
  },
  {
    name: 'hooks_execute',
    description: 'Execute hooks against an RDF store',
    inputSchema: {
      type: 'object',
      properties: {
        "config": {
          type: 'string',
          description: 'Hooks config file (JSON)',
        },
        "output": {
          type: 'string',
          description: 'Output file for results (JSON)',
        },
        "show-receipts": {
          type: 'boolean',
          description: 'Show receipt chain with hashes',
          default: 'false',
        },
        "store": {
          type: 'string',
          description: 'Store file (NQ/Turtle/N-Triples)',
        },
      },
      required: ["config", "store"],
    },
  },
  {
    name: 'hooks_list_conditions',
    description: 'List available condition kinds',
    inputSchema: {
      type: 'object',
      properties: {
      },
      required: [],
    },
  },
  {
    name: 'hooks_receipts',
    description: 'Display receipt chain from hook execution results',
    inputSchema: {
      type: 'object',
      properties: {
        "file": {
          type: 'string',
          description: 'Execution result file (JSON)',
        },
        "format": {
          type: 'string',
          description: 'Output format (json, table)',
          default: 'table',
        },
        "verify": {
          type: 'boolean',
          description: 'Verify receipt chain integrity',
          default: 'false',
        },
      },
      required: ["file"],
    },
  },
  {
    name: 'mcp_inspect',
    description: 'List all exposed tools, resources, and prompts',
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
  {
    name: 'mcp_start',
    description: 'Start the MCP server',
    inputSchema: {
      type: 'object',
      properties: {
        "port": {
          type: 'number',
          description: 'Port for SSE transport (when transport=sse)',
          default: '3001',
        },
        "transport": {
          type: 'string',
          description: 'Transport type: stdio or sse',
          default: 'stdio',
        },
      },
      required: [],
    },
  },
  {
    name: 'mcp_status',
    description: 'Show if MCP server is running',
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
  {
    name: 'mcp_stop',
    description: 'Stop the running MCP server',
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
  {
    name: 'query',
    description: 'Execute SPARQL query',
    inputSchema: {
      type: 'object',
      properties: {
        "file": {
          type: 'string',
          description: 'RDF data file',
        },
        "format": {
          type: 'string',
          description: 'Output format (table, json, csv)',
          default: 'table',
        },
        "query": {
          type: 'string',
          description: 'SPARQL query string',
        },
      },
      required: ["file", "query"],
    },
  },
  {
    name: 'query_file',
    description: 'Execute SPARQL query from file',
    inputSchema: {
      type: 'object',
      properties: {
        "data": {
          type: 'string',
          description: 'RDF data file',
        },
        "format": {
          type: 'string',
          description: 'Output format (table, json, csv)',
          default: 'table',
        },
        "query": {
          type: 'string',
          description: 'SPARQL query file (.sparql)',
        },
      },
      required: ["data", "query"],
    },
  },
  {
    name: 'sync',
    description: 'Generate synchronized code artifacts from RDF ontology',
    inputSchema: {
      type: 'object',
      properties: {
        "config": {
          type: 'string',
          description: 'Path to `.unrdf.toml` configuration file',
          default: '.unrdf.toml',
        },
        "dry-run": {
          type: 'boolean',
          description: 'Preview changes without writing files',
          default: 'false',
        },
        "force": {
          type: 'boolean',
          description: 'Overwrite existing files without prompting',
          default: 'false',
        },
        "output": {
          type: 'string',
          description: 'Output format: text or json',
          default: 'text',
        },
        "rule": {
          type: 'string',
          description: 'Run only the specified rule by name',
        },
        "verbose": {
          type: 'boolean',
          description: 'Enable verbose output',
          default: 'false',
        },
        "watch": {
          type: 'boolean',
          description: 'Watch ontology and template files for changes',
          default: 'false',
        },
      },
      required: [],
    },
  },
  {
    name: 'template_extract',
    description: 'Extract properties for a subject as JSON (template debugging)',
    inputSchema: {
      type: 'object',
      properties: {
        "file": {
          type: 'string',
          description: 'RDF data file',
        },
        "format": {
          type: 'string',
          description: 'Output format (json|yaml|table)',
          default: 'json',
        },
        "subject": {
          type: 'string',
          description: 'Subject URI',
        },
      },
      required: ["file"],
    },
  },
  {
    name: 'template_generate',
    description: 'Generate files from RDF + Nunjucks template (`--template`). RDF path can be positional or `rdf:` in template frontmatter.',
    inputSchema: {
      type: 'object',
      properties: {
        "batch": {
          type: 'boolean',
          description: 'One output per instance of `--class-uri`; SPARQL must use ?subject',
          default: 'false',
        },
        "classUri": {
          type: 'string',
          description: 'RDFS/OWL class IRI for batch mode (with `--batch`)',
        },
        "dryRun": {
          type: 'boolean',
          description: 'Print paths without writing',
          default: 'false',
        },
        "file": {
          type: 'string',
          description: 'RDF file (Turtle, N-Triples, N-Quads, …). Optional if template sets `rdf:` in frontmatter.',
        },
        "force": {
          type: 'boolean',
          description: 'Overwrite existing files',
          default: 'false',
        },
        "outputDir": {
          type: 'string',
          description: 'Output directory for generated files',
          default: './generated',
        },
        "sparql": {
          type: 'string',
          description: 'SPARQL SELECT (overrides frontmatter `sparql:`)',
        },
        "subject": {
          type: 'string',
          description: 'Focus subject URI; replaces ?subject in SPARQL when present',
        },
        "template": {
          type: 'string',
          description: 'Template .njk path',
        },
      },
      required: ["template"],
    },
  },
  {
    name: 'template_list',
    description: 'List discovered .njk templates (default: bundled sync templates)',
    inputSchema: {
      type: 'object',
      properties: {
      },
      required: [],
    },
  },
  {
    name: 'template_query',
    description: 'Run SPARQL SELECT on an RDF file and print template-style context. For CONSTRUCT/ASK/DESCRIBE use `unrdf query`.',
    inputSchema: {
      type: 'object',
      properties: {
        "file": {
          type: 'string',
          description: 'RDF data file',
        },
        "format": {
          type: 'string',
          description: 'Output format (table|json)',
          default: 'table',
        },
        "predicate": {
          type: 'string',
          description: 'Find all values of predicate',
        },
        "sparql": {
          type: 'string',
          description: 'SPARQL SELECT query',
        },
        "subject": {
          type: 'string',
          description: 'Find all predicates for subject',
        },
      },
      required: ["file"],
    },
  },
  {
    name: 'to_json',
    description: 'Convert RDF to JSON representation',
    inputSchema: {
      type: 'object',
      properties: {
        "input": {
          type: 'string',
          description: 'Input RDF file',
        },
        "output": {
          type: 'string',
          description: 'Output JSON file',
        },
      },
      required: ["input"],
    },
  },
  {
    name: 'to_ntriples',
    description: 'Convert RDF to N-Triples format',
    inputSchema: {
      type: 'object',
      properties: {
        "input": {
          type: 'string',
          description: 'Input RDF file',
        },
        "output": {
          type: 'string',
          description: 'Output N-Triples file',
        },
      },
      required: ["input"],
    },
  },
  {
    name: 'to_turtle',
    description: 'Convert RDF to Turtle format',
    inputSchema: {
      type: 'object',
      properties: {
        "input": {
          type: 'string',
          description: 'Input RDF file',
        },
        "output": {
          type: 'string',
          description: 'Output Turtle file',
        },
      },
      required: ["input"],
    },
  },
];

/**
 * Export as default for backward compatibility
 */
export default mcpGeneratedTools;

/**
 * Alias for compatibility with mcp.mjs inspect command
 */
export const mcpTools = mcpGeneratedTools;
