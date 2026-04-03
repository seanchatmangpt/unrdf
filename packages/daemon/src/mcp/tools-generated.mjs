/**
 * @file MCP Tool Definitions
 * @module @unrdf/daemon/mcp/tools-generated
 * @description Auto-generated tool JSON schema definitions
 * @generated 2026-04-03 15:01:06 from cli-commands.ttl
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
    description: 'Add a prefix mapping to a context',
    inputSchema: {
      type: 'object',
      properties: {
        file: {
          type: 'string',
          description: 'Context file',
        },
        namespace: {
          type: 'string',
          description: 'Namespace IRI',
        },
        prefix: {
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
        name: {
          type: 'string',
          description: 'Context name',
        },
        output: {
          type: 'string',
          description: 'Output file path',
        },
      },
      required: ["name"],
    },
  },
  {
    name: 'context_list',
    description: 'List all prefix mappings in a context',
    inputSchema: {
      type: 'object',
      properties: {
        file: {
          type: 'string',
          description: 'Context file',
        },
        format: {
          type: 'string',
          description: 'Output format',
          default: 'table',
        },
      },
      required: ["file"],
    },
  },
  {
    name: 'context_remove',
    description: 'Remove a prefix mapping from a context',
    inputSchema: {
      type: 'object',
      properties: {
        file: {
          type: 'string',
          description: 'Context file',
        },
        prefix: {
          type: 'string',
          description: 'Prefix to remove',
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
        from: {
          type: 'string',
          description: 'Source format',
        },
        input: {
          type: 'string',
          description: 'Input file',
        },
        output: {
          type: 'string',
          description: 'Output file',
        },
        to: {
          type: 'string',
          description: 'Target format',
        },
      },
      required: ["input", "output"],
    },
  },
  {
    name: 'convert_to_json',
    description: 'Convert RDF to JSON-LD format',
    inputSchema: {
      type: 'object',
      properties: {
        input: {
          type: 'string',
          description: 'Input file',
        },
        output: {
          type: 'string',
          description: 'Output file path',
        },
      },
      required: ["input"],
    },
  },
  {
    name: 'convert_to_ntriples',
    description: 'Convert RDF to N-Triples format',
    inputSchema: {
      type: 'object',
      properties: {
        input: {
          type: 'string',
          description: 'Input file',
        },
        output: {
          type: 'string',
          description: 'Output file path',
        },
      },
      required: ["input"],
    },
  },
  {
    name: 'convert_to_turtle',
    description: 'Convert RDF to Turtle format',
    inputSchema: {
      type: 'object',
      properties: {
        input: {
          type: 'string',
          description: 'Input file',
        },
        output: {
          type: 'string',
          description: 'Output file path',
        },
      },
      required: ["input"],
    },
  },
  {
    name: 'daemon_cluster',
    description: 'Manage daemon cluster',
    inputSchema: {
      type: 'object',
      properties: {
        include-metrics: {
          type: 'boolean',
          description: 'Include metrics',
        },
        json: {
          type: 'boolean',
          description: 'Output as JSON',
        },
      },
      required: [],
    },
  },
  {
    name: 'daemon_config',
    description: 'View daemon configuration',
    inputSchema: {
      type: 'object',
      properties: {
        json: {
          type: 'boolean',
          description: 'Output as JSON',
        },
      },
      required: [],
    },
  },
  {
    name: 'daemon_list',
    description: 'List all running daemon instances',
    inputSchema: {
      type: 'object',
      properties: {
        include-metadata: {
          type: 'boolean',
          description: 'Include metadata',
        },
        json: {
          type: 'boolean',
          description: 'Output as JSON',
        },
      },
      required: [],
    },
  },
  {
    name: 'daemon_logs',
    description: 'View daemon logs',
    inputSchema: {
      type: 'object',
      properties: {
        filter: {
          type: 'string',
          description: 'Log filter expression',
        },
        follow: {
          type: 'boolean',
          description: 'Follow log output',
        },
        json: {
          type: 'boolean',
          description: 'Output as JSON',
        },
        max-lines: {
          type: 'number',
          description: 'Maximum lines to display',
        },
      },
      required: [],
    },
  },
  {
    name: 'daemon_run',
    description: 'Run an operation on the daemon',
    inputSchema: {
      type: 'object',
      properties: {
        json: {
          type: 'boolean',
          description: 'Output as JSON',
        },
        operation: {
          type: 'string',
          description: 'Operation name',
        },
        payload: {
          type: 'string',
          description: 'Operation payload',
        },
        timeout: {
          type: 'number',
          description: 'Operation timeout in seconds',
        },
      },
      required: ["operation"],
    },
  },
  {
    name: 'daemon_schedule',
    description: 'Schedule an operation on the daemon',
    inputSchema: {
      type: 'object',
      properties: {
        json: {
          type: 'boolean',
          description: 'Output as JSON',
        },
        operation: {
          type: 'string',
          description: 'Operation name',
        },
        payload: {
          type: 'string',
          description: 'Operation payload',
        },
        trigger: {
          type: 'string',
          description: 'Trigger cron expression',
        },
      },
      required: ["operation", "trigger"],
    },
  },
  {
    name: 'daemon_status',
    description: 'Check daemon status and health',
    inputSchema: {
      type: 'object',
      properties: {
        include-metrics: {
          type: 'boolean',
          description: 'Include performance metrics',
        },
        json: {
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
        file: {
          type: 'string',
          description: 'Output file path',
          default: '<name>.nq',
        },
        name: {
          type: 'string',
          description: 'Graph name',
        },
      },
      required: ["name"],
    },
  },
  {
    name: 'graph_dump',
    description: 'Dump an RDF graph to a file',
    inputSchema: {
      type: 'object',
      properties: {
        file: {
          type: 'string',
          description: 'Graph file to dump',
        },
        format: {
          type: 'string',
          description: 'Output format',
          enum: ["jsonld", "ntriples", "turtle"],
          default: 'turtle',
        },
        output: {
          type: 'string',
          description: 'Output file path',
        },
      },
      required: ["file", "output"],
    },
  },
  {
    name: 'graph_load',
    description: 'Load RDF data into a graph from a file',
    inputSchema: {
      type: 'object',
      properties: {
        file: {
          type: 'string',
          description: 'RDF file to load',
        },
        format: {
          type: 'string',
          description: 'RDF format (turtle, ntriples, jsonld, trig)',
        },
        graph: {
          type: 'string',
          description: 'Target graph IRI',
        },
      },
      required: ["file"],
    },
  },
  {
    name: 'graph_query',
    description: 'Execute SPARQL query on a graph file',
    inputSchema: {
      type: 'object',
      properties: {
        file: {
          type: 'string',
          description: 'Graph file to query',
        },
        format: {
          type: 'string',
          description: 'Output format',
          enum: ["json", "table", "turtle"],
          default: 'json',
        },
        query: {
          type: 'string',
          description: 'SPARQL query string',
        },
      },
      required: ["file", "query"],
    },
  },
  {
    name: 'graph_stats',
    description: 'Get statistics about an RDF graph',
    inputSchema: {
      type: 'object',
      properties: {
        file: {
          type: 'string',
          description: 'Graph file',
        },
      },
      required: ["file"],
    },
  },
  {
    name: 'hooks_define',
    description: 'Define RDF hooks configuration',
    inputSchema: {
      type: 'object',
      properties: {
        config: {
          type: 'string',
          description: 'Configuration file to create',
        },
        output: {
          type: 'string',
          description: 'Output file path',
        },
        validate: {
          type: 'boolean',
          description: 'Validate configuration',
        },
      },
      required: ["config"],
    },
  },
  {
    name: 'hooks_evaluate_condition',
    description: 'Evaluate a hook condition against a store',
    inputSchema: {
      type: 'object',
      properties: {
        condition: {
          type: 'string',
          description: 'Condition expression',
        },
        config: {
          type: 'string',
          description: 'Configuration file',
        },
        store: {
          type: 'string',
          description: 'Store file',
        },
      },
      required: ["condition", "config", "store"],
    },
  },
  {
    name: 'hooks_execute',
    description: 'Execute registered RDF hooks on a store',
    inputSchema: {
      type: 'object',
      properties: {
        config: {
          type: 'string',
          description: 'Hooks configuration file',
        },
        output: {
          type: 'string',
          description: 'Output file path',
        },
        show-receipts: {
          type: 'boolean',
          description: 'Display execution receipts',
        },
        store: {
          type: 'string',
          description: 'Store file or identifier',
        },
      },
      required: ["config", "store"],
    },
  },
  {
    name: 'hooks_list_conditions',
    description: 'List all available hook conditions',
    inputSchema: {
      type: 'object',
      properties: {
      },
      required: [],
    },
  },
  {
    name: 'hooks_receipts',
    description: 'View execution receipts from hook runs',
    inputSchema: {
      type: 'object',
      properties: {
        file: {
          type: 'string',
          description: 'Receipts file',
        },
        format: {
          type: 'string',
          description: 'Output format',
          default: 'table',
        },
        verify: {
          type: 'boolean',
          description: 'Verify receipt signatures',
        },
      },
      required: ["file"],
    },
  },
  {
    name: 'query',
    description: 'Execute SPARQL query on a data file',
    inputSchema: {
      type: 'object',
      properties: {
        file: {
          type: 'string',
          description: 'Data file',
        },
        format: {
          type: 'string',
          description: 'Output format',
          default: 'json',
        },
        query: {
          type: 'string',
          description: 'SPARQL query',
        },
      },
      required: ["file", "query"],
    },
  },
  {
    name: 'query_file',
    description: 'Execute SPARQL query from a file',
    inputSchema: {
      type: 'object',
      properties: {
        data: {
          type: 'string',
          description: 'Data file',
        },
        format: {
          type: 'string',
          description: 'Output format',
        },
        query: {
          type: 'string',
          description: 'Query file',
        },
      },
      required: ["data", "query"],
    },
  },
  {
    name: 'sync',
    description: 'Synchronize and generate code from RDF ontology',
    inputSchema: {
      type: 'object',
      properties: {
        config: {
          type: 'string',
          description: 'Path to .unrdf.toml config',
        },
        dry-run: {
          type: 'boolean',
          description: 'Preview without writing files',
        },
        force: {
          type: 'boolean',
          description: 'Force regeneration',
        },
        output: {
          type: 'string',
          description: 'Output format (text or json)',
          default: 'text',
        },
        rule: {
          type: 'string',
          description: 'Run only specified rule',
        },
        verbose: {
          type: 'boolean',
          description: 'Verbose output',
        },
      },
      required: [],
    },
  },
  {
    name: 'template_extract',
    description: 'Extract data from RDF using template patterns',
    inputSchema: {
      type: 'object',
      properties: {
        file: {
          type: 'string',
          description: 'RDF file',
        },
        format: {
          type: 'string',
          description: 'Output format',
          default: 'json',
        },
        subject: {
          type: 'string',
          description: 'Subject IRI filter',
        },
      },
      required: ["file"],
    },
  },
  {
    name: 'template_generate',
    description: 'Generate files from a Nunjucks template',
    inputSchema: {
      type: 'object',
      properties: {
        batch: {
          type: 'boolean',
          description: 'Batch mode',
        },
        classUri: {
          type: 'string',
          description: 'Class URI filter',
        },
        dryRun: {
          type: 'boolean',
          description: 'Preview without writing',
        },
        file: {
          type: 'string',
          description: 'Data file (RDF)',
        },
        force: {
          type: 'boolean',
          description: 'Force overwrite',
        },
        outputDir: {
          type: 'string',
          description: 'Output directory',
        },
        sparql: {
          type: 'string',
          description: 'SPARQL query',
        },
        subject: {
          type: 'string',
          description: 'Subject filter',
        },
        template: {
          type: 'string',
          description: 'Template file path',
        },
      },
      required: ["template"],
    },
  },
  {
    name: 'template_list',
    description: 'List available templates',
    inputSchema: {
      type: 'object',
      properties: {
        dir: {
          type: 'string',
          description: 'Template directory',
        },
        format: {
          type: 'string',
          description: 'Output format',
          default: 'table',
        },
      },
      required: [],
    },
  },
  {
    name: 'template_query',
    description: 'Query template variables and context',
    inputSchema: {
      type: 'object',
      properties: {
        file: {
          type: 'string',
          description: 'Template file',
        },
        format: {
          type: 'string',
          description: 'Output format',
          default: 'json',
        },
        sparql: {
          type: 'string',
          description: 'SPARQL query',
        },
      },
      required: ["file"],
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
