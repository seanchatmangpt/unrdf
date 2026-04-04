/**
 * @file CLI Registry
 * @module @unrdf/daemon/mcp/cli-registry
 * @description Maps MCP tool names to their unrdf CLI invocation paths
 * @generated 2026-04-03 21:03:03 from cli-commands.ttl
 *
 * DO NOT EDIT — regenerate with: unrdf sync --rule mcp-cli-registry
 */

/**
 * Registry mapping each MCP tool name to its CLI path string.
 * Usage: spawn `unrdf ${cliRegistry[toolName]} [--arg value ...]`
 * @type {Record<string, string>}
 */
export const cliRegistry = {
  'context_add': 'context add',
  'context_create': 'context create',
  'context_list': 'context list',
  'context_remove': 'context remove',
  'convert': 'convert',
  'daemon_cluster': 'daemon cluster',
  'daemon_config': 'daemon config',
  'daemon_list': 'daemon list',
  'daemon_logs': 'daemon logs',
  'daemon_run': 'daemon run',
  'daemon_schedule': 'daemon schedule',
  'daemon_status': 'daemon status',
  'graph_create': 'graph create',
  'graph_dump': 'graph dump',
  'graph_load': 'graph load',
  'graph_query': 'graph query',
  'graph_stats': 'graph stats',
  'hooks_define': 'hooks define',
  'hooks_evaluate_condition': 'hooks evaluate-condition',
  'hooks_execute': 'hooks execute',
  'hooks_list_conditions': 'hooks list-conditions',
  'hooks_receipts': 'hooks receipts',
  'mcp_inspect': 'mcp inspect',
  'mcp_start': 'mcp start',
  'mcp_status': 'mcp status',
  'mcp_stop': 'mcp stop',
  'query': 'query',
  'query_file': 'query-file',
  'sync': 'sync',
  'template_extract': 'template extract',
  'template_generate': 'template generate',
  'template_list': 'template list',
  'template_query': 'template query',
  'to_json': 'to-json',
  'to_ntriples': 'to-ntriples',
  'to_turtle': 'to-turtle',
};

/**
 * Tool metadata for documentation and introspection
 * @type {Array<{name: string, cliPath: string, description: string}>}
 */
export const cliRegistryMetadata = [
  {
    name: 'context_add',
    cliPath: 'context add',
    description: 'Add prefix to context',
  },
  {
    name: 'context_create',
    cliPath: 'context create',
    description: 'Create a new JSON-LD context',
  },
  {
    name: 'context_list',
    cliPath: 'context list',
    description: 'List context prefixes',
  },
  {
    name: 'context_remove',
    cliPath: 'context remove',
    description: 'Remove prefix from context',
  },
  {
    name: 'convert',
    cliPath: 'convert',
    description: 'Convert RDF between formats',
  },
  {
    name: 'daemon_cluster',
    cliPath: 'daemon cluster',
    description: 'Show Raft cluster status and members',
  },
  {
    name: 'daemon_config',
    cliPath: 'daemon config',
    description: 'Display current daemon configuration',
  },
  {
    name: 'daemon_list',
    cliPath: 'daemon list',
    description: 'List all configured operations',
  },
  {
    name: 'daemon_logs',
    cliPath: 'daemon logs',
    description: 'View operation logs with filtering',
  },
  {
    name: 'daemon_run',
    cliPath: 'daemon run',
    description: 'Execute operation immediately',
  },
  {
    name: 'daemon_schedule',
    cliPath: 'daemon schedule',
    description: 'Add scheduled trigger to operation',
  },
  {
    name: 'daemon_status',
    cliPath: 'daemon status',
    description: 'Show daemon health and metrics',
  },
  {
    name: 'graph_create',
    cliPath: 'graph create',
    description: 'Create a new RDF graph',
  },
  {
    name: 'graph_dump',
    cliPath: 'graph dump',
    description: 'Export graph data to file',
  },
  {
    name: 'graph_load',
    cliPath: 'graph load',
    description: 'Load RDF data into a graph',
  },
  {
    name: 'graph_query',
    cliPath: 'graph query',
    description: 'Execute SPARQL query on graph',
  },
  {
    name: 'graph_stats',
    cliPath: 'graph stats',
    description: 'Show graph statistics',
  },
  {
    name: 'hooks_define',
    cliPath: 'hooks define',
    description: 'Define hooks from config file',
  },
  {
    name: 'hooks_evaluate_condition',
    cliPath: 'hooks evaluate-condition',
    description: 'Evaluate a single condition against a store',
  },
  {
    name: 'hooks_execute',
    cliPath: 'hooks execute',
    description: 'Execute hooks against an RDF store',
  },
  {
    name: 'hooks_list_conditions',
    cliPath: 'hooks list-conditions',
    description: 'List available condition kinds',
  },
  {
    name: 'hooks_receipts',
    cliPath: 'hooks receipts',
    description: 'Display receipt chain from hook execution results',
  },
  {
    name: 'mcp_inspect',
    cliPath: 'mcp inspect',
    description: 'List all exposed tools, resources, and prompts',
  },
  {
    name: 'mcp_start',
    cliPath: 'mcp start',
    description: 'Start the MCP server',
  },
  {
    name: 'mcp_status',
    cliPath: 'mcp status',
    description: 'Show if MCP server is running',
  },
  {
    name: 'mcp_stop',
    cliPath: 'mcp stop',
    description: 'Stop the running MCP server',
  },
  {
    name: 'query',
    cliPath: 'query',
    description: 'Execute SPARQL query',
  },
  {
    name: 'query_file',
    cliPath: 'query-file',
    description: 'Execute SPARQL query from file',
  },
  {
    name: 'sync',
    cliPath: 'sync',
    description: 'Generate synchronized code artifacts from RDF ontology',
  },
  {
    name: 'template_extract',
    cliPath: 'template extract',
    description: 'Extract properties for a subject as JSON (template debugging)',
  },
  {
    name: 'template_generate',
    cliPath: 'template generate',
    description: 'Generate files from RDF + Nunjucks template (`--template`). RDF path can be positional or `rdf:` in template frontmatter.',
  },
  {
    name: 'template_list',
    cliPath: 'template list',
    description: 'List discovered .njk templates (default: bundled sync templates)',
  },
  {
    name: 'template_query',
    cliPath: 'template query',
    description: 'Run SPARQL SELECT on an RDF file and print template-style context. For CONSTRUCT/ASK/DESCRIBE use `unrdf query`.',
  },
  {
    name: 'to_json',
    cliPath: 'to-json',
    description: 'Convert RDF to JSON representation',
  },
  {
    name: 'to_ntriples',
    cliPath: 'to-ntriples',
    description: 'Convert RDF to N-Triples format',
  },
  {
    name: 'to_turtle',
    cliPath: 'to-turtle',
    description: 'Convert RDF to Turtle format',
  },
];

/**
 * Export as default for convenience
 */
export default cliRegistry;
