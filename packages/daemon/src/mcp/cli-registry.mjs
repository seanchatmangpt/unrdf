/**
 * @file CLI Registry
 * @module @unrdf/daemon/mcp/cli-registry
 * @description Maps MCP tool names to their unrdf CLI invocation paths
 * @generated 2026-04-03 15:01:06 from cli-commands.ttl
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
  'convert_to_json': 'to-json',
  'convert_to_ntriples': 'to-ntriples',
  'convert_to_turtle': 'to-turtle',
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
  'query': 'query',
  'query_file': 'query-file',
  'sync': 'sync',
  'template_extract': 'template extract',
  'template_generate': 'template generate',
  'template_list': 'template list',
  'template_query': 'template query',
};

/**
 * Tool metadata for documentation and introspection
 * @type {Array<{name: string, cliPath: string, description: string}>}
 */
export const cliRegistryMetadata = [
  {
    name: 'context_add',
    cliPath: 'context add',
    description: 'Add a prefix mapping to a context',
  },
  {
    name: 'context_create',
    cliPath: 'context create',
    description: 'Create a new JSON-LD context',
  },
  {
    name: 'context_list',
    cliPath: 'context list',
    description: 'List all prefix mappings in a context',
  },
  {
    name: 'context_remove',
    cliPath: 'context remove',
    description: 'Remove a prefix mapping from a context',
  },
  {
    name: 'convert',
    cliPath: 'convert',
    description: 'Convert RDF between formats',
  },
  {
    name: 'convert_to_json',
    cliPath: 'to-json',
    description: 'Convert RDF to JSON-LD format',
  },
  {
    name: 'convert_to_ntriples',
    cliPath: 'to-ntriples',
    description: 'Convert RDF to N-Triples format',
  },
  {
    name: 'convert_to_turtle',
    cliPath: 'to-turtle',
    description: 'Convert RDF to Turtle format',
  },
  {
    name: 'daemon_cluster',
    cliPath: 'daemon cluster',
    description: 'Manage daemon cluster',
  },
  {
    name: 'daemon_config',
    cliPath: 'daemon config',
    description: 'View daemon configuration',
  },
  {
    name: 'daemon_list',
    cliPath: 'daemon list',
    description: 'List all running daemon instances',
  },
  {
    name: 'daemon_logs',
    cliPath: 'daemon logs',
    description: 'View daemon logs',
  },
  {
    name: 'daemon_run',
    cliPath: 'daemon run',
    description: 'Run an operation on the daemon',
  },
  {
    name: 'daemon_schedule',
    cliPath: 'daemon schedule',
    description: 'Schedule an operation on the daemon',
  },
  {
    name: 'daemon_status',
    cliPath: 'daemon status',
    description: 'Check daemon status and health',
  },
  {
    name: 'graph_create',
    cliPath: 'graph create',
    description: 'Create a new RDF graph',
  },
  {
    name: 'graph_dump',
    cliPath: 'graph dump',
    description: 'Dump an RDF graph to a file',
  },
  {
    name: 'graph_load',
    cliPath: 'graph load',
    description: 'Load RDF data into a graph from a file',
  },
  {
    name: 'graph_query',
    cliPath: 'graph query',
    description: 'Execute SPARQL query on a graph file',
  },
  {
    name: 'graph_stats',
    cliPath: 'graph stats',
    description: 'Get statistics about an RDF graph',
  },
  {
    name: 'hooks_define',
    cliPath: 'hooks define',
    description: 'Define RDF hooks configuration',
  },
  {
    name: 'hooks_evaluate_condition',
    cliPath: 'hooks evaluate-condition',
    description: 'Evaluate a hook condition against a store',
  },
  {
    name: 'hooks_execute',
    cliPath: 'hooks execute',
    description: 'Execute registered RDF hooks on a store',
  },
  {
    name: 'hooks_list_conditions',
    cliPath: 'hooks list-conditions',
    description: 'List all available hook conditions',
  },
  {
    name: 'hooks_receipts',
    cliPath: 'hooks receipts',
    description: 'View execution receipts from hook runs',
  },
  {
    name: 'query',
    cliPath: 'query',
    description: 'Execute SPARQL query on a data file',
  },
  {
    name: 'query_file',
    cliPath: 'query-file',
    description: 'Execute SPARQL query from a file',
  },
  {
    name: 'sync',
    cliPath: 'sync',
    description: 'Synchronize and generate code from RDF ontology',
  },
  {
    name: 'template_extract',
    cliPath: 'template extract',
    description: 'Extract data from RDF using template patterns',
  },
  {
    name: 'template_generate',
    cliPath: 'template generate',
    description: 'Generate files from a Nunjucks template',
  },
  {
    name: 'template_list',
    cliPath: 'template list',
    description: 'List available templates',
  },
  {
    name: 'template_query',
    cliPath: 'template query',
    description: 'Query template variables and context',
  },
];

/**
 * Export as default for convenience
 */
export default cliRegistry;
