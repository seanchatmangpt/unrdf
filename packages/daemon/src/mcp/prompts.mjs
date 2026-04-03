/**
 * @file MCP Prompts
 * @module @unrdf/daemon/mcp/prompts
 * @description Prompt definitions for the Model Context Protocol
 */

/**
 * Prompt handler functions
 */
const promptHandlers = {
  sparql_builder: async (args) => ({
    messages: [
      {
        role: 'user',
        content: `Build a SPARQL query for: ${args.task}\nTarget graph: ${args.graph || 'default'}`,
      },
    ],
  }),

  graph_analysis: async (args) => ({
    messages: [
      {
        role: 'user',
        content: `Analyze ${args.analysis_type} of graph: ${args.graph_iri}`,
      },
    ],
  }),

  data_transform: async (args) => ({
    messages: [
      {
        role: 'user',
        content: `Transform data from ${args.source_format} to ${args.target_format}${args.mapping ? '\nWith mapping: ' + args.mapping : ''}`,
      },
    ],
  }),

  ontology_doc: async (args) => ({
    messages: [
      {
        role: 'user',
        content: `Generate documentation for ontology: ${args.ontology_iri}${args.detail_level ? '\nDetail level: ' + args.detail_level : ''}`,
      },
    ],
  }),
};

/**
 * SPARQL query builder prompt
 * @returns {object} Prompt definition
 */
export function sparqlBuilderPrompt() {
  return {
    name: 'sparql_builder',
    description: 'Build SPARQL queries for RDF graph exploration',
    arguments: [
      {
        name: 'task',
        description: 'The query task to accomplish',
        required: true,
      },
      {
        name: 'graph',
        description: 'Target RDF graph IRI',
        required: false,
      },
    ],
  };
}

/**
 * Graph analysis prompt
 * @returns {object} Prompt definition
 */
export function graphAnalysisPrompt() {
  return {
    name: 'graph_analysis',
    description: 'Analyze RDF graph structure and content',
    arguments: [
      {
        name: 'analysis_type',
        description: 'Type of analysis: structure, patterns, quality, coverage',
        required: true,
      },
      {
        name: 'graph_iri',
        description: 'IRI of the graph to analyze',
        required: true,
      },
    ],
  };
}

/**
 * Data transformation prompt
 * @returns {object} Prompt definition
 */
export function dataTransformPrompt() {
  return {
    name: 'data_transform',
    description: 'Transform RDF data between formats',
    arguments: [
      {
        name: 'source_format',
        description: 'Source RDF format',
        required: true,
      },
      {
        name: 'target_format',
        description: 'Target RDF format',
        required: true,
      },
      {
        name: 'mapping',
        description: 'Optional transformation mapping',
        required: false,
      },
    ],
  };
}

/**
 * Ontology documentation prompt
 * @returns {object} Prompt definition
 */
export function ontologyDocPrompt() {
  return {
    name: 'ontology_doc',
    description: 'Generate documentation for RDF ontologies',
    arguments: [
      {
        name: 'ontology_iri',
        description: 'IRI of the ontology',
        required: true,
      },
      {
        name: 'detail_level',
        description: 'Detail level: brief, standard, comprehensive',
        required: false,
      },
    ],
  };
}

/**
 * All prompts available
 */
export const mcpPrompts = [
  sparqlBuilderPrompt(),
  graphAnalysisPrompt(),
  dataTransformPrompt(),
  ontologyDocPrompt(),
];

/**
 * Register all prompts with the MCP server
 * @param {Server} server - MCP server instance
 * @param {object} options - Configuration options
 */
export function registerPrompts(server, options = {}) {
  // Register list prompts handler
  server.setRequestHandler({ method: 'prompts/list' }, async () => ({
    prompts: mcpPrompts,
  }));

  // Register get prompt handler
  server.setRequestHandler({ method: 'prompts/get' }, async (request) => {
    const handler = promptHandlers[request.params.name];
    if (!handler) {
      throw new Error(`Unknown prompt: ${request.params.name}`);
    }
    const result = await handler(request.params.arguments || {});
    return result;
  });
}
