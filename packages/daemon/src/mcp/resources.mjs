/**
 * @file MCP Resources
 * @module @unrdf/daemon/mcp/resources
 * @description Resource definitions and registration for the Model Context Protocol
 */

/**
 * Resource handler functions
 */
const resourceHandlers = {
  'sparql://endpoints/config': async () => ({
    uri: 'sparql://endpoints/config',
    mimeType: 'application/json',
    contents: JSON.stringify({
      endpoints: [
        { name: 'default', url: 'http://localhost:8080/sparql' },
        { name: 'public', url: 'http://dbpedia.org/sparql' },
      ],
    }),
  }),

  'rdf://ontologies/catalog': async () => ({
    uri: 'rdf://ontologies/catalog',
    mimeType: 'application/json',
    contents: JSON.stringify({
      ontologies: [
        { name: 'SKOS', iri: 'http://www.w3.org/2004/02/skos/core#' },
        { name: 'FOAF', iri: 'http://xmlns.com/foaf/0.1/' },
      ],
    }),
  }),

  'graphs://metadata': async () => ({
    uri: 'graphs://metadata',
    mimeType: 'application/json',
    contents: JSON.stringify({
      graphs: [
        { iri: 'http://example.org/default', triples: 5000 },
        { iri: 'http://example.org/public', triples: 10000 },
      ],
    }),
  }),

  'queries://templates': async () => ({
    uri: 'queries://templates',
    mimeType: 'application/json',
    contents: JSON.stringify({
      templates: [
        {
          name: 'all_subjects',
          query: 'SELECT DISTINCT ?s WHERE { ?s ?p ?o }',
        },
        {
          name: 'all_predicates',
          query: 'SELECT DISTINCT ?p WHERE { ?s ?p ?o }',
        },
      ],
    }),
  }),
};

/**
 * Get SPARQL endpoint configuration
 * @returns {object} Resource definition
 */
function endpointConfigResource() {
  return {
    uri: 'sparql://endpoints/config',
    name: 'SPARQL Endpoints Configuration',
    description: 'Configuration for SPARQL endpoints',
    mimeType: 'application/json',
  };
}

/**
 * Get RDF ontology catalog
 * @returns {object} Resource definition
 */
function ontologyCatalogResource() {
  return {
    uri: 'rdf://ontologies/catalog',
    name: 'RDF Ontology Catalog',
    description: 'Catalog of available RDF ontologies',
    mimeType: 'application/json',
  };
}

/**
 * Get graph metadata
 * @returns {object} Resource definition
 */
function graphMetadataResource() {
  return {
    uri: 'graphs://metadata',
    name: 'Graph Metadata',
    description: 'Metadata about available RDF graphs',
    mimeType: 'application/json',
  };
}

/**
 * Get query templates
 * @returns {object} Resource definition
 */
function queryTemplatesResource() {
  return {
    uri: 'queries://templates',
    name: 'SPARQL Query Templates',
    description: 'Reusable SPARQL query templates',
    mimeType: 'application/json',
  };
}

/**
 * All resources available
 */
const mcpResources = [
  endpointConfigResource(),
  ontologyCatalogResource(),
  graphMetadataResource(),
  queryTemplatesResource(),
];

/**
 * Register all resources with the MCP server
 * @param {Server} server - MCP server instance
 * @param {object} options - Configuration options
 */
export function registerResources(server, options = {}) {
  // Register list resources handler
  server.setRequestHandler({ method: 'resources/list' }, async () => ({
    resources: mcpResources,
  }));

  // Register read resource handler
  server.setRequestHandler({ method: 'resources/read' }, async (request) => {
    const handler = resourceHandlers[request.params.uri];
    if (!handler) {
      throw new Error(`Unknown resource: ${request.params.uri}`);
    }
    const result = await handler();
    return {
      contents: [
        {
          uri: result.uri,
          mimeType: result.mimeType,
          text: result.contents,
        },
      ],
    };
  });
}

// Export resource definitions
export { mcpResources };
