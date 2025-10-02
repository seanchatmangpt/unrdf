/**
 * UNRDF Configuration
 */

export default {
  // Base IRI for the project
  baseIRI: 'http://example.org/',

  // Namespace prefixes
  prefixes: {
    'ex': 'http://example.org/',
    'foaf': 'http://xmlns.com/foaf/0.1/',
    'schema': 'https://schema.org/',
    'rdf': 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
    'rdfs': 'http://www.w3.org/2000/01/rdf-schema#',
    'owl': 'http://www.w3.org/2002/07/owl#',
    'xsd': 'http://www.w3.org/2001/XMLSchema#'
  },

  // Validation settings
  validation: {
    strict: true,
    validateOnLoad: true
  },

  // Knowledge Hooks configuration
  hooks: {
    enabled: true,
    basePath: './src/hooks',
    autoLoad: true
  },

  // Storage settings
  storage: {
    basePath: './.unrdf',
    receipts: true,
    baselines: true
  }
};
