/**
 * KGEN RDF/SPARQL Filters - P0 Essential Semantic Web Processing
 *
 * Mock implementations for testing and London BDD approach
 * These stubs provide deterministic behavior for template generation
 */

/**
 * Mock RDF prefixes for expansion
 */
const DEFAULT_PREFIXES = {
  'rdf': 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  'rdfs': 'http://www.w3.org/2000/01/rdf-schema#',
  'owl': 'http://www.w3.org/2002/07/owl#',
  'xsd': 'http://www.w3.org/2001/XMLSchema#',
  'dc': 'http://purl.org/dc/elements/1.1/',
  'foaf': 'http://xmlns.com/foaf/0.1/',
  'skos': 'http://www.w3.org/2004/02/skos/core#',
  'ex': 'http://example.org/',
  'schema': 'https://schema.org/'
};

/**
 * Mock SPARQL dataset for deterministic results
 */
const MOCK_SPARQL_RESULTS = {
  'SELECT ?s ?p ?o WHERE { ?s ?p ?o }': [
    { s: 'ex:Subject1', p: 'rdf:type', o: 'ex:Class1' },
    { s: 'ex:Subject2', p: 'rdfs:label', o: '"Example Label"' }
  ],
  'SELECT * WHERE { ?s rdf:type ?type }': [
    { s: 'ex:Instance1', type: 'ex:Person' },
    { s: 'ex:Instance2', type: 'ex:Organization' }
  ]
};

/**
 * Expand CURIE (Compact URI) to full URI
 * @param {string} curie - CURIE in format 'prefix:localName'
 * @param {Object} prefixes - Custom prefix mappings
 * @returns {string} Expanded URI
 */
export const expand = (curie, prefixes = {}) => {
  if (!curie || typeof curie !== 'string') return '';

  const allPrefixes = { ...DEFAULT_PREFIXES, ...prefixes };
  const [prefix, localName] = curie.split(':', 2);

  if (localName === undefined) return curie; // No prefix found
  if (!allPrefixes[prefix]) return curie; // Unknown prefix

  return allPrefixes[prefix] + localName;
};

/**
 * Contract full URI to CURIE if possible
 * @param {string} uri - Full URI
 * @param {Object} prefixes - Custom prefix mappings
 * @returns {string} CURIE or original URI
 */
export const contract = (uri, prefixes = {}) => {
  if (!uri || typeof uri !== 'string') return '';

  const allPrefixes = { ...DEFAULT_PREFIXES, ...prefixes };

  for (const [prefix, namespace] of Object.entries(allPrefixes)) {
    if (uri.startsWith(namespace)) {
      const localName = uri.substring(namespace.length);
      return `${prefix}:${localName}`;
    }
  }

  return uri;
};

/**
 * Execute mock SPARQL query (deterministic results)
 * @param {string} query - SPARQL query string
 * @param {any} dataset - Mock dataset (ignored in stub)
 * @returns {Array} Mock query results
 */
export const sparql = async (query, dataset = null) => {
  if (!query || typeof query !== 'string') return [];

  // Return mock results for known queries
  const normalizedQuery = query.trim().replace(/\s+/g, ' ');

  if (MOCK_SPARQL_RESULTS[normalizedQuery]) {
    return MOCK_SPARQL_RESULTS[normalizedQuery];
  }

  // Generate deterministic mock results based on query hash
  const queryHash = query.split('').reduce((hash, char) => {
    hash = ((hash << 5) - hash) + char.charCodeAt(0);
    return hash & hash;
  }, 0);

  const numResults = Math.abs(queryHash) % 5 + 1; // 1-5 results
  const results = [];

  for (let i = 0; i < numResults; i++) {
    results.push({
      s: `ex:MockSubject${i + 1}`,
      p: `ex:mockProperty${i + 1}`,
      o: `"Mock Object ${i + 1}"`
    });
  }

  return results;
};

/**
 * Create RDF literal with optional language tag or datatype
 * @param {any} value - Literal value
 * @param {string} langOrType - Language tag (e.g., 'en') or datatype CURIE
 * @returns {string} RDF literal string
 */
export const rdfLiteral = (value, langOrType = null) => {
  if (value === null || value === undefined) return '""';

  const literal = `"${String(value).replace(/"/g, '\\"')}"`;

  if (!langOrType) return literal;

  // Check if it's a language tag (2-3 letter code)
  if (/^[a-z]{2,3}(-[A-Z]{2})?$/.test(langOrType)) {
    return `${literal}@${langOrType}`;
  }

  // Treat as datatype
  return `${literal}^^${langOrType}`;
};

/**
 * Create RDF resource (URI reference)
 * @param {string} uri - URI string
 * @returns {string} URI wrapped in angle brackets
 */
export const rdfResource = (uri) => {
  if (!uri || typeof uri !== 'string') return '<>';

  // If already a CURIE or angle-bracketed URI, return as-is
  if (uri.includes(':') && !uri.startsWith('http')) return uri;
  if (uri.startsWith('<') && uri.endsWith('>')) return uri;

  return `<${uri}>`;
};

/**
 * Escape string for Turtle/N3 syntax
 * @param {string} str - String to escape
 * @returns {string} Turtle-escaped string
 */
export const turtleEscape = (str) => {
  if (!str || typeof str !== 'string') return '';

  return str
    .replace(/\\/g, '\\\\')
    .replace(/"/g, '\\"')
    .replace(/\n/g, '\\n')
    .replace(/\r/g, '\\r')
    .replace(/\t/g, '\\t');
};

/**
 * Create SPARQL variable
 * @param {string} name - Variable name
 * @returns {string} SPARQL variable with ? prefix
 */
export const sparqlVar = (name) => {
  if (!name || typeof name !== 'string') return '?var';

  // Remove any existing ? or $ prefix
  const cleanName = name.replace(/^[?$]/, '');

  // Ensure valid SPARQL variable name (alphanumeric + underscore)
  const validName = cleanName.replace(/[^a-zA-Z0-9_]/g, '_');

  return `?${validName}`;
};

/**
 * Create RDF list (collection)
 * @param {Array} items - Array items
 * @returns {string} RDF list syntax
 */
export const rdfList = (items) => {
  if (!Array.isArray(items) || items.length === 0) {
    return 'rdf:nil';
  }

  return `( ${items.join(' ')} )`;
};

/**
 * Create blank node identifier
 * @param {string} id - Optional identifier
 * @returns {string} Blank node with _: prefix
 */
export const blankNode = (id = null) => {
  if (id && typeof id === 'string') {
    const cleanId = id.replace(/[^a-zA-Z0-9_]/g, '_');
    return `_:${cleanId}`;
  }

  // Generate deterministic blank node ID
  const timestamp = new Date('2024-01-01T00:00:00.000Z').getTime();
  return `_:blank${timestamp % 10000}`;
};

/**
 * Add RDF datatype to literal
 * @param {any} value - Value to type
 * @param {string} datatype - XSD datatype (without prefix)
 * @returns {string} Typed literal
 */
export const rdfDatatype = (value, datatype = 'string') => {
  if (value === null || value === undefined) return '""';

  const literal = `"${String(value)}"`;
  return `${literal}^^xsd:${datatype}`;
};

/**
 * Mock SHACL validation (always passes in stub)
 * @param {any} data - Data to validate
 * @param {any} shape - SHACL shape
 * @returns {Object} Mock validation result
 */
export const shaclValidate = (data, shape) => {
  return {
    conforms: true,
    results: [],
    message: 'Mock SHACL validation passed'
  };
};

/**
 * Mock reasoning/inference (no-op in stub)
 * @param {any} graph - RDF graph
 * @param {string} reasoner - Reasoner type
 * @returns {any} Original graph (no inference applied)
 */
export const infer = (graph, reasoner = 'rdfs') => {
  return graph; // No-op for stub
};

// Collection of all RDF filters for easy import
export const rdfFilters = {
  expand,
  contract,
  sparql,
  rdfLiteral,
  rdfResource,
  turtleEscape,
  sparqlVar,
  rdfList,
  blankNode,
  rdfDatatype,
  shaclValidate,
  infer
};

export default rdfFilters;