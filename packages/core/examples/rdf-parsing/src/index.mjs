/**
 * @file RDF Parsing Examples
 * @description Demonstrates parsing RDF from various formats: Turtle, N-Triples, N-Quads
 */

import { createStore, dataFactory } from '@unrdf/oxigraph';
const { namedNode, literal } = dataFactory;

/**
 * Parses Turtle format RDF
 * @param {string} turtle - Turtle formatted RDF string
 * @returns {Promise<Store>} Store with parsed triples
 */
export async function parseTurtle(turtle) {
  try {
    const store = createStore();
    store.load(turtle, { format: 'text/turtle', base_iri: 'http://example.org/' });
    return store;
  } catch (error) {
    throw new Error(`Turtle parsing failed: ${error.message}`);
  }
}

/**
 * Parses N-Triples format RDF
 * @param {string} ntriples - N-Triples formatted RDF string
 * @returns {Promise<Store>} Store with parsed triples
 */
export async function parseNTriples(ntriples) {
  try {
    const store = createStore();
    store.load(ntriples, { format: 'application/n-triples' });
    return store;
  } catch (error) {
    throw new Error(`N-Triples parsing failed: ${error.message}`);
  }
}

/**
 * Parses N-Quads format RDF (includes graph names)
 * @param {string} nquads - N-Quads formatted RDF string
 * @returns {Promise<Store>} Store with parsed quads
 */
export async function parseNQuads(nquads) {
  try {
    const store = createStore();
    store.load(nquads, { format: 'application/n-quads' });
    return store;
  } catch (error) {
    throw new Error(`N-Quads parsing failed: ${error.message}`);
  }
}

/**
 * Merges multiple stores into one
 * @param {Array<Store>} stores - Array of stores to merge
 * @returns {Store} Merged store
 */
export function mergeStores(stores) {
  const merged = createStore();

  stores.forEach(store => {
    for (const quad of store.match()) {
      merged.add(quad);
    }
  });

  return merged;
}

/**
 * Gets unique graphs from a store
 * @param {Store} store - Store to analyze
 * @returns {Array<string>} List of unique graph URIs
 */
export function getGraphs(store) {
  const graphs = new Set();

  for (const quad of store.match()) {
    if (quad.graph && quad.graph.value) {
      graphs.add(quad.graph.value);
    }
  }

  return Array.from(graphs);
}

/**
 * Canonicalizes RDF by sorting triples
 * @param {Store} store - Store to canonicalize
 * @returns {string} Canonical N-Triples representation
 */
export function canonicalize(store) {
  // Use Oxigraph's built-in N-Triples serialization
  const ntriples = store.dump({ format: 'application/n-triples' });

  // Sort lines alphabetically for canonical form
  return ntriples
    .split('\n')
    .filter(line => line.trim())
    .sort()
    .join('\n');
}

/**
 * Validates RDF syntax
 * @param {string} rdf - RDF string to validate
 * @param {string} format - Format (turtle, ntriples, nquads)
 * @returns {object} Validation result
 */
export async function validateRDF(rdf, format = 'turtle') {
  const formatMap = {
    turtle: 'text/turtle',
    ntriples: 'application/n-triples',
    nquads: 'application/n-quads'
  };

  try {
    const store = createStore();
    store.load(rdf, { format: formatMap[format], base_iri: 'http://example.org/' });

    return {
      valid: true,
      tripleCount: store.size,
      errors: []
    };
  } catch (error) {
    return {
      valid: false,
      tripleCount: 0,
      errors: [error.message]
    };
  }
}

/**
 * Converts between RDF formats
 * @param {string} input - Input RDF string
 * @param {string} inputFormat - Input format
 * @param {string} outputFormat - Output format
 * @returns {Promise<string>} Converted RDF
 */
export async function convertFormat(input, inputFormat, outputFormat) {
  let store;

  switch (inputFormat) {
    case 'turtle':
      store = await parseTurtle(input);
      break;
    case 'ntriples':
      store = await parseNTriples(input);
      break;
    case 'nquads':
      store = await parseNQuads(input);
      break;
    default:
      throw new Error(`Unsupported input format: ${inputFormat}`);
  }

  switch (outputFormat) {
    case 'ntriples':
    case 'canonical':
      return canonicalize(store);
    default:
      throw new Error(`Unsupported output format: ${outputFormat}`);
  }
}

// Sample data for examples
const SAMPLE_TURTLE = `
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:alice foaf:name "Alice" ;
         foaf:knows ex:bob ;
         foaf:age 30 .

ex:bob foaf:name "Bob" .
`;

const SAMPLE_NTRIPLES = `
<http://example.org/alice> <http://xmlns.com/foaf/0.1/name> "Alice" .
<http://example.org/alice> <http://xmlns.com/foaf/0.1/knows> <http://example.org/bob> .
<http://example.org/alice> <http://xmlns.com/foaf/0.1/age> "30"^^<http://www.w3.org/2001/XMLSchema#integer> .
<http://example.org/bob> <http://xmlns.com/foaf/0.1/name> "Bob" .
`;

const SAMPLE_NQUADS = `
<http://example.org/alice> <http://xmlns.com/foaf/0.1/name> "Alice" <http://example.org/graph1> .
<http://example.org/bob> <http://xmlns.com/foaf/0.1/name> "Bob" <http://example.org/graph2> .
`;

// Main execution
async function main() {
  console.log('=== RDF Parsing Examples ===\n');

  // 1. Parse Turtle
  console.log('1. Parsing Turtle format:');
  const turtleStore = await parseTurtle(SAMPLE_TURTLE);
  console.log(`   Parsed ${turtleStore.size} triples from Turtle\n`);

  // 2. Parse N-Triples
  console.log('2. Parsing N-Triples format:');
  const ntriplesStore = await parseNTriples(SAMPLE_NTRIPLES);
  console.log(`   Parsed ${ntriplesStore.size} triples from N-Triples\n`);

  // 3. Parse N-Quads
  console.log('3. Parsing N-Quads format (with graphs):');
  const nquadsStore = await parseNQuads(SAMPLE_NQUADS);
  console.log(`   Parsed ${nquadsStore.size} quads from N-Quads`);
  const graphs = getGraphs(nquadsStore);
  console.log(`   Found ${graphs.length} graphs: ${graphs.join(', ')}\n`);

  // 4. Merge stores
  console.log('4. Merging multiple stores:');
  const merged = mergeStores([turtleStore, nquadsStore]);
  console.log(`   Merged store contains ${merged.size} quads\n`);

  // 5. Canonicalize
  console.log('5. Canonical N-Triples output:');
  const canonical = canonicalize(turtleStore);
  console.log(canonical);
  console.log();

  // 6. Validate RDF
  console.log('6. Validating RDF syntax:');
  const valid = await validateRDF(SAMPLE_TURTLE, 'turtle');
  console.log(`   Valid: ${valid.valid}`);
  console.log(`   Triple count: ${valid.tripleCount}\n`);

  // 7. Invalid RDF
  console.log('7. Validating invalid RDF:');
  const invalid = await validateRDF('invalid rdf syntax', 'turtle');
  console.log(`   Valid: ${invalid.valid}`);
  console.log(`   Errors: ${invalid.errors.join(', ')}\n`);

  // 8. Format conversion
  console.log('8. Converting Turtle to N-Triples:');
  const converted = await convertFormat(SAMPLE_TURTLE, 'turtle', 'ntriples');
  console.log('   First 100 chars:', converted.substring(0, 100) + '...\n');
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main().catch(console.error);
}
