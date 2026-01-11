/**
 * @file Minimal RDF Template Example
 * @module examples/rdf-kgn/01-minimal-rdf-template
 * @description Simplest example demonstrating RDF generation with template-like patterns
 *
 * Time Estimate: 5-10 minutes
 * Difficulty: Beginner
 * Prerequisites: Basic understanding of RDF triples
 */

import { createStore, namedNode, literal, quad, COMMON_PREFIXES } from '../../packages/core/src/index.mjs';

/**
 * Simple template function for generating RDF triples
 * @param {Object} data - Data to convert to RDF
 * @returns {Array} Array of quads
 */
function generatePersonTriples(data) {
  const ex = (name) => namedNode(`http://example.org/${name}`);
  const schema = (name) => namedNode(`http://schema.org/${name}`);
  const rdfType = namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');

  const subject = ex(data.id);

  return [
    quad(subject, rdfType, schema('Person')),
    quad(subject, schema('name'), literal(data.name)),
    quad(subject, schema('email'), literal(data.email)),
    quad(subject, schema('jobTitle'), literal(data.role)),
  ];
}

/**
 * Minimal RDF template demonstrating basic triple generation
 * @returns {Promise<void>}
 */
async function minimalRdfTemplateDemo() {
  console.log('=== Minimal RDF Template Example ===\n');

  // Create RDF store
  const store = createStore();

  // Define person data (this would come from a template in real use)
  const personData = {
    id: 'john-doe',
    name: 'John Doe',
    email: 'john.doe@example.com',
    role: 'Software Engineer',
  };

  // Generate triples using template function
  const triples = generatePersonTriples(personData);

  // Add to store
  for (const triple of triples) {
    store.add(triple);
  }

  console.log('Generated RDF triples:');
  console.log('='.repeat(60));

  // Display triples in N-Triples format
  const quads = store.getQuads();
  for (const q of quads) {
    console.log(`<${q.subject.value}> <${q.predicate.value}> ${formatObject(q.object)} .`);
  }

  console.log('='.repeat(60));
  console.log(`\n✓ Generated ${quads.length} triples`);
  console.log('✓ Template rendered successfully');
}

/**
 * Format RDF object for display
 * @param {Object} obj - RDF term
 * @returns {string} Formatted string
 */
function formatObject(obj) {
  if (obj.termType === 'Literal') {
    return `"${obj.value}"`;
  } else if (obj.termType === 'NamedNode') {
    return `<${obj.value}>`;
  }
  return obj.value;
}

// Execute demo
try {
  await minimalRdfTemplateDemo();
  console.log('\n✓ Example completed successfully');
  process.exit(0);
} catch (error) {
  console.error('\n✗ Example failed:', error.message);
  console.error(error.stack);
  process.exit(1);
}
