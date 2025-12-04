/**
 * @file Basic RDF Store Operations Example
 * @description Demonstrates core RDF store functionality: create, add, query, remove, export
 */

import { createStore, dataFactory } from '@unrdf/oxigraph';

const { namedNode, literal, quad } = dataFactory;

/**
 * Creates a new RDF store with sample data
 * @returns {Store} Populated RDF store
 */
export function createSampleStore() {
  const store = createStore();

  // Add some sample triples about people
  store.addQuad(
    namedNode('http://example.org/alice'),
    namedNode('http://xmlns.com/foaf/0.1/name'),
    literal('Alice')
  );

  store.addQuad(
    namedNode('http://example.org/alice'),
    namedNode('http://xmlns.com/foaf/0.1/age'),
    literal('30', namedNode('http://www.w3.org/2001/XMLSchema#integer'))
  );

  store.addQuad(
    namedNode('http://example.org/bob'),
    namedNode('http://xmlns.com/foaf/0.1/name'),
    literal('Bob')
  );

  store.addQuad(
    namedNode('http://example.org/alice'),
    namedNode('http://xmlns.com/foaf/0.1/knows'),
    namedNode('http://example.org/bob')
  );

  return store;
}

/**
 * Queries the store for all quads matching a pattern
 * @param {Store} store - RDF store to query
 * @param {object} pattern - Query pattern { subject, predicate, object, graph }
 * @returns {Array} Array of matching quads
 */
export function queryStore(store, pattern = {}) {
  return store.getQuads(
    pattern.subject || null,
    pattern.predicate || null,
    pattern.object || null,
    pattern.graph || null
  );
}

/**
 * Removes quads matching a pattern from the store
 * @param {Store} store - RDF store to modify
 * @param {object} pattern - Removal pattern
 * @returns {number} Number of quads removed
 */
export function removeQuads(store, pattern = {}) {
  const toRemove = queryStore(store, pattern);
  toRemove.forEach(quad => store.removeQuad(quad));
  return toRemove.length;
}

/**
 * Exports store to N-Triples format
 * @param {Store} store - RDF store to export
 * @returns {string} N-Triples serialization
 */
export function exportToNTriples(store) {
  const quads = store.getQuads(null, null, null, null);
  return quads.map(q => {
    const subject = q.subject.termType === 'NamedNode' ? `<${q.subject.value}>` : q.subject.value;
    const predicate = `<${q.predicate.value}>`;
    const object = q.object.termType === 'NamedNode'
      ? `<${q.object.value}>`
      : q.object.termType === 'Literal'
        ? q.object.datatype && q.object.datatype.value !== 'http://www.w3.org/2001/XMLSchema#string'
          ? `"${q.object.value}"^^<${q.object.datatype.value}>`
          : `"${q.object.value}"`
        : q.object.value;
    return `${subject} ${predicate} ${object} .`;
  }).join('\n');
}

/**
 * Gets store statistics
 * @param {Store} store - RDF store to analyze
 * @returns {object} Statistics about the store
 */
export function getStoreStats(store) {
  const allQuads = store.getQuads(null, null, null, null);
  const subjects = new Set(allQuads.map(q => q.subject.value));
  const predicates = new Set(allQuads.map(q => q.predicate.value));
  const objects = new Set(allQuads.map(q => q.object.value));

  return {
    totalQuads: allQuads.length,
    uniqueSubjects: subjects.size,
    uniquePredicates: predicates.size,
    uniqueObjects: objects.size
  };
}

// Main execution
function main() {
  console.log('=== Basic RDF Store Operations ===\n');

  // 1. Create store
  console.log('1. Creating store with sample data...');
  const store = createSampleStore();
  console.log(`   Store created with ${store.size} quads\n`);

  // 2. Query all quads
  console.log('2. Querying all quads:');
  const allQuads = queryStore(store);
  allQuads.forEach(q => {
    console.log(`   ${q.subject.value} ${q.predicate.value} ${q.object.value}`);
  });
  console.log();

  // 3. Query specific pattern
  console.log('3. Querying quads about Alice:');
  const aliceQuads = queryStore(store, {
    subject: namedNode('http://example.org/alice')
  });
  console.log(`   Found ${aliceQuads.length} quads about Alice\n`);

  // 4. Get statistics
  console.log('4. Store statistics:');
  const stats = getStoreStats(store);
  console.log(`   Total quads: ${stats.totalQuads}`);
  console.log(`   Unique subjects: ${stats.uniqueSubjects}`);
  console.log(`   Unique predicates: ${stats.uniquePredicates}`);
  console.log(`   Unique objects: ${stats.uniqueObjects}\n`);

  // 5. Export to N-Triples
  console.log('5. Export to N-Triples format:');
  const ntriples = exportToNTriples(store);
  console.log(ntriples);
  console.log();

  // 6. Remove quads
  console.log('6. Removing age information:');
  const removed = removeQuads(store, {
    predicate: namedNode('http://xmlns.com/foaf/0.1/age')
  });
  console.log(`   Removed ${removed} quad(s)`);
  console.log(`   Store now has ${store.size} quads\n`);
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main();
}
