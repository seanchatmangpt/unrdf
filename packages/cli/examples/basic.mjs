/**
 * @unrdf/cli - Basic Example
 *
 * Demonstrates basic usage of the CLI package.
 */

import { createStore, addQuad, namedNode, literal, quad } from '@unrdf/core';
import { loadGraph, saveGraph } from '@unrdf/cli';
import { mkdir } from 'node:fs/promises';
import path from 'node:path';

async function main() {
  console.log('🚀 @unrdf/cli Basic Example\n');

  // Create output directory
  const outputDir = path.join(process.cwd(), 'examples-output');
  await mkdir(outputDir, { recursive: true });

  // 1. Create a graph programmatically
  console.log('1️⃣  Creating RDF graph...');
  const store = createStore();

  // Add some triples
  const alice = namedNode('http://example.org/alice');
  const bob = namedNode('http://example.org/bob');
  const knows = namedNode('http://xmlns.com/foaf/0.1/knows');
  const name = namedNode('http://xmlns.com/foaf/0.1/name');

  addQuad(store, quad(alice, name, literal('Alice')));
  addQuad(store, quad(bob, name, literal('Bob')));
  addQuad(store, quad(alice, knows, bob));

  console.log(`   Added ${store.getQuads().length} triples\n`);

  // 2. Save graph to file
  console.log('2️⃣  Saving graph to Turtle format...');
  const turtlePath = path.join(outputDir, 'example.ttl');
  await saveGraph(store, turtlePath, 'turtle');
  console.log(`   Saved to: ${turtlePath}\n`);

  // 3. Load graph from file
  console.log('3️⃣  Loading graph from file...');
  const loadedStore = await loadGraph(turtlePath);
  console.log(`   Loaded ${loadedStore.getQuads().length} triples\n`);

  // 4. Convert to N-Triples
  console.log('4️⃣  Converting to N-Triples...');
  const ntPath = path.join(outputDir, 'example.nt');
  await saveGraph(loadedStore, ntPath, 'ntriples');
  console.log(`   Saved to: ${ntPath}\n`);

  // 5. Display graph statistics
  console.log('5️⃣  Graph Statistics:');
  const quads = loadedStore.getQuads();
  const subjects = new Set(quads.map(q => q.subject.value));
  const predicates = new Set(quads.map(q => q.predicate.value));
  const objects = new Set(quads.map(q => q.object.value));

  console.log(`   Total triples: ${quads.length}`);
  console.log(`   Unique subjects: ${subjects.size}`);
  console.log(`   Unique predicates: ${predicates.size}`);
  console.log(`   Unique objects: ${objects.size}\n`);

  // 6. Display all triples
  console.log('6️⃣  All Triples:');
  quads.forEach((q, i) => {
    console.log(`   ${i + 1}. ${q.subject.value}`);
    console.log(`      ${q.predicate.value}`);
    console.log(`      ${q.object.value}\n`);
  });

  console.log('✅ Example completed successfully!');
  console.log(`📁 Output directory: ${outputDir}`);
}

main().catch(error => {
  console.error('❌ Error:', error.message);
  process.exit(1);
});
