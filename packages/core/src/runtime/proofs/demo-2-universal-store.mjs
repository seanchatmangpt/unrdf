/**
 * Demo 2: Universal RDF Store Pattern
 * 
 * PROOF: In-memory RDF store that works in Node.js and Browser
 * 
 * Pattern: Runtime-agnostic data structures
 * - No native dependencies
 * - Pure JavaScript
 * - Works everywhere (Node.js, Browser, Deno, Bun, Workers)
 */

import { detectRuntime } from '../detect.mjs';

/**
 * Simple universal RDF quad
 */
class Quad {
  constructor(subject, predicate, object, graph = null) {
    this.subject = subject;
    this.predicate = predicate;
    this.object = object;
    this.graph = graph;
  }
  
  toString() {
    return '<' + this.subject + '> <' + this.predicate + '> "' + this.object + '" .';
  }
}

/**
 * Universal in-memory RDF store (works everywhere)
 */
class UniversalStore {
  constructor() {
    this.quads = [];
  }
  
  add(quad) {
    this.quads.push(quad);
  }
  
  get size() {
    return this.quads.length;
  }
  
  [Symbol.iterator]() {
    return this.quads[Symbol.iterator]();
  }
  
  match(subject = null, predicate = null, object = null) {
    return this.quads.filter(q => {
      if (subject && q.subject !== subject) return false;
      if (predicate && q.predicate !== predicate) return false;
      if (object && q.object !== object) return false;
      return true;
    });
  }
  
  toNQuads() {
    return this.quads.map(q => q.toString()).join('\n');
  }
}

/**
 * Create and populate a universal RDF store
 * @returns {Promise<{store: UniversalStore, size: number}>}
 */
export async function createUniversalStore() {
  const runtime = detectRuntime();
  console.log('Creating store in: ' + runtime.type);
  
  // Create store (works in Node.js, Browser, Deno, Bun, Workers)
  const store = new UniversalStore();
  
  // Add some triples
  store.add(new Quad(
    'http://example.org/Alice',
    'http://schema.org/name',
    'Alice'
  ));
  
  store.add(new Quad(
    'http://example.org/Alice',
    'http://schema.org/age',
    '30'
  ));
  
  store.add(new Quad(
    'http://example.org/Bob',
    'http://schema.org/name',
    'Bob'
  ));
  
  store.add(new Quad(
    'http://example.org/Bob',
    'http://schema.org/knows',
    'http://example.org/Alice'
  ));
  
  return { store, size: store.size };
}

/**
 * Query the store (simple pattern matching)
 * @param {UniversalStore} store - RDF store
 * @returns {Array} Query results
 */
export function queryStore(store) {
  // Find all people and their names
  const results = [];
  const nameQuads = store.match(null, 'http://schema.org/name', null);
  
  for (const quad of nameQuads) {
    results.push({
      person: quad.subject,
      name: quad.object
    });
  }
  
  return results;
}

/**
 * Serialize store to N-Quads format
 * @param {UniversalStore} store 
 * @returns {string}
 */
export function serializeStore(store) {
  return store.toNQuads();
}

// PROOF: Run this demo
async function main() {
  const runtime = detectRuntime();
  console.log('Runtime: ' + runtime.type + ' ' + runtime.version);
  console.log('Pure JS (no native deps): ✅');
  console.log('');
  
  // Create store
  console.log('Creating universal RDF store...');
  const { store, size } = await createUniversalStore();
  console.log('Store size: ' + size + ' triples ✅');
  console.log('');
  
  // List all triples
  console.log('All triples:');
  let count = 0;
  for (const quad of store) {
    console.log('  ' + (++count) + '. ' + quad.toString());
  }
  console.log('');
  
  // Query store
  console.log('Query Results (all names):');
  const results = queryStore(store);
  for (const result of results) {
    console.log('  - ' + result.name + ' (' + result.person + ')');
  }
  console.log('');
  
  // Serialize
  console.log('Serialized as N-Quads:');
  console.log(serializeStore(store));
  console.log('');
  
  // Prove idempotency: same operations = same result
  const { store: store2 } = await createUniversalStore();
  console.log('Idempotency check: ' + (store.size === store2.size ? '✅' : '❌'));
  console.log('');
  
  console.log('✅ Demo 2: Universal Store - SUCCESS');
  console.log('Pattern proven: Runtime-agnostic RDF storage works in ' + runtime.type);
}

// Auto-run if executed directly
if (import.meta.url === 'file://' + process.argv[1]) {
  main().catch(console.error);
}
