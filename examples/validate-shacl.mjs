/**
 * @file SHACL Validation Example
 * @description Demonstrates validateShacl and validateQuad API surface
 *
 * API demonstrated: packages/streaming/src/validate.mjs
 * Docs: packages/streaming/README.md — "SHACL Validation" section
 *
 * IMPLEMENTATION STATUS: validateShacl is a stub. It extracts shape metadata
 * but does not evaluate SHACL constraints. It always returns conforms: true.
 * This example tests the actual current behavior, not aspirational behavior.
 *
 * FUTURE IMPLEMENTOR NOTE: The assertion marked FUTURE below is what should
 * change when real SHACL evaluation is added. Flip CONFORMS_STUB_ALWAYS_TRUE
 * to false and this example becomes the witness that the implementation works.
 *
 * EXPECTED OUTPUT:
 *   [validateShacl] result has required shape: { conforms, results, timestamp }
 *   [validateShacl] stub returns conforms: true (FUTURE: false for violating data)
 *   [validateQuad] wraps quad in temp store, returns same shape
 *   [options] strict and maxViolations accepted without error
 */

import { createStore, dataFactory } from '../packages/oxigraph/src/index.mjs';
import { validateShacl, validateQuad } from '../packages/streaming/src/validate.mjs';

const df = dataFactory;

// Stub flag: flip to false when validateShacl actually detects violations
const CONFORMS_STUB_ALWAYS_TRUE = true;

// ─── Setup ────────────────────────────────────────────────────────────────────

const RDF_TYPE = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type';
const SHACL_NODE_SHAPE = 'http://www.w3.org/ns/shacl#NodeShape';
const FOAF_PERSON = 'http://xmlns.com/foaf/0.1/Person';

function rdfQuad(s, p, o) {
  return {
    subject: df.namedNode(s),
    predicate: df.namedNode(p),
    object: df.namedNode(o),
    graph: df.defaultGraph(),
  };
}

// A data store with a Person who lacks foaf:name (violates a name-required shape)
const dataStore = createStore();
dataStore.addQuad(rdfQuad('https://example.org/Alice', RDF_TYPE, FOAF_PERSON));

// A shapes store with a NodeShape (PersonShape requires foaf:name — not yet enforced by stub)
const shapesStore = createStore();
shapesStore.addQuad(rdfQuad(
  'https://example.org/shapes/PersonShape',
  RDF_TYPE,
  SHACL_NODE_SHAPE,
));

// ─── 1. validateShacl — result structure ─────────────────────────────────────

const result = await validateShacl(dataStore, shapesStore);

if (typeof result.conforms !== 'boolean') throw new Error('result.conforms must be boolean');
if (!Array.isArray(result.results)) throw new Error('result.results must be an array');
if (typeof result.timestamp !== 'number') throw new Error('result.timestamp must be a number');

console.log(`[validateShacl] ✅ result has required shape: { conforms: ${result.conforms}, results: [], timestamp: <number> }`);

// Current stub behavior: always conforms regardless of actual constraint violations
if (CONFORMS_STUB_ALWAYS_TRUE) {
  if (result.conforms !== true) {
    throw new Error(`[STUB] Expected conforms: true (stub always returns true), got ${result.conforms}`);
  }
  console.log('[validateShacl] ✅ stub returns conforms: true (FUTURE: false — Alice violates PersonShape by lacking foaf:name)');
} else {
  if (result.conforms !== false) {
    throw new Error('Expected conforms: false — Alice violates PersonShape (missing foaf:name)');
  }
  console.log('[validateShacl] ✅ real impl: conforms: false, violations detected');
}

// ─── 2. validateQuad — wraps single quad ─────────────────────────────────────

const quadResult = await validateQuad(
  rdfQuad('https://example.org/Bob', RDF_TYPE, FOAF_PERSON),
  shapesStore,
);

if (typeof quadResult.conforms !== 'boolean') throw new Error('quadResult.conforms must be boolean');
if (!Array.isArray(quadResult.results)) throw new Error('quadResult.results must be an array');

console.log(`[validateQuad] ✅ wraps quad in temp store, returns { conforms: ${quadResult.conforms}, results: [] }`);

// ─── 3. options accepted without error ───────────────────────────────────────

const optResult = await validateShacl(dataStore, shapesStore, {
  strict: false,
  maxViolations: 10,
  includeDetails: true,
});

if (typeof optResult.conforms !== 'boolean') throw new Error('Options variant must return result shape');

console.log('[options] ✅ strict and maxViolations accepted without error');

console.log('\n✅ All SHACL validation API surface assertions passed.');
console.log('   ⚠️  Note: conforms detection is a stub — see README for implementation status.');
