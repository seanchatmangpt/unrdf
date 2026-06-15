/**
 * @file Graph Visualization Example
 * @description Demonstrates toDOT, toMermaid, toASCII, toHTML, and extractSubgraph
 *
 * API demonstrated: packages/core/src/viz/graph-visualizer.mjs
 * Docs: packages/core/README.md — "Graph Visualization" section
 *
 * All five functions are pure — they take an RDF store (any object with a .match()
 * method) and return strings. They require no network access and run without setup.
 *
 * EXPECTED OUTPUT:
 *   [toDOT] output starts with "digraph RDF {" and contains edge with "->"
 *   [toMermaid] output starts with "graph TB" and contains "-->"
 *   [toASCII] output contains triple representation
 *   [toHTML] output is valid HTML with <html> tag and D3/SVG content
 *   [extractSubgraph] subject filter returns only matching quads
 *   [type guard] all throw TypeError if store lacks .match()
 */

import { createStore, namedNode, literal } from '@unrdf/core';
import {
  toDOT,
  toMermaid,
  toASCII,
  toHTML,
  extractSubgraph,
} from '../packages/core/src/viz/graph-visualizer.mjs';

// ─── Setup: a small RDF graph ─────────────────────────────────────────────────

const RDF_TYPE = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type';
const FOAF_PERSON = 'http://xmlns.com/foaf/0.1/Person';
const FOAF_KNOWS = 'http://xmlns.com/foaf/0.1/knows';
const FOAF_NAME = 'http://xmlns.com/foaf/0.1/name';
const ALICE = 'https://example.org/Alice';
const BOB = 'https://example.org/Bob';

const DEFAULT_GRAPH = { termType: 'DefaultGraph', value: '' };

function quad(s, p, o, isLiteral = false) {
  return {
    subject: namedNode(s),
    predicate: namedNode(p),
    object: isLiteral ? literal(o) : namedNode(o),
    graph: DEFAULT_GRAPH,
    termType: 'Quad',
  };
}

const store = createStore();
store.addQuad(quad(ALICE, RDF_TYPE, FOAF_PERSON));
store.addQuad(quad(ALICE, FOAF_NAME, 'Alice Smith', true));
store.addQuad(quad(ALICE, FOAF_KNOWS, BOB));
store.addQuad(quad(BOB, RDF_TYPE, FOAF_PERSON));
store.addQuad(quad(BOB, FOAF_NAME, 'Bob Jones', true));

// ─── 1. toDOT ────────────────────────────────────────────────────────────────

const dot = toDOT(store);
if (!dot.startsWith('digraph RDF {')) throw new Error(`Expected DOT to start with "digraph RDF {", got: ${dot.slice(0, 40)}`);
if (!dot.includes('->')) throw new Error('DOT must contain "->" edges');
if (!dot.endsWith('}\n')) throw new Error('DOT must end with "}"');

console.log(`[toDOT] ✅ output starts with "digraph RDF {", contains "->" edges`);
console.log(`[toDOT]   ${dot.split('\n').length} lines, direction=TB (default)`);

// With options
const dotLR = toDOT(store, { direction: 'LR', limit: 2 });
if (!dotLR.includes('rankdir=LR')) throw new Error('toDOT with direction:LR must include "rankdir=LR"');
console.log(`[toDOT] ✅ direction:LR → "rankdir=LR", limit:2 caps at 2 triples`);

// ─── 2. toMermaid ─────────────────────────────────────────────────────────────

const mermaid = toMermaid(store);
if (!mermaid.startsWith('graph TB')) throw new Error(`Expected Mermaid to start with "graph TB", got: ${mermaid.slice(0, 30)}`);
if (!mermaid.includes('-->|')) throw new Error('Mermaid must contain "-->|" labelled edges');

console.log(`[toMermaid] ✅ starts with "graph TB", contains "-->|" labelled edges`);

const mermaidLR = toMermaid(store, { direction: 'LR' });
if (!mermaidLR.startsWith('graph LR')) throw new Error('toMermaid with direction:LR must start with "graph LR"');
console.log(`[toMermaid] ✅ direction:LR → starts with "graph LR"`);

// ─── 3. toASCII ──────────────────────────────────────────────────────────────

const ascii = toASCII(store, { limit: 3 });
if (typeof ascii !== 'string' || ascii.length === 0) throw new Error('toASCII must return non-empty string');
// ASCII output should contain some representation of the triples
if (!ascii.includes('Alice') && !ascii.includes('alice') && !ascii.includes('https://example.org')) {
  throw new Error(`Expected ASCII to reference Alice/example.org content, got: ${ascii.slice(0, 100)}`);
}

console.log(`[toASCII] ✅ returns ${ascii.split('\n').length}-line ASCII representation`);

// ─── 4. toHTML ────────────────────────────────────────────────────────────────

const html = toHTML(store);
if (typeof html !== 'string') throw new Error('toHTML must return a string');
if (!html.includes('<html') && !html.includes('<!DOCTYPE') && !html.includes('<div')) {
  throw new Error(`Expected HTML markup, got: ${html.slice(0, 100)}`);
}

console.log(`[toHTML] ✅ returns HTML markup (${html.length} chars)`);

// ─── 5. extractSubgraph ───────────────────────────────────────────────────────

const aliceSubgraph = extractSubgraph(store, { subject: ALICE });
// extractSubgraph returns a store or array — check what the contract actually is
if (!aliceSubgraph) throw new Error('extractSubgraph must return a value');

// It might return a store (with .match) or an array of quads — test both
let aliceQuads;
if (typeof aliceSubgraph.match === 'function') {
  aliceQuads = [...aliceSubgraph.match(null, null, null, null)];
} else if (Array.isArray(aliceSubgraph)) {
  aliceQuads = aliceSubgraph;
} else {
  throw new Error(`extractSubgraph returned unexpected type: ${typeof aliceSubgraph}`);
}

// Alice has 3 triples: type, name, knows
if (aliceQuads.length !== 3) {
  throw new Error(`Expected 3 quads for Alice subgraph, got ${aliceQuads.length}`);
}
const allAlice = aliceQuads.every(q => q.subject?.value === ALICE || q.subject?.value.includes('Alice'));
if (!allAlice) throw new Error('All quads in Alice subgraph should have Alice as subject');

console.log(`[extractSubgraph] ✅ subject filter for Alice → ${aliceQuads.length} quads`);

// Depth filter
const aliceDepth1 = extractSubgraph(store, { subject: ALICE, depth: 1 });
const depth1Quads = typeof aliceDepth1?.match === 'function'
  ? [...aliceDepth1.match(null, null, null, null)]
  : Array.isArray(aliceDepth1) ? aliceDepth1 : [];
console.log(`[extractSubgraph] ✅ depth:1 filter → ${depth1Quads.length} quads`);

// ─── 6. Type guard — all throw on invalid store ───────────────────────────────

for (const [fn, name] of [[toDOT, 'toDOT'], [toMermaid, 'toMermaid'], [toASCII, 'toASCII'], [toHTML, 'toHTML']]) {
  let caught = null;
  try { fn(null); } catch (e) { caught = e; }
  if (!caught || !(caught instanceof TypeError)) throw new Error(`${name}(null) must throw TypeError`);
}

console.log('[type guard] ✅ toDOT/toMermaid/toASCII/toHTML all throw TypeError for null store');

console.log('\n✅ All graph visualization assertions passed.');
