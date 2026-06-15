/**
 * @file RDF Stream Parser Example
 * @description Demonstrates RDFStreamParser for memory-efficient parsing of large RDF datasets
 *
 * API demonstrated: packages/streaming/src/rdf-stream-parser.mjs
 * Docs: packages/streaming/README.md — "RDF Stream Parser" section
 *
 * The RDFStreamParser is a Node.js Transform stream that parses RDF text into
 * quad objects. It emits quads in configurable chunk sizes, respects downstream
 * backpressure so it never overwhelms memory, and reports parsing metrics.
 * `parseRDFStream` is the convenience wrapper for when you want all quads at once.
 *
 * EXPECTED OUTPUT:
 *   [RDFStreamParser] Received chunk with N quads
 *   [RDFStreamParser] Parsing complete: N quads in M chunks
 *   [parseRDFStream] Collected N quads via convenience wrapper
 *   [parseRDFStream] First quad subject: <expected URI>
 *   [format error] Parser emits error for invalid RDF input
 */

import { Readable } from 'stream';
import {
  RDFStreamParser,
  createRDFStreamParser,
  parseRDFStream,
} from '../packages/streaming/src/rdf-stream-parser.mjs';

// Minimal N-Triples (the most reliable format — avoids prefix resolution)
const SAMPLE_NT = `<https://example.org/Alice> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://xmlns.com/foaf/0.1/Person> .
<https://example.org/Alice> <http://xmlns.com/foaf/0.1/name> "Alice Smith" .
<https://example.org/Alice> <http://xmlns.com/foaf/0.1/knows> <https://example.org/Bob> .
<https://example.org/Bob> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://xmlns.com/foaf/0.1/Person> .
<https://example.org/Bob> <http://xmlns.com/foaf/0.1/name> "Bob Jones" .
`;

function makeStream(text) {
  return Readable.from([text]);
}

// ─── 1. RDFStreamParser class — chunk-based streaming ────────────────────────

await new Promise((resolve, reject) => {
  const parser = createRDFStreamParser({ format: 'n-triples', chunkSize: 3 });

  let chunksReceived = 0;
  let quadsTotal = 0;
  let finalMetrics = null;

  parser.on('data', chunk => {
    if (chunk.type === 'quads') {
      chunksReceived++;
      quadsTotal += chunk.count;
      console.log(`[RDFStreamParser] Received chunk with ${chunk.count} quads`);
    }
    if (chunk.type === 'complete') {
      finalMetrics = chunk.metrics;
    }
  });

  parser.on('end', () => {
    if (quadsTotal !== 5) {
      reject(new Error(`Expected 5 quads total, got ${quadsTotal}`));
      return;
    }
    if (!finalMetrics) {
      reject(new Error('Expected complete event with metrics'));
      return;
    }
    console.log(
      `[RDFStreamParser] Parsing complete: ${finalMetrics.quadsProcessed} quads in ${finalMetrics.chunksEmitted} chunk(s)`
    );
    resolve();
  });

  parser.on('error', reject);

  makeStream(SAMPLE_NT).pipe(parser);
});

// ─── 2. parseRDFStream convenience wrapper ────────────────────────────────────

const quads = await parseRDFStream(makeStream(SAMPLE_NT), { format: 'n-triples' });

if (quads.length !== 5) {
  throw new Error(`Expected 5 quads from parseRDFStream, got ${quads.length}`);
}

const firstSubject = quads[0].subject.value;
if (firstSubject !== 'https://example.org/Alice') {
  throw new Error(`Expected Alice as first subject, got ${firstSubject}`);
}

console.log(`[parseRDFStream] Collected ${quads.length} quads via convenience wrapper`);
console.log(`[parseRDFStream] First quad subject: <${firstSubject}>`);

// ─── 3. Format error — parser must reject invalid RDF ─────────────────────────

const badRdf = 'this is not valid RDF at all !!!';

let parseError = null;
await new Promise(resolve => {
  const parser = createRDFStreamParser({ format: 'n-triples', onError: err => { parseError = err; } });
  parser.on('error', () => resolve()); // consume error event to avoid unhandled
  parser.on('end', resolve);
  makeStream(badRdf).pipe(parser);
}).catch(() => {}); // absorb any remaining rejection

if (!parseError) {
  throw new Error('Expected a parse error for invalid RDF input, but none was emitted');
}
console.log(`[format error] ✅ Parser emits error for invalid RDF: ${parseError.message?.slice(0, 60)}…`);

// ─── 4. RDFStreamParser is a real Transform stream instance ──────────────────

const parser = new RDFStreamParser({ format: 'n-triples' });
const { Transform } = await import('stream');
if (!(parser instanceof Transform)) {
  throw new Error('RDFStreamParser must extend Transform');
}
if (typeof parser.getMetrics !== 'function') {
  throw new Error('RDFStreamParser must have getMetrics()');
}

console.log('\n✅ All RDF stream parser assertions passed.');
