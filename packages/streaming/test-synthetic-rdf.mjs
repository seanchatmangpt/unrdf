#!/usr/bin/env node
/**
 * Test synthetic RDF generation and parsing
 */

import { Readable } from 'stream';
import { createRDFStreamParser } from './src/rdf-stream-parser.mjs';
import { generateSyntheticRDF, createReadableStreamFromString } from './src/benchmarks.mjs';

console.log('Testing synthetic RDF parsing...\n');

const rdfData = generateSyntheticRDF(10, 'n-triples');
console.log('Generated RDF (10 quads):');
console.log(rdfData.substring(0, 200) + '...\n');

const stream = createReadableStreamFromString(rdfData);
const parser = createRDFStreamParser({ format: 'n-triples' });

let quadCount = 0;
let events = [];

parser.on('data', (chunk) => {
  console.log('DATA:', chunk.type, chunk.count || '');
  events.push({ type: chunk.type, count: chunk.count });
  if (chunk.type === 'quads') {
    quadCount += chunk.count;
  }
});

parser.on('end', () => {
  console.log('\nEND event received');
  console.log('Total quads:', quadCount);
  console.log('Metrics:', parser.getMetrics());
  console.log('\n✓ Success');
  process.exit(0);
});

parser.on('error', (err) => {
  console.error('\n✗ ERROR:', err);
  process.exit(1);
});

console.log('Starting stream...');
stream.pipe(parser);

setTimeout(() => {
  console.error('\n✗ TIMEOUT after 5 seconds');
  console.error('Events:', events);
  console.error('Quads counted:', quadCount);
  process.exit(1);
}, 5000);
