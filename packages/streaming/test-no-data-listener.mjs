#!/usr/bin/env node
/**
 * Test parsing WITHOUT data event listener (like the failing tests)
 */

import { Readable } from 'stream';
import { createRDFStreamParser } from './src/rdf-stream-parser.mjs';
import { generateSyntheticRDF, createReadableStreamFromString } from './src/benchmarks.mjs';

console.log('Testing WITHOUT data event listener...\n');

const rdfData = generateSyntheticRDF(100, 'n-triples');
const stream = createReadableStreamFromString(rdfData);
const parser = createRDFStreamParser({ format: 'n-triples' });

// Replicate the failing test pattern - NO data listener
const promise = new Promise((resolve, reject) => {
  stream.pipe(parser);
  parser.on('end', () => {
    console.log('END event received');
    const metrics = parser.getMetrics();
    console.log('Metrics:', metrics);
    resolve(metrics);
  });
  parser.on('error', reject);
});

console.log('Waiting for end event...');

Promise.race([
  promise,
  new Promise((_, reject) => setTimeout(() => reject(new Error('TIMEOUT')), 3000))
]).then(
  metrics => {
    console.log('\n✓ Success - quads processed:', metrics.quadsProcessed);
    process.exit(0);
  },
  error => {
    console.error('\n✗ Failed:', error.message);
    console.error('This matches the test failure pattern!');
    process.exit(1);
  }
);
