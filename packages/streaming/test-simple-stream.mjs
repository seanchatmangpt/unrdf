#!/usr/bin/env node
/**
 * Simple test of RDF Stream Parser
 */

import { Readable } from 'stream';
import { createRDFStreamParser } from './src/rdf-stream-parser.mjs';

function createReadableStreamFromString(data) {
  return new Readable({
    read() {
      this.push(data);
      this.push(null);
    },
  });
}

const rdfData = `
<http://example.org/s1> <http://example.org/p> "Object 1" .
<http://example.org/s2> <http://example.org/p> "Object 2" .
<http://example.org/s3> <http://example.org/p> "Object 3" .
`;

console.log('Testing RDF Stream Parser...\n');

const stream = createReadableStreamFromString(rdfData);
const parser = createRDFStreamParser({ format: 'n-triples' });

let events = [];

parser.on('data', (chunk) => {
  console.log('DATA event:', chunk.type, chunk.count || 'N/A');
  events.push({ type: 'data', chunkType: chunk.type });
});

parser.on('end', () => {
  console.log('END event received');
  events.push({ type: 'end' });

  const metrics = parser.getMetrics();
  console.log('\nMetrics:', metrics);
  console.log('Events:', events);

  console.log('\n✓ Stream parsing complete');
  process.exit(0);
});

parser.on('error', (err) => {
  console.error('ERROR event:', err);
  events.push({ type: 'error', error: err });
  process.exit(1);
});

console.log('Piping stream to parser...');
stream.pipe(parser);

// Timeout after 5 seconds
setTimeout(() => {
  console.error('\n✗ TIMEOUT: Stream did not complete in 5 seconds');
  console.error('Events received:', events);
  process.exit(1);
}, 5000);
