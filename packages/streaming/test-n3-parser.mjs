#!/usr/bin/env node
/**
 * Test N3 Parser callback behavior
 */

import { Parser } from '@unrdf/core/rdf/n3-justified-only';

const parser = new Parser({ baseIRI: 'http://example.org/' });

const rdfData = `
<http://example.org/s1> <http://example.org/p> "Object 1" .
<http://example.org/s2> <http://example.org/p> "Object 2" .
<http://example.org/s3> <http://example.org/p> "Object 3" .
`;

console.log('Testing N3 Parser callback behavior...\n');

let quadCount = 0;
let calledWithNull = false;

parser.parse(rdfData, (error, quad) => {
  if (error) {
    console.error('Error:', error);
    return;
  }

  if (quad) {
    quadCount++;
    console.log(`Quad ${quadCount}:`, quad.subject.value, quad.predicate.value, quad.object.value);
  } else {
    calledWithNull = true;
    console.log('\n✓ Parser called callback with null (end of parsing)');
  }
});

// Give it a moment to complete
setTimeout(() => {
  console.log(`\nTotal quads: ${quadCount}`);
  console.log(`Called with null: ${calledWithNull}`);

  if (!calledWithNull) {
    console.log('\n⚠️  WARNING: Parser did NOT call callback with null!');
    console.log('This means we cannot wait for parsing to complete asynchronously.');
  }
}, 100);
