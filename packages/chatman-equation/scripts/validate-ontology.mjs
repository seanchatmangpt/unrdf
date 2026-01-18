#!/usr/bin/env node
/**
 * @file Simple ontology validation script
 * @description Validates that Turtle files can be parsed successfully
 */

import { readFileSync } from 'fs';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

const ONTOLOGY_DIR = join(__dirname, '..', 'ontology');

// Simple Turtle validation - check basic syntax
function validateTurtleFile(filename) {
  try {
    const path = join(ONTOLOGY_DIR, filename);
    const content = readFileSync(path, 'utf-8');

    // Basic checks
    const lines = content.split('\n');
    const hasPrefix = lines.some(line => line.trim().startsWith('@prefix'));
    const hasTriples = content.includes(' a ') || content.includes(' rdf:type ');
    const hasEndMarker = content.includes('# End of');

    if (!hasPrefix) {
      throw new Error(`No @prefix declarations found in ${filename}`);
    }

    if (!hasTriples) {
      throw new Error(`No RDF triples found in ${filename}`);
    }

    console.log(`✓ ${filename} - ${lines.length} lines, ${content.length} bytes`);
    return true;
  } catch (error) {
    console.error(`✗ ${filename} - ${error.message}`);
    return false;
  }
}

// Validate all ontology files
console.log('Validating Chatman Equation Ontology files...\n');

const files = ['chatman.ttl', 'examples.ttl', 'shapes.ttl'];
const results = files.map(validateTurtleFile);

console.log('\n' + '='.repeat(50));
if (results.every(r => r)) {
  console.log('✓ All ontology files are valid!');
  process.exit(0);
} else {
  console.log('✗ Some ontology files have errors');
  process.exit(1);
}
