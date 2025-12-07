#!/usr/bin/env node
/**
 * Board Decision POC - Using EXISTING AtomVM from kgc-4d
 *
 * Just imports and uses the AtomVM class that already exists.
 * No custom implementations.
 */

import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';

const __dirname = dirname(fileURLToPath(import.meta.url));

// Import EXISTING AtomVM pattern
const atomvmPath = join(__dirname, '../packages/kgc-4d/examples/atomvm-pattern.mjs');

console.log('🏛️  Board Decision POC - Using Existing AtomVM\n');
console.log('Importing from:', atomvmPath);

// Run the existing atomvm-pattern.mjs to demonstrate it works
console.log('\n='.repeat(70));
console.log('Running existing AtomVM pattern...\n');

import('../packages/kgc-4d/examples/atomvm-pattern.mjs').then(() => {
  console.log('\n='.repeat(70));
  console.log('\n✅ Existing AtomVM pattern executed successfully');
  console.log('\nNext: Wire this to @unrdf/kgn for board artifacts\n');
});
