#!/usr/bin/env node
/**
 * @fileoverview Executable demonstration of lens functionality
 */

import { demonstrateCustomerLens } from './customer-lens.mjs';

console.log('\n╔════════════════════════════════════════════╗');
console.log('║   UNRDF Lens Compiler - Live Demo        ║');
console.log('╚════════════════════════════════════════════╝\n');

try {
  const results = demonstrateCustomerLens();

  console.log('\n📊 Summary:');
  console.log('─────────────────────────────────────────────');
  console.log(`Lens ID:          ${results.lens.id}`);
  console.log(`Lens Version:     ${results.lens.version}`);
  console.log(`Compiled Hash:    ${results.compiled.canonicalHash}`);
  console.log(`Quads Generated:  ${results.quads.length}`);
  console.log(`Deterministic:    ${results.isDeterministic ? '✓ YES' : '✗ NO'}`);
  console.log(`Lossless:         ${results.isLossless ? '✓ YES' : '✗ NO'}`);
  console.log('─────────────────────────────────────────────\n');

  if (results.isDeterministic && results.isLossless) {
    console.log('✅ All checks passed! Lens is working correctly.\n');
    process.exit(0);
  } else {
    console.log('⚠️  Some checks failed. Review the output above.\n');
    process.exit(1);
  }
} catch (error) {
  console.error('\n❌ Demo failed with error:\n');
  console.error(error);
  process.exit(1);
}
