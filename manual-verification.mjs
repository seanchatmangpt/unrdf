#!/usr/bin/env node
/**
 * Manual Import Verification
 * Extracts package names from multi-line import statements
 */

import { readFileSync } from 'fs';

function extractPackages(filePath) {
  const content = readFileSync(filePath, 'utf-8');
  const packages = new Set();

  // Match all 'from' clauses (handles multiline imports)
  const fromMatches = content.match(/from\s+['"]([^'"]+)['"]/g) || [];

  for (const match of fromMatches) {
    const pkg = match.match(/from\s+['"]([^'"]+)['"]/)[1];
    if (!pkg.startsWith('.') && !pkg.startsWith('/')) {
      packages.add(pkg);
    }
  }

  return Array.from(packages).sort();
}

const files = {
  'max-combo-10-mega-framework.mjs': '/home/user/unrdf/max-combo-10-mega-framework.mjs',
  'max-combo-10-mega-framework-standalone.mjs': '/home/user/unrdf/max-combo-10-mega-framework-standalone.mjs',
  'microfw-9-graph-routing.mjs': '/home/user/unrdf/microfw-9-graph-routing.mjs',
};

console.log('╔════════════════════════════════════════════════════════════╗');
console.log('║ MANUAL PACKAGE VERIFICATION                                ║');
console.log('╚════════════════════════════════════════════════════════════╝\n');

for (const [name, path] of Object.entries(files)) {
  const packages = extractPackages(path);
  const unrdf = packages.filter(p => p.startsWith('@unrdf/'));
  const external = packages.filter(p => !p.startsWith('@unrdf/'));

  console.log(`${name}:`);
  console.log(`  Total Packages: ${packages.length}`);
  console.log(`  @unrdf Packages: ${unrdf.length}`);
  console.log(`  External: ${external.length}\n`);

  if (packages.length > 0) {
    console.log('  All Packages:');
    packages.forEach((p, i) => {
      const prefix = p.startsWith('@unrdf/') ? '    ✓' : '    •';
      console.log(`${prefix} ${i + 1}. ${p}`);
    });
    console.log('');
  }
}

// ADVERSARIAL CLAIM VERIFICATION
console.log('╔════════════════════════════════════════════════════════════╗');
console.log('║ ADVERSARIAL CLAIM VERIFICATION                             ║');
console.log('╚════════════════════════════════════════════════════════════╝\n');

const megaPkgs = extractPackages(files['max-combo-10-mega-framework.mjs']);
console.log('CLAIM: "12-Package Integration"');
console.log(`ACTUAL: ${megaPkgs.length} packages`);
console.log(`VERDICT: ${megaPkgs.length === 12 ? '✅ PASS' : `❌ FAIL (off by ${12 - megaPkgs.length})`}\n`);

console.log('CLAIM: "10 frameworks with 3-12 package integrations"');
console.log('ACTUAL: 3 frameworks found (excluding test file)');
console.log('VERDICT: ❌ FAIL (missing 7 frameworks)\n');

const standalonePkgs = extractPackages(files['max-combo-10-mega-framework-standalone.mjs']);
const microPkgs = extractPackages(files['microfw-9-graph-routing.mjs']);

console.log('Integration Count Check (3-12 range):');
console.log(`  max-combo-10-mega-framework.mjs: ${megaPkgs.length} ${megaPkgs.length >= 3 && megaPkgs.length <= 12 ? '✅' : '❌'}`);
console.log(`  max-combo-10-mega-framework-standalone.mjs: ${standalonePkgs.length} ${standalonePkgs.length >= 3 && standalonePkgs.length <= 12 ? '✅' : '❌'}`);
console.log(`  microfw-9-graph-routing.mjs: ${microPkgs.length} ${microPkgs.length >= 3 && microPkgs.length <= 12 ? '✅' : '❌'}`);
