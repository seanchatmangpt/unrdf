#!/usr/bin/env node
/**
 * Validate import syntax in all YAWL modules
 * Checks for common import issues without running full build
 */

import { readFileSync, readdirSync, statSync } from 'fs';
import { resolve, join, relative } from 'path';

const srcDir = '/home/user/unrdf/packages/yawl/src';

console.log('🔍 Validating import syntax...\n');

let errors = 0;
let warnings = 0;

// Find all .mjs files recursively
function findMjsFiles(dir) {
  const files = [];

  function traverse(currentDir) {
    const entries = readdirSync(currentDir);

    for (const entry of entries) {
      const fullPath = join(currentDir, entry);
      const stat = statSync(fullPath);

      if (stat.isDirectory()) {
        traverse(fullPath);
      } else if (entry.endsWith('.mjs')) {
        files.push(relative(srcDir, fullPath));
      }
    }
  }

  traverse(dir);
  return files;
}

// Get all .mjs files
const files = findMjsFiles(srcDir);

console.log(`Analyzing ${files.length} modules...\n`);

for (const file of files) {
  const filePath = resolve(srcDir, file);
  const content = readFileSync(filePath, 'utf8');
  const lines = content.split('\n');

  // Check for CommonJS usage
  if (/\brequire\s*\(/.test(content)) {
    console.error(`❌ ${file}: Uses require() instead of import`);
    errors++;
  }

  if (/module\.exports\s*=/.test(content)) {
    console.error(`❌ ${file}: Uses module.exports instead of export`);
    errors++;
  }

  // Check for imports from 'n3' (should use @unrdf/oxigraph)
  if (content.includes("from 'n3'") && !file.includes('justified')) {
    console.warn(`⚠️  ${file}: Imports from 'n3' (should use @unrdf/oxigraph)`);
    warnings++;
  }

  // Check for missing .mjs extension in relative imports
  const importMatches = content.matchAll(/from\s+['"](\.[^'"]+)['"]/g);
  for (const match of importMatches) {
    const importPath = match[1];
    if (!importPath.endsWith('.mjs') && !importPath.endsWith('.json')) {
      const lineNum = lines.findIndex(l => l.includes(match[0])) + 1;
      console.warn(`⚠️  ${file}:${lineNum}: Import missing .mjs: ${importPath}`);
      warnings++;
    }
  }
}

console.log('\n' + '='.repeat(60));
console.log('VALIDATION SUMMARY');
console.log('='.repeat(60));
console.log(`\n📦 Modules analyzed: ${files.length}`);
console.log(`❌ Errors: ${errors}`);
console.log(`⚠️  Warnings: ${warnings}`);

if (errors === 0 && warnings === 0) {
  console.log('\n✅ All imports are valid!');
  process.exit(0);
} else if (errors === 0) {
  console.log('\n✅ No critical errors (warnings only)');
  process.exit(0);
} else {
  console.log('\n❌ Import validation failed');
  process.exit(1);
}
