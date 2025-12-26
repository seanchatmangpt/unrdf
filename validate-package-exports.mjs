#!/usr/bin/env node
/**
 * Production Validation: Package Exports vs Documentation
 *
 * Validates that all documented APIs actually exist in source code exports.
 * Critical for DX - ensures users can import what docs claim exists.
 *
 * @module validation/package-exports
 */

import { readFile } from 'fs/promises';
import { join } from 'path';

const PACKAGES = [
  {
    name: '@unrdf/core',
    path: 'packages/core/src/index.mjs',
    docsPath: 'docs/PACKAGES.md',
    section: 'core'
  },
  {
    name: '@unrdf/oxigraph',
    path: 'packages/oxigraph/src/index.mjs',
    docsPath: 'docs/PACKAGES.md',
    section: 'oxigraph'
  },
  {
    name: '@unrdf/hooks',
    path: 'packages/hooks/src/index.mjs',
    docsPath: 'docs/PACKAGES.md',
    section: 'hooks'
  },
  {
    name: '@unrdf/streaming',
    path: 'packages/streaming/src/index.mjs',
    docsPath: 'docs/PACKAGES.md',
    section: 'streaming'
  }
];

/**
 * Extract named exports from index.mjs file
 * @param {string} filePath - Path to index.mjs
 * @returns {Promise<Set<string>>} Set of exported names
 */
async function extractExports(filePath) {
  const content = await readFile(filePath, 'utf-8');
  const exports = new Set();

  // Match: export { foo, bar } from './module.mjs'
  const reExportMatches = content.matchAll(/export\s*\{([^}]+)\}\s*from/g);
  for (const match of reExportMatches) {
    const names = match[1].split(',').map(n => {
      // Handle "foo as bar" -> extract "bar" (what's exposed)
      const asMatch = n.trim().match(/(\w+)\s+as\s+(\w+)/);
      return asMatch ? asMatch[2] : n.trim();
    });
    names.forEach(name => exports.add(name));
  }

  // Match: export const foo = ...
  const constMatches = content.matchAll(/export\s+(?:const|let|var)\s+(\w+)/g);
  for (const match of constMatches) {
    exports.add(match[1]);
  }

  // Match: export function foo()
  const funcMatches = content.matchAll(/export\s+(?:async\s+)?function\s+(\w+)/g);
  for (const match of funcMatches) {
    exports.add(match[1]);
  }

  // Match: export class Foo
  const classMatches = content.matchAll(/export\s+class\s+(\w+)/g);
  for (const match of classMatches) {
    exports.add(match[1]);
  }

  // Match: export { default }
  const defaultMatches = content.matchAll(/export\s+\{\s*default\s*\}/g);
  for (const match of defaultMatches) {
    exports.add('default');
  }

  return exports;
}

/**
 * Extract documented APIs from PACKAGES.md
 * @param {string} docsPath - Path to docs file
 * @param {string} section - Section to extract (core, oxigraph, etc.)
 * @returns {Promise<Set<string>>} Set of documented API names
 */
async function extractDocumentedAPIs(docsPath, section) {
  const content = await readFile(docsPath, 'utf-8');
  const documented = new Set();

  // Find the section in docs
  const sectionRegex = new RegExp(`### .*?${section}[\\s\\S]*?\\*\\*Exports:\\*\\*[\\s\\S]*?\`\`\`javascript([\\s\\S]*?)\`\`\``, 'i');
  const match = content.match(sectionRegex);

  if (!match) {
    console.warn(`⚠️  Could not find exports section for ${section} in ${docsPath}`);
    return documented;
  }

  const exportsBlock = match[1];

  // Extract imported names from the exports block
  // Match patterns like:
  // - createKnowledgeSubstrateCore,
  // - parseRdf,
  const apiMatches = exportsBlock.matchAll(/^\s*([a-zA-Z_$][a-zA-Z0-9_$]*),?\s*$/gm);
  for (const apiMatch of apiMatches) {
    const name = apiMatch[1].trim();
    if (name && name !== 'import' && name !== 'from' && name !== '}') {
      documented.add(name);
    }
  }

  // Also check inline documentation like `defineHook(config)`
  const inlineMatches = exportsBlock.matchAll(/\b([a-zA-Z_$][a-zA-Z0-9_$]*)\s*\(/g);
  for (const inlineMatch of inlineMatches) {
    documented.add(inlineMatch[1]);
  }

  return documented;
}

/**
 * Extract documented APIs from main README.md
 * @returns {Promise<Map<string, Set<string>>>} Map of package -> documented APIs
 */
async function extractMainREADMEAPIs() {
  const content = await readFile('README.md', 'utf-8');
  const apis = new Map();

  // Extract from Quick Start example
  const quickStartMatch = content.match(/```javascript\s*import\s*\{([^}]+)\}\s*from\s*'@unrdf\/(\w+)'/g);
  if (quickStartMatch) {
    for (const match of quickStartMatch) {
      const importMatch = match.match(/import\s*\{([^}]+)\}\s*from\s*'@unrdf\/(\w+)'/);
      if (importMatch) {
        const [, imports, pkg] = importMatch;
        const pkgKey = `@unrdf/${pkg}`;
        if (!apis.has(pkgKey)) {
          apis.set(pkgKey, new Set());
        }
        imports.split(',').forEach(name => {
          const cleaned = name.trim();
          if (cleaned) apis.get(pkgKey).add(cleaned);
        });
      }
    }
  }

  return apis;
}

/**
 * Validate a single package
 */
async function validatePackage(pkg) {
  console.log(`\n${'='.repeat(80)}`);
  console.log(`📦 Validating: ${pkg.name}`);
  console.log(`${'='.repeat(80)}\n`);

  try {
    // Extract actual exports
    const actualExports = await extractExports(pkg.path);
    console.log(`✓ Found ${actualExports.size} actual exports in ${pkg.path}`);

    // Extract documented APIs
    const documentedAPIs = await extractDocumentedAPIs(pkg.docsPath, pkg.section);
    console.log(`✓ Found ${documentedAPIs.size} documented APIs in ${pkg.docsPath}`);

    // Compare
    const missing = [...documentedAPIs].filter(api => !actualExports.has(api));
    const undocumented = [...actualExports].filter(exp => !documentedAPIs.has(exp));

    let status = '✅ PASS';
    let issues = [];

    if (missing.length > 0) {
      status = '❌ FAIL';
      issues.push(`\n⚠️  APIs documented but NOT exported (${missing.length}):`);
      missing.forEach(api => issues.push(`   - ${api}`));
      issues.push('\n   ⚡ ACTION: Either export these or remove from docs');
    }

    if (undocumented.length > 0) {
      // Only warn, not fail - undocumented exports are less critical
      issues.push(`\n⚠️  APIs exported but NOT documented (${undocumented.length}):`);
      undocumented.forEach(api => issues.push(`   - ${api}`));
      issues.push('\n   💡 SUGGESTION: Add to documentation for better DX');
    }

    console.log(`\n${status} ${pkg.name}`);
    if (issues.length > 0) {
      console.log(issues.join('\n'));
    } else {
      console.log(`   🎉 All documented APIs exist in exports!`);
    }

    return {
      package: pkg.name,
      passed: missing.length === 0,
      actualCount: actualExports.size,
      documentedCount: documentedAPIs.size,
      missing,
      undocumented
    };

  } catch (error) {
    console.error(`❌ ERROR validating ${pkg.name}:`, error.message);
    return {
      package: pkg.name,
      passed: false,
      error: error.message
    };
  }
}

/**
 * Main validation
 */
async function main() {
  console.log(`
╔════════════════════════════════════════════════════════════════════════════╗
║                    PRODUCTION VALIDATION REPORT                            ║
║                Package Exports vs Documentation                            ║
╚════════════════════════════════════════════════════════════════════════════╝
`);

  console.log('📋 VALIDATION SCOPE:');
  console.log('   - Ensure documented APIs actually exist in exports');
  console.log('   - Identify missing or undocumented exports');
  console.log('   - Critical for Developer Experience (DX)\n');

  const results = [];

  for (const pkg of PACKAGES) {
    const result = await validatePackage(pkg);
    results.push(result);
  }

  // Summary
  console.log(`\n${'='.repeat(80)}`);
  console.log(`📊 VALIDATION SUMMARY`);
  console.log(`${'='.repeat(80)}\n`);

  const passed = results.filter(r => r.passed).length;
  const failed = results.filter(r => !r.passed).length;

  results.forEach(result => {
    const icon = result.passed ? '✅' : '❌';
    const status = result.passed ? 'PASS' : 'FAIL';
    console.log(`${icon} ${result.package.padEnd(25)} ${status}`);
    if (result.missing?.length > 0) {
      console.log(`   └─ Missing exports: ${result.missing.length}`);
    }
    if (result.undocumented?.length > 0) {
      console.log(`   └─ Undocumented: ${result.undocumented.length}`);
    }
  });

  console.log(`\n${'─'.repeat(80)}`);
  console.log(`Total: ${results.length} packages`);
  console.log(`Passed: ${passed}`);
  console.log(`Failed: ${failed}`);

  // Final verdict
  if (failed === 0) {
    console.log(`\n✅ PRODUCTION READY: All documented APIs exist in exports!\n`);
    process.exit(0);
  } else {
    console.log(`\n❌ PRODUCTION BLOCKED: ${failed} package(s) have missing exports!\n`);
    console.log(`⚡ ACTION REQUIRED:`);
    console.log(`   1. Review missing exports above`);
    console.log(`   2. Either add exports to source OR remove from docs`);
    console.log(`   3. Re-run validation\n`);
    process.exit(1);
  }
}

main().catch(error => {
  console.error('❌ VALIDATION FAILED:', error);
  process.exit(1);
});
