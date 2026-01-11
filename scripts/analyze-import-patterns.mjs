#!/usr/bin/env node
/**
 * @file Import Pattern Analysis
 * @description Analyzes actual import patterns across packages
 */

import { readFileSync, readdirSync, statSync } from 'fs';
import { join } from 'path';
import { execSync } from 'child_process';

const PACKAGES_DIR = '/home/user/unrdf/packages';

function getAllSourceFiles() {
  const files = [];
  const packages = readdirSync(PACKAGES_DIR);

  for (const pkg of packages) {
    const srcPath = join(PACKAGES_DIR, pkg, 'src');
    try {
      if (!statSync(srcPath).isDirectory()) continue;

      const findCmd = `find ${srcPath} -name "*.mjs" 2>/dev/null`;
      const pkgFiles = execSync(findCmd, { encoding: 'utf-8' })
        .split('\n')
        .filter(Boolean);

      files.push(...pkgFiles.map(f => ({ file: f, package: pkg })));
    } catch {
      // Skip packages without src directory
    }
  }

  return files;
}

function analyzeImports(files) {
  const importMap = new Map(); // package -> { from: pkg, imports: [...] }
  const functionImports = new Map(); // function name -> count

  for (const { file, package: pkg } of files) {
    try {
      const content = readFileSync(file, 'utf-8');

      // Match: import { x, y, z } from '@unrdf/package'
      // or: import * as name from '@unrdf/package'
      // or: import name from '@unrdf/package'
      const importRegex = /import\s+(?:{([^}]+)}|\*\s+as\s+(\w+)|(\w+))\s+from\s+['"]@unrdf\/([^'"]+)['"]/g;

      let match;
      while ((match = importRegex.exec(content)) !== null) {
        const [, namedImports, namespaceImport, defaultImport, fromPkg] = match;

        const key = `${pkg}:${fromPkg}`;
        if (!importMap.has(key)) {
          importMap.set(key, { from: pkg, to: fromPkg, imports: [] });
        }

        if (namedImports) {
          const imports = namedImports
            .split(',')
            .map(s => s.trim())
            .filter(Boolean);

          importMap.get(key).imports.push(...imports);

          // Track function import frequency
          for (const imp of imports) {
            functionImports.set(imp, (functionImports.get(imp) || 0) + 1);
          }
        } else if (namespaceImport) {
          importMap.get(key).imports.push(`* as ${namespaceImport}`);
        } else if (defaultImport) {
          importMap.get(key).imports.push(`default as ${defaultImport}`);
        }
      }
    } catch (err) {
      // Skip files that can't be read
    }
  }

  return { importMap, functionImports };
}

function analyzeIntegrationPatterns(importMap) {
  const patterns = new Map();

  for (const [key, value] of importMap) {
    const pattern = `${value.from} → ${value.to}`;
    patterns.set(pattern, (patterns.get(pattern) || 0) + value.imports.length);
  }

  return patterns;
}

console.log('🔗 IMPORT PATTERN ANALYSIS\n');
console.log('═'.repeat(80));

const sourceFiles = getAllSourceFiles();
console.log(`\nAnalyzing ${sourceFiles.length} source files...\n`);

const { importMap, functionImports } = analyzeImports(sourceFiles);

console.log('📊 CROSS-PACKAGE IMPORT STATISTICS\n');
console.log(`Total Cross-Package Import Statements: ${importMap.size}`);
console.log(`Total Unique Functions Imported: ${functionImports.size}`);

// Most common integration patterns
const patterns = analyzeIntegrationPatterns(importMap);
const sortedPatterns = Array.from(patterns.entries())
  .sort((a, b) => b[1] - a[1])
  .slice(0, 20);

console.log('\n🔥 MOST COMMON INTEGRATION PATTERNS (Top 20)\n');
sortedPatterns.forEach(([pattern, count], i) => {
  const bar = '█'.repeat(Math.min(count, 50));
  console.log(`${String(i + 1).padStart(2)}. ${pattern.padEnd(40)} ${bar} ${count}`);
});

// Most imported functions
const topFunctions = Array.from(functionImports.entries())
  .sort((a, b) => b[1] - a[1])
  .slice(0, 20);

console.log('\n⭐ MOST FREQUENTLY IMPORTED FUNCTIONS (Top 20)\n');
topFunctions.forEach(([fn, count], i) => {
  console.log(`${String(i + 1).padStart(2)}. ${fn.padEnd(40)} ${count} imports`);
});

// Integration pattern diversity
const packagesWithImports = new Set();
for (const [key] of importMap) {
  const [pkg] = key.split(':');
  packagesWithImports.add(pkg);
}

console.log('\n📈 INTEGRATION METRICS\n');
console.log(`Packages with Cross-Package Imports: ${packagesWithImports.size}`);
console.log(`Average Imports per Package: ${(importMap.size / packagesWithImports.size).toFixed(2)}`);

// Find packages with most diverse imports
const diversityMap = new Map();
for (const [key, value] of importMap) {
  const from = value.from;
  if (!diversityMap.has(from)) {
    diversityMap.set(from, new Set());
  }
  diversityMap.get(from).add(value.to);
}

const diversity = Array.from(diversityMap.entries())
  .map(([pkg, deps]) => ({ pkg, diversity: deps.size }))
  .sort((a, b) => b.diversity - a.diversity)
  .slice(0, 10);

console.log('\n🌐 HIGHEST INTEGRATION DIVERSITY (imports from most packages)\n');
diversity.forEach((item, i) => {
  console.log(`${String(i + 1).padStart(2)}. ${item.pkg.padEnd(30)} imports from ${item.diversity} packages`);
});

console.log('\n═'.repeat(80));
console.log('✓ Import pattern analysis complete');
