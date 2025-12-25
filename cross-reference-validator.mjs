#!/usr/bin/env node
/**
 * Cross-Reference & Import Integrity Validator
 * Adversarial testing tool - VERIFY all imports/exports
 */

import { readFileSync, existsSync, readdirSync, statSync } from 'fs';
import { resolve, dirname, join, extname } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));

// Get changed files from git
const changedFilesRaw = `docs/agents/reference/implementation.mjs
max-combo-10-mega-framework-standalone.mjs
max-combo-10-mega-framework.mjs
microfw-9-graph-routing.mjs
packages/atomvm/src/app.mjs
packages/atomvm/test/service-worker-manager.test.mjs
packages/atomvm/vitest.browser.config.mjs
packages/cli/examples/validate-cli.mjs
packages/core/examples/production-rdf-pipeline.mjs
packages/federation/src/federation/coordinator.mjs
packages/federation/src/federation/data-replication.mjs
packages/federation/src/federation/distributed-query-engine.mjs
packages/federation/src/federation/federation-coordinator.mjs
packages/federation/src/federation/peer-manager.mjs
packages/federation/src/index.mjs
packages/federation/test/federation.test.mjs
packages/hooks/examples/validate-hooks.mjs
packages/oxigraph/examples/production-benchmark.mjs
packages/streaming/examples/validate-streaming.mjs
packages/streaming/src/streaming/change-feed.mjs
packages/streaming/src/streaming/real-time-validator.mjs
packages/streaming/src/streaming/stream-processor.mjs
packages/streaming/src/streaming/subscription-manager.mjs
packages/streaming/test/streaming.test.mjs
packages/yawl/examples/resource-allocation.mjs
packages/yawl/src/api/workflow-api.mjs
packages/yawl/src/cancellation/index.mjs
packages/yawl/src/cancellation/yawl-cancellation.mjs
packages/yawl/src/case.mjs
packages/yawl/src/engine.mjs
packages/yawl/src/events/yawl-events.mjs
packages/yawl/src/hooks/yawl-hooks.mjs
packages/yawl/src/index.mjs
packages/yawl/src/ontology/yawl-ontology.mjs
packages/yawl/src/patterns.mjs
packages/yawl/src/receipt.mjs
packages/yawl/src/resource.mjs
packages/yawl/src/resources/yawl-resources.mjs
packages/yawl/src/store/yawl-store.mjs
packages/yawl/src/task.mjs
packages/yawl/src/types/yawl-schemas.mjs
packages/yawl/src/types/yawl-types.mjs
packages/yawl/src/workflow.mjs
packages/yawl/test/cancellation.test.mjs
packages/yawl/test/receipt.test.mjs
packages/yawl/test/workflow-api.test.mjs
packages/yawl/test/yawl-events.test.mjs
packages/yawl/test/yawl-hooks.test.mjs
packages/yawl/test/yawl-patterns.test.mjs
packages/yawl/test/yawl-resources.test.mjs
packages/yawl/test/yawl.test.mjs
packages/yawl/validation/press-release-validation.mjs
packages/yawl/vitest.config.mjs`.split('\n');

const results = {
  totalFiles: 0,
  totalImports: 0,
  totalExports: 0,
  brokenImports: [],
  brokenExports: [],
  deadExports: [],
  validImports: 0,
  validExports: 0,
  importGraph: new Map(),
  exportGraph: new Map(),
  packageDeps: new Map(),
  usedPackages: new Set(),
  unusedPackages: new Set(),
};

/**
 * Extract imports from source code
 */
function extractImports(source, filePath) {
  const imports = [];

  // Match: import ... from 'path'
  const importRegex = /import\s+(?:(?:\{[^}]*\}|\*\s+as\s+\w+|\w+)(?:\s*,\s*(?:\{[^}]*\}|\*\s+as\s+\w+|\w+))*\s+from\s+)?['"]([^'"]+)['"]/g;

  let match;
  let lineNumber = 1;
  const lines = source.split('\n');

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    const lineImports = line.matchAll(/import\s+(?:(?:\{[^}]*\}|\*\s+as\s+\w+|\w+)(?:\s*,\s*(?:\{[^}]*\}|\*\s+as\s+\w+|\w+))*\s+from\s+)?['"]([^'"]+)['"]/g);

    for (const match of lineImports) {
      const importPath = match[1];
      imports.push({
        path: importPath,
        line: i + 1,
        raw: line.trim(),
        isRelative: importPath.startsWith('.') || importPath.startsWith('/'),
        isPackage: !importPath.startsWith('.') && !importPath.startsWith('/'),
      });
    }
  }

  return imports;
}

/**
 * Extract exports from source code
 */
function extractExports(source, filePath) {
  const exports = [];
  const lines = source.split('\n');

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];

    // export function/class/const/let/var
    if (line.match(/export\s+(async\s+)?(function|class|const|let|var)\s+(\w+)/)) {
      const match = line.match(/export\s+(async\s+)?(function|class|const|let|var)\s+(\w+)/);
      exports.push({
        name: match[3],
        type: match[2],
        line: i + 1,
        raw: line.trim(),
      });
    }

    // export { ... }
    if (line.match(/export\s+\{([^}]+)\}/)) {
      const match = line.match(/export\s+\{([^}]+)\}/);
      const names = match[1].split(',').map(n => n.trim().split(/\s+as\s+/)[0]);
      names.forEach(name => {
        exports.push({
          name: name.trim(),
          type: 'named',
          line: i + 1,
          raw: line.trim(),
        });
      });
    }

    // export default
    if (line.match(/export\s+default/)) {
      exports.push({
        name: 'default',
        type: 'default',
        line: i + 1,
        raw: line.trim(),
      });
    }
  }

  return exports;
}

/**
 * Resolve relative import path
 */
function resolveImportPath(importPath, fromFile) {
  const baseDir = dirname(resolve(__dirname, fromFile));
  let targetPath = resolve(baseDir, importPath);

  // Try with .mjs extension if not present
  if (!existsSync(targetPath)) {
    if (!extname(targetPath)) {
      targetPath = targetPath + '.mjs';
    }
  }

  // Try index.mjs if it's a directory
  if (existsSync(targetPath) && statSync(targetPath).isDirectory()) {
    targetPath = join(targetPath, 'index.mjs');
  }

  return targetPath;
}

/**
 * Load package.json dependencies
 */
function loadPackageDeps(packagePath) {
  try {
    const pkg = JSON.parse(readFileSync(packagePath, 'utf8'));
    const deps = new Set([
      ...Object.keys(pkg.dependencies || {}),
      ...Object.keys(pkg.devDependencies || {}),
      ...Object.keys(pkg.peerDependencies || {}),
    ]);
    return deps;
  } catch {
    return new Set();
  }
}

/**
 * Find nearest package.json
 */
function findPackageJson(fromFile) {
  let dir = dirname(resolve(__dirname, fromFile));

  while (dir !== '/' && !dir.endsWith('unrdf')) {
    const pkgPath = join(dir, 'package.json');
    if (existsSync(pkgPath)) {
      return pkgPath;
    }
    dir = dirname(dir);
  }

  // Try root
  const rootPkg = join(__dirname, 'package.json');
  return existsSync(rootPkg) ? rootPkg : null;
}

/**
 * Validate all imports and exports
 */
function validateFile(filePath) {
  const fullPath = resolve(__dirname, filePath);

  if (!existsSync(fullPath)) {
    console.error(`❌ File not found: ${filePath}`);
    return;
  }

  results.totalFiles++;

  const source = readFileSync(fullPath, 'utf8');
  const imports = extractImports(source, filePath);
  const exports = extractExports(source, filePath);

  results.totalImports += imports.length;
  results.totalExports += exports.length;

  // Validate imports
  imports.forEach(imp => {
    if (imp.isRelative) {
      const targetPath = resolveImportPath(imp.path, filePath);

      if (!existsSync(targetPath)) {
        results.brokenImports.push({
          file: filePath,
          line: imp.line,
          import: imp.path,
          resolved: targetPath,
          error: 'File not found',
        });
      } else {
        results.validImports++;

        // Track in graph
        if (!results.importGraph.has(filePath)) {
          results.importGraph.set(filePath, []);
        }
        results.importGraph.get(filePath).push(targetPath);
      }
    } else if (imp.isPackage) {
      const packageName = imp.path.split('/')[0];
      results.usedPackages.add(packageName);

      // Find package.json and verify
      const pkgPath = findPackageJson(filePath);
      if (pkgPath) {
        const deps = loadPackageDeps(pkgPath);

        if (!results.packageDeps.has(pkgPath)) {
          results.packageDeps.set(pkgPath, deps);
        }

        if (!deps.has(packageName) && !packageName.startsWith('@unrdf/')) {
          results.brokenImports.push({
            file: filePath,
            line: imp.line,
            import: imp.path,
            package: packageName,
            error: `Package not in package.json: ${pkgPath}`,
          });
        } else {
          results.validImports++;
        }
      }
    }
  });

  // Track exports
  exports.forEach(exp => {
    if (!results.exportGraph.has(filePath)) {
      results.exportGraph.set(filePath, []);
    }
    results.exportGraph.get(filePath).push(exp);
    results.validExports++;
  });
}

/**
 * Find dead exports (never imported)
 */
function findDeadExports() {
  const allImportedFiles = new Set();

  for (const [file, imports] of results.importGraph.entries()) {
    imports.forEach(imp => allImportedFiles.add(imp));
  }

  for (const [file, exports] of results.exportGraph.entries()) {
    const fullPath = resolve(__dirname, file);

    // If file is never imported, all exports are dead (except entry points)
    if (!allImportedFiles.has(fullPath) &&
        !file.includes('index.mjs') &&
        !file.includes('examples/') &&
        !file.includes('test/')) {
      results.deadExports.push({
        file,
        exports: exports.map(e => e.name),
        reason: 'File never imported',
      });
    }
  }
}

/**
 * Find unused package.json dependencies
 */
function findUnusedPackages() {
  for (const [pkgPath, deps] of results.packageDeps.entries()) {
    deps.forEach(dep => {
      if (!results.usedPackages.has(dep) &&
          !dep.startsWith('@types/') &&
          !dep.startsWith('@unrdf/') &&
          dep !== 'vitest' &&
          dep !== 'typescript') {
        results.unusedPackages.add(dep);
      }
    });
  }
}

/**
 * Generate report
 */
function generateReport() {
  console.log('\n' + '='.repeat(80));
  console.log('📊 CROSS-REFERENCE & IMPORT INTEGRITY VALIDATION REPORT');
  console.log('='.repeat(80) + '\n');

  console.log('## Summary\n');
  console.log(`✅ Files Analyzed: ${results.totalFiles}`);
  console.log(`✅ Total Imports: ${results.totalImports}`);
  console.log(`✅ Total Exports: ${results.totalExports}`);
  console.log(`✅ Valid Imports: ${results.validImports}`);
  console.log(`✅ Valid Exports: ${results.validExports}`);
  console.log(`❌ Broken Imports: ${results.brokenImports.length}`);
  console.log(`⚠️  Dead Exports: ${results.deadExports.length}`);
  console.log(`⚠️  Unused Packages: ${results.unusedPackages.size}\n`);

  // Broken imports (CRITICAL)
  if (results.brokenImports.length > 0) {
    console.log('## ❌ BROKEN IMPORTS (CRITICAL)\n');
    results.brokenImports.forEach((broken, idx) => {
      console.log(`${idx + 1}. ${broken.file}:${broken.line}`);
      console.log(`   Import: "${broken.import}"`);
      console.log(`   Error: ${broken.error}`);
      if (broken.resolved) {
        console.log(`   Resolved to: ${broken.resolved}`);
      }
      console.log('');
    });
  } else {
    console.log('## ✅ NO BROKEN IMPORTS\n');
  }

  // Dead exports
  if (results.deadExports.length > 0) {
    console.log('## ⚠️  DEAD EXPORTS (Never Imported)\n');
    results.deadExports.slice(0, 10).forEach((dead, idx) => {
      console.log(`${idx + 1}. ${dead.file}`);
      console.log(`   Exports: ${dead.exports.join(', ')}`);
      console.log(`   Reason: ${dead.reason}\n`);
    });

    if (results.deadExports.length > 10) {
      console.log(`   ... and ${results.deadExports.length - 10} more\n`);
    }
  } else {
    console.log('## ✅ NO DEAD EXPORTS DETECTED\n');
  }

  // Unused packages
  if (results.unusedPackages.size > 0) {
    console.log('## ⚠️  POTENTIALLY UNUSED PACKAGES\n');
    console.log([...results.unusedPackages].join(', ') + '\n');
  }

  // Import graph stats
  console.log('## 📈 Import Graph Statistics\n');
  console.log(`Files with imports: ${results.importGraph.size}`);
  console.log(`Files with exports: ${results.exportGraph.size}`);
  console.log(`Unique packages used: ${results.usedPackages.size}`);
  console.log(`Package.json files scanned: ${results.packageDeps.size}\n`);

  // Most imported files (top 10)
  const importCounts = new Map();
  for (const [file, imports] of results.importGraph.entries()) {
    imports.forEach(imp => {
      importCounts.set(imp, (importCounts.get(imp) || 0) + 1);
    });
  }

  const topImports = [...importCounts.entries()]
    .sort((a, b) => b[1] - a[1])
    .slice(0, 10);

  if (topImports.length > 0) {
    console.log('## 🔥 Most Imported Files (Top 10)\n');
    topImports.forEach(([file, count], idx) => {
      const relPath = file.replace(__dirname + '/', '');
      console.log(`${idx + 1}. ${relPath} (${count} imports)`);
    });
    console.log('');
  }

  // Final verdict
  console.log('='.repeat(80));
  if (results.brokenImports.length === 0) {
    console.log('✅ VALIDATION PASSED: All imports are valid');
  } else {
    console.log(`❌ VALIDATION FAILED: ${results.brokenImports.length} broken imports found`);
  }
  console.log('='.repeat(80) + '\n');

  // Exit code
  process.exit(results.brokenImports.length > 0 ? 1 : 0);
}

// Main execution
console.log('🔍 Starting cross-reference validation...\n');

changedFilesRaw.forEach(file => {
  validateFile(file.trim());
});

findDeadExports();
findUnusedPackages();
generateReport();
