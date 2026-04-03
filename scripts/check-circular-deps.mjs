#!/usr/bin/env node
/**
 * Circular Dependency Checker for UNRDF Monorepo
 *
 * Detects circular dependencies in package.json files across workspace packages.
 * This catches dev-time circular dependencies (devDependencies) that tools like
 * madge won't detect (madge only analyzes runtime imports).
 *
 * Usage:
 *   node scripts/check-circular-deps.mjs
 *   pnpm run check:circular
 *
 * Exit codes:
 *   0 - No circular dependencies found
 *   1 - Circular dependencies detected
 */

import { readFileSync } from 'fs';
import { glob } from 'glob';
import { resolve, dirname } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));
const rootDir = resolve(__dirname, '..');

/**
 * Load all package.json files from workspace
 */
function loadPackages() {
  const packageFiles = glob.sync('packages/*/package.json', { cwd: rootDir });
  const packages = {};

  for (const file of packageFiles) {
    const fullPath = resolve(rootDir, file);
    const content = JSON.parse(readFileSync(fullPath, 'utf8'));
    const pkgName = content.name;

    if (!pkgName) continue;

    const deps = content.dependencies || {};
    const devDeps = content.devDependencies || {};

    // Extract only @unrdf/* dependencies
    const internalDeps = Object.keys(deps).filter(d => d.startsWith('@unrdf/'));
    const internalDevDeps = Object.keys(devDeps).filter(d => d.startsWith('@unrdf/'));

    packages[pkgName] = {
      path: file,
      dependencies: internalDeps,
      devDependencies: internalDevDeps,
      allDeps: [...internalDeps, ...internalDevDeps],
    };
  }

  return packages;
}

/**
 * Detect circular dependencies using Depth-First Search
 *
 * @param {string} pkg - Package name to start from
 * @param {Object} packages - All packages map
 * @param {Set} visited - Global visited set
 * @param {Set} stack - Current DFS stack
 * @param {Array} path - Current dependency path
 * @returns {Object|null} Circular dependency info or null
 */
function detectCircular(pkg, packages, visited = new Set(), stack = new Set(), path = []) {
  // Cycle detected - current package is already in the stack
  if (stack.has(pkg)) {
    const cycleStart = path.indexOf(pkg);
    const cycle = path.slice(cycleStart).concat(pkg);
    return { cycle, type: 'circular' };
  }

  // Already visited in another branch - no cycle here
  if (visited.has(pkg)) {
    return null;
  }

  visited.add(pkg);
  stack.add(pkg);
  path.push(pkg);

  const pkgInfo = packages[pkg];
  if (!pkgInfo) {
    stack.delete(pkg);
    return null;
  }

  // Check all dependencies (runtime + dev)
  for (const dep of pkgInfo.allDeps) {
    const result = detectCircular(dep, packages, visited, new Set(stack), [...path]);
    if (result) {
      return result;
    }
  }

  stack.delete(pkg);
  return null;
}

/**
 * Find all circular dependencies in the workspace
 */
function findAllCircularDependencies(packages) {
  const cycles = [];
  const seenCycles = new Set();

  for (const pkg of Object.keys(packages)) {
    const result = detectCircular(pkg, packages);

    if (result && result.type === 'circular') {
      // Normalize cycle to avoid duplicates (A→B→A is same as B→A→B)
      const normalized = result.cycle.slice().sort().join('→');

      if (!seenCycles.has(normalized)) {
        seenCycles.add(normalized);
        cycles.push(result.cycle);
      }
    }
  }

  return cycles;
}

/**
 * Get dependency type (runtime, dev, or both)
 */
function getDependencyType(from, to, packages) {
  const pkg = packages[from];
  if (!pkg) return 'unknown';

  const isRuntime = pkg.dependencies.includes(to);
  const isDev = pkg.devDependencies.includes(to);

  if (isRuntime && isDev) return 'both';
  if (isRuntime) return 'runtime';
  if (isDev) return 'dev';
  return 'unknown';
}

/**
 * Format cycle for display
 */
function formatCycle(cycle, packages) {
  const arrows = [];
  for (let i = 0; i < cycle.length - 1; i++) {
    const from = cycle[i];
    const to = cycle[i + 1];
    const type = getDependencyType(from, to, packages);

    let arrow = '→';
    if (type === 'dev') arrow = '⇢ (dev)';
    if (type === 'runtime') arrow = '→ (runtime)';
    if (type === 'both') arrow = '⇔ (both)';

    arrows.push(arrow);
  }

  let result = cycle[0];
  for (let i = 0; i < arrows.length; i++) {
    result += ` ${arrows[i]} ${cycle[i + 1]}`;
  }

  return result;
}

/**
 * Main execution
 */
function main() {
  console.log('🔍 Checking for circular dependencies in UNRDF workspace...\n');

  const packages = loadPackages();
  const packageCount = Object.keys(packages).length;

  console.log(`📦 Found ${packageCount} packages\n`);

  const cycles = findAllCircularDependencies(packages);

  if (cycles.length === 0) {
    console.log('✅ No circular dependencies detected!\n');
    return 0;
  }

  console.log(`❌ Found ${cycles.length} circular dependency cycle(s):\n`);

  cycles.forEach((cycle, index) => {
    console.log(`${index + 1}. ${formatCycle(cycle, packages)}`);
  });

  console.log('\n⚠️  Circular dependencies detected! Please resolve before unification.\n');
  console.log('Recommendations:');
  console.log('  1. Extract shared test utilities to @unrdf/test-utils');
  console.log('  2. Inline minimal test fixtures instead of importing from core');
  console.log('  3. Review package boundaries and layer architecture\n');

  return 1;
}

// Run and exit with appropriate code
const exitCode = main();
process.exit(exitCode);
