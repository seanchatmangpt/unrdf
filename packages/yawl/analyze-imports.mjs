#!/usr/bin/env node
/**
 * Import Graph Analyzer - Detects circular dependencies in YAWL package
 * @fileoverview Maps dependency graph and identifies circular imports
 */

import { readFileSync, readdirSync, statSync } from 'fs';
import { join, dirname, resolve, relative } from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

/**
 * Parse all import statements from a file
 * @param {string} filePath - Absolute path to file
 * @returns {string[]} - Array of import paths
 */
function extractImports(filePath) {
  try {
    const content = readFileSync(filePath, 'utf8');
    const imports = [];

    // Match: import ... from 'path'
    const importRegex = /import\s+(?:(?:\{[^}]*\}|\*\s+as\s+\w+|\w+)\s*,?\s*)*from\s+['"]([^'"]+)['"]/g;
    let match;

    while ((match = importRegex.exec(content)) !== null) {
      imports.push(match[1]);
    }

    // Match: export ... from 'path'
    const exportFromRegex = /export\s+(?:\{[^}]*\}|\*)\s+from\s+['"]([^'"]+)['"]/g;
    while ((match = exportFromRegex.exec(content)) !== null) {
      imports.push(match[1]);
    }

    return imports;
  } catch (error) {
    console.error(`Error reading ${filePath}: ${error.message}`);
    return [];
  }
}

/**
 * Resolve relative import to absolute path
 * @param {string} importPath - Import path (relative or package)
 * @param {string} fromFile - File doing the importing
 * @param {string} srcDir - Source directory root
 * @returns {string|null} - Resolved absolute path or null if external
 */
function resolveImport(importPath, fromFile, srcDir) {
  // Skip external packages
  if (!importPath.startsWith('.')) {
    return null;
  }

  const fromDir = dirname(fromFile);
  let resolved = resolve(fromDir, importPath);

  // Add .mjs extension if missing
  if (!resolved.endsWith('.mjs')) {
    resolved += '.mjs';
  }

  // Only return if within src directory
  if (resolved.startsWith(srcDir)) {
    return resolved;
  }

  return null;
}

/**
 * Find all .mjs files recursively
 * @param {string} dir - Directory to search
 * @returns {string[]} - Array of absolute file paths
 */
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
        files.push(fullPath);
      }
    }
  }

  traverse(dir);
  return files;
}

/**
 * Build dependency graph
 * @param {string} srcDir - Source directory
 * @returns {Map<string, Set<string>>} - Adjacency list (file -> dependencies)
 */
function buildDependencyGraph(srcDir) {
  const files = findMjsFiles(srcDir);
  const graph = new Map();

  for (const file of files) {
    const imports = extractImports(file);
    const dependencies = new Set();

    for (const imp of imports) {
      const resolved = resolveImport(imp, file, srcDir);
      if (resolved) {
        dependencies.add(resolved);
      }
    }

    graph.set(file, dependencies);
  }

  return graph;
}

/**
 * Detect circular dependencies using DFS with cycle deduplication
 * @param {Map<string, Set<string>>} graph - Dependency graph
 * @returns {Array<string[]>} - Array of unique cycles
 */
function detectCircularDependencies(graph) {
  const WHITE = 0;
  const GRAY = 1;
  const BLACK = 2;

  const colors = new Map();
  const parent = new Map();
  const cyclesSet = new Set();

  // Initialize all nodes as WHITE (unvisited)
  for (const node of graph.keys()) {
    colors.set(node, WHITE);
  }

  function dfsVisit(u) {
    colors.set(u, GRAY);

    const dependencies = graph.get(u) || new Set();

    for (const v of dependencies) {
      if (!colors.has(v)) continue; // Skip if not in graph

      if (colors.get(v) === GRAY) {
        // Back edge found - cycle detected
        const cycle = [];
        let curr = u;
        while (curr !== v && curr) {
          cycle.push(curr);
          curr = parent.get(curr);
        }
        cycle.push(v);
        cycle.reverse();

        // Normalize cycle for deduplication (start with lexicographically smallest)
        const normalized = normalizeCycle(cycle);
        cyclesSet.add(normalized);
      } else if (colors.get(v) === WHITE) {
        parent.set(v, u);
        dfsVisit(v);
      }
    }

    colors.set(u, BLACK);
  }

  for (const node of graph.keys()) {
    if (colors.get(node) === WHITE) {
      dfsVisit(node);
    }
  }

  return Array.from(cyclesSet).map(s => s.split('|||'));
}

/**
 * Normalize cycle for deduplication
 * @param {string[]} cycle - Array of file paths
 * @returns {string} - Normalized cycle string
 */
function normalizeCycle(cycle) {
  // Find minimum element index
  let minIdx = 0;
  for (let i = 1; i < cycle.length; i++) {
    if (cycle[i] < cycle[minIdx]) {
      minIdx = i;
    }
  }

  // Rotate to start with minimum element
  const rotated = [...cycle.slice(minIdx), ...cycle.slice(0, minIdx)];
  return rotated.join('|||');
}

/**
 * Format file path relative to src/
 * @param {string} filePath - Absolute file path
 * @param {string} srcDir - Source directory
 * @returns {string} - Relative path
 */
function formatPath(filePath, srcDir) {
  return 'src/' + relative(srcDir, filePath);
}

/**
 * Main analysis function
 */
function analyzeImports() {
  const srcDir = resolve(__dirname, 'src');

  console.log('🔍 YAWL Import Graph Analysis\n');
  console.log(`Analyzing: ${srcDir}\n`);

  // Build graph
  const graph = buildDependencyGraph(srcDir);
  console.log(`📊 Total modules: ${graph.size}`);

  // Calculate statistics
  let totalDeps = 0;
  let maxDeps = 0;
  let maxDepsFile = '';

  for (const [file, deps] of graph.entries()) {
    totalDeps += deps.size;
    if (deps.size > maxDeps) {
      maxDeps = deps.size;
      maxDepsFile = file;
    }
  }

  const avgDeps = (totalDeps / graph.size).toFixed(2);
  console.log(`📈 Total imports: ${totalDeps}`);
  console.log(`📊 Average imports per module: ${avgDeps}`);
  console.log(`📌 Max imports: ${maxDeps} in ${formatPath(maxDepsFile, srcDir)}\n`);

  // Detect cycles
  console.log('🔄 Detecting circular dependencies...\n');
  const cycles = detectCircularDependencies(graph);

  if (cycles.length === 0) {
    console.log('✅ No circular dependencies found!\n');
  } else {
    console.log(`❌ Found ${cycles.length} circular dependency chain(s):\n`);

    cycles.forEach((cycle, idx) => {
      console.log(`\n🔴 Cycle #${idx + 1}:`);
      cycle.forEach((file, i) => {
        const arrow = i < cycle.length - 1 ? ' →' : '';
        console.log(`  ${formatPath(file, srcDir)}${arrow}`);
      });
    });

    console.log('\n');
  }

  // Output dependency map for top modules
  console.log('📦 Top 10 Modules by Import Count:\n');
  const sortedByDeps = Array.from(graph.entries())
    .sort((a, b) => b[1].size - a[1].size)
    .slice(0, 10);

  sortedByDeps.forEach(([file, deps]) => {
    console.log(`${formatPath(file, srcDir)} (${deps.size} imports)`);
    if (deps.size > 0 && deps.size <= 5) {
      // Show imports for modules with few dependencies
      for (const dep of deps) {
        console.log(`  → ${formatPath(dep, srcDir)}`);
      }
    }
  });

  console.log('\n');

  // Return results for programmatic use
  return {
    totalModules: graph.size,
    totalImports: totalDeps,
    avgImports: parseFloat(avgDeps),
    cycles: cycles.length,
    graph
  };
}

// Run analysis
const results = analyzeImports();

// Exit with error code if cycles found
if (results.cycles > 0) {
  process.exit(1);
}
