#!/usr/bin/env node
/**
 * @file Cross-Package Integration Matrix Analysis
 * @description Analyzes combinatorial complexity in 56-package monorepo
 */

import { readFileSync, readdirSync, statSync } from 'fs';
import { join } from 'path';

const PACKAGES_DIR = '/home/user/unrdf/packages';

// Layer definitions from CLAUDE.md
const LAYERS = {
  L5_APPLICATION: ['cli', 'react', 'nextra'],
  L4_KNOWLEDGE_SUBSTRATE: ['hooks', 'federation', 'streaming', 'knowledge-engine'],
  L3_KGC: ['kgc-4d', 'kgc-runtime', 'kgc-substrate', 'receipts', 'v6-core', 'v6-compat',
           'kgc-cli', 'kgc-claude', 'kgc-probe', 'kgc-multiverse', 'kgc-docs', 'kgc-swarm', 'kgc-tools'],
  L2_RDF_CORE: ['core', 'oxigraph', 'rdf-graphql', 'semantic-search'],
  L1_INFRASTRUCTURE: ['consensus', 'observability', 'blockchain', 'daemon'],
  YAWL_ECOSYSTEM: ['yawl', 'yawl-ai', 'yawl-api', 'yawl-durable', 'yawl-kafka',
                    'yawl-langchain', 'yawl-observability', 'yawl-queue', 'yawl-realtime', 'yawl-viz'],
  SPECIALIZED: ['graph-analytics', 'ml-inference', 'ml-versioning', 'serverless',
                'caching', 'validation', 'test-utils', 'composables', 'collab',
                'fusion', 'dark-matter', 'decision-fabric', 'domain', 'project-engine',
                'atomvm', 'engine-gateway', 'kgn', 'integration-tests', 'docs', 'diataxis-kit']
};

function getAllPackages() {
  return readdirSync(PACKAGES_DIR).filter(name => {
    const path = join(PACKAGES_DIR, name);
    return statSync(path).isDirectory();
  });
}

function readPackageJson(pkgName) {
  try {
    const path = join(PACKAGES_DIR, pkgName, 'package.json');
    return JSON.parse(readFileSync(path, 'utf-8'));
  } catch {
    return null;
  }
}

function extractInternalDeps(pkg) {
  const deps = new Set();
  const allDeps = {
    ...pkg.dependencies,
    ...pkg.devDependencies,
    ...pkg.peerDependencies
  };

  for (const [name] of Object.entries(allDeps || {})) {
    if (name.startsWith('@unrdf/')) {
      deps.add(name.replace('@unrdf/', ''));
    }
  }

  return Array.from(deps);
}

function buildDependencyGraph() {
  const packages = getAllPackages();
  const graph = new Map();

  for (const pkgName of packages) {
    const pkg = readPackageJson(pkgName);
    if (!pkg) continue;

    graph.set(pkgName, {
      name: pkgName,
      fullName: pkg.name,
      version: pkg.version,
      dependencies: extractInternalDeps(pkg),
      layer: findLayer(pkgName)
    });
  }

  return graph;
}

function findLayer(pkgName) {
  for (const [layer, packages] of Object.entries(LAYERS)) {
    if (packages.includes(pkgName)) return layer;
  }
  return 'UNKNOWN';
}

function calculateGraphStats(graph) {
  const stats = {
    totalPackages: graph.size,
    totalEdges: 0,
    inDegree: new Map(),
    outDegree: new Map(),
    maxDepth: 0,
    circularDeps: []
  };

  // Initialize degrees
  for (const [pkgName] of graph) {
    stats.inDegree.set(pkgName, 0);
    stats.outDegree.set(pkgName, 0);
  }

  // Calculate degrees
  for (const [pkgName, info] of graph) {
    stats.outDegree.set(pkgName, info.dependencies.length);
    stats.totalEdges += info.dependencies.length;

    for (const dep of info.dependencies) {
      stats.inDegree.set(dep, (stats.inDegree.get(dep) || 0) + 1);
    }
  }

  return stats;
}

function findCircularDeps(graph, node, visited = new Set(), path = []) {
  if (path.includes(node)) {
    return [path.slice(path.indexOf(node)).concat(node)];
  }

  if (visited.has(node)) return [];

  visited.add(node);
  path.push(node);

  const circles = [];
  const info = graph.get(node);
  if (info) {
    for (const dep of info.dependencies) {
      circles.push(...findCircularDeps(graph, dep, visited, [...path]));
    }
  }

  return circles;
}

function calculateDepth(graph, node, memo = new Map(), visiting = new Set()) {
  if (memo.has(node)) return memo.get(node);
  if (visiting.has(node)) return 0; // Circular dependency, return 0

  visiting.add(node);

  const info = graph.get(node);
  if (!info || info.dependencies.length === 0) {
    memo.set(node, 0);
    visiting.delete(node);
    return 0;
  }

  let maxDepth = 0;
  for (const dep of info.dependencies) {
    const depth = calculateDepth(graph, dep, memo, visiting);
    maxDepth = Math.max(maxDepth, depth + 1);
  }

  memo.set(node, maxDepth);
  visiting.delete(node);
  return maxDepth;
}

function analyzeLayerInteractions(graph) {
  const interactions = new Map();

  for (const [pkgName, info] of graph) {
    const fromLayer = info.layer;

    for (const dep of info.dependencies) {
      const depInfo = graph.get(dep);
      if (!depInfo) continue;

      const toLayer = depInfo.layer;
      const key = `${fromLayer} → ${toLayer}`;

      interactions.set(key, (interactions.get(key) || 0) + 1);
    }
  }

  return interactions;
}

function analyzeHubs(graph, stats) {
  const packages = Array.from(graph.keys());

  // Sort by out-degree (most dependencies)
  const mostDependent = packages
    .map(pkg => ({ pkg, degree: stats.outDegree.get(pkg) }))
    .sort((a, b) => b.degree - a.degree)
    .slice(0, 10);

  // Sort by in-degree (most dependents)
  const mostUsed = packages
    .map(pkg => ({ pkg, degree: stats.inDegree.get(pkg) }))
    .sort((a, b) => b.degree - a.degree)
    .slice(0, 10);

  return { mostDependent, mostUsed };
}

function countAPIExports(pkgName) {
  try {
    const srcPath = join(PACKAGES_DIR, pkgName, 'src');
    if (!statSync(srcPath).isDirectory()) return 0;

    const indexPath = join(srcPath, 'index.mjs');
    try {
      const content = readFileSync(indexPath, 'utf-8');
      const exportMatches = content.match(/export\s+(function|class|const|let|var|\{|\*)/g);
      return exportMatches ? exportMatches.length : 0;
    } catch {
      return 0;
    }
  } catch {
    return 0;
  }
}

// Main analysis
console.log('🔍 UNRDF Cross-Package Integration Matrix Analysis\n');
console.log('═'.repeat(80));

const graph = buildDependencyGraph();
const stats = calculateGraphStats(graph);

console.log('\n📊 DEPENDENCY GRAPH STATISTICS\n');
console.log(`Total Packages: ${stats.totalPackages}`);
console.log(`Total Dependency Edges: ${stats.totalEdges}`);
console.log(`Average Dependencies per Package: ${(stats.totalEdges / stats.totalPackages).toFixed(2)}`);
console.log(`Potential Combinations (N×N): ${stats.totalPackages * stats.totalPackages}`);

// Calculate max depth
const depths = new Map();
for (const [pkgName] of graph) {
  depths.set(pkgName, calculateDepth(graph, pkgName));
}
const maxDepth = Math.max(...depths.values());
console.log(`Maximum Dependency Depth: ${maxDepth}`);

// Hub analysis
const hubs = analyzeHubs(graph, stats);

console.log('\n🎯 MOST CONNECTED PACKAGES (Hubs)\n');
console.log('Most Dependencies (Consumers):');
hubs.mostDependent.forEach((item, i) => {
  const layer = graph.get(item.pkg)?.layer || 'UNKNOWN';
  console.log(`  ${i + 1}. ${item.pkg.padEnd(25)} ${item.degree} deps   [${layer}]`);
});

console.log('\nMost Dependents (Providers):');
hubs.mostUsed.forEach((item, i) => {
  const layer = graph.get(item.pkg)?.layer || 'UNKNOWN';
  console.log(`  ${i + 1}. ${item.pkg.padEnd(25)} ${item.degree} rdeps  [${layer}]`);
});

// Layer interactions
console.log('\n🏗️  LAYER INTERACTION HEAT MAP\n');
const layerInteractions = analyzeLayerInteractions(graph);
const sortedInteractions = Array.from(layerInteractions.entries())
  .sort((a, b) => b[1] - a[1]);

for (const [interaction, count] of sortedInteractions) {
  const bar = '█'.repeat(Math.min(count, 50));
  console.log(`${interaction.padEnd(60)} ${bar} ${count}`);
}

// Circular dependencies
console.log('\n🔄 CIRCULAR DEPENDENCY DETECTION\n');
const visited = new Set();
const allCircles = [];
for (const [pkgName] of graph) {
  if (!visited.has(pkgName)) {
    const circles = findCircularDeps(graph, pkgName);
    allCircles.push(...circles);
    visited.add(pkgName);
  }
}

if (allCircles.length === 0) {
  console.log('✓ No circular dependencies detected');
} else {
  console.log(`⚠ Found ${allCircles.length} circular dependency chains:`);
  allCircles.slice(0, 10).forEach(circle => {
    console.log(`  ${circle.join(' → ')}`);
  });
}

// API Surface
console.log('\n📐 API SURFACE COMPLEXITY\n');
let totalExports = 0;
const exportCounts = [];
for (const [pkgName] of graph) {
  const count = countAPIExports(pkgName);
  totalExports += count;
  if (count > 0) {
    exportCounts.push({ pkg: pkgName, exports: count });
  }
}

exportCounts.sort((a, b) => b.exports - a.exports);
console.log(`Total Exported Functions/Classes: ${totalExports}`);
console.log(`Average Exports per Package: ${(totalExports / stats.totalPackages).toFixed(2)}`);
console.log(`\nTop API Surface Area:`);
exportCounts.slice(0, 10).forEach((item, i) => {
  console.log(`  ${i + 1}. ${item.pkg.padEnd(25)} ${item.exports} exports`);
});

// Integration matrix size
console.log('\n💥 INTEGRATION MATRIX EXPLOSION\n');
const essentialTier = LAYERS.L2_RDF_CORE.length + 3; // core, oxigraph, hooks, etc.
const extendedTier = LAYERS.L4_KNOWLEDGE_SUBSTRATE.length;
const optionalTier = LAYERS.YAWL_ECOSYSTEM.length;

console.log(`Essential Tier Packages: ${essentialTier}`);
console.log(`Extended Tier Packages: ${extendedTier}`);
console.log(`Optional Tier Packages: ${optionalTier}`);
console.log(`Total Specialized: ${LAYERS.SPECIALIZED.length}`);
console.log();
console.log(`Potential Integration Combinations:`);
console.log(`  Essential × Extended: ${essentialTier * extendedTier}`);
console.log(`  Essential × Optional: ${essentialTier * optionalTier}`);
console.log(`  All × All: ${stats.totalPackages * stats.totalPackages}`);
console.log(`  Actual Dependencies: ${stats.totalEdges} (${(stats.totalEdges / (stats.totalPackages * stats.totalPackages) * 100).toFixed(2)}% of potential)`);

// Package tiers
console.log('\n📦 PACKAGE DISTRIBUTION BY LAYER\n');
for (const [layer, packages] of Object.entries(LAYERS)) {
  console.log(`${layer.padEnd(25)} ${packages.length} packages`);
}

console.log('\n═'.repeat(80));
console.log('✓ Analysis complete');
