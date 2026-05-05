#!/usr/bin/env node
/**
 * @file Layer Violation Detection
 * @description Detects architectural layer violations in dependency graph
 */

import { readFileSync, readdirSync } from 'fs';
import { join } from 'path';

const PACKAGES_DIR = '/home/user/unrdf/packages';

// Layer hierarchy (lower number = lower in stack)
const LAYER_HIERARCHY = {
  L1_INFRASTRUCTURE: 1,
  L2_RDF_CORE: 2,
  L3_KGC: 3,
  L4_KNOWLEDGE_SUBSTRATE: 4,
  L5_APPLICATION: 5,
  YAWL_ECOSYSTEM: 3, // Same level as KGC
  SPECIALIZED: 6 // Can depend on anything
};

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

function findLayer(pkgName) {
  for (const [layer, packages] of Object.entries(LAYERS)) {
    if (packages.includes(pkgName)) return layer;
  }
  return 'UNKNOWN';
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

function detectViolations() {
  const packages = readdirSync(PACKAGES_DIR);
  const violations = [];
  const stats = {
    total: 0,
    byType: {
      upward: 0,      // Lower layer depending on higher layer
      lateral: 0,      // Same-level inappropriate dependency
      crossDomain: 0   // YAWL ↔ KGC cross-dependencies
    }
  };

  for (const pkgName of packages) {
    const pkg = readPackageJson(pkgName);
    if (!pkg) continue;

    const fromLayer = findLayer(pkgName);
    const fromLevel = LAYER_HIERARCHY[fromLayer] || 99;
    const deps = extractInternalDeps(pkg);

    for (const dep of deps) {
      const toLayer = findLayer(dep);
      const toLevel = LAYER_HIERARCHY[toLayer] || 99;

      // Check for upward dependencies (violation)
      if (fromLevel < toLevel && toLayer !== 'SPECIALIZED') {
        violations.push({
          type: 'upward',
          from: pkgName,
          to: dep,
          fromLayer,
          toLayer,
          severity: toLevel - fromLevel
        });
        stats.byType.upward++;
        stats.total++;
      }

      // Check for cross-domain (YAWL ↔ KGC at same level)
      if (fromLayer === 'YAWL_ECOSYSTEM' && toLayer === 'L3_KGC') {
        violations.push({
          type: 'crossDomain',
          from: pkgName,
          to: dep,
          fromLayer,
          toLayer,
          severity: 1
        });
        stats.byType.crossDomain++;
        stats.total++;
      }
    }
  }

  return { violations, stats };
}

function analyzeVersions() {
  const packages = readdirSync(PACKAGES_DIR);
  const versions = new Map();

  for (const pkgName of packages) {
    const pkg = readPackageJson(pkgName);
    if (!pkg) continue;

    versions.set(pkgName, pkg.version || '0.0.0');
  }

  // Group by version
  const versionGroups = new Map();
  for (const [pkg, version] of versions) {
    if (!versionGroups.has(version)) {
      versionGroups.set(version, []);
    }
    versionGroups.get(version).push(pkg);
  }

  return { versions, versionGroups };
}

console.log('🔍 LAYER VIOLATION ANALYSIS\n');
console.log('═'.repeat(80));

const { violations, stats } = detectViolations();

console.log('\n📊 VIOLATION STATISTICS\n');
console.log(`Total Violations: ${stats.total}`);
console.log(`  Upward Dependencies (Lower → Higher): ${stats.byType.upward}`);
console.log(`  Cross-Domain (YAWL ↔ KGC): ${stats.byType.crossDomain}`);

if (violations.length > 0) {
  console.log('\n⚠️  DETECTED VIOLATIONS\n');

  const sortedViolations = violations.sort((a, b) => b.severity - a.severity);

  sortedViolations.forEach((v, i) => {
    const severity = v.severity > 2 ? '🔴 CRITICAL' :
                     v.severity > 1 ? '🟡 WARNING' : '🟢 MINOR';
    console.log(`${String(i + 1).padStart(2)}. ${severity} ${v.type.toUpperCase()}`);
    console.log(`    ${v.from} [${v.fromLayer}] → ${v.to} [${v.toLayer}]`);
  });
} else {
  console.log('\n✅ No layer violations detected');
}

console.log('\n🏗️  LAYER DEPENDENCY RULES\n');
console.log('Allowed dependency directions (lower → higher):');
console.log('  L1 (Infrastructure) ← L2 (RDF Core) ← L3 (KGC) ← L4 (Knowledge) ← L5 (App)');
console.log('  YAWL Ecosystem (parallel to L3) should minimize KGC dependencies');
console.log('  SPECIALIZED can depend on anything');

const { versions, versionGroups } = analyzeVersions();

console.log('\n📦 VERSION COMPATIBILITY MATRIX\n');
console.log(`Unique Versions: ${versionGroups.size}`);
console.log(`Total Packages: ${versions.size}`);

const sortedVersions = Array.from(versionGroups.entries())
  .sort((a, b) => b[1].length - a[1].length);

console.log('\nVersion Distribution:');
sortedVersions.forEach(([version, packages]) => {
  console.log(`  v${version.padEnd(12)} ${String(packages.length).padStart(2)} packages`);
  if (packages.length <= 5) {
    packages.forEach(pkg => console.log(`    - ${pkg}`));
  }
});

// Calculate version compatibility matrix size
const depCount = violations.length + stats.total;
console.log('\n💥 COMPATIBILITY MATRIX EXPLOSION\n');
console.log(`Theoretical Version Combinations: ${Math.pow(versionGroups.size, versions.size)}`);
console.log(`Actual Dependency Edges: ${depCount}`);
console.log(`Per-Version Compatibility Tests Needed: ${depCount * versionGroups.size}`);

console.log('\n═'.repeat(80));
console.log('✓ Layer violation analysis complete');
