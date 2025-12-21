#!/usr/bin/env node
/**
 * Dependency Graph Generator for UNRDF Monorepo
 *
 * Generates Mermaid diagram and ASCII tree visualization of package dependencies.
 *
 * Usage:
 *   node scripts/generate-dep-graph.mjs
 *   node scripts/generate-dep-graph.mjs --format mermaid
 *   node scripts/generate-dep-graph.mjs --format ascii
 *
 * Output:
 *   - Mermaid flowchart (paste into GitHub markdown or mermaid.live)
 *   - ASCII tree diagram
 */

import { readFileSync } from 'fs';
import { glob } from 'glob';
import { resolve, dirname } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));
const rootDir = resolve(__dirname, '..');

/**
 * Load all package.json files
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

    const internalDeps = Object.keys(deps).filter(d => d.startsWith('@unrdf/'));
    const internalDevDeps = Object.keys(devDeps).filter(d => d.startsWith('@unrdf/'));

    packages[pkgName] = {
      dependencies: internalDeps,
      devDependencies: internalDevDeps,
      totalInternal: internalDeps.length + internalDevDeps.length,
    };
  }

  return packages;
}

/**
 * Generate Mermaid flowchart
 */
function generateMermaid(packages) {
  const lines = ['```mermaid', 'graph TD'];

  // Define nodes with categories
  const leafPackages = [];
  const midLevelPackages = [];
  const hubPackages = [];

  for (const [pkg, data] of Object.entries(packages)) {
    const shortName = pkg.replace('@unrdf/', '');
    const count = data.totalInternal;

    if (count === 0) {
      leafPackages.push(pkg);
      lines.push(`  ${shortName}[${shortName}]:::leaf`);
    } else if (count <= 3) {
      midLevelPackages.push(pkg);
      lines.push(`  ${shortName}[${shortName}]:::mid`);
    } else {
      hubPackages.push(pkg);
      lines.push(`  ${shortName}[${shortName}]:::hub`);
    }
  }

  lines.push('');

  // Define edges
  for (const [pkg, data] of Object.entries(packages)) {
    const shortName = pkg.replace('@unrdf/', '');

    // Runtime dependencies (solid arrows)
    for (const dep of data.dependencies) {
      const depShort = dep.replace('@unrdf/', '');
      lines.push(`  ${shortName} --> ${depShort}`);
    }

    // Dev dependencies (dashed arrows)
    for (const dep of data.devDependencies) {
      const depShort = dep.replace('@unrdf/', '');
      lines.push(`  ${shortName} -.-> ${depShort}`);
    }
  }

  lines.push('');

  // Styling
  lines.push('  classDef leaf fill:#e8f5e9,stroke:#4caf50,stroke-width:2px');
  lines.push('  classDef mid fill:#fff3e0,stroke:#ff9800,stroke-width:2px');
  lines.push('  classDef hub fill:#ffebee,stroke:#f44336,stroke-width:2px');

  lines.push('```');

  return lines.join('\n');
}

/**
 * Calculate dependency depth
 */
function getDepth(pkg, packages, visited = new Set()) {
  if (visited.has(pkg)) return 0;
  visited.add(pkg);

  const data = packages[pkg];
  if (!data) return 0;

  const allDeps = [...data.dependencies, ...data.devDependencies];
  if (allDeps.length === 0) return 0;

  return 1 + Math.max(...allDeps.map(d => getDepth(d, packages, new Set(visited))));
}

/**
 * Generate ASCII tree
 */
function generateASCII(packages) {
  const lines = ['UNRDF Package Dependency Tree', '='.repeat(50), ''];

  // Group by depth
  const depths = {};
  for (const pkg of Object.keys(packages)) {
    const depth = getDepth(pkg, packages);
    if (!depths[depth]) depths[depth] = [];
    depths[depth].push(pkg);
  }

  // Sort depths
  const sortedDepths = Object.keys(depths)
    .map(Number)
    .sort((a, b) => a - b);

  for (const depth of sortedDepths) {
    lines.push(`LAYER ${depth} (Depth: ${depth})`);
    lines.push('─'.repeat(50));

    for (const pkg of depths[depth]) {
      const shortName = pkg.replace('@unrdf/', '');
      const data = packages[pkg];

      const depList = [...data.dependencies, ...data.devDependencies].map(d =>
        d.replace('@unrdf/', '')
      );

      if (depList.length === 0) {
        lines.push(`├─ ${shortName} (leaf)`);
      } else {
        lines.push(`├─ ${shortName}`);
        depList.forEach((dep, i) => {
          const isLast = i === depList.length - 1;
          const prefix = isLast ? '   └─→ ' : '   ├─→ ';
          lines.push(`${prefix}${dep}`);
        });
      }
    }

    lines.push('');
  }

  return lines.join('\n');
}

/**
 * Generate statistics summary
 */
function generateStats(packages) {
  const lines = ['Package Statistics', '='.repeat(50), ''];

  const totalPackages = Object.keys(packages).length;
  const leafCount = Object.values(packages).filter(p => p.totalInternal === 0).length;
  const midCount = Object.values(packages).filter(p => p.totalInternal >= 1 && p.totalInternal <= 3)
    .length;
  const hubCount = Object.values(packages).filter(p => p.totalInternal >= 4).length;

  lines.push(`Total Packages: ${totalPackages}`);
  lines.push(`Leaf Packages (0 deps): ${leafCount} (${((leafCount / totalPackages) * 100).toFixed(1)}%)`);
  lines.push(
    `Mid-Level Packages (1-3 deps): ${midCount} (${((midCount / totalPackages) * 100).toFixed(1)}%)`
  );
  lines.push(`Hub Packages (4+ deps): ${hubCount} (${((hubCount / totalPackages) * 100).toFixed(1)}%)`);
  lines.push('');

  // Most depended upon
  const dependedUpon = {};
  for (const data of Object.values(packages)) {
    for (const dep of [...data.dependencies, ...data.devDependencies]) {
      dependedUpon[dep] = (dependedUpon[dep] || 0) + 1;
    }
  }

  const sorted = Object.entries(dependedUpon)
    .sort(([, a], [, b]) => b - a)
    .slice(0, 5);

  lines.push('Most Depended Upon:');
  for (const [pkg, count] of sorted) {
    const shortName = pkg.replace('@unrdf/', '');
    lines.push(`  ${count}x ${shortName}`);
  }

  lines.push('');

  return lines.join('\n');
}

/**
 * Main execution
 */
function main() {
  const args = process.argv.slice(2);
  const format = args.includes('--format')
    ? args[args.indexOf('--format') + 1]
    : 'all';

  const packages = loadPackages();

  if (format === 'mermaid' || format === 'all') {
    console.log('\n' + generateMermaid(packages) + '\n');
  }

  if (format === 'ascii' || format === 'all') {
    console.log(generateASCII(packages));
  }

  if (format === 'stats' || format === 'all') {
    console.log(generateStats(packages));
  }
}

main();
