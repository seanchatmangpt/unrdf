#!/usr/bin/env node

/**
 * @fileoverview Auto-generates capability index and cross-reference matrix
 *
 * Creates:
 * - Master index of all capability maps
 * - Dependency graph visualization
 * - Cross-reference validation report
 * - Package categorization
 *
 * @module scripts/auto-index-capabilities
 */

import { readFile, writeFile, readdir } from 'node:fs/promises';
import { resolve, join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import { existsSync } from 'node:fs';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const ROOT_DIR = resolve(__dirname, '..');
const PACKAGES_DIR = join(ROOT_DIR, 'packages');
const DOCS_DIR = join(ROOT_DIR, 'docs', 'capabilities');

/**
 * Parse command line arguments
 */
function parseArgs() {
  const args = process.argv.slice(2);
  return {
    verbose: args.includes('--verbose') || args.includes('-v'),
  };
}

/**
 * Get all packages with capability maps
 * @returns {Promise<string[]>}
 */
async function getPackagesWithMaps() {
  const entries = await readdir(PACKAGES_DIR, { withFileTypes: true });
  const packages = entries
    .filter(entry => entry.isDirectory() && !entry.name.startsWith('.'))
    .map(entry => entry.name);

  const withMaps = [];
  for (const pkg of packages) {
    const mapPath = join(DOCS_DIR, pkg, 'capability-map.md');
    if (existsSync(mapPath)) {
      withMaps.push(pkg);
    }
  }

  return withMaps.sort();
}

/**
 * Extract metadata from capability map
 * @param {string} packageName
 * @returns {Promise<object|null>}
 */
async function extractMetadata(packageName) {
  const mapPath = join(DOCS_DIR, packageName, 'capability-map.md');

  try {
    const content = await readFile(mapPath, 'utf-8');

    const versionMatch = content.match(/\*\*Version\*\*:\s*([^\s]+)/);
    const statusMatch = content.match(/\*\*Status\*\*:\s*(.+)/);
    const generatedMatch = content.match(/\*\*Generated\*\*:\s*(.+)/);
    const descMatch = content.match(/## Overview\n\n(.+)/);

    // Extract internal dependencies
    const internalDepsSection = content.match(/### Internal Dependencies \((\d+)\)\n([\s\S]*?)(?=\n###|\n##|$)/);
    const internalDeps = [];
    if (internalDepsSection) {
      const depsText = internalDepsSection[2];
      const depMatches = [...depsText.matchAll(/`@unrdf\/([^`]+)`/g)];
      internalDeps.push(...depMatches.map(m => m[1]));
    }

    return {
      package: packageName,
      version: versionMatch?.[1] || 'unknown',
      status: statusMatch?.[1]?.trim() || 'unknown',
      generated: generatedMatch?.[1]?.trim() || 'unknown',
      description: descMatch?.[1]?.trim() || 'No description',
      internalDeps,
    };
  } catch (error) {
    console.warn(`⚠️  Could not extract metadata for ${packageName}: ${error.message}`);
    return null;
  }
}

/**
 * Categorize packages by type
 * @param {object[]} packages
 * @returns {object}
 */
function categorizePackages(packages) {
  const categories = {
    core: [],
    integration: [],
    tooling: [],
    workflow: [],
    knowledge: [],
    observability: [],
    compatibility: [],
    other: [],
  };

  for (const pkg of packages) {
    const name = pkg.package;

    if (name.includes('core') || name.includes('oxigraph')) {
      categories.core.push(pkg);
    } else if (name.includes('hooks') || name.includes('react') || name.includes('browser')) {
      categories.integration.push(pkg);
    } else if (name.includes('cli') || name.includes('tools') || name.includes('test')) {
      categories.tooling.push(pkg);
    } else if (name.includes('yawl') || name.includes('workflow') || name.includes('queue')) {
      categories.workflow.push(pkg);
    } else if (name.includes('knowledge') || name.includes('kgc') || name.includes('kgn')) {
      categories.knowledge.push(pkg);
    } else if (name.includes('observability') || name.includes('otel')) {
      categories.observability.push(pkg);
    } else if (name.includes('v6') || name.includes('compat')) {
      categories.compatibility.push(pkg);
    } else {
      categories.other.push(pkg);
    }
  }

  return categories;
}

/**
 * Generate dependency graph in Mermaid format
 * @param {object[]} packages
 * @returns {string}
 */
function generateDependencyGraph(packages) {
  const lines = ['```mermaid', 'graph TD'];

  for (const pkg of packages) {
    const pkgId = pkg.package.replace(/-/g, '_');
    lines.push(`  ${pkgId}["@unrdf/${pkg.package}"]`);

    for (const dep of pkg.internalDeps) {
      const depId = dep.replace(/-/g, '_');
      lines.push(`  ${pkgId} --> ${depId}`);
    }
  }

  lines.push('```');
  return lines.join('\n');
}

/**
 * Generate reverse dependency map
 * @param {object[]} packages
 * @returns {Map<string, string[]>}
 */
function generateReverseDepMap(packages) {
  const reverseMap = new Map();

  // Initialize
  for (const pkg of packages) {
    reverseMap.set(pkg.package, []);
  }

  // Build reverse dependencies
  for (const pkg of packages) {
    for (const dep of pkg.internalDeps) {
      if (reverseMap.has(dep)) {
        reverseMap.get(dep).push(pkg.package);
      }
    }
  }

  return reverseMap;
}

/**
 * Generate index content
 * @param {object[]} packages
 * @param {object} categories
 * @param {Map<string, string[]>} reverseDepMap
 * @returns {string}
 */
function generateIndexContent(packages, categories, reverseDepMap) {
  const timestamp = new Date().toISOString();

  let content = `# UNRDF Capability Maps Index

**Total Packages**: ${packages.length}
**Last Updated**: ${timestamp}

## Quick Navigation

`;

  // Add categories
  for (const [category, pkgs] of Object.entries(categories)) {
    if (pkgs.length > 0) {
      content += `- [${category.charAt(0).toUpperCase() + category.slice(1)}](#${category}) (${pkgs.length})\n`;
    }
  }

  content += '\n---\n\n';

  // Add categorized packages
  for (const [category, pkgs] of Object.entries(categories)) {
    if (pkgs.length === 0) continue;

    content += `## ${category.charAt(0).toUpperCase() + category.slice(1)}\n\n`;

    for (const pkg of pkgs) {
      const usedBy = reverseDepMap.get(pkg.package) || [];
      const statusIcon = pkg.status.includes('✅') ? '✅' : '⚠️';

      content += `### [@unrdf/${pkg.package}](${pkg.package}/capability-map.md) ${statusIcon}\n\n`;
      content += `**Version**: ${pkg.version}  \n`;
      content += `${pkg.description}\n\n`;

      if (pkg.internalDeps.length > 0) {
        content += `**Dependencies**: ${pkg.internalDeps.map(d => `[\`@unrdf/${d}\`](${d}/capability-map.md)`).join(', ')}\n\n`;
      }

      if (usedBy.length > 0) {
        content += `**Used by**: ${usedBy.map(d => `[\`@unrdf/${d}\`](${d}/capability-map.md)`).join(', ')}\n\n`;
      }

      content += '\n';
    }
  }

  // Add dependency graph
  content += '## Dependency Graph\n\n';
  content += generateDependencyGraph(packages);
  content += '\n\n';

  // Add statistics
  content += '## Statistics\n\n';
  content += `- **Total Packages**: ${packages.length}\n`;
  content += `- **Tested Packages**: ${packages.filter(p => p.status.includes('✅')).length}\n`;
  content += `- **Packages Without Tests**: ${packages.filter(p => p.status.includes('⚠️')).length}\n`;
  content += `- **Internal Dependencies**: ${packages.reduce((sum, p) => sum + p.internalDeps.length, 0)}\n`;
  content += `- **Standalone Packages**: ${packages.filter(p => p.internalDeps.length === 0).length}\n`;

  // Most depended on
  const depCounts = Array.from(reverseDepMap.entries())
    .map(([pkg, deps]) => ({ pkg, count: deps.length }))
    .filter(x => x.count > 0)
    .sort((a, b) => b.count - a.count)
    .slice(0, 5);

  if (depCounts.length > 0) {
    content += '\n### Most Depended On\n\n';
    for (const { pkg, count } of depCounts) {
      content += `- [\`@unrdf/${pkg}\`](${pkg}/capability-map.md): ${count} package(s)\n`;
    }
  }

  content += `\n---\n\n`;
  content += `**Auto-generated** by \`scripts/auto-index-capabilities.mjs\`  \n`;
  content += `**Last updated**: ${timestamp}\n`;

  return content;
}

/**
 * Main execution
 */
async function main() {
  const config = parseArgs();

  console.log('📚 Generating capability index...\n');

  const packages = await getPackagesWithMaps();

  if (packages.length === 0) {
    console.log('⚠️  No capability maps found');
    return;
  }

  console.log(`📋 Found ${packages.length} capability maps\n`);

  // Extract metadata for all packages
  const metadata = [];
  for (const pkg of packages) {
    if (config.verbose) {
      console.log(`   Processing: ${pkg}`);
    }
    const meta = await extractMetadata(pkg);
    if (meta) {
      metadata.push(meta);
    }
  }

  // Categorize packages
  const categories = categorizePackages(metadata);

  // Generate reverse dependency map
  const reverseDepMap = generateReverseDepMap(metadata);

  // Generate index
  const indexContent = generateIndexContent(metadata, categories, reverseDepMap);

  // Write index file
  const indexPath = join(DOCS_DIR, 'index.md');
  await writeFile(indexPath, indexContent, 'utf-8');

  console.log(`✅ Generated index: ${indexPath}`);
  console.log(`\n📊 Statistics:`);
  console.log(`   - Total packages: ${metadata.length}`);
  console.log(`   - Tested: ${metadata.filter(p => p.status.includes('✅')).length}`);
  console.log(`   - Internal deps: ${metadata.reduce((sum, p) => sum + p.internalDeps.length, 0)}`);

  if (config.verbose) {
    console.log(`\n📂 Categories:`);
    for (const [category, pkgs] of Object.entries(categories)) {
      if (pkgs.length > 0) {
        console.log(`   - ${category}: ${pkgs.length}`);
      }
    }
  }
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main().catch(error => {
    console.error('❌ Fatal error:', error);
    process.exit(1);
  });
}

export { generateIndexContent, categorizePackages, generateDependencyGraph };
