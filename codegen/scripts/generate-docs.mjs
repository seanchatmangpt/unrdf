#!/usr/bin/env node

import { REGISTRY, getAll, getEssential, getExtended, getOptional } from '../generated/index.mjs';
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const projectRoot = path.resolve(__dirname, '../..');

// Generate package list
function generateList() {
  const essential = getEssential();
  const extended = getExtended();
  const optional = getOptional();

  let list = '# UNRDF Package Inventory\n\n';
  list += `Generated: ${new Date().toISOString()}\n`;
  list += `Total: ${REGISTRY.total} packages (${essential.length} essential, ${extended.length} extended, ${optional.length} optional)\n\n`;

  list += '## Essential Packages (Core)\n\n';
  essential.forEach(p => {
    list += `- **${p.name}** (${p.version})\n`;
    list += `  - Path: \`${p.path}\`\n`;
    list += `  - Description: ${p.description}\n`;
    list += `  - Dependencies: ${p.dependencies.filter(d => d.startsWith('@unrdf')).length} internal, ${p.dependencies.filter(d => !d.startsWith('@unrdf')).length} external\n\n`;
  });

  list += '## Extended Packages (Common Use Cases)\n\n';
  extended.forEach(p => {
    list += `- **${p.name}** (${p.version})\n`;
    list += `  - Path: \`${p.path}\`\n`;
    list += `  - Description: ${p.description}\n`;
    list += `  - Dependencies: ${p.dependencies.filter(d => d.startsWith('@unrdf')).length} internal\n\n`;
  });

  list += `## Optional Packages (${optional.length} packages)\n\n`;
  optional.forEach(p => {
    list += `- ${p.name} (${p.version})\n`;
  });

  return list;
}

// Generate package stats
function generateStats() {
  const all = getAll();
  const stats = {
    generated: new Date().toISOString(),
    total: all.length,
    tiers: {
      essential: getEssential().length,
      extended: getExtended().length,
      optional: getOptional().length
    },
    versions: {},
    dependencies: {
      internal: new Set(),
      external: new Set()
    }
  };

  all.forEach(p => {
    const version = p.version;
    stats.versions[version] = (stats.versions[version] || 0) + 1;

    p.dependencies.forEach(dep => {
      if (dep.startsWith('@unrdf')) {
        stats.dependencies.internal.add(dep);
      } else {
        stats.dependencies.external.add(dep);
      }
    });
  });

  stats.dependencies.internal = Array.from(stats.dependencies.internal).length;
  stats.dependencies.external = Array.from(stats.dependencies.external).length;

  return stats;
}

// Generate dependency graph
function generateDependencyGraph() {
  const all = getAll();
  const graph = {};

  all.forEach(p => {
    graph[p.name] = {
      version: p.version,
      tier: p.tier,
      dependencies: p.dependencies.filter(d => d.startsWith('@unrdf')),
      dependents: []
    };
  });

  // Calculate dependents
  all.forEach(p => {
    p.dependencies.filter(d => d.startsWith('@unrdf')).forEach(dep => {
      if (graph[dep]) {
        graph[dep].dependents.push(p.name);
      }
    });
  });

  return graph;
}

// Main
async function main() {
  const docsDir = path.join(projectRoot, 'docs', 'generated');
  fs.mkdirSync(docsDir, { recursive: true });

  console.log('📊 Generating package list...');
  const list = generateList();
  fs.writeFileSync(path.join(docsDir, 'packages.md'), list);
  console.log(`   ✓ Generated: packages.md`);

  console.log('📈 Generating package stats...');
  const stats = generateStats();
  fs.writeFileSync(path.join(docsDir, 'stats.json'), JSON.stringify(stats, null, 2));
  console.log(`   ✓ Generated: stats.json`);

  console.log('🔗 Generating dependency graph...');
  const graph = generateDependencyGraph();
  fs.writeFileSync(path.join(docsDir, 'dependencies.json'), JSON.stringify(graph, null, 2));
  console.log(`   ✓ Generated: dependencies.json`);

  console.log('\n✅ Generated documentation artifacts');
  console.log(`   Location: ${docsDir}`);
}

main().catch(console.error);
