#!/usr/bin/env node
/**
 * Generate comprehensive capability cross-reference index
 * Parses capability-map/*.md files and generates:
 * - CAPABILITY-INDEX.md (searchable index)
 * - COMPOSITION-CHAINS.md (dependency sequences)
 * - PARETO-FRONTIER.md (value/complexity analysis)
 * - capability-index.json (structured data)
 */

import { readFileSync, writeFileSync, readdirSync } from 'fs';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const projectRoot = join(__dirname, '..');

/**
 * Parse a capability map markdown file
 * @param {string} filePath - Path to markdown file
 * @returns {Object} Parsed capability data
 */
function parseCapabilityMap(filePath) {
  const content = readFileSync(filePath, 'utf-8');
  const lines = content.split('\n');

  // Extract package name and metadata
  const packageMatch = content.match(/# Capability Map: (@unrdf\/[\w-]+)/);
  const packageName = packageMatch ? packageMatch[1] : 'unknown';

  const versionMatch = content.match(/\*\*Package Version\*\*: ([\d\w.-]+)/);
  const version = versionMatch ? versionMatch[1] : 'unknown';

  const runtimeMatch = content.match(/\*\*Runtime Support\*\*: ([^\n]+)/);
  const runtime = runtimeMatch ? runtimeMatch[1] : 'unknown';

  // Extract capability atoms
  const atoms = [];
  let currentSection = null;

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];

    // Detect capability section headers (### N. Section Name)
    const sectionMatch = line.match(/^### (\d+)\. (.+)$/);
    if (sectionMatch) {
      currentSection = sectionMatch[2];
      continue;
    }

    // Extract capabilities from markdown tables
    const capabilityMatch = line.match(/^\| `([^`]+)` \| ([^|]+) \| \[([^\]]+)\]\(([^)]+)\) \| ([^|]+) \|/);
    if (capabilityMatch && currentSection) {
      atoms.push({
        name: capabilityMatch[1],
        type: capabilityMatch[2].trim(),
        evidence: capabilityMatch[3].trim(),
        evidenceLink: capabilityMatch[4].trim(),
        runtime: capabilityMatch[5].trim(),
        section: currentSection,
        package: packageName
      });
    }
  }

  // Extract compositions
  const compositions = [];
  const compositionRegex = /### (C\d+): ([^(]+)\(([^)]+)\)/g;
  let match;

  while ((match = compositionRegex.exec(content)) !== null) {
    const compositionId = match[1];
    const compositionName = match[2].trim();
    const compositionDesc = match[3].trim();

    // Find the corresponding composition block
    const compositionStart = match.index;
    const nextCompositionMatch = content.indexOf('###', compositionStart + 1);
    const compositionEnd = nextCompositionMatch > 0 ? nextCompositionMatch : content.length;
    const compositionBlock = content.substring(compositionStart, compositionEnd);

    // Extract value, complexity, evidence
    const valueMatch = compositionBlock.match(/\*\*Value\*\*: ([^\n]+)/);
    const complexityMatch = compositionBlock.match(/\*\*Complexity\*\*: ([^\n]+)/);
    const evidenceMatch = compositionBlock.match(/\*\*Evidence\*\*: ([^\n]+)/);

    // Extract dependencies from code example
    const codeMatch = compositionBlock.match(/```javascript\n([\s\S]+?)```/);
    const dependencies = [];
    if (codeMatch) {
      const code = codeMatch[1];
      const importMatches = code.matchAll(/from ['"](@unrdf\/[\w-]+)['"]/g);
      for (const importMatch of importMatches) {
        if (!dependencies.includes(importMatch[1])) {
          dependencies.push(importMatch[1]);
        }
      }
    }

    compositions.push({
      id: compositionId,
      name: compositionName,
      description: compositionDesc,
      value: valueMatch ? valueMatch[1] : 'Unknown',
      complexity: complexityMatch ? complexityMatch[1] : 'Unknown',
      evidence: evidenceMatch ? evidenceMatch[1] : 'Unknown',
      dependencies,
      package: packageName
    });
  }

  return {
    package: packageName,
    version,
    runtime,
    atoms,
    compositions
  };
}

/**
 * Build inverted index: capability → packages
 * @param {Array} allAtoms - All capability atoms
 * @returns {Object} Inverted index
 */
function buildInvertedIndex(allAtoms) {
  const index = {};

  for (const atom of allAtoms) {
    const key = atom.name;
    if (!index[key]) {
      index[key] = {
        capability: key,
        type: atom.type,
        packages: []
      };
    }

    index[key].packages.push({
      package: atom.package,
      section: atom.section,
      evidence: atom.evidence,
      runtime: atom.runtime
    });
  }

  return index;
}

/**
 * Identify composition chains A→B→C
 * @param {Array} allCompositions - All compositions
 * @returns {Array} Composition chains
 */
function identifyCompositionChains(allCompositions) {
  const chains = [];

  // Build dependency graph
  const graph = {};
  for (const comp of allCompositions) {
    const key = `${comp.package}:${comp.id}`;
    graph[key] = comp.dependencies.map(dep => dep);
  }

  // Find chains (simple path detection)
  for (const comp of allCompositions) {
    const key = `${comp.package}:${comp.id}`;
    const chain = [key];

    // Trace dependencies
    for (const dep of comp.dependencies) {
      chain.push(dep);
    }

    if (chain.length > 1) {
      chains.push({
        composition: comp.id,
        package: comp.package,
        name: comp.name,
        chain: chain,
        depth: chain.length - 1
      });
    }
  }

  // Sort by depth (longest chains first)
  chains.sort((a, b) => b.depth - a.depth);

  return chains;
}

/**
 * Calculate Pareto frontier (value vs complexity)
 * @param {Array} allCompositions - All compositions
 * @returns {Array} Pareto frontier compositions
 */
function calculateParetoFrontier(allCompositions) {
  // Score compositions based on value and complexity
  const scored = allCompositions.map(comp => {
    // Parse complexity to numeric score (lower is better)
    let complexityScore = 5; // default medium
    const complexityStr = comp.complexity.toLowerCase();
    if (complexityStr.includes('o(1)')) complexityScore = 1;
    else if (complexityStr.includes('o(log')) complexityScore = 2;
    else if (complexityStr.includes('o(n)') && !complexityStr.includes('log')) complexityScore = 3;
    else if (complexityStr.includes('o(n log n)')) complexityScore = 4;
    else if (complexityStr.includes('o(n^2)')) complexityScore = 6;

    // Parse value to numeric score (higher is better)
    let valueScore = 5; // default medium
    const valueStr = comp.value.toLowerCase();
    if (valueStr.includes('unique') || valueStr.includes('fastest')) valueScore = 10;
    else if (valueStr.includes('high') || valueStr.includes('deterministic')) valueScore = 8;
    else if (valueStr.includes('complete') || valueStr.includes('auditable')) valueScore = 7;
    else if (valueStr.includes('type-safe') || valueStr.includes('governed')) valueScore = 6;

    // Pareto score: value / complexity (higher is better)
    const paretoScore = valueScore / complexityScore;

    return {
      ...comp,
      valueScore,
      complexityScore,
      paretoScore
    };
  });

  // Sort by Pareto score (descending)
  scored.sort((a, b) => b.paretoScore - a.paretoScore);

  return scored;
}

/**
 * Generate CAPABILITY-INDEX.md
 */
function generateCapabilityIndex(index, outputPath) {
  const entries = Object.values(index).sort((a, b) =>
    a.capability.localeCompare(b.capability)
  );

  let content = `# UNRDF Capability Cross-Reference Index

**Generated**: ${new Date().toISOString()}
**Total Capabilities**: ${entries.length}
**Total Packages Analyzed**: ${new Set(entries.flatMap(e => e.packages.map(p => p.package))).size}

---

## Overview

This index provides a comprehensive cross-reference of all capabilities across the UNRDF ecosystem. Each capability lists all packages that provide or use it, enabling developers to:

1. Find implementations of specific capabilities
2. Discover alternative packages for the same capability
3. Understand capability distribution across packages
4. Identify potential composition opportunities

---

## Index by Capability

`;

  for (const entry of entries) {
    content += `### ${entry.capability}\n\n`;
    content += `**Type**: ${entry.type}\n\n`;
    content += `**Provided by**:\n\n`;

    for (const pkg of entry.packages) {
      content += `- **${pkg.package}** (${pkg.section})\n`;
      content += `  - Evidence: ${pkg.evidence}\n`;
      content += `  - Runtime: ${pkg.runtime}\n`;
    }

    content += '\n';
  }

  content += `---

## Index by Package

`;

  // Group by package
  const byPackage = {};
  for (const entry of entries) {
    for (const pkg of entry.packages) {
      if (!byPackage[pkg.package]) {
        byPackage[pkg.package] = [];
      }
      byPackage[pkg.package].push({
        capability: entry.capability,
        type: entry.type,
        section: pkg.section
      });
    }
  }

  for (const [packageName, capabilities] of Object.entries(byPackage).sort()) {
    content += `### ${packageName}\n\n`;
    content += `**Total Capabilities**: ${capabilities.length}\n\n`;

    // Group by section
    const bySection = {};
    for (const cap of capabilities) {
      if (!bySection[cap.section]) {
        bySection[cap.section] = [];
      }
      bySection[cap.section].push(cap);
    }

    for (const [section, caps] of Object.entries(bySection)) {
      content += `#### ${section}\n\n`;
      for (const cap of caps) {
        content += `- \`${cap.capability}\` (${cap.type})\n`;
      }
      content += '\n';
    }
  }

  content += `---

**Last Updated**: ${new Date().toISOString()}
**Source**: Automated capability map analysis
`;

  writeFileSync(outputPath, content, 'utf-8');
  console.log(`✅ Generated: ${outputPath}`);
}

/**
 * Generate COMPOSITION-CHAINS.md
 */
function generateCompositionChains(chains, outputPath) {
  let content = `# UNRDF Composition Dependency Chains

**Generated**: ${new Date().toISOString()}
**Total Composition Chains**: ${chains.length}

---

## Overview

This document maps composition dependency chains, showing how capabilities build upon each other. Understanding these chains helps:

1. Identify foundational capabilities (most dependencies)
2. Understand composition complexity (chain depth)
3. Plan integration strategies (minimal dependency paths)
4. Detect circular dependencies (should be none)

---

## Chains by Depth

`;

  // Group by depth
  const byDepth = {};
  for (const chain of chains) {
    if (!byDepth[chain.depth]) {
      byDepth[chain.depth] = [];
    }
    byDepth[chain.depth].push(chain);
  }

  for (const [depth, depthChains] of Object.entries(byDepth).sort((a, b) => b[0] - a[0])) {
    content += `### Depth ${depth} (${depthChains.length} chains)\n\n`;

    for (const chain of depthChains) {
      content += `#### ${chain.composition}: ${chain.name}\n\n`;
      content += `**Package**: ${chain.package}\n\n`;
      content += `**Dependency Chain**:\n\n`;
      content += '```\n';
      content += chain.chain.join(' → ');
      content += '\n```\n\n';
    }
  }

  content += `---

## All Chains (Alphabetical)

`;

  for (const chain of chains.sort((a, b) => a.composition.localeCompare(b.composition))) {
    content += `- **${chain.composition}** (${chain.package}): ${chain.chain.join(' → ')}\n`;
  }

  content += `\n---

**Last Updated**: ${new Date().toISOString()}
**Source**: Automated composition chain analysis
`;

  writeFileSync(outputPath, content, 'utf-8');
  console.log(`✅ Generated: ${outputPath}`);
}

/**
 * Generate PARETO-FRONTIER.md
 */
function generateParetoFrontier(scored, outputPath) {
  const top20 = scored.slice(0, Math.min(20, scored.length));

  let content = `# UNRDF Pareto Frontier Analysis

**Generated**: ${new Date().toISOString()}
**Total Compositions Analyzed**: ${scored.length}
**Pareto Frontier**: Top 20 compositions by value/complexity ratio

---

## Overview

The Pareto frontier identifies compositions that provide maximum value with minimum complexity. This analysis helps:

1. Prioritize integration efforts (80/20 rule)
2. Identify high-ROI capabilities
3. Understand trade-offs between value and complexity
4. Guide architectural decisions

### Scoring Methodology

- **Value Score** (1-10): Higher is better
  - 10: Unique/fastest capabilities
  - 8: High value/deterministic
  - 7: Complete/auditable
  - 6: Type-safe/governed
  - 5: Default

- **Complexity Score** (1-6): Lower is better
  - 1: O(1) constant time
  - 2: O(log n) logarithmic
  - 3: O(n) linear
  - 4: O(n log n) linearithmic
  - 5: Default
  - 6: O(n²) quadratic

- **Pareto Score**: value / complexity (higher is better)

---

## Top 20 Compositions

`;

  for (let i = 0; i < top20.length; i++) {
    const comp = top20[i];
    content += `### ${i + 1}. ${comp.id}: ${comp.name}\n\n`;
    content += `**Package**: ${comp.package}\n\n`;
    content += `**Description**: ${comp.description}\n\n`;
    content += `**Value**: ${comp.value} (Score: ${comp.valueScore}/10)\n\n`;
    content += `**Complexity**: ${comp.complexity} (Score: ${comp.complexityScore}/6)\n\n`;
    content += `**Pareto Score**: ${comp.paretoScore.toFixed(2)}\n\n`;
    content += `**Dependencies**: ${comp.dependencies.length > 0 ? comp.dependencies.join(', ') : 'None'}\n\n`;
    content += `**Evidence**: ${comp.evidence}\n\n`;
    content += '---\n\n';
  }

  content += `## Pareto Frontier Visualization

\`\`\`
Value/Complexity Scatter Plot (Pareto Score)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

10 │ ${top20[0]?.id || ''}
 9 │ ${top20[1]?.id || ''}
 8 │ ${top20[2]?.id || ''}
 7 │ ${top20[3]?.id || ''}
 6 │ ${top20[4]?.id || ''}
 5 │
 4 │
 3 │
 2 │
 1 │
 0 └─────────────────────────────────────
   0  1  2  3  4  5  6  7  8  9  10
              Complexity →
\`\`\`

---

## Complete Ranking

`;

  for (let i = 0; i < scored.length; i++) {
    const comp = scored[i];
    content += `${i + 1}. **${comp.id}** (${comp.package}) - Pareto: ${comp.paretoScore.toFixed(2)} (V:${comp.valueScore}, C:${comp.complexityScore})\n`;
  }

  content += `\n---

**Last Updated**: ${new Date().toISOString()}
**Source**: Automated Pareto frontier analysis
**Methodology**: Value/Complexity ratio optimization
`;

  writeFileSync(outputPath, content, 'utf-8');
  console.log(`✅ Generated: ${outputPath}`);
}

/**
 * Generate capability-index.json
 */
function generateJSON(data, outputPath) {
  const json = {
    metadata: {
      generated: new Date().toISOString(),
      totalPackages: data.packages.length,
      totalAtoms: data.allAtoms.length,
      totalCompositions: data.allCompositions.length,
      totalChains: data.chains.length
    },
    packages: data.packages,
    invertedIndex: data.index,
    compositionChains: data.chains,
    paretoFrontier: data.paretoFrontier.slice(0, 20)
  };

  writeFileSync(outputPath, JSON.stringify(json, null, 2), 'utf-8');
  console.log(`✅ Generated: ${outputPath}`);
}

/**
 * Main execution
 */
function main() {
  console.log('🔍 Parsing capability map files...\n');

  const capabilityMapDir = join(projectRoot, 'docs', 'capability-map');
  const files = readdirSync(capabilityMapDir)
    .filter(f => f.endsWith('.md') && f !== 'README.md')
    .map(f => join(capabilityMapDir, f));

  console.log(`Found ${files.length} capability map files:\n`);
  files.forEach(f => console.log(`  - ${f}`));
  console.log('');

  // Parse all files
  const packages = [];
  const allAtoms = [];
  const allCompositions = [];

  for (const file of files) {
    console.log(`Parsing: ${file}`);
    const parsed = parseCapabilityMap(file);
    packages.push(parsed);
    allAtoms.push(...parsed.atoms);
    allCompositions.push(...parsed.compositions);
  }

  console.log(`\n✅ Parsed ${packages.length} packages`);
  console.log(`   - Total atoms: ${allAtoms.length}`);
  console.log(`   - Total compositions: ${allCompositions.length}\n`);

  // Build inverted index
  console.log('🔨 Building inverted index...');
  const index = buildInvertedIndex(allAtoms);
  console.log(`✅ Index built: ${Object.keys(index).length} unique capabilities\n`);

  // Identify composition chains
  console.log('🔗 Identifying composition chains...');
  const chains = identifyCompositionChains(allCompositions);
  console.log(`✅ Found ${chains.length} composition chains\n`);

  // Calculate Pareto frontier
  console.log('📊 Calculating Pareto frontier...');
  const paretoFrontier = calculateParetoFrontier(allCompositions);
  console.log(`✅ Ranked ${paretoFrontier.length} compositions\n`);

  // Generate outputs
  console.log('📝 Generating output files...\n');

  const outputDir = join(projectRoot, 'docs', 'capability-map');

  generateCapabilityIndex(
    index,
    join(outputDir, 'CAPABILITY-INDEX.md')
  );

  generateCompositionChains(
    chains,
    join(outputDir, 'COMPOSITION-CHAINS.md')
  );

  generateParetoFrontier(
    paretoFrontier,
    join(outputDir, 'PARETO-FRONTIER.md')
  );

  generateJSON(
    {
      packages,
      allAtoms,
      allCompositions,
      index,
      chains,
      paretoFrontier
    },
    join(outputDir, 'capability-index.json')
  );

  console.log('\n✅ All outputs generated successfully!\n');
  console.log('📁 Output files:');
  console.log(`   - ${join(outputDir, 'CAPABILITY-INDEX.md')}`);
  console.log(`   - ${join(outputDir, 'COMPOSITION-CHAINS.md')}`);
  console.log(`   - ${join(outputDir, 'PARETO-FRONTIER.md')}`);
  console.log(`   - ${join(outputDir, 'capability-index.json')}`);
}

main();
