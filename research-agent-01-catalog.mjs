#!/usr/bin/env node
/**
 * Agent Catalog Generator
 * Systematically analyzes all agent definitions in .claude/agents/
 */

import { readdir, readFile } from 'fs/promises';
import { join } from 'path';

const AGENT_DIR = '/home/user/unrdf/.claude/agents';

/**
 * Parse YAML frontmatter from markdown file
 */
function parseFrontmatter(content) {
  const match = content.match(/^---\n([\s\S]*?)\n---/);
  if (!match) return null;

  const yaml = match[1];
  const metadata = {};

  // Simple YAML parser (handles basic key: value pairs)
  const lines = yaml.split('\n');
  let currentKey = null;
  let currentArray = null;

  for (const line of lines) {
    if (line.trim() === '') continue;

    // Array item
    if (line.trim().startsWith('- ')) {
      if (currentArray) {
        currentArray.push(line.trim().substring(2));
      }
      continue;
    }

    // Key-value pair
    const colonIndex = line.indexOf(':');
    if (colonIndex > 0) {
      const key = line.substring(0, colonIndex).trim();
      const value = line.substring(colonIndex + 1).trim();

      if (value === '') {
        // Start of array
        currentKey = key;
        currentArray = [];
        metadata[key] = currentArray;
      } else {
        metadata[key] = value.replace(/^["']|["']$/g, ''); // Remove quotes
        currentKey = key;
        currentArray = null;
      }
    }
  }

  return metadata;
}

/**
 * Recursively find all .md files
 */
async function findAgentFiles(dir, files = []) {
  const entries = await readdir(dir, { withFileTypes: true });

  for (const entry of entries) {
    const fullPath = join(dir, entry.name);
    if (entry.isDirectory()) {
      await findAgentFiles(fullPath, files);
    } else if (entry.name.endsWith('.md')) {
      files.push(fullPath);
    }
  }

  return files;
}

/**
 * Analyze single agent file
 */
async function analyzeAgent(filePath) {
  const content = await readFile(filePath, 'utf-8');
  const metadata = parseFrontmatter(content);

  const relativePath = filePath.replace(AGENT_DIR + '/', '');
  const category = relativePath.includes('/')
    ? relativePath.split('/')[0]
    : 'root';

  // Extract description from content if not in frontmatter
  let description = metadata?.description || '';
  if (!description) {
    const match = content.match(/^# (.+)$/m);
    if (match) description = match[1];
  }

  return {
    name: metadata?.name || relativePath.replace('.md', ''),
    type: metadata?.type || 'unspecified',
    category,
    color: metadata?.color || '',
    description,
    capabilities: metadata?.capabilities || [],
    priority: metadata?.priority || 'normal',
    hasHooks: !!(metadata?.hooks),
    filePath: relativePath
  };
}

/**
 * Generate statistics
 */
function generateStats(agents) {
  const stats = {
    total: agents.length,
    byCategory: {},
    byType: {},
    byPriority: {},
    withHooks: 0,
    withCapabilities: 0
  };

  for (const agent of agents) {
    // Category
    stats.byCategory[agent.category] = (stats.byCategory[agent.category] || 0) + 1;

    // Type
    stats.byType[agent.type] = (stats.byType[agent.type] || 0) + 1;

    // Priority
    stats.byPriority[agent.priority] = (stats.byPriority[agent.priority] || 0) + 1;

    // Hooks
    if (agent.hasHooks) stats.withHooks++;

    // Capabilities
    if (agent.capabilities.length > 0) stats.withCapabilities++;
  }

  return stats;
}

/**
 * Main execution
 */
async function main() {
  console.log('📊 Agent Catalog Generator\n');
  console.log(`Scanning: ${AGENT_DIR}\n`);

  const agentFiles = await findAgentFiles(AGENT_DIR);
  console.log(`Found ${agentFiles.length} agent definition files\n`);

  const agents = [];
  for (const file of agentFiles) {
    try {
      const agent = await analyzeAgent(file);
      agents.push(agent);
    } catch (error) {
      console.error(`Error analyzing ${file}: ${error.message}`);
    }
  }

  // Generate statistics
  const stats = generateStats(agents);

  // Output results
  console.log('=== AGENT CATALOG ===\n');
  console.log(`Total Agents: ${stats.total}`);
  console.log(`With Hooks: ${stats.withHooks}`);
  console.log(`With Capabilities: ${stats.withCapabilities}\n`);

  console.log('By Category:');
  Object.entries(stats.byCategory)
    .sort(([,a], [,b]) => b - a)
    .forEach(([cat, count]) => {
      console.log(`  ${cat.padEnd(20)} ${count}`);
    });

  console.log('\nBy Type:');
  Object.entries(stats.byType)
    .sort(([,a], [,b]) => b - a)
    .forEach(([type, count]) => {
      console.log(`  ${type.padEnd(20)} ${count}`);
    });

  console.log('\nBy Priority:');
  Object.entries(stats.byPriority)
    .sort(([,a], [,b]) => b - a)
    .forEach(([priority, count]) => {
      console.log(`  ${priority.padEnd(20)} ${count}`);
    });

  console.log('\n=== AGENT DIRECTORY ===\n');

  // Group by category
  const byCategory = {};
  for (const agent of agents) {
    if (!byCategory[agent.category]) byCategory[agent.category] = [];
    byCategory[agent.category].push(agent);
  }

  // Print organized list
  Object.entries(byCategory)
    .sort(([a], [b]) => a.localeCompare(b))
    .forEach(([category, categoryAgents]) => {
      console.log(`\n### ${category.toUpperCase()} (${categoryAgents.length})`);
      categoryAgents
        .sort((a, b) => a.name.localeCompare(b.name))
        .forEach(agent => {
          const caps = agent.capabilities.length > 0
            ? ` [${agent.capabilities.slice(0, 3).join(', ')}${agent.capabilities.length > 3 ? '...' : ''}]`
            : '';
          console.log(`  - ${agent.name} (${agent.type})${caps}`);
          if (agent.description && agent.description.length < 100) {
            console.log(`    ${agent.description}`);
          }
        });
    });

  // Export as JSON
  const output = {
    generated: new Date().toISOString(),
    statistics: stats,
    agents: agents.map(a => ({
      name: a.name,
      type: a.type,
      category: a.category,
      description: a.description,
      capabilities: a.capabilities,
      priority: a.priority,
      hasHooks: a.hasHooks,
      path: a.filePath
    }))
  };

  console.log('\n\n=== JSON OUTPUT ===\n');
  console.log(JSON.stringify(output, null, 2));
}

main().catch(console.error);
