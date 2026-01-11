/**
 * Dependency Analyzer - Circular Dependency Detection
 *
 * Builds dependency graph and detects cycles using DFS
 */

import fs from 'node:fs/promises';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const AUTONOMIC_ROOT = path.resolve(__dirname, '../..');

/**
 * Build dependency graph from all agent modules
 * @returns {Promise<{nodes: Set, edges: Map, nodeCount: number}>}
 */
export async function buildDependencyGraph() {
  const nodes = new Set();
  const edges = new Map();

  const agentDirs = ['agent-2', 'agent-3', 'agent-4', 'agent-5', 'agent-6', 'agent-7', 'agent-8', 'agent-9', 'agent-10'];

  for (const agentDir of agentDirs) {
    const srcDir = path.join(AUTONOMIC_ROOT, agentDir, 'src');

    try {
      const files = await fs.readdir(srcDir);

      for (const file of files) {
        if (!file.endsWith('.mjs')) continue;

        const filePath = path.join(srcDir, file);
        const modulePath = `${agentDir}/src/${file}`;
        nodes.add(modulePath);

        const content = await fs.readFile(filePath, 'utf-8');
        const imports = extractImports(content);

        const dependencies = [];
        for (const imp of imports) {
          // Resolve relative imports
          if (imp.source.startsWith('.')) {
            const resolvedPath = path.resolve(srcDir, imp.source);
            const relPath = path.relative(AUTONOMIC_ROOT, resolvedPath);
            dependencies.push(relPath);
          } else if (imp.source.startsWith('../')) {
            // Cross-agent import
            const resolvedPath = path.resolve(srcDir, imp.source);
            const relPath = path.relative(AUTONOMIC_ROOT, resolvedPath);
            dependencies.push(relPath);
          }
        }

        edges.set(modulePath, dependencies);
      }
    } catch (error) {
      // Directory doesn't exist - skip
      if (error.code !== 'ENOENT') {
        throw error;
      }
    }
  }

  return {
    nodes,
    edges,
    nodeCount: nodes.size
  };
}

/**
 * Find cycles in dependency graph using DFS
 * @param {Object} graph - Dependency graph with nodes and edges
 * @returns {Array<Array<string>>} Array of cycles (each cycle is array of module paths)
 */
export function findCycles(graph) {
  const { nodes, edges } = graph;
  const visited = new Set();
  const recursionStack = new Set();
  const cycles = [];

  /**
   * DFS helper to detect cycles
   * @param {string} node - Current node
   * @param {Array<string>} path - Current path
   */
  function dfs(node, path) {
    visited.add(node);
    recursionStack.add(node);
    path.push(node);

    const neighbors = edges.get(node) || [];

    for (const neighbor of neighbors) {
      // Normalize neighbor path
      const normalizedNeighbor = neighbor.endsWith('.mjs') ? neighbor : `${neighbor}.mjs`;

      if (!nodes.has(normalizedNeighbor)) {
        // Dependency doesn't exist in graph - skip
        continue;
      }

      if (!visited.has(normalizedNeighbor)) {
        dfs(normalizedNeighbor, [...path]);
      } else if (recursionStack.has(normalizedNeighbor)) {
        // Found cycle
        const cycleStart = path.indexOf(normalizedNeighbor);
        if (cycleStart !== -1) {
          const cycle = [...path.slice(cycleStart), normalizedNeighbor];
          cycles.push(cycle);
        }
      }
    }

    recursionStack.delete(node);
  }

  // Run DFS from each unvisited node
  for (const node of nodes) {
    if (!visited.has(node)) {
      dfs(node, []);
    }
  }

  return cycles;
}

/**
 * Extract import statements from JavaScript file
 * @param {string} content - File content
 * @returns {Array<{source: string, specifiers: Array}>}
 */
function extractImports(content) {
  const imports = [];

  // Match: import ... from '...'
  const importRegex = /import\s+(?:(?:\{[^}]+\}|\*\s+as\s+\w+|\w+)(?:\s*,\s*(?:\{[^}]+\}|\*\s+as\s+\w+|\w+))*\s+)?from\s+['"]([^'"]+)['"]/g;

  let match;
  while ((match = importRegex.exec(content)) !== null) {
    const source = match[1];
    imports.push({
      source,
      specifiers: [] // We don't need detailed specifiers for cycle detection
    });
  }

  // Match: import '...' (side-effect imports)
  const sideEffectRegex = /import\s+['"]([^'"]+)['"]/g;
  while ((match = sideEffectRegex.exec(content)) !== null) {
    const source = match[1];
    if (!imports.some(imp => imp.source === source)) {
      imports.push({
        source,
        specifiers: []
      });
    }
  }

  return imports;
}
