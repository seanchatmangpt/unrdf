/**
 * @file Dependency Graph - Topological sort with cycle detection for parallel hook execution
 * @module hooks/dependency-graph
 */

import { z } from 'zod';

/* ========================================================================= */
/* Zod Schemas                                                              */
/* ========================================================================= */

export const HookDependencySchema = z.object({
  hookId: z.string(),
  dependsOn: z.array(z.string()).optional().default([]),
  metadata: z.record(z.string(), z.unknown()).optional(),
});

/* ========================================================================= */
/* Dependency Graph Class                                                     */
/* ========================================================================= */

/**
 * Dependency graph for topological sorting and cycle detection.
 *
 * @class DependencyGraph
 */
export class DependencyGraph {
  /**
   * Create a new dependency graph.
   */
  constructor() {
    /** @type {Map<string, Set<string>>} - hookId → set of dependencies */
    this.dependencies = new Map();

    /** @type {Map<string, Set<string>>} - hookId → set of dependents (reverse graph) */
    this.dependents = new Map();

    /** @type {Set<string>} - all registered hook IDs */
    this.nodes = new Set();
  }

  /**
   * Add a node (hook) to the graph.
   *
   * @param {string} hookId - Hook identifier
   * @param {string[]} [dependencies=[]] - List of hook IDs this hook depends on
   */
  addNode(hookId, dependencies = []) {
    const nodeExists = this.nodes.has(hookId);

    // Initialize dependency and dependent sets
    this.dependencies.set(hookId, new Set(dependencies));
    this.dependents.set(hookId, new Set());
    this.nodes.add(hookId);

    // Update reverse graph (dependents)
    for (const depId of dependencies) {
      // Add the dependency as a node if it doesn't exist
      if (!this.nodes.has(depId)) {
        this.nodes.add(depId);
        this.dependencies.set(depId, new Set());
        this.dependents.set(depId, new Set());
      }

      // Add current hook as dependent of the dependency
      this.dependents.get(depId).add(hookId);

      // If node already existed, remove old dependency relationships
      if (nodeExists) {
        // Note: We're keeping the new dependencies, so old ones are effectively replaced
        // The dependents sets are updated above
      }
    }
  }

  /**
   * Add multiple nodes to the graph.
   *
   * @param {Array<{hookId: string, dependsOn?: string[]}>} nodes - Nodes to add
   */
  addNodes(nodes) {
    for (const node of nodes) {
      this.addNode(node.hookId, node.dependsOn || []);
    }
  }

  /**
   * Remove a node from the graph.
   *
   * @param {string} hookId - Hook identifier to remove
   * @returns {boolean} True if node was removed
   */
  removeNode(hookId) {
    if (!this.nodes.has(hookId)) {
      return false;
    }

    // Remove from dependency sets of dependent nodes
    const dependents = this.dependents.get(hookId) || new Set();
    for (const dependentId of dependents) {
      this.dependencies.get(dependentId)?.delete(hookId);
    }

    // Remove from dependent sets of dependency nodes
    const dependencies = this.dependencies.get(hookId) || new Set();
    for (const depId of dependencies) {
      this.dependents.get(depId)?.delete(hookId);
    }

    // Clean up maps
    this.dependencies.delete(hookId);
    this.dependents.delete(hookId);
    this.nodes.delete(hookId);

    return true;
  }

  /**
   * Check if the graph contains a cycle.
   *
   * Uses depth-first search with coloring (white/gray/black) to detect back edges.
   *
   * @returns {{hasCycle: boolean, cycle?: string[]}} - Cycle detection result
   */
  detectCycle() {
    const WHITE = 0; // Unvisited
    const GRAY = 1; // Visiting (in current path)
    const BLACK = 2; // Visited (not in current path)

    /** @type {Map<string, number>} */
    const color = new Map();

    // Initialize all nodes as white
    for (const node of this.nodes) {
      color.set(node, WHITE);
    }

    /** @type {string[]} */
    const cycle = [];

    /**
     * Depth-first search to detect back edges.
     *
     * @param {string} node - Current node
     * @returns {boolean} - True if cycle found
     */
    const dfs = (node) => {
      color.set(node, GRAY);

      const dependencies = this.dependencies.get(node) || new Set();
      for (const dep of dependencies) {
        const depColor = color.get(dep);

        if (depColor === GRAY) {
          // Back edge found - cycle detected
          cycle.push(node, dep);
          return true;
        }

        if (depColor === WHITE) {
          if (dfs(dep)) {
            cycle.push(node);
            return true;
          }
        }
      }

      color.set(node, BLACK);
      return false;
    };

    // Run DFS from each white node
    for (const node of this.nodes) {
      if (color.get(node) === WHITE) {
        if (dfs(node)) {
          // Reverse to get correct order
          cycle.reverse();
          return { hasCycle: true, cycle };
        }
      }
    }

    return { hasCycle: false };
  }

  /**
   * Perform topological sort (Kahn's algorithm).
   *
   * Returns an array of layers, where each layer contains hooks that can be
   * executed in parallel. Hooks in layer N depend only on hooks in layers < N.
   *
   * @returns {{layers: string[][], hasCycle: boolean, cycle?: string[]}} - Topological sort result
   */
  topologicalSort() {
    // First check for cycles
    const cycleDetection = this.detectCycle();
    if (cycleDetection.hasCycle) {
      return {
        layers: [],
        hasCycle: true,
        cycle: cycleDetection.cycle,
      };
    }

    /** @type {Map<string, number>} - hookId → in-degree (number of unmet dependencies) */
    const inDegree = new Map();

    // Initialize in-degrees
    for (const node of this.nodes) {
      const deps = this.dependencies.get(node) || new Set();
      // Only count dependencies that are actually registered nodes
      inDegree.set(node, [...deps].filter(dep => this.nodes.has(dep)).length);
    }

    /** @type {string[]} - Queue of nodes with in-degree 0 */
    const queue = [];

    // Find all nodes with in-degree 0
    for (const [node, degree] of inDegree) {
      if (degree === 0) {
        queue.push(node);
      }
    }

    /** @type {string[][]} - Result layers */
    const layers = [];

    // Process nodes in batches (layers)
    while (queue.length > 0) {
      // Current layer: all nodes currently in queue can execute in parallel
      layers.push([...queue]);

      // Process all nodes in current layer
      /** @type {string[]} */
      const currentLayer = [...queue];
      queue.length = 0; // Clear queue

      // For each node in current layer, reduce in-degree of its dependents
      for (const node of currentLayer) {
        const dependents = this.dependents.get(node) || new Set();
        for (const dependent of dependents) {
          const newDegree = (inDegree.get(dependent) || 0) - 1;
          inDegree.set(dependent, newDegree);

          // If in-degree becomes 0, add to next layer
          if (newDegree === 0) {
            queue.push(dependent);
          }
        }
      }
    }

    // Check if all nodes were processed (should be true if no cycles)
    const processedCount = layers.reduce((sum, layer) => sum + layer.length, 0);
    const hasCycle = processedCount !== this.nodes.size;

    return {
      layers,
      hasCycle,
      cycle: hasCycle ? [] : undefined,
    };
  }

  /**
   * Get all dependencies for a hook (transitive closure).
   *
   * @param {string} hookId - Hook identifier
   * @returns {Set<string>} - Set of all transitive dependencies
   */
  getTransitiveDependencies(hookId) {
    const visited = new Set();
    const result = new Set();

    const dfs = (node) => {
      if (visited.has(node)) {
        return;
      }

      visited.add(node);
      const deps = this.dependencies.get(node) || new Set();
      for (const dep of deps) {
        result.add(dep);
        dfs(dep);
      }
    };

    dfs(hookId);
    return result;
  }

  /**
   * Get all dependents for a hook (transitive closure).
   *
   * @param {string} hookId - Hook identifier
   * @returns {Set<string>} - Set of all transitive dependents
   */
  getTransitiveDependents(hookId) {
    const visited = new Set();
    const result = new Set();

    const dfs = (node) => {
      if (visited.has(node)) {
        return;
      }

      visited.add(node);
      const deps = this.dependents.get(node) || new Set();
      for (const dep of deps) {
        result.add(dep);
        dfs(dep);
      }
    };

    dfs(hookId);
    return result;
  }

  /**
   * Get statistics about the dependency graph.
   *
   * @returns {{nodeCount: number, edgeCount: number, maxDepth: number, avgBranchingFactor: number}}
   */
  getStats() {
    const nodeCount = this.nodes.size;
    let edgeCount = 0;

    for (const [, deps] of this.dependencies) {
      edgeCount += deps.size;
    }

    // Calculate max depth (longest path)
    const { layers } = this.topologicalSort();
    const maxDepth = layers.length;

    // Calculate average branching factor (avg dependents per node)
    const avgBranchingFactor =
      nodeCount > 0
        ? [...this.dependents.values()].reduce((sum, deps) => sum + deps.size, 0) / nodeCount
        : 0;

    return {
      nodeCount,
      edgeCount,
      maxDepth,
      avgBranchingFactor: parseFloat(avgBranchingFactor.toFixed(2)),
    };
  }

  /**
   * Clear all nodes from the graph.
   */
  clear() {
    this.dependencies.clear();
    this.dependents.clear();
    this.nodes.clear();
  }
}

/* ========================================================================= */
/* Factory Functions                                                          */
/* ========================================================================= */

/**
 * Create a dependency graph from hook definitions.
 *
 * @param {Array<{id: string, dependsOn?: string[]}>} hooks - Hook definitions
 * @returns {DependencyGraph} - Populated dependency graph
 */
export function createDependencyGraph(hooks) {
  const graph = new DependencyGraph();

  for (const hook of hooks) {
    graph.addNode(hook.id, hook.dependsOn || []);
  }

  return graph;
}

/**
 * Build execution layers from hooks (convenience function).
 *
 * @param {Array<{id: string, dependsOn?: string[]}>} hooks - Hook definitions
 * @returns {{layers: any[][], hasCycle: boolean, cycle?: string[]}} - Topological sort result
 */
export function buildExecutionLayers(hooks) {
  const graph = createDependencyGraph(hooks);
  return graph.topologicalSort();
}
