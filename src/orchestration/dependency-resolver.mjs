/**
 * @fileoverview Dependency Resolver - Build topological order for multi-package changes
 *
 * **Purpose**: Analyze package dependencies and compute:
 * - Dependency closure (all affected packages)
 * - Topological sort (safe execution order)
 * - Cycle detection (prevent circular dependencies)
 * - Impact analysis (transitive dependency tracking)
 *
 * **Algorithm**: Kahn's algorithm for topological sorting
 * - O(V + E) time complexity
 * - Detects cycles during sort
 * - Returns stable ordering
 *
 * @module orchestration/dependency-resolver
 */

import { z } from 'zod';

/**
 * Package dependency schema
 */
export const PackageDependencySchema = z.object({
  name: z.string().min(1),
  version: z.string().optional(),
  path: z.string().optional(),
  dependencies: z.array(z.string()).default([]),
  devDependencies: z.array(z.string()).default([]),
  peerDependencies: z.array(z.string()).default([]),
  optionalDependencies: z.array(z.string()).default([]),
  metadata: z.record(z.any()).optional()
});

/**
 * Dependency graph schema
 */
export const DependencyGraphSchema = z.object({
  packages: z.map(z.string(), PackageDependencySchema),
  edges: z.array(z.tuple([z.string(), z.string()])),
  rootPackages: z.array(z.string()),
  leafPackages: z.array(z.string())
});

/**
 * Resolution result schema
 */
export const ResolutionResultSchema = z.object({
  success: z.boolean(),
  order: z.array(z.string()),
  closure: z.array(z.string()),
  levels: z.array(z.array(z.string())),
  hasCycle: z.boolean(),
  cyclePackages: z.array(z.string()).optional(),
  impactAnalysis: z.record(z.object({
    directDependents: z.array(z.string()),
    transitiveDependents: z.array(z.string()),
    impactLevel: z.enum(['none', 'low', 'medium', 'high', 'critical'])
  })).optional()
});

/**
 * Dependency Resolver - Compute safe execution order for packages
 *
 * @class DependencyResolver
 *
 * @example
 * const resolver = new DependencyResolver();
 * resolver.addPackage('core', { dependencies: [] });
 * resolver.addPackage('utils', { dependencies: ['core'] });
 * resolver.addPackage('app', { dependencies: ['core', 'utils'] });
 *
 * const result = resolver.resolve(['core']);
 * // result.order = ['core', 'utils', 'app']
 * // result.closure = ['core', 'utils', 'app']
 */
export class DependencyResolver {
  /**
   * Create a new dependency resolver
   *
   * @param {Object} [options] - Resolver options
   * @param {boolean} [options.includeDevDependencies=false] - Include dev dependencies
   * @param {boolean} [options.includePeerDependencies=true] - Include peer dependencies
   * @param {boolean} [options.includeOptionalDependencies=false] - Include optional dependencies
   */
  constructor(options = {}) {
    /** @type {Map<string, Object>} Package metadata */
    this.packages = new Map();

    /** @type {Map<string, Set<string>>} Forward edges (package -> dependents) */
    this.dependents = new Map();

    /** @type {Map<string, Set<string>>} Reverse edges (package -> dependencies) */
    this.dependencies = new Map();

    /** @type {Object} Resolver options */
    this.options = {
      includeDevDependencies: options.includeDevDependencies ?? false,
      includePeerDependencies: options.includePeerDependencies ?? true,
      includeOptionalDependencies: options.includeOptionalDependencies ?? false
    };
  }

  /**
   * Add a package to the resolver
   *
   * @param {string} name - Package name
   * @param {Object} pkg - Package metadata
   * @returns {DependencyResolver} this (for chaining)
   */
  addPackage(name, pkg) {
    const validated = PackageDependencySchema.parse({ name, ...pkg });

    // Store package
    this.packages.set(name, validated);

    // Initialize edge sets
    if (!this.dependents.has(name)) {
      this.dependents.set(name, new Set());
    }
    if (!this.dependencies.has(name)) {
      this.dependencies.set(name, new Set());
    }

    // Collect all relevant dependencies
    const allDeps = new Set(validated.dependencies);

    if (this.options.includeDevDependencies) {
      validated.devDependencies.forEach(d => allDeps.add(d));
    }
    if (this.options.includePeerDependencies) {
      validated.peerDependencies.forEach(d => allDeps.add(d));
    }
    if (this.options.includeOptionalDependencies) {
      validated.optionalDependencies.forEach(d => allDeps.add(d));
    }

    // Add edges
    for (const dep of allDeps) {
      // Skip self-dependencies
      if (dep === name) continue;

      // Forward edge: dep -> name (name depends on dep)
      if (!this.dependents.has(dep)) {
        this.dependents.set(dep, new Set());
      }
      this.dependents.get(dep).add(name);

      // Reverse edge: name -> dep
      this.dependencies.get(name).add(dep);
    }

    return this;
  }

  /**
   * Add multiple packages at once
   *
   * @param {Object<string, Object>} packages - Map of name -> package metadata
   * @returns {DependencyResolver} this (for chaining)
   */
  addPackages(packages) {
    for (const [name, pkg] of Object.entries(packages)) {
      this.addPackage(name, pkg);
    }
    return this;
  }

  /**
   * Get all dependents of a package (direct and transitive)
   *
   * @param {string} packageName - Package to analyze
   * @returns {Set<string>} All dependent packages
   */
  getDependentsClosure(packageName) {
    const closure = new Set();
    const queue = [packageName];

    while (queue.length > 0) {
      const current = queue.shift();
      const deps = this.dependents.get(current) || new Set();

      for (const dep of deps) {
        if (!closure.has(dep)) {
          closure.add(dep);
          queue.push(dep);
        }
      }
    }

    return closure;
  }

  /**
   * Get all dependencies of a package (direct and transitive)
   *
   * @param {string} packageName - Package to analyze
   * @returns {Set<string>} All dependency packages
   */
  getDependenciesClosure(packageName) {
    const closure = new Set();
    const queue = [packageName];

    while (queue.length > 0) {
      const current = queue.shift();
      const deps = this.dependencies.get(current) || new Set();

      for (const dep of deps) {
        if (!closure.has(dep)) {
          closure.add(dep);
          queue.push(dep);
        }
      }
    }

    return closure;
  }

  /**
   * Compute full closure for a set of changed packages
   *
   * @param {string[]} changedPackages - Packages being changed
   * @returns {Set<string>} All affected packages (changed + dependents)
   */
  computeClosure(changedPackages) {
    const closure = new Set(changedPackages);

    for (const pkg of changedPackages) {
      const dependents = this.getDependentsClosure(pkg);
      for (const dep of dependents) {
        closure.add(dep);
      }
    }

    return closure;
  }

  /**
   * Detect cycles in the dependency graph
   *
   * Uses DFS with coloring to detect back edges.
   *
   * @returns {{hasCycle: boolean, cyclePackages: string[]}}
   */
  detectCycle() {
    const WHITE = 0; // Not visited
    const GRAY = 1;  // Being processed
    const BLACK = 2; // Finished

    const color = new Map();
    const parent = new Map();
    let cyclePackages = [];

    for (const pkg of this.packages.keys()) {
      color.set(pkg, WHITE);
    }

    /**
     * DFS visit function
     * @param {string} node - Current node
     * @returns {boolean} True if cycle found
     */
    const dfs = (node) => {
      color.set(node, GRAY);

      const deps = this.dependencies.get(node) || new Set();
      for (const dep of deps) {
        if (!this.packages.has(dep)) continue; // Skip external deps

        if (color.get(dep) === GRAY) {
          // Found cycle - reconstruct path
          cyclePackages = [dep];
          let current = node;
          while (current !== dep) {
            cyclePackages.push(current);
            current = parent.get(current);
          }
          cyclePackages.push(dep);
          cyclePackages.reverse();
          return true;
        }

        if (color.get(dep) === WHITE) {
          parent.set(dep, node);
          if (dfs(dep)) return true;
        }
      }

      color.set(node, BLACK);
      return false;
    };

    for (const pkg of this.packages.keys()) {
      if (color.get(pkg) === WHITE) {
        if (dfs(pkg)) {
          return { hasCycle: true, cyclePackages };
        }
      }
    }

    return { hasCycle: false, cyclePackages: [] };
  }

  /**
   * Topological sort using Kahn's algorithm
   *
   * @param {Set<string>} subset - Subset of packages to sort (or all if not provided)
   * @returns {{success: boolean, order: string[], levels: string[][]}}
   */
  topologicalSort(subset = null) {
    const packages = subset || new Set(this.packages.keys());
    const inDegree = new Map();
    const order = [];
    const levels = [];

    // Initialize in-degree for subset
    for (const pkg of packages) {
      let degree = 0;
      const deps = this.dependencies.get(pkg) || new Set();
      for (const dep of deps) {
        if (packages.has(dep)) {
          degree++;
        }
      }
      inDegree.set(pkg, degree);
    }

    // Find initial nodes with no dependencies (in subset)
    let queue = [];
    for (const [pkg, degree] of inDegree) {
      if (degree === 0) {
        queue.push(pkg);
      }
    }

    // Process level by level
    while (queue.length > 0) {
      const level = [...queue];
      levels.push(level);
      order.push(...level);

      const nextQueue = [];

      for (const pkg of queue) {
        const dependents = this.dependents.get(pkg) || new Set();
        for (const dep of dependents) {
          if (!packages.has(dep)) continue;

          const newDegree = inDegree.get(dep) - 1;
          inDegree.set(dep, newDegree);

          if (newDegree === 0) {
            nextQueue.push(dep);
          }
        }
      }

      queue = nextQueue;
    }

    // Check if all packages were processed
    const success = order.length === packages.size;

    return { success, order, levels };
  }

  /**
   * Analyze impact of changing packages
   *
   * @param {string[]} changedPackages - Packages being changed
   * @returns {Object<string, Object>} Impact analysis per package
   */
  analyzeImpact(changedPackages) {
    const impact = {};

    for (const pkg of changedPackages) {
      const directDependents = Array.from(this.dependents.get(pkg) || new Set());
      const transitiveDependents = Array.from(this.getDependentsClosure(pkg));

      // Calculate impact level
      let impactLevel = 'none';
      const transitiveCount = transitiveDependents.length;

      if (transitiveCount === 0) {
        impactLevel = 'none';
      } else if (transitiveCount <= 2) {
        impactLevel = 'low';
      } else if (transitiveCount <= 5) {
        impactLevel = 'medium';
      } else if (transitiveCount <= 10) {
        impactLevel = 'high';
      } else {
        impactLevel = 'critical';
      }

      impact[pkg] = {
        directDependents,
        transitiveDependents,
        impactLevel
      };
    }

    return impact;
  }

  /**
   * Resolve dependencies and compute execution order
   *
   * Main entry point for dependency resolution.
   *
   * @param {string[]} changedPackages - Packages being changed
   * @returns {Object} Resolution result
   */
  resolve(changedPackages) {
    // Validate input packages exist
    for (const pkg of changedPackages) {
      if (!this.packages.has(pkg)) {
        return {
          success: false,
          order: [],
          closure: [],
          levels: [],
          hasCycle: false,
          error: `Package not found: ${pkg}`
        };
      }
    }

    // Check for cycles
    const cycleResult = this.detectCycle();
    if (cycleResult.hasCycle) {
      return {
        success: false,
        order: [],
        closure: [],
        levels: [],
        hasCycle: true,
        cyclePackages: cycleResult.cyclePackages,
        error: `Cycle detected: ${cycleResult.cyclePackages.join(' -> ')}`
      };
    }

    // Compute closure
    const closure = this.computeClosure(changedPackages);

    // Topological sort of closure
    const sortResult = this.topologicalSort(closure);

    if (!sortResult.success) {
      return {
        success: false,
        order: sortResult.order,
        closure: Array.from(closure),
        levels: sortResult.levels,
        hasCycle: true,
        error: 'Failed to sort - cycle in closure'
      };
    }

    // Compute impact analysis
    const impactAnalysis = this.analyzeImpact(changedPackages);

    return {
      success: true,
      order: sortResult.order,
      closure: Array.from(closure),
      levels: sortResult.levels,
      hasCycle: false,
      impactAnalysis,
      stats: {
        totalPackages: this.packages.size,
        affectedPackages: closure.size,
        executionLevels: sortResult.levels.length,
        changedPackages: changedPackages.length
      }
    };
  }

  /**
   * Get root packages (no dependencies in graph)
   *
   * @returns {string[]}
   */
  getRootPackages() {
    const roots = [];
    for (const [pkg, deps] of this.dependencies) {
      const internalDeps = Array.from(deps).filter(d => this.packages.has(d));
      if (internalDeps.length === 0) {
        roots.push(pkg);
      }
    }
    return roots;
  }

  /**
   * Get leaf packages (no dependents in graph)
   *
   * @returns {string[]}
   */
  getLeafPackages() {
    const leaves = [];
    for (const [pkg, deps] of this.dependents) {
      if (deps.size === 0) {
        leaves.push(pkg);
      }
    }
    return leaves;
  }

  /**
   * Export dependency graph
   *
   * @returns {Object} Graph representation
   */
  toJSON() {
    const edges = [];
    for (const [pkg, deps] of this.dependencies) {
      for (const dep of deps) {
        if (this.packages.has(dep)) {
          edges.push([pkg, dep]);
        }
      }
    }

    return {
      packages: Object.fromEntries(this.packages),
      edges,
      rootPackages: this.getRootPackages(),
      leafPackages: this.getLeafPackages(),
      stats: {
        totalPackages: this.packages.size,
        totalEdges: edges.length
      }
    };
  }

  /**
   * Create resolver from package.json files
   *
   * @param {Object<string, Object>} packageJsons - Map of name -> package.json content
   * @param {Object} [options] - Resolver options
   * @returns {DependencyResolver}
   */
  static fromPackageJsons(packageJsons, options = {}) {
    const resolver = new DependencyResolver(options);

    for (const [name, packageJson] of Object.entries(packageJsons)) {
      resolver.addPackage(name, {
        version: packageJson.version,
        path: packageJson.path,
        dependencies: Object.keys(packageJson.dependencies || {}),
        devDependencies: Object.keys(packageJson.devDependencies || {}),
        peerDependencies: Object.keys(packageJson.peerDependencies || {}),
        optionalDependencies: Object.keys(packageJson.optionalDependencies || {})
      });
    }

    return resolver;
  }
}

/**
 * Create a dependency resolver
 *
 * @param {Object} [options] - Resolver options
 * @returns {DependencyResolver}
 */
export function createDependencyResolver(options = {}) {
  return new DependencyResolver(options);
}
