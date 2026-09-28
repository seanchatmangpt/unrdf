/**
 * @fileoverview Dependency Graph Builder for Monorepo Analysis
 *
 * Loads all package.json files from a monorepo and builds a directed acyclic graph (DAG)
 * representing the dependency relationships between packages.
 *
 * @module dependency-analyzer/dependency-graph
 */

import { readFileSync, readdirSync, statSync, existsSync } from 'node:fs';
import { join, dirname, basename, resolve } from 'node:path';

/**
 * @typedef {Object} PackageInfo
 * @property {string} name - Package name (e.g., "@unrdf/core")
 * @property {string} version - Package version
 * @property {string} path - Absolute path to the package directory
 * @property {string} packageJsonPath - Absolute path to package.json
 * @property {string[]} workspaceDependencies - List of workspace package names this depends on
 * @property {string[]} externalDependencies - List of external package names
 * @property {string[]} devDependencies - List of dev dependency names
 * @property {string[]} peerDependencies - List of peer dependency names
 * @property {string[]} optionalDependencies - List of optional dependency names
 * @property {boolean} isPrivate - Whether the package is private
 * @property {Object} rawPackageJson - The raw package.json content
 */

/**
 * @typedef {Object} DependencyEdge
 * @property {string} from - Source package name
 * @property {string} to - Target package name
 * @property {string} type - Dependency type: 'production' | 'dev' | 'peer' | 'optional'
 */

/**
 * @typedef {Object} DependencyGraph
 * @property {Map<string, PackageInfo>} packages - Map of package name to PackageInfo
 * @property {DependencyEdge[]} edges - All dependency edges
 * @property {Map<string, Set<string>>} adjacencyList - Adjacency list for forward traversal
 * @property {Map<string, Set<string>>} reverseAdjacencyList - Reverse adjacency list (dependents)
 * @property {string} rootPath - Root path of the monorepo
 * @property {Date} analyzedAt - Timestamp of analysis
 */

/**
 * Configuration options for the dependency graph builder
 * @typedef {Object} GraphBuilderOptions
 * @property {string} rootPath - Root path of the monorepo
 * @property {string[]} [packageGlobs=['packages/*']] - Glob patterns for package directories
 * @property {boolean} [includeDevDependencies=false] - Include dev dependencies in graph
 * @property {boolean} [includePeerDependencies=false] - Include peer dependencies in graph
 * @property {boolean} [includeOptionalDependencies=true] - Include optional dependencies in graph
 * @property {string[]} [excludePatterns=['node_modules', '.git']] - Patterns to exclude
 */

const DEFAULT_OPTIONS = {
  packageGlobs: ['packages/*'],
  includeDevDependencies: false,
  includePeerDependencies: false,
  includeOptionalDependencies: true,
  excludePatterns: ['node_modules', '.git', 'dist', 'coverage', '.nyc_output']
};

/**
 * Recursively find all package.json files in a directory
 * @param {string} dir - Directory to search
 * @param {string[]} excludePatterns - Patterns to exclude
 * @param {number} [maxDepth=5] - Maximum recursion depth
 * @param {number} [currentDepth=0] - Current recursion depth
 * @returns {string[]} Array of package.json file paths
 */
function findPackageJsonFiles(dir, excludePatterns, maxDepth = 5, currentDepth = 0) {
  if (currentDepth > maxDepth) {
    return [];
  }

  const results = [];

  try {
    const entries = readdirSync(dir, { withFileTypes: true });

    for (const entry of entries) {
      const fullPath = join(dir, entry.name);

      // Skip excluded patterns
      if (excludePatterns.some(pattern => entry.name.includes(pattern))) {
        continue;
      }

      if (entry.isDirectory()) {
        results.push(...findPackageJsonFiles(fullPath, excludePatterns, maxDepth, currentDepth + 1));
      } else if (entry.name === 'package.json') {
        results.push(fullPath);
      }
    }
  } catch (error) {
    // Skip directories we can't read
    if (error.code !== 'ENOENT' && error.code !== 'EACCES') {
      console.warn(`Warning: Could not read directory ${dir}: ${error.message}`);
    }
  }

  return results;
}

/**
 * Parse a package.json file and extract relevant information
 * @param {string} packageJsonPath - Path to the package.json file
 * @returns {PackageInfo|null} Parsed package information or null if invalid
 */
function parsePackageJson(packageJsonPath) {
  try {
    const content = readFileSync(packageJsonPath, 'utf-8');
    const pkg = JSON.parse(content);

    if (!pkg.name) {
      console.warn(`Warning: Package at ${packageJsonPath} has no name, skipping`);
      return null;
    }

    const packageDir = dirname(packageJsonPath);

    // Extract workspace dependencies (those using workspace: protocol)
    const extractWorkspaceDeps = (deps) => {
      if (!deps) return [];
      return Object.entries(deps)
        .filter(([_, version]) => String(version).startsWith('workspace:'))
        .map(([name]) => name);
    };

    // Extract external dependencies (non-workspace)
    const extractExternalDeps = (deps) => {
      if (!deps) return [];
      return Object.entries(deps)
        .filter(([_, version]) => !String(version).startsWith('workspace:'))
        .map(([name]) => name);
    };

    // Combine all production workspace dependencies
    const workspaceDependencies = [
      ...extractWorkspaceDeps(pkg.dependencies),
      ...extractWorkspaceDeps(pkg.optionalDependencies)
    ];

    return {
      name: pkg.name,
      version: pkg.version || '0.0.0',
      path: packageDir,
      packageJsonPath,
      workspaceDependencies: [...new Set(workspaceDependencies)],
      externalDependencies: extractExternalDeps(pkg.dependencies),
      devDependencies: Object.keys(pkg.devDependencies || {}),
      peerDependencies: Object.keys(pkg.peerDependencies || {}),
      optionalDependencies: extractWorkspaceDeps(pkg.optionalDependencies),
      isPrivate: pkg.private === true,
      rawPackageJson: pkg
    };
  } catch (error) {
    console.warn(`Warning: Could not parse ${packageJsonPath}: ${error.message}`);
    return null;
  }
}

/**
 * Build a dependency graph from all packages in the monorepo
 * @param {GraphBuilderOptions} options - Configuration options
 * @returns {DependencyGraph} The constructed dependency graph
 */
export function buildDependencyGraph(options) {
  const config = { ...DEFAULT_OPTIONS, ...options };
  const { rootPath, excludePatterns, includeDevDependencies, includePeerDependencies, includeOptionalDependencies } = config;

  // Find all package.json files
  console.log(`\n[DependencyGraph] Scanning ${rootPath} for packages...`);
  const packageJsonPaths = findPackageJsonFiles(rootPath, excludePatterns);
  console.log(`[DependencyGraph] Found ${packageJsonPaths.length} package.json files`);

  // Parse all packages
  /** @type {Map<string, PackageInfo>} */
  const packages = new Map();
  const workspacePackageNames = new Set();

  for (const pkgPath of packageJsonPaths) {
    const info = parsePackageJson(pkgPath);
    if (info) {
      // Filter to only include @unrdf packages or packages in the packages directory
      const isMonorepoPackage = info.name.startsWith('@unrdf/') ||
        pkgPath.includes('/packages/') ||
        pkgPath.includes('/src/') ||
        pkgPath.includes('/apps/');

      if (isMonorepoPackage) {
        packages.set(info.name, info);
        workspacePackageNames.add(info.name);
      }
    }
  }

  console.log(`[DependencyGraph] Parsed ${packages.size} workspace packages`);

  // Build edges
  /** @type {DependencyEdge[]} */
  const edges = [];
  /** @type {Map<string, Set<string>>} */
  const adjacencyList = new Map();
  /** @type {Map<string, Set<string>>} */
  const reverseAdjacencyList = new Map();

  // Initialize adjacency lists for all packages
  for (const pkgName of packages.keys()) {
    adjacencyList.set(pkgName, new Set());
    reverseAdjacencyList.set(pkgName, new Set());
  }

  // Build edges from dependencies
  for (const [pkgName, pkgInfo] of packages) {
    // Production workspace dependencies
    for (const dep of pkgInfo.workspaceDependencies) {
      if (workspacePackageNames.has(dep)) {
        edges.push({ from: pkgName, to: dep, type: 'production' });
        adjacencyList.get(pkgName).add(dep);
        reverseAdjacencyList.get(dep)?.add(pkgName);
      }
    }

    // Optional dependencies (workspace only)
    if (includeOptionalDependencies) {
      for (const dep of pkgInfo.optionalDependencies) {
        if (workspacePackageNames.has(dep) && !pkgInfo.workspaceDependencies.includes(dep)) {
          edges.push({ from: pkgName, to: dep, type: 'optional' });
          adjacencyList.get(pkgName).add(dep);
          reverseAdjacencyList.get(dep)?.add(pkgName);
        }
      }
    }

    // Dev dependencies (if enabled, check for workspace: deps)
    if (includeDevDependencies) {
      const rawDev = pkgInfo.rawPackageJson.devDependencies || {};
      for (const [depName, depVersion] of Object.entries(rawDev)) {
        if (String(depVersion).startsWith('workspace:') && workspacePackageNames.has(depName)) {
          edges.push({ from: pkgName, to: depName, type: 'dev' });
          adjacencyList.get(pkgName).add(depName);
          reverseAdjacencyList.get(depName)?.add(pkgName);
        }
      }
    }

    // Peer dependencies (if enabled, check for workspace: deps)
    if (includePeerDependencies) {
      const rawPeer = pkgInfo.rawPackageJson.peerDependencies || {};
      for (const [depName, depVersion] of Object.entries(rawPeer)) {
        if (String(depVersion).startsWith('workspace:') && workspacePackageNames.has(depName)) {
          edges.push({ from: pkgName, to: depName, type: 'peer' });
          adjacencyList.get(pkgName).add(depName);
          reverseAdjacencyList.get(depName)?.add(pkgName);
        }
      }
    }
  }

  console.log(`[DependencyGraph] Built ${edges.length} dependency edges`);

  return {
    packages,
    edges,
    adjacencyList,
    reverseAdjacencyList,
    rootPath,
    analyzedAt: new Date()
  };
}

/**
 * Get all packages that depend on a given package (direct dependents)
 * @param {DependencyGraph} graph - The dependency graph
 * @param {string} packageName - The package name to check
 * @returns {string[]} List of package names that depend on this package
 */
export function getDirectDependents(graph, packageName) {
  return Array.from(graph.reverseAdjacencyList.get(packageName) || []);
}

/**
 * Get all packages that a given package depends on (direct dependencies)
 * @param {DependencyGraph} graph - The dependency graph
 * @param {string} packageName - The package name to check
 * @returns {string[]} List of package names this package depends on
 */
export function getDirectDependencies(graph, packageName) {
  return Array.from(graph.adjacencyList.get(packageName) || []);
}

/**
 * Get all transitive dependents of a package (impact analysis)
 * @param {DependencyGraph} graph - The dependency graph
 * @param {string} packageName - The package name to check
 * @returns {Set<string>} Set of all package names that transitively depend on this package
 */
export function getTransitiveDependents(graph, packageName) {
  const visited = new Set();
  const stack = [packageName];

  while (stack.length > 0) {
    const current = stack.pop();
    if (visited.has(current)) continue;
    visited.add(current);

    const dependents = graph.reverseAdjacencyList.get(current) || new Set();
    for (const dep of dependents) {
      if (!visited.has(dep)) {
        stack.push(dep);
      }
    }
  }

  visited.delete(packageName); // Remove the package itself
  return visited;
}

/**
 * Get all transitive dependencies of a package
 * @param {DependencyGraph} graph - The dependency graph
 * @param {string} packageName - The package name to check
 * @returns {Set<string>} Set of all package names this package transitively depends on
 */
export function getTransitiveDependencies(graph, packageName) {
  const visited = new Set();
  const stack = [packageName];

  while (stack.length > 0) {
    const current = stack.pop();
    if (visited.has(current)) continue;
    visited.add(current);

    const dependencies = graph.adjacencyList.get(current) || new Set();
    for (const dep of dependencies) {
      if (!visited.has(dep)) {
        stack.push(dep);
      }
    }
  }

  visited.delete(packageName); // Remove the package itself
  return visited;
}

/**
 * Find all leaf packages (packages with no dependencies)
 * @param {DependencyGraph} graph - The dependency graph
 * @returns {string[]} List of leaf package names
 */
export function findLeafPackages(graph) {
  const leaves = [];
  for (const [pkgName, deps] of graph.adjacencyList) {
    if (deps.size === 0) {
      leaves.push(pkgName);
    }
  }
  return leaves.sort();
}

/**
 * Find all root packages (packages with no dependents)
 * @param {DependencyGraph} graph - The dependency graph
 * @returns {string[]} List of root package names
 */
export function findRootPackages(graph) {
  const roots = [];
  for (const [pkgName, dependents] of graph.reverseAdjacencyList) {
    if (dependents.size === 0) {
      roots.push(pkgName);
    }
  }
  return roots.sort();
}

/**
 * Perform topological sort on the graph
 * @param {DependencyGraph} graph - The dependency graph
 * @returns {{ sorted: string[], hasCycle: boolean }} Topologically sorted packages or empty with hasCycle=true
 */
export function topologicalSort(graph) {
  const inDegree = new Map();
  const sorted = [];
  const queue = [];

  // Initialize in-degrees
  for (const pkgName of graph.packages.keys()) {
    inDegree.set(pkgName, 0);
  }

  // Calculate in-degrees
  for (const [pkgName, deps] of graph.adjacencyList) {
    for (const dep of deps) {
      inDegree.set(dep, (inDegree.get(dep) || 0) + 1);
    }
  }

  // Find initial nodes with zero in-degree
  for (const [pkgName, degree] of inDegree) {
    if (degree === 0) {
      queue.push(pkgName);
    }
  }

  // Process queue
  while (queue.length > 0) {
    queue.sort(); // Ensure deterministic order
    const current = queue.shift();
    sorted.push(current);

    const deps = graph.adjacencyList.get(current) || new Set();
    for (const dep of deps) {
      const newDegree = inDegree.get(dep) - 1;
      inDegree.set(dep, newDegree);
      if (newDegree === 0) {
        queue.push(dep);
      }
    }
  }

  const hasCycle = sorted.length !== graph.packages.size;

  return { sorted, hasCycle };
}

/**
 * Generate a summary of the dependency graph
 * @param {DependencyGraph} graph - The dependency graph
 * @returns {Object} Summary statistics
 */
export function generateGraphSummary(graph) {
  const leafPackages = findLeafPackages(graph);
  const rootPackages = findRootPackages(graph);
  const { sorted, hasCycle } = topologicalSort(graph);

  // Count edges by type
  const edgesByType = { production: 0, dev: 0, peer: 0, optional: 0 };
  for (const edge of graph.edges) {
    edgesByType[edge.type]++;
  }

  // Calculate average dependencies
  let totalDeps = 0;
  let totalDependents = 0;
  for (const [_, deps] of graph.adjacencyList) {
    totalDeps += deps.size;
  }
  for (const [_, deps] of graph.reverseAdjacencyList) {
    totalDependents += deps.size;
  }

  const avgDeps = graph.packages.size > 0 ? totalDeps / graph.packages.size : 0;
  const avgDependents = graph.packages.size > 0 ? totalDependents / graph.packages.size : 0;

  return {
    totalPackages: graph.packages.size,
    totalEdges: graph.edges.length,
    edgesByType,
    leafPackages: leafPackages.length,
    rootPackages: rootPackages.length,
    hasCycle,
    avgDependencies: avgDeps.toFixed(2),
    avgDependents: avgDependents.toFixed(2),
    analyzedAt: graph.analyzedAt.toISOString()
  };
}

/**
 * Export the graph to a JSON-serializable format
 * @param {DependencyGraph} graph - The dependency graph
 * @returns {Object} JSON-serializable representation
 */
export function exportGraphToJSON(graph) {
  const packages = [];
  for (const [name, info] of graph.packages) {
    packages.push({
      name,
      version: info.version,
      path: info.path,
      isPrivate: info.isPrivate,
      workspaceDependencies: info.workspaceDependencies,
      externalDependencies: info.externalDependencies.length,
      devDependencies: info.devDependencies.length
    });
  }

  const adjacencyList = {};
  for (const [pkg, deps] of graph.adjacencyList) {
    adjacencyList[pkg] = Array.from(deps);
  }

  return {
    metadata: {
      rootPath: graph.rootPath,
      analyzedAt: graph.analyzedAt.toISOString(),
      summary: generateGraphSummary(graph)
    },
    packages,
    edges: graph.edges,
    adjacencyList
  };
}

/**
 * Print a text-based visualization of the graph
 * @param {DependencyGraph} graph - The dependency graph
 * @returns {string} Text visualization
 */
export function visualizeGraphAsText(graph) {
  const lines = [];
  lines.push('='.repeat(80));
  lines.push('DEPENDENCY GRAPH VISUALIZATION');
  lines.push('='.repeat(80));
  lines.push('');

  const summary = generateGraphSummary(graph);
  lines.push(`Analyzed: ${summary.analyzedAt}`);
  lines.push(`Total Packages: ${summary.totalPackages}`);
  lines.push(`Total Edges: ${summary.totalEdges}`);
  lines.push(`Has Cycles: ${summary.hasCycle ? 'YES (CRITICAL!)' : 'No'}`);
  lines.push('');

  // Sort packages by number of dependents (most depended on first)
  const sortedByDependents = Array.from(graph.packages.keys()).sort((a, b) => {
    const depsA = graph.reverseAdjacencyList.get(a)?.size || 0;
    const depsB = graph.reverseAdjacencyList.get(b)?.size || 0;
    return depsB - depsA;
  });

  lines.push('-'.repeat(80));
  lines.push('PACKAGES BY DEPENDENTS (Most Critical First)');
  lines.push('-'.repeat(80));

  for (const pkgName of sortedByDependents) {
    const deps = graph.adjacencyList.get(pkgName) || new Set();
    const dependents = graph.reverseAdjacencyList.get(pkgName) || new Set();

    const shortName = pkgName.replace('@unrdf/', '');
    const depsStr = deps.size > 0 ? ` -> [${Array.from(deps).map(d => d.replace('@unrdf/', '')).join(', ')}]` : '';
    const dependentsCount = dependents.size;

    lines.push(`[${dependentsCount.toString().padStart(2)}] ${shortName.padEnd(25)}${depsStr}`);
  }

  lines.push('');
  lines.push('-'.repeat(80));
  lines.push('LEGEND');
  lines.push('-'.repeat(80));
  lines.push('[N] = Number of packages that depend on this package');
  lines.push('->  = Dependencies this package has');
  lines.push('');

  return lines.join('\n');
}

export default {
  buildDependencyGraph,
  getDirectDependents,
  getDirectDependencies,
  getTransitiveDependents,
  getTransitiveDependencies,
  findLeafPackages,
  findRootPackages,
  topologicalSort,
  generateGraphSummary,
  exportGraphToJSON,
  visualizeGraphAsText
};
