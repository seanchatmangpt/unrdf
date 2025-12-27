/**
 * @fileoverview Metrics Computation for Dependency Graphs
 *
 * Computes comprehensive metrics for monorepo dependency analysis including:
 * - Depth: Maximum transitive dependency chain length
 * - Fan-in: Number of packages depending on each package (brittleness indicator)
 * - Fan-out: Number of dependencies each package has (coupling indicator)
 * - Unused: Dependencies declared but not actually imported
 * - Stability: Martin's instability metric
 * - Health scores: Overall package health assessment
 *
 * @module dependency-analyzer/metrics-computer
 */

import { readFileSync, readdirSync, existsSync } from 'node:fs';
import { join, relative } from 'node:path';

/**
 * @typedef {Object} PackageMetrics
 * @property {string} name - Package name
 * @property {DepthMetrics} depth - Depth-related metrics
 * @property {FanMetrics} fan - Fan-in and fan-out metrics
 * @property {StabilityMetrics} stability - Martin's stability metrics
 * @property {UsageMetrics} usage - Usage and unused dependency metrics
 * @property {number} healthScore - Overall health score (0-100)
 * @property {string[]} warnings - List of warnings for this package
 */

/**
 * @typedef {Object} DepthMetrics
 * @property {number} maxDepth - Maximum depth to any leaf
 * @property {number} avgDepth - Average depth to leaves
 * @property {string[]} longestPath - The longest dependency path
 * @property {number} level - Level in topological order (0 = leaf)
 */

/**
 * @typedef {Object} FanMetrics
 * @property {number} fanIn - Number of packages depending on this
 * @property {number} fanOut - Number of dependencies this has
 * @property {number} transitiveFanIn - Transitive dependents count
 * @property {number} transitiveFanOut - Transitive dependencies count
 * @property {string[]} directDependents - List of direct dependents
 * @property {string[]} directDependencies - List of direct dependencies
 */

/**
 * @typedef {Object} StabilityMetrics
 * @property {number} instability - I = Ce / (Ca + Ce), 0 = stable, 1 = unstable
 * @property {number} abstractness - A = abstract elements / total elements (approximation)
 * @property {number} distanceFromMain - D = |A + I - 1|, distance from main sequence
 * @property {string} zone - 'zone_of_pain' | 'zone_of_uselessness' | 'balanced'
 */

/**
 * @typedef {Object} UsageMetrics
 * @property {string[]} declaredDependencies - All declared workspace dependencies
 * @property {string[]} usedDependencies - Dependencies that are actually imported
 * @property {string[]} unusedDependencies - Declared but not imported
 * @property {string[]} phantomDependencies - Imported but not declared
 * @property {number} unusedRatio - Ratio of unused to declared
 */

/**
 * @typedef {Object} MonorepoMetrics
 * @property {Map<string, PackageMetrics>} packageMetrics - Per-package metrics
 * @property {AggregateMetrics} aggregate - Aggregate metrics across all packages
 * @property {string[]} criticalPackages - Packages with high brittleness risk
 * @property {string[]} unstablePackages - Packages in zone of pain
 * @property {Date} computedAt - Timestamp of computation
 */

/**
 * @typedef {Object} AggregateMetrics
 * @property {number} totalPackages - Total number of packages
 * @property {number} totalEdges - Total dependency edges
 * @property {number} maxDepth - Maximum depth across all packages
 * @property {number} avgFanIn - Average fan-in
 * @property {number} avgFanOut - Average fan-out
 * @property {number} maxFanIn - Maximum fan-in (most depended on)
 * @property {number} maxFanOut - Maximum fan-out (most coupled)
 * @property {number} avgInstability - Average instability
 * @property {number} overallHealthScore - Overall monorepo health
 */

/**
 * Compute all metrics for a dependency graph
 * @param {import('./dependency-graph.mjs').DependencyGraph} graph - The dependency graph
 * @param {Object} options - Computation options
 * @param {boolean} [options.analyzeUsage=false] - Whether to analyze actual import usage
 * @returns {MonorepoMetrics} Complete metrics
 */
export function computeMetrics(graph, options = {}) {
  const { analyzeUsage = false } = options;

  console.log('\n[MetricsComputer] Computing dependency metrics...');

  // Compute per-package metrics
  const packageMetrics = new Map();
  const depthResults = computeAllDepths(graph);
  const fanResults = computeAllFanMetrics(graph);
  const stabilityResults = computeAllStabilityMetrics(graph);

  // Optionally analyze actual usage
  let usageResults = null;
  if (analyzeUsage) {
    console.log('[MetricsComputer] Analyzing import usage (this may take a moment)...');
    usageResults = analyzeAllUsage(graph);
  }

  // Combine results for each package
  for (const [pkgName, pkgInfo] of graph.packages) {
    const depth = depthResults.get(pkgName);
    const fan = fanResults.get(pkgName);
    const stability = stabilityResults.get(pkgName);
    const usage = usageResults ? usageResults.get(pkgName) : createDefaultUsageMetrics(pkgInfo);

    const warnings = generatePackageWarnings(pkgName, depth, fan, stability, usage);
    const healthScore = calculateHealthScore(depth, fan, stability, usage);

    packageMetrics.set(pkgName, {
      name: pkgName,
      depth,
      fan,
      stability,
      usage,
      healthScore,
      warnings
    });
  }

  // Compute aggregate metrics
  const aggregate = computeAggregateMetrics(packageMetrics, graph);

  // Find critical and unstable packages
  const criticalPackages = findCriticalPackages(packageMetrics);
  const unstablePackages = findUnstablePackages(packageMetrics);

  console.log(`[MetricsComputer] Computed metrics for ${packageMetrics.size} packages`);

  return {
    packageMetrics,
    aggregate,
    criticalPackages,
    unstablePackages,
    computedAt: new Date()
  };
}

/**
 * Compute depth metrics for all packages
 * @param {import('./dependency-graph.mjs').DependencyGraph} graph - The dependency graph
 * @returns {Map<string, DepthMetrics>} Depth metrics per package
 */
function computeAllDepths(graph) {
  const results = new Map();
  const memo = new Map(); // Memoization for depth calculations

  /**
   * Recursively compute max depth and longest path for a package
   * @param {string} pkgName - Package name
   * @param {Set<string>} visiting - Set of packages currently being visited (cycle detection)
   * @returns {{ depth: number, path: string[] }} Depth and path
   */
  function computeDepth(pkgName, visiting = new Set()) {
    if (memo.has(pkgName)) {
      return memo.get(pkgName);
    }

    // Cycle detection
    if (visiting.has(pkgName)) {
      return { depth: 0, path: [pkgName] };
    }

    const deps = graph.adjacencyList.get(pkgName) || new Set();

    if (deps.size === 0) {
      // Leaf package
      const result = { depth: 0, path: [pkgName] };
      memo.set(pkgName, result);
      return result;
    }

    visiting.add(pkgName);

    let maxDepth = 0;
    let longestPath = [pkgName];

    for (const dep of deps) {
      const { depth, path } = computeDepth(dep, visiting);
      if (depth + 1 > maxDepth) {
        maxDepth = depth + 1;
        longestPath = [pkgName, ...path];
      }
    }

    visiting.delete(pkgName);

    const result = { depth: maxDepth, path: longestPath };
    memo.set(pkgName, result);
    return result;
  }

  // Compute level (topological order position)
  const levels = computeTopologicalLevels(graph);

  for (const pkgName of graph.packages.keys()) {
    const { depth, path } = computeDepth(pkgName);

    // Calculate average depth (simplified - just max depth for now)
    results.set(pkgName, {
      maxDepth: depth,
      avgDepth: depth, // Could be refined with more analysis
      longestPath: path,
      level: levels.get(pkgName) || 0
    });
  }

  return results;
}

/**
 * Compute topological levels (0 = leaf, higher = more dependencies)
 * @param {import('./dependency-graph.mjs').DependencyGraph} graph - The dependency graph
 * @returns {Map<string, number>} Level per package
 */
function computeTopologicalLevels(graph) {
  const levels = new Map();
  const visited = new Set();

  function computeLevel(pkgName) {
    if (levels.has(pkgName)) {
      return levels.get(pkgName);
    }

    if (visited.has(pkgName)) {
      return 0; // Cycle fallback
    }
    visited.add(pkgName);

    const deps = graph.adjacencyList.get(pkgName) || new Set();

    if (deps.size === 0) {
      levels.set(pkgName, 0);
      return 0;
    }

    let maxDepLevel = 0;
    for (const dep of deps) {
      maxDepLevel = Math.max(maxDepLevel, computeLevel(dep));
    }

    const level = maxDepLevel + 1;
    levels.set(pkgName, level);
    return level;
  }

  for (const pkgName of graph.packages.keys()) {
    computeLevel(pkgName);
  }

  return levels;
}

/**
 * Compute fan-in and fan-out metrics for all packages
 * @param {import('./dependency-graph.mjs').DependencyGraph} graph - The dependency graph
 * @returns {Map<string, FanMetrics>} Fan metrics per package
 */
function computeAllFanMetrics(graph) {
  const results = new Map();

  // Pre-compute transitive closures
  const transitiveDepsMemo = new Map();
  const transitiveDepsMemoReverse = new Map();

  function getTransitiveDeps(pkgName, adjList, memo, visited = new Set()) {
    if (memo.has(pkgName)) {
      return memo.get(pkgName);
    }

    if (visited.has(pkgName)) {
      return new Set();
    }
    visited.add(pkgName);

    const result = new Set();
    const deps = adjList.get(pkgName) || new Set();

    for (const dep of deps) {
      result.add(dep);
      const transitive = getTransitiveDeps(dep, adjList, memo, visited);
      for (const t of transitive) {
        result.add(t);
      }
    }

    memo.set(pkgName, result);
    return result;
  }

  for (const pkgName of graph.packages.keys()) {
    const directDeps = graph.adjacencyList.get(pkgName) || new Set();
    const directDependents = graph.reverseAdjacencyList.get(pkgName) || new Set();

    // Compute transitive closures
    const transitiveDeps = getTransitiveDeps(pkgName, graph.adjacencyList, transitiveDepsMemo);
    const transitiveDependents = getTransitiveDeps(pkgName, graph.reverseAdjacencyList, transitiveDepsMemoReverse);

    results.set(pkgName, {
      fanIn: directDependents.size,
      fanOut: directDeps.size,
      transitiveFanIn: transitiveDependents.size,
      transitiveFanOut: transitiveDeps.size,
      directDependents: Array.from(directDependents).sort(),
      directDependencies: Array.from(directDeps).sort()
    });
  }

  return results;
}

/**
 * Compute Martin's stability metrics for all packages
 * Based on Robert Martin's metrics from "Agile Software Development"
 *
 * @param {import('./dependency-graph.mjs').DependencyGraph} graph - The dependency graph
 * @returns {Map<string, StabilityMetrics>} Stability metrics per package
 */
function computeAllStabilityMetrics(graph) {
  const results = new Map();

  for (const pkgName of graph.packages.keys()) {
    const Ca = (graph.reverseAdjacencyList.get(pkgName) || new Set()).size; // Afferent coupling
    const Ce = (graph.adjacencyList.get(pkgName) || new Set()).size;       // Efferent coupling

    // Instability: I = Ce / (Ca + Ce)
    // 0 = maximally stable, 1 = maximally unstable
    const instability = (Ca + Ce) === 0 ? 0 : Ce / (Ca + Ce);

    // Abstractness: approximation based on package characteristics
    // We estimate this based on whether the package is a utility/interface package
    const pkgInfo = graph.packages.get(pkgName);
    const abstractness = estimateAbstractness(pkgInfo);

    // Distance from main sequence: D = |A + I - 1|
    // 0 = on the main sequence, higher = problematic
    const distanceFromMain = Math.abs(abstractness + instability - 1);

    // Determine zone
    let zone = 'balanced';
    if (abstractness < 0.3 && instability < 0.3) {
      zone = 'zone_of_pain'; // Concrete and stable - hard to modify
    } else if (abstractness > 0.7 && instability > 0.7) {
      zone = 'zone_of_uselessness'; // Abstract but dependent - unused abstractions
    }

    results.set(pkgName, {
      instability: Math.round(instability * 100) / 100,
      abstractness: Math.round(abstractness * 100) / 100,
      distanceFromMain: Math.round(distanceFromMain * 100) / 100,
      zone
    });
  }

  return results;
}

/**
 * Estimate abstractness of a package based on its characteristics
 * This is an approximation since we don't parse actual code
 *
 * @param {Object} pkgInfo - Package information
 * @returns {number} Estimated abstractness (0-1)
 */
function estimateAbstractness(pkgInfo) {
  if (!pkgInfo) return 0.5;

  const name = pkgInfo.name.toLowerCase();

  // Heuristics for abstractness
  // Interface/abstract packages tend to have certain naming patterns
  if (name.includes('interface') || name.includes('abstract') || name.includes('types')) {
    return 0.9;
  }

  if (name.includes('core') || name.includes('common') || name.includes('shared')) {
    return 0.7;
  }

  if (name.includes('utils') || name.includes('helpers') || name.includes('lib')) {
    return 0.6;
  }

  if (name.includes('impl') || name.includes('concrete') || name.includes('service')) {
    return 0.2;
  }

  if (name.includes('app') || name.includes('cli') || name.includes('web')) {
    return 0.1;
  }

  // Default based on dependency count
  const depCount = pkgInfo.workspaceDependencies?.length || 0;
  if (depCount === 0) return 0.6; // Likely a utility/base package
  if (depCount > 5) return 0.2;   // Likely a concrete implementation

  return 0.4; // Default
}

/**
 * Analyze actual import usage in source files
 * @param {import('./dependency-graph.mjs').DependencyGraph} graph - The dependency graph
 * @returns {Map<string, UsageMetrics>} Usage metrics per package
 */
function analyzeAllUsage(graph) {
  const results = new Map();

  for (const [pkgName, pkgInfo] of graph.packages) {
    const declaredDeps = pkgInfo.workspaceDependencies;
    const usedDeps = new Set();
    const phantomDeps = new Set();

    // Scan source files for imports
    const srcDir = join(pkgInfo.path, 'src');
    if (existsSync(srcDir)) {
      const imports = scanDirectoryForImports(srcDir);

      for (const imp of imports) {
        // Check if it's a workspace package
        const matchingDep = declaredDeps.find(d =>
          imp.startsWith(d) || imp === d.replace('@unrdf/', '@unrdf/')
        );

        if (matchingDep) {
          usedDeps.add(matchingDep);
        } else if (imp.startsWith('@unrdf/')) {
          // Imported but not declared
          phantomDeps.add(imp.split('/').slice(0, 2).join('/'));
        }
      }
    }

    const unusedDeps = declaredDeps.filter(d => !usedDeps.has(d));
    const unusedRatio = declaredDeps.length > 0
      ? unusedDeps.length / declaredDeps.length
      : 0;

    results.set(pkgName, {
      declaredDependencies: declaredDeps,
      usedDependencies: Array.from(usedDeps),
      unusedDependencies: unusedDeps,
      phantomDependencies: Array.from(phantomDeps),
      unusedRatio: Math.round(unusedRatio * 100) / 100
    });
  }

  return results;
}

/**
 * Scan a directory for import statements
 * @param {string} dir - Directory to scan
 * @returns {Set<string>} Set of imported package names
 */
function scanDirectoryForImports(dir) {
  const imports = new Set();

  try {
    const entries = readdirSync(dir, { withFileTypes: true });

    for (const entry of entries) {
      const fullPath = join(dir, entry.name);

      if (entry.isDirectory() && !entry.name.startsWith('.')) {
        // Recursively scan subdirectories
        const subImports = scanDirectoryForImports(fullPath);
        for (const imp of subImports) {
          imports.add(imp);
        }
      } else if (entry.name.endsWith('.mjs') || entry.name.endsWith('.js')) {
        // Parse file for imports
        try {
          const content = readFileSync(fullPath, 'utf-8');
          const fileImports = extractImports(content);
          for (const imp of fileImports) {
            imports.add(imp);
          }
        } catch (err) {
          // Skip files we can't read
        }
      }
    }
  } catch (err) {
    // Skip directories we can't read
  }

  return imports;
}

/**
 * Extract import statements from JavaScript/TypeScript content
 * @param {string} content - File content
 * @returns {string[]} Array of imported module names
 */
function extractImports(content) {
  const imports = [];

  // Match: import ... from 'package'
  // Match: import 'package'
  // Match: require('package')
  const patterns = [
    /import\s+(?:.*?\s+from\s+)?['"]([^'"]+)['"]/g,
    /require\s*\(\s*['"]([^'"]+)['"]\s*\)/g,
    /import\s*\(\s*['"]([^'"]+)['"]\s*\)/g
  ];

  for (const pattern of patterns) {
    let match;
    while ((match = pattern.exec(content)) !== null) {
      const moduleName = match[1];
      // Only capture external packages (not relative imports)
      if (!moduleName.startsWith('.') && !moduleName.startsWith('/')) {
        imports.push(moduleName);
      }
    }
  }

  return imports;
}

/**
 * Create default usage metrics when not analyzing actual usage
 * @param {Object} pkgInfo - Package information
 * @returns {UsageMetrics} Default usage metrics
 */
function createDefaultUsageMetrics(pkgInfo) {
  return {
    declaredDependencies: pkgInfo.workspaceDependencies || [],
    usedDependencies: pkgInfo.workspaceDependencies || [], // Assume all used
    unusedDependencies: [],
    phantomDependencies: [],
    unusedRatio: 0
  };
}

/**
 * Generate warnings for a package based on its metrics
 * @param {string} pkgName - Package name
 * @param {DepthMetrics} depth - Depth metrics
 * @param {FanMetrics} fan - Fan metrics
 * @param {StabilityMetrics} stability - Stability metrics
 * @param {UsageMetrics} usage - Usage metrics
 * @returns {string[]} List of warnings
 */
function generatePackageWarnings(pkgName, depth, fan, stability, usage) {
  const warnings = [];

  // Depth warnings
  if (depth.maxDepth > 5) {
    warnings.push(`Deep dependency chain (${depth.maxDepth} levels) - consider flattening`);
  }

  // Fan-in warnings (brittleness)
  if (fan.fanIn >= 10) {
    warnings.push(`Critical package: ${fan.fanIn} packages depend on this - changes require extra care`);
  } else if (fan.fanIn >= 5) {
    warnings.push(`High fan-in (${fan.fanIn}) - this package is widely used`);
  }

  // Fan-out warnings (coupling)
  if (fan.fanOut >= 8) {
    warnings.push(`High coupling: ${fan.fanOut} direct dependencies - consider splitting`);
  }

  // Stability warnings
  if (stability.zone === 'zone_of_pain') {
    warnings.push('Zone of pain: concrete and stable - difficult to modify');
  } else if (stability.zone === 'zone_of_uselessness') {
    warnings.push('Zone of uselessness: abstract but unstable - may indicate unused abstractions');
  }

  if (stability.distanceFromMain > 0.4) {
    warnings.push(`Far from main sequence (D=${stability.distanceFromMain}) - may need refactoring`);
  }

  // Usage warnings
  if (usage.unusedDependencies.length > 0) {
    warnings.push(`Unused dependencies: ${usage.unusedDependencies.join(', ')}`);
  }

  if (usage.phantomDependencies.length > 0) {
    warnings.push(`Phantom dependencies (imported but not declared): ${usage.phantomDependencies.join(', ')}`);
  }

  return warnings;
}

/**
 * Calculate overall health score for a package
 * @param {DepthMetrics} depth - Depth metrics
 * @param {FanMetrics} fan - Fan metrics
 * @param {StabilityMetrics} stability - Stability metrics
 * @param {UsageMetrics} usage - Usage metrics
 * @returns {number} Health score (0-100)
 */
function calculateHealthScore(depth, fan, stability, usage) {
  let score = 100;

  // Depth penalty (max 20 points)
  if (depth.maxDepth > 3) {
    score -= Math.min(20, (depth.maxDepth - 3) * 4);
  }

  // Coupling penalty (max 20 points)
  if (fan.fanOut > 4) {
    score -= Math.min(20, (fan.fanOut - 4) * 4);
  }

  // Stability penalty (max 20 points)
  score -= stability.distanceFromMain * 20;

  // Zone penalties (max 15 points)
  if (stability.zone === 'zone_of_pain') {
    score -= 15;
  } else if (stability.zone === 'zone_of_uselessness') {
    score -= 10;
  }

  // Usage penalty (max 15 points)
  score -= usage.unusedRatio * 15;

  // Phantom dependency penalty
  score -= usage.phantomDependencies.length * 5;

  return Math.max(0, Math.round(score));
}

/**
 * Compute aggregate metrics across all packages
 * @param {Map<string, PackageMetrics>} packageMetrics - Per-package metrics
 * @param {import('./dependency-graph.mjs').DependencyGraph} graph - The dependency graph
 * @returns {AggregateMetrics} Aggregate metrics
 */
function computeAggregateMetrics(packageMetrics, graph) {
  let maxDepth = 0;
  let totalFanIn = 0;
  let totalFanOut = 0;
  let maxFanIn = 0;
  let maxFanOut = 0;
  let totalInstability = 0;
  let totalHealth = 0;

  for (const metrics of packageMetrics.values()) {
    maxDepth = Math.max(maxDepth, metrics.depth.maxDepth);
    totalFanIn += metrics.fan.fanIn;
    totalFanOut += metrics.fan.fanOut;
    maxFanIn = Math.max(maxFanIn, metrics.fan.fanIn);
    maxFanOut = Math.max(maxFanOut, metrics.fan.fanOut);
    totalInstability += metrics.stability.instability;
    totalHealth += metrics.healthScore;
  }

  const count = packageMetrics.size || 1;

  return {
    totalPackages: packageMetrics.size,
    totalEdges: graph.edges.length,
    maxDepth,
    avgFanIn: Math.round((totalFanIn / count) * 100) / 100,
    avgFanOut: Math.round((totalFanOut / count) * 100) / 100,
    maxFanIn,
    maxFanOut,
    avgInstability: Math.round((totalInstability / count) * 100) / 100,
    overallHealthScore: Math.round(totalHealth / count)
  };
}

/**
 * Find packages with high brittleness risk
 * @param {Map<string, PackageMetrics>} packageMetrics - Per-package metrics
 * @returns {string[]} List of critical package names
 */
function findCriticalPackages(packageMetrics) {
  const critical = [];

  for (const [pkgName, metrics] of packageMetrics) {
    if (metrics.fan.fanIn >= 5 || metrics.fan.transitiveFanIn >= 10) {
      critical.push(pkgName);
    }
  }

  return critical.sort((a, b) => {
    const aFanIn = packageMetrics.get(a).fan.fanIn;
    const bFanIn = packageMetrics.get(b).fan.fanIn;
    return bFanIn - aFanIn;
  });
}

/**
 * Find packages in problematic stability zones
 * @param {Map<string, PackageMetrics>} packageMetrics - Per-package metrics
 * @returns {string[]} List of unstable package names
 */
function findUnstablePackages(packageMetrics) {
  const unstable = [];

  for (const [pkgName, metrics] of packageMetrics) {
    if (metrics.stability.zone !== 'balanced' || metrics.stability.distanceFromMain > 0.4) {
      unstable.push(pkgName);
    }
  }

  return unstable.sort((a, b) => {
    const aDist = packageMetrics.get(a).stability.distanceFromMain;
    const bDist = packageMetrics.get(b).stability.distanceFromMain;
    return bDist - aDist;
  });
}

/**
 * Format metrics as a detailed report
 * @param {MonorepoMetrics} metrics - Computed metrics
 * @returns {string} Formatted report
 */
export function formatMetricsReport(metrics) {
  const lines = [];

  lines.push('='.repeat(80));
  lines.push('DEPENDENCY METRICS REPORT');
  lines.push('='.repeat(80));
  lines.push(`Computed: ${metrics.computedAt.toISOString()}`);
  lines.push('');

  // Aggregate metrics
  lines.push('-'.repeat(80));
  lines.push('AGGREGATE METRICS');
  lines.push('-'.repeat(80));
  lines.push(`Total Packages: ${metrics.aggregate.totalPackages}`);
  lines.push(`Total Edges: ${metrics.aggregate.totalEdges}`);
  lines.push(`Max Depth: ${metrics.aggregate.maxDepth}`);
  lines.push(`Avg Fan-In: ${metrics.aggregate.avgFanIn}`);
  lines.push(`Avg Fan-Out: ${metrics.aggregate.avgFanOut}`);
  lines.push(`Max Fan-In: ${metrics.aggregate.maxFanIn} (most depended on)`);
  lines.push(`Max Fan-Out: ${metrics.aggregate.maxFanOut} (most coupled)`);
  lines.push(`Avg Instability: ${metrics.aggregate.avgInstability}`);
  lines.push(`Overall Health: ${metrics.aggregate.overallHealthScore}/100`);
  lines.push('');

  // Critical packages
  if (metrics.criticalPackages.length > 0) {
    lines.push('-'.repeat(80));
    lines.push('CRITICAL PACKAGES (High Brittleness Risk)');
    lines.push('-'.repeat(80));
    for (const pkgName of metrics.criticalPackages.slice(0, 10)) {
      const pkg = metrics.packageMetrics.get(pkgName);
      lines.push(`  ${pkgName}: fan-in=${pkg.fan.fanIn}, transitive=${pkg.fan.transitiveFanIn}`);
    }
    lines.push('');
  }

  // Unstable packages
  if (metrics.unstablePackages.length > 0) {
    lines.push('-'.repeat(80));
    lines.push('UNSTABLE PACKAGES (Problematic Zones)');
    lines.push('-'.repeat(80));
    for (const pkgName of metrics.unstablePackages.slice(0, 10)) {
      const pkg = metrics.packageMetrics.get(pkgName);
      lines.push(`  ${pkgName}: zone=${pkg.stability.zone}, D=${pkg.stability.distanceFromMain}`);
    }
    lines.push('');
  }

  // Per-package summary (top 20 by health issues)
  lines.push('-'.repeat(80));
  lines.push('PACKAGE DETAILS (Sorted by Health Score, Ascending)');
  lines.push('-'.repeat(80));

  const sortedPackages = Array.from(metrics.packageMetrics.entries())
    .sort((a, b) => a[1].healthScore - b[1].healthScore)
    .slice(0, 20);

  for (const [pkgName, pkg] of sortedPackages) {
    const shortName = pkgName.replace('@unrdf/', '');
    lines.push(`\n${shortName} (Health: ${pkg.healthScore}/100)`);
    lines.push(`  Depth: ${pkg.depth.maxDepth}, Level: ${pkg.depth.level}`);
    lines.push(`  Fan-in: ${pkg.fan.fanIn}, Fan-out: ${pkg.fan.fanOut}`);
    lines.push(`  Instability: ${pkg.stability.instability}, Zone: ${pkg.stability.zone}`);

    if (pkg.warnings.length > 0) {
      lines.push('  Warnings:');
      for (const warning of pkg.warnings) {
        lines.push(`    - ${warning}`);
      }
    }
  }

  lines.push('');
  lines.push('='.repeat(80));

  return lines.join('\n');
}

/**
 * Export metrics to JSON format
 * @param {MonorepoMetrics} metrics - Computed metrics
 * @returns {Object} JSON-serializable metrics
 */
export function exportMetricsToJSON(metrics) {
  const packages = [];

  for (const [name, pkg] of metrics.packageMetrics) {
    packages.push({
      name,
      healthScore: pkg.healthScore,
      depth: pkg.depth,
      fan: {
        fanIn: pkg.fan.fanIn,
        fanOut: pkg.fan.fanOut,
        transitiveFanIn: pkg.fan.transitiveFanIn,
        transitiveFanOut: pkg.fan.transitiveFanOut
      },
      stability: pkg.stability,
      warnings: pkg.warnings
    });
  }

  return {
    computedAt: metrics.computedAt.toISOString(),
    aggregate: metrics.aggregate,
    criticalPackages: metrics.criticalPackages,
    unstablePackages: metrics.unstablePackages,
    packages
  };
}

export default {
  computeMetrics,
  formatMetricsReport,
  exportMetricsToJSON
};
