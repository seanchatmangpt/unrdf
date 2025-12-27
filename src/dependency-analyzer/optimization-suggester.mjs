/**
 * @fileoverview Optimization Suggester for Dependency Graphs
 *
 * Analyzes dependency patterns and suggests architectural improvements:
 * - Package extraction recommendations
 * - Dependency reduction strategies
 * - Layer organization suggestions
 * - Coupling reduction techniques
 * - Interface extraction opportunities
 *
 * @module dependency-analyzer/optimization-suggester
 */

/**
 * @typedef {Object} OptimizationSuggestion
 * @property {string} type - Type of suggestion
 * @property {string} priority - 'critical' | 'high' | 'medium' | 'low'
 * @property {string} title - Short title
 * @property {string} description - Detailed description
 * @property {string[]} affectedPackages - Packages affected by this suggestion
 * @property {string[]} actions - Concrete action items
 * @property {number} estimatedImpact - Estimated impact score (1-10)
 * @property {number} estimatedEffort - Estimated effort score (1-10)
 */

/**
 * @typedef {Object} LayerAnalysis
 * @property {string[][]} layers - Packages organized by layer
 * @property {string[]} violations - Layer violation descriptions
 * @property {Map<string, number>} packageLayers - Package to layer mapping
 */

/**
 * @typedef {Object} ClusterAnalysis
 * @property {string[][]} clusters - Groups of tightly coupled packages
 * @property {Map<string, number>} clusterAssignments - Package to cluster mapping
 */

/**
 * @typedef {Object} OptimizationReport
 * @property {OptimizationSuggestion[]} suggestions - All optimization suggestions
 * @property {LayerAnalysis} layerAnalysis - Layer organization analysis
 * @property {ClusterAnalysis} clusterAnalysis - Coupling cluster analysis
 * @property {string} summary - Executive summary
 * @property {number} overallScore - Current architecture score (0-100)
 * @property {number} potentialScore - Potential score after optimizations (0-100)
 */

/**
 * Suggestion types
 */
const SuggestionType = {
  EXTRACT_UTILITY: 'extract_utility',
  MERGE_PACKAGES: 'merge_packages',
  BREAK_CYCLE: 'break_cycle',
  REDUCE_COUPLING: 'reduce_coupling',
  EXTRACT_INTERFACE: 'extract_interface',
  REORGANIZE_LAYERS: 'reorganize_layers',
  REMOVE_UNUSED: 'remove_unused',
  STABILIZE_CORE: 'stabilize_core'
};

/**
 * Generate optimization suggestions for a dependency graph
 * @param {import('./dependency-graph.mjs').DependencyGraph} graph - The dependency graph
 * @param {import('./metrics-computer.mjs').MonorepoMetrics} metrics - Computed metrics
 * @param {import('./cycle-detector.mjs').CycleDetectionResult} cycleResult - Cycle detection results
 * @returns {OptimizationReport} Comprehensive optimization report
 */
export function generateOptimizations(graph, metrics, cycleResult) {
  console.log('\n[OptimizationSuggester] Analyzing architecture for optimization opportunities...');

  const suggestions = [];

  // Analyze layers
  const layerAnalysis = analyzeLayerOrganization(graph, metrics);

  // Analyze clusters
  const clusterAnalysis = analyzeCouplingClusters(graph, metrics);

  // Generate cycle-breaking suggestions
  if (cycleResult.hasCycles) {
    suggestions.push(...generateCycleBreakingSuggestions(cycleResult, graph, metrics));
  }

  // Generate utility extraction suggestions
  suggestions.push(...generateUtilityExtractionSuggestions(graph, metrics));

  // Generate coupling reduction suggestions
  suggestions.push(...generateCouplingReductionSuggestions(graph, metrics, clusterAnalysis));

  // Generate interface extraction suggestions
  suggestions.push(...generateInterfaceExtractionSuggestions(graph, metrics));

  // Generate layer reorganization suggestions
  suggestions.push(...generateLayerReorganizationSuggestions(layerAnalysis, graph, metrics));

  // Generate core stabilization suggestions
  suggestions.push(...generateCoreStabilizationSuggestions(graph, metrics));

  // Generate unused dependency suggestions
  suggestions.push(...generateUnusedDependencySuggestions(metrics));

  // Sort by priority and impact
  suggestions.sort((a, b) => {
    const priorityOrder = { critical: 0, high: 1, medium: 2, low: 3 };
    if (priorityOrder[a.priority] !== priorityOrder[b.priority]) {
      return priorityOrder[a.priority] - priorityOrder[b.priority];
    }
    return b.estimatedImpact - a.estimatedImpact;
  });

  // Calculate scores
  const overallScore = calculateArchitectureScore(metrics, cycleResult, layerAnalysis);
  const potentialScore = calculatePotentialScore(overallScore, suggestions);

  // Generate summary
  const summary = generateExecutiveSummary(suggestions, overallScore, potentialScore);

  console.log(`[OptimizationSuggester] Generated ${suggestions.length} optimization suggestions`);

  return {
    suggestions,
    layerAnalysis,
    clusterAnalysis,
    summary,
    overallScore,
    potentialScore
  };
}

/**
 * Analyze layer organization of packages
 * @param {import('./dependency-graph.mjs').DependencyGraph} graph - The dependency graph
 * @param {import('./metrics-computer.mjs').MonorepoMetrics} metrics - Computed metrics
 * @returns {LayerAnalysis} Layer analysis results
 */
function analyzeLayerOrganization(graph, metrics) {
  // Group packages by their topological level (depth from leaves)
  const packageLayers = new Map();
  const maxLevel = Math.max(...Array.from(metrics.packageMetrics.values()).map(m => m.depth.level));

  // Normalize levels to 4 architectural layers
  const numLayers = Math.min(4, maxLevel + 1);

  for (const [pkgName, pkgMetrics] of metrics.packageMetrics) {
    const normalizedLayer = Math.floor((pkgMetrics.depth.level / (maxLevel + 1)) * numLayers);
    packageLayers.set(pkgName, normalizedLayer);
  }

  // Organize into layer arrays
  const layers = Array.from({ length: numLayers }, () => []);
  for (const [pkgName, layer] of packageLayers) {
    layers[layer].push(pkgName);
  }

  // Detect layer violations (higher layer depending on lower layer is normal, reverse is violation)
  const violations = [];
  for (const edge of graph.edges) {
    const fromLayer = packageLayers.get(edge.from) || 0;
    const toLayer = packageLayers.get(edge.to) || 0;

    // A violation is when a lower layer depends on a higher layer
    // (we want dependencies to flow from high to low)
    if (fromLayer < toLayer) {
      violations.push(
        `Layer violation: ${edge.from} (layer ${fromLayer}) depends on ${edge.to} (layer ${toLayer})`
      );
    }
  }

  // Sort packages within each layer
  for (const layer of layers) {
    layer.sort();
  }

  return {
    layers,
    violations,
    packageLayers
  };
}

/**
 * Analyze coupling clusters using simple heuristics
 * @param {import('./dependency-graph.mjs').DependencyGraph} graph - The dependency graph
 * @param {import('./metrics-computer.mjs').MonorepoMetrics} metrics - Computed metrics
 * @returns {ClusterAnalysis} Cluster analysis results
 */
function analyzeCouplingClusters(graph, metrics) {
  const clusters = [];
  const clusterAssignments = new Map();
  const visited = new Set();

  // Find tightly coupled packages using shared dependencies
  const packageDeps = new Map();
  for (const [pkgName, deps] of graph.adjacencyList) {
    packageDeps.set(pkgName, new Set(deps));
  }

  // Simple clustering: packages that share >50% of dependencies
  for (const [pkgA, depsA] of packageDeps) {
    if (visited.has(pkgA)) continue;

    const cluster = [pkgA];
    visited.add(pkgA);

    for (const [pkgB, depsB] of packageDeps) {
      if (visited.has(pkgB) || pkgA === pkgB) continue;

      // Calculate Jaccard similarity of dependencies
      const intersection = new Set([...depsA].filter(x => depsB.has(x)));
      const union = new Set([...depsA, ...depsB]);

      if (union.size > 0) {
        const similarity = intersection.size / union.size;
        if (similarity > 0.5) {
          cluster.push(pkgB);
          visited.add(pkgB);
        }
      }
    }

    if (cluster.length > 1) {
      const clusterIndex = clusters.length;
      clusters.push(cluster.sort());
      for (const pkg of cluster) {
        clusterAssignments.set(pkg, clusterIndex);
      }
    }
  }

  return {
    clusters,
    clusterAssignments
  };
}

/**
 * Generate suggestions for breaking cycles
 * @param {import('./cycle-detector.mjs').CycleDetectionResult} cycleResult - Cycle detection results
 * @param {import('./dependency-graph.mjs').DependencyGraph} graph - The dependency graph
 * @param {import('./metrics-computer.mjs').MonorepoMetrics} metrics - Computed metrics
 * @returns {OptimizationSuggestion[]} Cycle-breaking suggestions
 */
function generateCycleBreakingSuggestions(cycleResult, graph, metrics) {
  const suggestions = [];

  for (const breakpoint of cycleResult.breakpointSuggestions.slice(0, 5)) {
    suggestions.push({
      type: SuggestionType.BREAK_CYCLE,
      priority: 'critical',
      title: `Break cycle: ${breakpoint.from.replace('@unrdf/', '')} -> ${breakpoint.to.replace('@unrdf/', '')}`,
      description: `This edge participates in circular dependencies. Breaking it would eliminate cycles and restore DAG properties.`,
      affectedPackages: [breakpoint.from, breakpoint.to],
      actions: [
        `Remove dependency from ${breakpoint.from} to ${breakpoint.to}`,
        `Consider extracting shared code to a new utility package`,
        `Use dependency injection or interfaces to decouple`,
        `If functionality is tightly coupled, consider merging packages`
      ],
      estimatedImpact: 10,
      estimatedEffort: 6
    });
  }

  // Suggest cycle-specific strategies
  for (const cycle of cycleResult.cycles.slice(0, 3)) {
    if (cycle.length === 2) {
      // Simple A <-> B cycle
      suggestions.push({
        type: SuggestionType.MERGE_PACKAGES,
        priority: 'high',
        title: `Consider merging: ${cycle.path[0].replace('@unrdf/', '')} and ${cycle.path[1].replace('@unrdf/', '')}`,
        description: 'These two packages have a bidirectional dependency, indicating they might represent a single concern.',
        affectedPackages: cycle.path.slice(0, 2),
        actions: [
          'Review if these packages should be a single package',
          'Extract a shared interface package if they must stay separate',
          'Use event-based communication to break the coupling'
        ],
        estimatedImpact: 8,
        estimatedEffort: 7
      });
    } else if (cycle.length <= 4) {
      // Small cycle - suggest interface extraction
      suggestions.push({
        type: SuggestionType.EXTRACT_INTERFACE,
        priority: 'high',
        title: `Extract interfaces for cycle: ${cycle.description}`,
        description: 'This cycle could be broken by extracting shared interfaces to a common package.',
        affectedPackages: cycle.path.slice(0, -1),
        actions: [
          'Identify the shared contracts between these packages',
          'Create a new @unrdf/shared-interfaces or similar package',
          'Move interfaces/types to the shared package',
          'Have concrete packages depend only on interfaces'
        ],
        estimatedImpact: 9,
        estimatedEffort: 8
      });
    }
  }

  return suggestions;
}

/**
 * Generate suggestions for extracting utility packages
 * @param {import('./dependency-graph.mjs').DependencyGraph} graph - The dependency graph
 * @param {import('./metrics-computer.mjs').MonorepoMetrics} metrics - Computed metrics
 * @returns {OptimizationSuggestion[]} Utility extraction suggestions
 */
function generateUtilityExtractionSuggestions(graph, metrics) {
  const suggestions = [];

  // Find packages that are depended on by many others
  const highFanInPackages = [];
  for (const [pkgName, pkgMetrics] of metrics.packageMetrics) {
    if (pkgMetrics.fan.fanIn >= 5) {
      highFanInPackages.push({ name: pkgName, fanIn: pkgMetrics.fan.fanIn });
    }
  }

  highFanInPackages.sort((a, b) => b.fanIn - a.fanIn);

  // For very high fan-in packages, suggest careful maintenance
  for (const pkg of highFanInPackages.slice(0, 3)) {
    const pkgMetrics = metrics.packageMetrics.get(pkg.name);

    if (pkgMetrics.fan.fanOut > 3) {
      suggestions.push({
        type: SuggestionType.EXTRACT_UTILITY,
        priority: 'medium',
        title: `Split ${pkg.name.replace('@unrdf/', '')}: too many dependencies for a core package`,
        description: `This package has ${pkg.fanIn} dependents but also ${pkgMetrics.fan.fanOut} dependencies. Core packages should have minimal dependencies.`,
        affectedPackages: [pkg.name, ...pkgMetrics.fan.directDependencies],
        actions: [
          'Identify functionality that can be extracted to separate packages',
          'Move implementation details to dependent packages',
          'Keep only core abstractions in this package',
          'Consider a "core-impl" split pattern'
        ],
        estimatedImpact: 7,
        estimatedEffort: 6
      });
    }
  }

  // Find common dependency patterns that could be extracted
  const depPatterns = new Map();
  for (const [pkgName, deps] of graph.adjacencyList) {
    if (deps.size >= 2) {
      const depsKey = Array.from(deps).sort().join(',');
      if (!depPatterns.has(depsKey)) {
        depPatterns.set(depsKey, []);
      }
      depPatterns.get(depsKey).push(pkgName);
    }
  }

  // Find patterns shared by multiple packages
  for (const [pattern, packages] of depPatterns) {
    if (packages.length >= 3 && pattern.split(',').length >= 2) {
      const deps = pattern.split(',');
      suggestions.push({
        type: SuggestionType.EXTRACT_UTILITY,
        priority: 'low',
        title: `Common dependency pattern found in ${packages.length} packages`,
        description: `${packages.length} packages all depend on the same set of packages: ${deps.map(d => d.replace('@unrdf/', '')).join(', ')}. Consider creating a composite utility package.`,
        affectedPackages: packages,
        actions: [
          'Create a composite package that re-exports common dependencies',
          'Have dependent packages import from the composite instead',
          'This simplifies dependency management and version updates'
        ],
        estimatedImpact: 4,
        estimatedEffort: 3
      });
    }
  }

  return suggestions;
}

/**
 * Generate suggestions for reducing coupling
 * @param {import('./dependency-graph.mjs').DependencyGraph} graph - The dependency graph
 * @param {import('./metrics-computer.mjs').MonorepoMetrics} metrics - Computed metrics
 * @param {ClusterAnalysis} clusterAnalysis - Cluster analysis results
 * @returns {OptimizationSuggestion[]} Coupling reduction suggestions
 */
function generateCouplingReductionSuggestions(graph, metrics, clusterAnalysis) {
  const suggestions = [];

  // High fan-out packages
  for (const [pkgName, pkgMetrics] of metrics.packageMetrics) {
    if (pkgMetrics.fan.fanOut >= 6) {
      suggestions.push({
        type: SuggestionType.REDUCE_COUPLING,
        priority: 'medium',
        title: `High coupling in ${pkgName.replace('@unrdf/', '')}: ${pkgMetrics.fan.fanOut} dependencies`,
        description: 'This package has many dependencies, making it fragile to changes in any of them.',
        affectedPackages: [pkgName, ...pkgMetrics.fan.directDependencies],
        actions: [
          'Review each dependency - is it truly needed?',
          'Consider facade pattern to hide internal dependencies',
          'Split package by concern if it does too many things',
          'Use lazy loading for optional dependencies'
        ],
        estimatedImpact: 6,
        estimatedEffort: 5
      });
    }
  }

  // Cluster-based suggestions
  for (const cluster of clusterAnalysis.clusters) {
    if (cluster.length >= 3) {
      suggestions.push({
        type: SuggestionType.MERGE_PACKAGES,
        priority: 'low',
        title: `Consider consolidating cluster: ${cluster.map(p => p.replace('@unrdf/', '')).join(', ')}`,
        description: 'These packages share many dependencies and might represent a single domain.',
        affectedPackages: cluster,
        actions: [
          'Review if these packages have a unified domain concept',
          'Consider creating a meta-package that depends on all of them',
          'If functionality overlaps, merge to reduce complexity'
        ],
        estimatedImpact: 5,
        estimatedEffort: 7
      });
    }
  }

  return suggestions;
}

/**
 * Generate suggestions for extracting interfaces
 * @param {import('./dependency-graph.mjs').DependencyGraph} graph - The dependency graph
 * @param {import('./metrics-computer.mjs').MonorepoMetrics} metrics - Computed metrics
 * @returns {OptimizationSuggestion[]} Interface extraction suggestions
 */
function generateInterfaceExtractionSuggestions(graph, metrics) {
  const suggestions = [];

  // Find packages in zone of pain
  for (const [pkgName, pkgMetrics] of metrics.packageMetrics) {
    if (pkgMetrics.stability.zone === 'zone_of_pain') {
      suggestions.push({
        type: SuggestionType.EXTRACT_INTERFACE,
        priority: 'medium',
        title: `Extract interfaces from ${pkgName.replace('@unrdf/', '')}`,
        description: 'This package is concrete and stable - hard to change. Extract interfaces to improve flexibility.',
        affectedPackages: [pkgName],
        actions: [
          'Identify the public API of this package',
          'Create a new @unrdf/...-interfaces package with pure type definitions',
          'Have dependents depend on interfaces, not implementation',
          'This allows swapping implementations without breaking dependents'
        ],
        estimatedImpact: 6,
        estimatedEffort: 5
      });
    }
  }

  return suggestions;
}

/**
 * Generate suggestions for layer reorganization
 * @param {LayerAnalysis} layerAnalysis - Layer analysis results
 * @param {import('./dependency-graph.mjs').DependencyGraph} graph - The dependency graph
 * @param {import('./metrics-computer.mjs').MonorepoMetrics} metrics - Computed metrics
 * @returns {OptimizationSuggestion[]} Layer reorganization suggestions
 */
function generateLayerReorganizationSuggestions(layerAnalysis, graph, metrics) {
  const suggestions = [];

  if (layerAnalysis.violations.length > 0) {
    // Group violations by pattern
    const violationCount = layerAnalysis.violations.length;

    if (violationCount > 5) {
      suggestions.push({
        type: SuggestionType.REORGANIZE_LAYERS,
        priority: 'high',
        title: `${violationCount} layer violations detected`,
        description: 'Multiple packages depend on packages in higher layers, violating the dependency rule.',
        affectedPackages: [],
        actions: [
          'Define clear layer boundaries (e.g., core, domain, application, infrastructure)',
          'Dependencies should only flow downward (application -> domain -> core)',
          'Use dependency inversion to allow lower layers to call higher layers',
          'Consider an architectural fitness function to prevent future violations'
        ],
        estimatedImpact: 8,
        estimatedEffort: 9
      });
    }

    // Add specific violation suggestions
    for (const violation of layerAnalysis.violations.slice(0, 3)) {
      suggestions.push({
        type: SuggestionType.REORGANIZE_LAYERS,
        priority: 'medium',
        title: violation,
        description: 'This dependency violates the layered architecture principle.',
        affectedPackages: [],
        actions: [
          'Move the shared functionality to the lower layer',
          'Create an interface in the lower layer and implement in higher',
          'Use events or callbacks instead of direct dependencies'
        ],
        estimatedImpact: 5,
        estimatedEffort: 4
      });
    }
  }

  return suggestions;
}

/**
 * Generate suggestions for stabilizing core packages
 * @param {import('./dependency-graph.mjs').DependencyGraph} graph - The dependency graph
 * @param {import('./metrics-computer.mjs').MonorepoMetrics} metrics - Computed metrics
 * @returns {OptimizationSuggestion[]} Core stabilization suggestions
 */
function generateCoreStabilizationSuggestions(graph, metrics) {
  const suggestions = [];

  // Find critical packages that aren't stable enough
  for (const pkgName of metrics.criticalPackages) {
    const pkgMetrics = metrics.packageMetrics.get(pkgName);

    if (pkgMetrics.stability.instability > 0.3) {
      suggestions.push({
        type: SuggestionType.STABILIZE_CORE,
        priority: 'high',
        title: `Stabilize ${pkgName.replace('@unrdf/', '')}: too unstable for a core package`,
        description: `This package has ${pkgMetrics.fan.fanIn} dependents but instability of ${pkgMetrics.stability.instability}. Core packages should be stable.`,
        affectedPackages: [pkgName, ...pkgMetrics.fan.directDependencies],
        actions: [
          'Reduce or eliminate dependencies on unstable packages',
          'Move volatile functionality to separate packages',
          'Increase test coverage to enable confident refactoring',
          'Consider semantic versioning with strict API compatibility'
        ],
        estimatedImpact: 8,
        estimatedEffort: 6
      });
    }
  }

  return suggestions;
}

/**
 * Generate suggestions for removing unused dependencies
 * @param {import('./metrics-computer.mjs').MonorepoMetrics} metrics - Computed metrics
 * @returns {OptimizationSuggestion[]} Unused dependency suggestions
 */
function generateUnusedDependencySuggestions(metrics) {
  const suggestions = [];

  for (const [pkgName, pkgMetrics] of metrics.packageMetrics) {
    if (pkgMetrics.usage.unusedDependencies.length > 0) {
      suggestions.push({
        type: SuggestionType.REMOVE_UNUSED,
        priority: 'low',
        title: `Remove unused dependencies in ${pkgName.replace('@unrdf/', '')}`,
        description: `Found ${pkgMetrics.usage.unusedDependencies.length} declared but unused workspace dependencies.`,
        affectedPackages: [pkgName, ...pkgMetrics.usage.unusedDependencies],
        actions: [
          `Remove: ${pkgMetrics.usage.unusedDependencies.map(d => d.replace('@unrdf/', '')).join(', ')}`,
          'Verify removal doesn\'t break runtime behavior',
          'Update package.json and run tests'
        ],
        estimatedImpact: 3,
        estimatedEffort: 1
      });
    }

    if (pkgMetrics.usage.phantomDependencies.length > 0) {
      suggestions.push({
        type: SuggestionType.REMOVE_UNUSED,
        priority: 'medium',
        title: `Fix phantom dependencies in ${pkgName.replace('@unrdf/', '')}`,
        description: `Found ${pkgMetrics.usage.phantomDependencies.length} imported but undeclared dependencies.`,
        affectedPackages: [pkgName],
        actions: [
          `Add to package.json: ${pkgMetrics.usage.phantomDependencies.join(', ')}`,
          'Or remove the imports if not actually needed',
          'This ensures predictable builds and installations'
        ],
        estimatedImpact: 4,
        estimatedEffort: 1
      });
    }
  }

  return suggestions;
}

/**
 * Calculate overall architecture score
 * @param {import('./metrics-computer.mjs').MonorepoMetrics} metrics - Computed metrics
 * @param {import('./cycle-detector.mjs').CycleDetectionResult} cycleResult - Cycle detection results
 * @param {LayerAnalysis} layerAnalysis - Layer analysis results
 * @returns {number} Architecture score (0-100)
 */
function calculateArchitectureScore(metrics, cycleResult, layerAnalysis) {
  let score = 100;

  // Heavy penalties for cycles
  score -= cycleResult.cycles.length * 15;

  // Penalty for layer violations
  score -= Math.min(20, layerAnalysis.violations.length * 2);

  // Penalty for low overall health
  score -= (100 - metrics.aggregate.overallHealthScore) * 0.3;

  // Penalty for high coupling
  if (metrics.aggregate.avgFanOut > 3) {
    score -= (metrics.aggregate.avgFanOut - 3) * 3;
  }

  // Penalty for deep hierarchies
  if (metrics.aggregate.maxDepth > 5) {
    score -= (metrics.aggregate.maxDepth - 5) * 2;
  }

  // Penalty for too many critical packages
  score -= Math.min(10, metrics.criticalPackages.length);

  return Math.max(0, Math.round(score));
}

/**
 * Calculate potential score after applying optimizations
 * @param {number} currentScore - Current architecture score
 * @param {OptimizationSuggestion[]} suggestions - Optimization suggestions
 * @returns {number} Potential score (0-100)
 */
function calculatePotentialScore(currentScore, suggestions) {
  let potentialGain = 0;

  for (const suggestion of suggestions) {
    // Weight by priority
    const priorityMultiplier = {
      critical: 1.0,
      high: 0.8,
      medium: 0.5,
      low: 0.3
    }[suggestion.priority];

    // Estimate improvement based on impact and effort ratio
    const improvement = (suggestion.estimatedImpact / suggestion.estimatedEffort) * priorityMultiplier * 2;
    potentialGain += improvement;
  }

  return Math.min(100, Math.round(currentScore + potentialGain));
}

/**
 * Generate executive summary
 * @param {OptimizationSuggestion[]} suggestions - Optimization suggestions
 * @param {number} overallScore - Current score
 * @param {number} potentialScore - Potential score
 * @returns {string} Executive summary
 */
function generateExecutiveSummary(suggestions, overallScore, potentialScore) {
  const critical = suggestions.filter(s => s.priority === 'critical').length;
  const high = suggestions.filter(s => s.priority === 'high').length;
  const medium = suggestions.filter(s => s.priority === 'medium').length;
  const low = suggestions.filter(s => s.priority === 'low').length;

  const lines = [];

  lines.push('EXECUTIVE SUMMARY');
  lines.push('');
  lines.push(`Current Architecture Score: ${overallScore}/100`);
  lines.push(`Potential Score (after optimizations): ${potentialScore}/100`);
  lines.push('');
  lines.push(`Total Suggestions: ${suggestions.length}`);
  lines.push(`  - Critical: ${critical}`);
  lines.push(`  - High: ${high}`);
  lines.push(`  - Medium: ${medium}`);
  lines.push(`  - Low: ${low}`);
  lines.push('');

  if (critical > 0) {
    lines.push('IMMEDIATE ACTION REQUIRED:');
    lines.push('Critical issues (e.g., circular dependencies) must be addressed before deployment.');
  } else if (high > 0) {
    lines.push('RECOMMENDED:');
    lines.push('Address high-priority items to improve maintainability and reduce risk.');
  } else {
    lines.push('ARCHITECTURE STATUS: Good');
    lines.push('No critical issues. Consider medium/low priority improvements for optimization.');
  }

  return lines.join('\n');
}

/**
 * Format optimization report as text
 * @param {OptimizationReport} report - Optimization report
 * @returns {string} Formatted report
 */
export function formatOptimizationReport(report) {
  const lines = [];

  lines.push('='.repeat(80));
  lines.push('ARCHITECTURE OPTIMIZATION REPORT');
  lines.push('='.repeat(80));
  lines.push('');
  lines.push(report.summary);
  lines.push('');

  // Layer analysis
  lines.push('-'.repeat(80));
  lines.push('LAYER ORGANIZATION');
  lines.push('-'.repeat(80));

  for (let i = 0; i < report.layerAnalysis.layers.length; i++) {
    const layer = report.layerAnalysis.layers[i];
    const layerNames = ['Foundation (Leaf)', 'Core', 'Domain', 'Application'][i] || `Layer ${i}`;
    lines.push(`\n${layerNames}:`);
    for (const pkg of layer) {
      lines.push(`  - ${pkg.replace('@unrdf/', '')}`);
    }
  }

  if (report.layerAnalysis.violations.length > 0) {
    lines.push('\nLayer Violations:');
    for (const violation of report.layerAnalysis.violations.slice(0, 10)) {
      lines.push(`  ! ${violation}`);
    }
    if (report.layerAnalysis.violations.length > 10) {
      lines.push(`  ... and ${report.layerAnalysis.violations.length - 10} more`);
    }
  }

  lines.push('');

  // Suggestions by priority
  lines.push('-'.repeat(80));
  lines.push('OPTIMIZATION SUGGESTIONS');
  lines.push('-'.repeat(80));

  const byPriority = {
    critical: [],
    high: [],
    medium: [],
    low: []
  };

  for (const suggestion of report.suggestions) {
    byPriority[suggestion.priority].push(suggestion);
  }

  for (const priority of ['critical', 'high', 'medium', 'low']) {
    const suggestions = byPriority[priority];
    if (suggestions.length === 0) continue;

    lines.push(`\n[${priority.toUpperCase()}] (${suggestions.length} suggestions)`);
    lines.push('-'.repeat(40));

    for (const suggestion of suggestions.slice(0, 5)) {
      lines.push(`\n${suggestion.title}`);
      lines.push(`  Type: ${suggestion.type}`);
      lines.push(`  Impact: ${suggestion.estimatedImpact}/10, Effort: ${suggestion.estimatedEffort}/10`);
      lines.push(`  ${suggestion.description}`);
      lines.push('  Actions:');
      for (const action of suggestion.actions.slice(0, 3)) {
        lines.push(`    - ${action}`);
      }
    }

    if (suggestions.length > 5) {
      lines.push(`\n  ... and ${suggestions.length - 5} more ${priority} suggestions`);
    }
  }

  lines.push('');
  lines.push('='.repeat(80));

  return lines.join('\n');
}

/**
 * Export optimization report to JSON
 * @param {OptimizationReport} report - Optimization report
 * @returns {Object} JSON-serializable report
 */
export function exportOptimizationToJSON(report) {
  return {
    summary: report.summary,
    scores: {
      current: report.overallScore,
      potential: report.potentialScore
    },
    layerAnalysis: {
      layers: report.layerAnalysis.layers,
      violationCount: report.layerAnalysis.violations.length,
      violations: report.layerAnalysis.violations.slice(0, 20)
    },
    clusterAnalysis: {
      clusterCount: report.clusterAnalysis.clusters.length,
      clusters: report.clusterAnalysis.clusters
    },
    suggestions: report.suggestions.map(s => ({
      type: s.type,
      priority: s.priority,
      title: s.title,
      description: s.description,
      affectedPackages: s.affectedPackages,
      impact: s.estimatedImpact,
      effort: s.estimatedEffort
    }))
  };
}

export default {
  generateOptimizations,
  formatOptimizationReport,
  exportOptimizationToJSON
};
