/**
 * @fileoverview Dependency Analyzer - Main Entry Point
 *
 * Comprehensive dependency analysis toolkit for monorepo management.
 * Provides DAG construction, cycle detection, metrics computation, and
 * optimization suggestions for maintaining healthy package dependencies.
 *
 * @module dependency-analyzer
 * @version 1.0.0
 *
 * @example
 * ```javascript
 * import { analyzeDependencies } from '@unrdf/dependency-analyzer';
 *
 * const analysis = await analyzeDependencies('/path/to/monorepo', {
 *   analyzeUsage: true,
 *   outputFormat: 'json'
 * });
 *
 * console.log(analysis.summary);
 * ```
 */

// Re-export all modules
export * from './dependency-graph.mjs';
export * from './cycle-detector.mjs';
export * from './metrics-computer.mjs';
export * from './optimization-suggester.mjs';

// Import for composite API
import {
  buildDependencyGraph,
  generateGraphSummary,
  exportGraphToJSON,
  visualizeGraphAsText,
  topologicalSort
} from './dependency-graph.mjs';

import {
  detectCycles,
  findStronglyConnectedComponents,
  generateCircularDependencyDenialReceipt,
  formatCycleReport
} from './cycle-detector.mjs';

import {
  computeMetrics,
  formatMetricsReport,
  exportMetricsToJSON
} from './metrics-computer.mjs';

import {
  generateOptimizations,
  formatOptimizationReport,
  exportOptimizationToJSON
} from './optimization-suggester.mjs';

/**
 * @typedef {Object} AnalysisOptions
 * @property {boolean} [analyzeUsage=false] - Analyze actual import usage in source files
 * @property {boolean} [includeDevDependencies=false] - Include dev dependencies in the graph
 * @property {boolean} [includePeerDependencies=false] - Include peer dependencies in the graph
 * @property {boolean} [includeOptionalDependencies=true] - Include optional dependencies
 * @property {'text' | 'json' | 'both'} [outputFormat='text'] - Output format preference
 * @property {boolean} [verbose=false] - Enable verbose logging
 */

/**
 * @typedef {Object} FullAnalysisResult
 * @property {import('./dependency-graph.mjs').DependencyGraph} graph - The dependency graph
 * @property {import('./cycle-detector.mjs').CycleDetectionResult} cycles - Cycle detection results
 * @property {import('./metrics-computer.mjs').MonorepoMetrics} metrics - Computed metrics
 * @property {import('./optimization-suggester.mjs').OptimizationReport} optimizations - Optimization suggestions
 * @property {string} summary - Text summary of the analysis
 * @property {Object} json - JSON-serializable representation
 * @property {Date} analyzedAt - Timestamp of analysis
 */

/**
 * Perform comprehensive dependency analysis on a monorepo
 *
 * @param {string} rootPath - Root path of the monorepo
 * @param {AnalysisOptions} [options={}] - Analysis options
 * @returns {FullAnalysisResult} Complete analysis results
 */
export function analyzeDependencies(rootPath, options = {}) {
  const {
    analyzeUsage = false,
    includeDevDependencies = false,
    includePeerDependencies = false,
    includeOptionalDependencies = true,
    outputFormat = 'text',
    verbose = false
  } = options;

  const startTime = Date.now();

  if (verbose) {
    console.log('\n========================================');
    console.log('DEPENDENCY ANALYZER');
    console.log('========================================');
    console.log(`Root: ${rootPath}`);
    console.log(`Options: ${JSON.stringify(options, null, 2)}`);
  }

  // Step 1: Build dependency graph
  console.log('\n[1/4] Building dependency graph...');
  const graph = buildDependencyGraph({
    rootPath,
    includeDevDependencies,
    includePeerDependencies,
    includeOptionalDependencies
  });

  // Step 2: Detect cycles
  console.log('[2/4] Detecting circular dependencies...');
  const cycles = detectCycles(graph);

  // Step 3: Compute metrics
  console.log('[3/4] Computing dependency metrics...');
  const metrics = computeMetrics(graph, { analyzeUsage });

  // Step 4: Generate optimizations
  console.log('[4/4] Generating optimization suggestions...');
  const optimizations = generateOptimizations(graph, metrics, cycles);

  const elapsed = Date.now() - startTime;

  // Generate summary
  const summary = generateTextSummary(graph, cycles, metrics, optimizations, elapsed);

  // Generate JSON representation
  const json = {
    graph: exportGraphToJSON(graph),
    cycles: {
      hasCycles: cycles.hasCycles,
      count: cycles.cycles.length,
      involvedPackages: cycles.involvedPackages,
      breakpointSuggestions: cycles.breakpointSuggestions.slice(0, 5)
    },
    metrics: exportMetricsToJSON(metrics),
    optimizations: exportOptimizationToJSON(optimizations),
    meta: {
      analyzedAt: new Date().toISOString(),
      elapsedMs: elapsed,
      options
    }
  };

  console.log(`\n[Complete] Analysis finished in ${elapsed}ms`);

  return {
    graph,
    cycles,
    metrics,
    optimizations,
    summary,
    json,
    analyzedAt: new Date()
  };
}

/**
 * Generate a text summary of the full analysis
 * @param {import('./dependency-graph.mjs').DependencyGraph} graph - The graph
 * @param {import('./cycle-detector.mjs').CycleDetectionResult} cycles - Cycle results
 * @param {import('./metrics-computer.mjs').MonorepoMetrics} metrics - Metrics
 * @param {import('./optimization-suggester.mjs').OptimizationReport} optimizations - Optimizations
 * @param {number} elapsed - Time elapsed in ms
 * @returns {string} Text summary
 */
function generateTextSummary(graph, cycles, metrics, optimizations, elapsed) {
  const lines = [];

  lines.push('#'.repeat(80));
  lines.push('#  MONOREPO DEPENDENCY ANALYSIS REPORT');
  lines.push('#'.repeat(80));
  lines.push('');
  lines.push(`Generated: ${new Date().toISOString()}`);
  lines.push(`Analysis Time: ${elapsed}ms`);
  lines.push('');

  // Quick Stats
  lines.push('## QUICK STATS');
  lines.push('-'.repeat(40));
  lines.push(`Total Packages: ${graph.packages.size}`);
  lines.push(`Total Dependencies: ${graph.edges.length}`);
  lines.push(`Circular Dependencies: ${cycles.hasCycles ? `YES (${cycles.cycles.length} cycles!)` : 'None'}`);
  lines.push(`Overall Health Score: ${metrics.aggregate.overallHealthScore}/100`);
  lines.push(`Architecture Score: ${optimizations.overallScore}/100`);
  lines.push('');

  // Status Indicators
  lines.push('## STATUS');
  lines.push('-'.repeat(40));

  if (cycles.hasCycles) {
    lines.push('[CRITICAL] Circular dependencies detected - must be fixed');
  } else {
    lines.push('[OK] No circular dependencies');
  }

  if (metrics.aggregate.overallHealthScore >= 80) {
    lines.push('[OK] Package health is good');
  } else if (metrics.aggregate.overallHealthScore >= 60) {
    lines.push('[WARNING] Package health needs attention');
  } else {
    lines.push('[CRITICAL] Package health is poor');
  }

  if (optimizations.overallScore >= 80) {
    lines.push('[OK] Architecture is well-organized');
  } else if (optimizations.overallScore >= 60) {
    lines.push('[WARNING] Architecture could be improved');
  } else {
    lines.push('[CRITICAL] Architecture needs significant work');
  }

  lines.push('');

  // Critical Packages
  if (metrics.criticalPackages.length > 0) {
    lines.push('## CRITICAL PACKAGES (High Impact)');
    lines.push('-'.repeat(40));
    lines.push('These packages have many dependents - changes require extra care:');
    for (const pkg of metrics.criticalPackages.slice(0, 5)) {
      const pkgMetrics = metrics.packageMetrics.get(pkg);
      lines.push(`  - ${pkg.replace('@unrdf/', '')}: ${pkgMetrics.fan.fanIn} direct dependents`);
    }
    lines.push('');
  }

  // Top Suggestions
  const criticalSuggestions = optimizations.suggestions.filter(s => s.priority === 'critical');
  const highSuggestions = optimizations.suggestions.filter(s => s.priority === 'high');

  if (criticalSuggestions.length > 0 || highSuggestions.length > 0) {
    lines.push('## TOP RECOMMENDATIONS');
    lines.push('-'.repeat(40));

    for (const suggestion of [...criticalSuggestions, ...highSuggestions].slice(0, 5)) {
      lines.push(`[${suggestion.priority.toUpperCase()}] ${suggestion.title}`);
    }
    lines.push('');
  }

  // Dependency Layers
  lines.push('## PACKAGE LAYERS');
  lines.push('-'.repeat(40));

  const layerNames = ['Foundation (Leaf packages)', 'Core Layer', 'Domain Layer', 'Application Layer'];
  for (let i = 0; i < optimizations.layerAnalysis.layers.length; i++) {
    const layer = optimizations.layerAnalysis.layers[i];
    const name = layerNames[i] || `Layer ${i}`;
    lines.push(`${name}: ${layer.length} packages`);
  }
  lines.push('');

  // Build Order
  const { sorted, hasCycle } = topologicalSort(graph);
  if (!hasCycle) {
    lines.push('## BUILD ORDER (Topological Sort)');
    lines.push('-'.repeat(40));
    lines.push('Build packages in this order for correct dependency resolution:');
    const shortNames = sorted.map((p, i) => `  ${i + 1}. ${p.replace('@unrdf/', '')}`);
    lines.push(shortNames.slice(0, 20).join('\n'));
    if (sorted.length > 20) {
      lines.push(`  ... and ${sorted.length - 20} more`);
    }
    lines.push('');
  }

  lines.push('#'.repeat(80));
  lines.push('# END OF REPORT');
  lines.push('#'.repeat(80));

  return lines.join('\n');
}

/**
 * Generate detailed reports for all aspects of the analysis
 * @param {FullAnalysisResult} analysis - The full analysis result
 * @returns {Object} Object with all detailed reports
 */
export function generateDetailedReports(analysis) {
  return {
    graph: visualizeGraphAsText(analysis.graph),
    cycles: formatCycleReport(analysis.cycles),
    metrics: formatMetricsReport(analysis.metrics),
    optimizations: formatOptimizationReport(analysis.optimizations),
    summary: analysis.summary
  };
}

/**
 * Generate a GOS denial receipt if there are issues
 * @param {FullAnalysisResult} analysis - The full analysis result
 * @returns {Object} GOS-compliant receipt
 */
export function generateGOSReceipt(analysis) {
  const issues = [];

  if (analysis.cycles.hasCycles) {
    issues.push({
      type: 'circular_dependency',
      severity: 'critical',
      count: analysis.cycles.cycles.length,
      details: analysis.cycles.cycles.map(c => c.description)
    });
  }

  if (analysis.metrics.aggregate.overallHealthScore < 60) {
    issues.push({
      type: 'low_health_score',
      severity: 'warning',
      score: analysis.metrics.aggregate.overallHealthScore,
      threshold: 60
    });
  }

  if (analysis.optimizations.overallScore < 50) {
    issues.push({
      type: 'poor_architecture',
      severity: 'warning',
      score: analysis.optimizations.overallScore,
      threshold: 50
    });
  }

  const passed = issues.filter(i => i.severity === 'critical').length === 0;

  return {
    type: passed ? 'APPROVAL' : 'DENIAL',
    guard: 'H_dependency_analysis',
    status: passed ? 'PASSED' : 'FAILED',
    timestamp: new Date().toISOString(),
    summary: {
      totalPackages: analysis.graph.packages.size,
      totalEdges: analysis.graph.edges.length,
      hasCycles: analysis.cycles.hasCycles,
      healthScore: analysis.metrics.aggregate.overallHealthScore,
      architectureScore: analysis.optimizations.overallScore
    },
    issues,
    remediation: passed ? null : [
      'Fix circular dependencies immediately',
      'Review critical packages for stability',
      'Apply suggested optimizations',
      'Run analysis again after fixes'
    ]
  };
}

/**
 * Quick health check - returns pass/fail with minimal analysis
 * @param {string} rootPath - Root path of the monorepo
 * @returns {{ passed: boolean, message: string, details: Object }}
 */
export function quickHealthCheck(rootPath) {
  const graph = buildDependencyGraph({ rootPath });
  const cycles = detectCycles(graph);
  const summary = generateGraphSummary(graph);

  const passed = !cycles.hasCycles;

  return {
    passed,
    message: passed
      ? `Healthy: ${summary.totalPackages} packages, ${summary.totalEdges} dependencies, no cycles`
      : `Issues found: ${cycles.cycles.length} circular dependencies`,
    details: {
      totalPackages: summary.totalPackages,
      totalEdges: summary.totalEdges,
      hasCycles: cycles.hasCycles,
      cycleCount: cycles.cycles.length
    }
  };
}

/**
 * Get impact analysis for a specific package
 * Returns all packages that would be affected if this package changes
 *
 * @param {string} rootPath - Root path of the monorepo
 * @param {string} packageName - Package name to analyze
 * @returns {{ direct: string[], transitive: string[], total: number }}
 */
export function getChangeImpact(rootPath, packageName) {
  const graph = buildDependencyGraph({ rootPath });

  const directDependents = graph.reverseAdjacencyList.get(packageName) || new Set();

  // Get transitive dependents
  const transitive = new Set();
  const queue = [...directDependents];
  const seen = new Set(directDependents);

  while (queue.length > 0) {
    const current = queue.shift();
    const dependents = graph.reverseAdjacencyList.get(current) || new Set();

    for (const dep of dependents) {
      if (!seen.has(dep)) {
        seen.add(dep);
        transitive.add(dep);
        queue.push(dep);
      }
    }
  }

  return {
    direct: Array.from(directDependents).sort(),
    transitive: Array.from(transitive).sort(),
    total: directDependents.size + transitive.size
  };
}

// Default export for convenience
export default {
  analyzeDependencies,
  generateDetailedReports,
  generateGOSReceipt,
  quickHealthCheck,
  getChangeImpact,

  // Re-exported functions
  buildDependencyGraph,
  detectCycles,
  computeMetrics,
  generateOptimizations
};
