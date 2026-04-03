/**
 * Optimization Engine
 * Analyze and suggest package structure improvements
 */

export class OptimizationEngine {
  constructor(registry, graphAnalysis) {
    this.registry = registry;
    this.graph = graphAnalysis;
  }

  /**
   * Analyze for optimization opportunities
   */
  analyzeOptimizations() {
    const recommendations = [];

    // 1. Find overly complex packages
    const importance = this.graph.analyzeImportance();
    const complex = importance.filter(p => p.outDegree > 5);
    if (complex.length > 0) {
      recommendations.push({
        type: 'COMPLEXITY',
        severity: 'HIGH',
        packages: complex.map(p => p.package),
        suggestion: 'Consider splitting these packages into smaller modules'
      });
    }

    // 2. Find isolated packages
    const isolated = importance.filter(p => p.inDegree === 0 && p.outDegree === 0);
    if (isolated.length > 0) {
      recommendations.push({
        type: 'ISOLATION',
        severity: 'MEDIUM',
        packages: isolated.map(p => p.package),
        suggestion: 'These packages have no dependencies - consider bundling or removing'
      });
    }

    // 3. Circular dependencies
    const circles = this.graph.detectCircles();
    if (circles.length > 0) {
      recommendations.push({
        type: 'CIRCULAR_DEPS',
        severity: 'CRITICAL',
        circles,
        suggestion: 'Break circular dependencies by extracting shared code'
      });
    }

    // 4. Bottlenecks
    const bottlenecks = this.graph.findBottlenecks();
    if (bottlenecks.length > 0) {
      recommendations.push({
        type: 'BOTTLENECK',
        severity: 'MEDIUM',
        packages: bottlenecks,
        suggestion: 'Consider stabilizing these critical packages'
      });
    }

    return recommendations;
  }

  /**
   * Estimate impact of removing a package
   */
  estimateRemovalImpact(packageName) {
    const paths = [];

    for (const pkg of this.registry.packages) {
      const pathsFromPkg = this.graph.findPaths(pkg.name, packageName);
      if (pathsFromPkg.length > 0) {
        paths.push(...pathsFromPkg);
      }
    }

    return {
      package: packageName,
      affectedPackages: new Set(paths.flat()).size,
      pathCount: paths.length,
      risk: paths.length > 5 ? 'HIGH' : 'LOW',
      recommendation: paths.length > 5 ? 'Risky to remove - many dependencies' : 'Safe to remove'
    };
  }

  /**
   * Suggest reorg strategy
   */
  suggestReorg() {
    const currentLayers = this.graph.analyzeLayers();
    const recommendations = this.analyzeOptimizations();

    return {
      currentState: currentLayers,
      issues: recommendations,
      targetArchitecture: {
        essentialCount: this.registry.essential.length,
        extendedCount: this.registry.extended.length,
        optionalCount: this.registry.optional.length,
        suggestion: 'Current layering is healthy'
      }
    };
  }
}

export default OptimizationEngine;
