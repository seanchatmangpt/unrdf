#!/usr/bin/env node

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const projectRoot = path.resolve(__dirname, '../..');

// Generate graph analysis engine
function generateGraphAnalysis() {
  const code = `/**
 * Advanced Graph Analysis Engine
 * Package dependency analysis, impact mapping, optimization
 */

export class GraphAnalysis {
  constructor(registry) {
    this.registry = registry;
    this.graph = this._buildGraph();
  }

  _buildGraph() {
    const nodes = new Map();
    const edges = [];

    for (const pkg of this.registry.packages) {
      nodes.set(pkg.name, {
        id: pkg.name,
        version: pkg.version,
        tier: pkg.tier,
        size: 0,
        inDegree: 0,
        outDegree: 0,
        betweenness: 0
      });
    }

    for (const pkg of this.registry.packages) {
      for (const dep of pkg.dependencies.filter(d => d.startsWith('@unrdf'))) {
        edges.push({
          source: pkg.name,
          target: dep,
          weight: 1
        });
        nodes.get(pkg.name).outDegree++;
        if (nodes.has(dep)) nodes.get(dep).inDegree++;
      }
    }

    return { nodes, edges };
  }

  /**
   * Find all paths between packages
   */
  findPaths(source, target, maxDepth = 10) {
    const paths = [];
    const visited = new Set();

    const dfs = (current, targetNode, path, depth) => {
      if (depth > maxDepth) return;
      if (current === targetNode) {
        paths.push([...path, current]);
        return;
      }

      visited.add(current);

      const deps = this.registry.packages
        .find(p => p.name === current)?.dependencies
        .filter(d => d.startsWith('@unrdf')) || [];

      for (const dep of deps) {
        if (!visited.has(dep)) {
          dfs(dep, targetNode, [...path, current], depth + 1);
        }
      }

      visited.delete(current);
    };

    dfs(source, target, [], 0);
    return paths;
  }

  /**
   * Analyze package centrality (importance in dependency graph)
   */
  analyzeImportance() {
    const importance = [];

    for (const [name, node] of this.graph.nodes) {
      const centrality = {
        package: name,
        version: node.version,
        tier: node.tier,
        inDegree: node.inDegree,    // How many depend on this
        outDegree: node.outDegree,  // How many this depends on
        importance: node.inDegree * 2 + node.outDegree
      };
      importance.push(centrality);
    }

    return importance.sort((a, b) => b.importance - a.importance);
  }

  /**
   * Find bottlenecks (critical packages many depend on)
   */
  findBottlenecks(threshold = 5) {
    return this.analyzeImportance()
      .filter(p => p.inDegree >= threshold)
      .map(p => ({
        package: p.package,
        dependentCount: p.inDegree,
        risk: 'CRITICAL'
      }));
  }

  /**
   * Detect circular dependencies
   */
  detectCircles() {
    const visited = new Set();
    const circles = [];

    const dfs = (node, path, pathSet) => {
      if (pathSet.has(node)) {
        const circleStart = path.indexOf(node);
        circles.push(path.slice(circleStart).concat(node));
        return;
      }

      if (visited.has(node)) return;
      visited.add(node);

      const deps = this.registry.packages
        .find(p => p.name === node)?.dependencies
        .filter(d => d.startsWith('@unrdf')) || [];

      for (const dep of deps) {
        dfs(dep, [...path, node], new Set([...pathSet, node]));
      }
    };

    for (const pkg of this.registry.packages) {
      dfs(pkg.name, [], new Set());
    }

    return Array.from(new Set(circles.map(c => JSON.stringify(c)))).map(c => JSON.parse(c));
  }

  /**
   * Layer graph by tier
   */
  analyzeLayers() {
    const layers = {
      essential: [],
      extended: [],
      optional: []
    };

    for (const pkg of this.registry.packages) {
      const node = this.graph.nodes.get(pkg.name);
      layers[pkg.tier].push({
        package: pkg.name,
        inDegree: node.inDegree,
        outDegree: node.outDegree
      });
    }

    return {
      essentialBottlenecks: layers.essential.filter(p => p.inDegree > 2),
      extendedBottlenecks: layers.extended.filter(p => p.inDegree > 2),
      optionalBottlenecks: layers.optional.filter(p => p.inDegree > 2)
    };
  }
}

export default GraphAnalysis;
`;

  return code;
}

// Generate observability layer
function generateObservability() {
  const code = `/**
 * Advanced Observability Layer
 * Tracing, metrics, and distributed context propagation
 */

export class Observability {
  constructor() {
    this.spans = [];
    this.metrics = new Map();
    this.traces = new Map();
    this.context = new Map();
  }

  /**
   * Start distributed trace
   */
  startTrace(name, attributes = {}) {
    const traceId = \`trace_\${Date.now()}_\${Math.random().toString(36).substring(7)}\`;
    const trace = {
      id: traceId,
      name,
      startTime: Date.now(),
      spans: [],
      attributes,
      status: 'ACTIVE'
    };
    this.traces.set(traceId, trace);
    return traceId;
  }

  /**
   * Create span within trace
   */
  startSpan(traceId, spanName, attributes = {}) {
    const trace = this.traces.get(traceId);
    if (!trace) throw new Error(\`Trace not found: \${traceId}\`);

    const span = {
      id: \`span_\${Date.now()}_\${Math.random().toString(36).substring(7)}\`,
      traceId,
      name: spanName,
      startTime: Date.now(),
      endTime: null,
      duration: null,
      attributes,
      events: [],
      status: 'ACTIVE'
    };

    trace.spans.push(span);
    return span.id;
  }

  /**
   * End span and record duration
   */
  endSpan(spanId, status = 'OK', attributes = {}) {
    let span = null;

    for (const trace of this.traces.values()) {
      span = trace.spans.find(s => s.id === spanId);
      if (span) break;
    }

    if (!span) throw new Error(\`Span not found: \${spanId}\`);

    span.endTime = Date.now();
    span.duration = span.endTime - span.startTime;
    span.status = status;
    span.attributes = { ...span.attributes, ...attributes };

    return {
      spanId,
      duration: span.duration,
      status
    };
  }

  /**
   * Record metric
   */
  recordMetric(name, value, attributes = {}) {
    if (!this.metrics.has(name)) {
      this.metrics.set(name, []);
    }

    this.metrics.get(name).push({
      value,
      timestamp: Date.now(),
      attributes
    });
  }

  /**
   * Set context value (for distributed tracing)
   */
  setContextValue(key, value) {
    this.context.set(key, {
      value,
      timestamp: Date.now()
    });
  }

  /**
   * Get aggregated metrics
   */
  getMetrics(name) {
    const values = this.metrics.get(name) || [];

    if (values.length === 0) return null;

    const nums = values.map(v => v.value);
    return {
      name,
      count: values.length,
      sum: nums.reduce((a, b) => a + b, 0),
      min: Math.min(...nums),
      max: Math.max(...nums),
      avg: nums.reduce((a, b) => a + b, 0) / nums.length,
      latest: values[values.length - 1].value
    };
  }

  /**
   * Export trace for analysis
   */
  exportTrace(traceId) {
    const trace = this.traces.get(traceId);
    if (!trace) return null;

    trace.status = 'COMPLETED';
    trace.endTime = Date.now();
    trace.duration = trace.endTime - trace.startTime;

    return {
      ...trace,
      spanCount: trace.spans.length,
      criticalPath: this._getCriticalPath(trace)
    };
  }

  _getCriticalPath(trace) {
    return trace.spans
      .sort((a, b) => b.duration - a.duration)
      .slice(0, 5)
      .map(s => ({ name: s.name, duration: s.duration }));
  }

  /**
   * Get observability summary
   */
  getSummary() {
    const traceCount = this.traces.size;
    const totalSpans = Array.from(this.traces.values()).reduce((sum, t) => sum + t.spans.length, 0);
    const metricCount = this.metrics.size;

    return {
      traces: traceCount,
      spans: totalSpans,
      metrics: metricCount,
      contextSize: this.context.size,
      timestamp: new Date()
    };
  }
}

export default Observability;
`;

  return code;
}

// Generate optimization engine
function generateOptimization() {
  const code = `/**
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
`;

  return code;
}

// Main
async function main() {
  console.log('🚀 Generating advanced analytics...\n');

  const outputDir = path.join(projectRoot, 'ggen', 'generated');
  fs.mkdirSync(outputDir, { recursive: true });

  // Generate graph analysis
  console.log('   📊 Graph Analysis...');
  fs.writeFileSync(path.join(outputDir, 'graph-analysis.mjs'), generateGraphAnalysis());

  // Generate observability
  console.log('   📈 Observability...');
  fs.writeFileSync(path.join(outputDir, 'observability.mjs'), generateObservability());

  // Generate optimization
  console.log('   🔧 Optimization Engine...');
  fs.writeFileSync(path.join(outputDir, 'optimization-engine.mjs'), generateOptimization());

  console.log(`\n✅ Generated advanced analytics systems`);
  console.log(`   - Graph Analysis: Package importance, bottleneck detection`);
  console.log(`   - Observability: Distributed tracing, metrics, context`);
  console.log(`   - Optimization: Structure analysis, impact estimation`);
}

main().catch(console.error);
