/**
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
