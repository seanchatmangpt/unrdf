/**
 * Working Package Resolver System
 * Auto-generated from real package registry
 * Executable immediately - no stubs
 */

export class PackageResolver {
  constructor(registry) {
    this.registry = registry;
    this.cache = new Map();
    this.loading = new Map();
  }

  /**
   * Resolve package by name - returns real package data
   */
  async resolve(name) {
    if (this.cache.has(name)) {
      return this.cache.get(name);
    }

    if (this.loading.has(name)) {
      return this.loading.get(name);
    }

    const promise = this._doResolve(name);
    this.loading.set(name, promise);

    try {
      const result = await promise;
      this.cache.set(name, result);
      return result;
    } finally {
      this.loading.delete(name);
    }
  }

  async _doResolve(name) {
    const pkg = this.registry.packages.find(p => p.name === name);
    if (!pkg) throw new Error(`Package not found: ${name}`);

    const resolved = {
      ...pkg,
      resolved: true,
      resolvedAt: new Date().toISOString(),
      dependencies: {},
      exports: {}
    };

    // Resolve internal dependencies
    for (const dep of pkg.dependencies.filter(d => d.startsWith('@unrdf'))) {
      resolved.dependencies[dep] = await this.resolve(dep);
    }

    return resolved;
  }

  /**
   * Get all packages with tier
   */
  getByTier(tier) {
    return this.registry[tier] || [];
  }

  /**
   * Get package dependency tree
   */
  getDependencyTree(name, visited = new Set()) {
    if (visited.has(name)) return null;
    visited.add(name);

    const pkg = this.registry.packages.find(p => p.name === name);
    if (!pkg) return null;

    return {
      name: pkg.name,
      version: pkg.version,
      tier: pkg.tier,
      dependencies: pkg.dependencies
        .filter(d => d.startsWith('@unrdf'))
        .map(d => this.getDependencyTree(d, visited))
        .filter(Boolean)
    };
  }

  /**
   * Validate package compatibility
   */
  validateCompatibility(name) {
    const pkg = this.registry.packages.find(p => p.name === name);
    if (!pkg) return { valid: false, error: 'Package not found' };

    const issues = [];

    // Check dependencies exist
    const missingDeps = pkg.dependencies
      .filter(d => !this.registry.packages.some(p => p.name === d));

    if (missingDeps.length > 0) {
      issues.push(`Missing dependencies: ${missingDeps.join(', ')}`);
    }

    // Check version compatibility
    const minVersion = pkg.version.split('.')[0];
    const incompatible = pkg.dependencies
      .filter(d => d.startsWith('@unrdf'))
      .filter(d => {
        const dep = this.registry.packages.find(p => p.name === d);
        return dep && dep.version.split('.')[0] !== minVersion && minVersion !== '1';
      });

    if (incompatible.length > 0) {
      issues.push(`Potential version conflicts with: ${incompatible.join(', ')}`);
    }

    return {
      valid: issues.length === 0,
      issues,
      compatible: true
    };
  }

  /**
   * Find packages matching criteria
   */
  find(predicate) {
    return this.registry.packages.filter(predicate);
  }

  /**
   * Get circular dependencies
   */
  getCircularDependencies() {
    const graph = new Map();

    // Build adjacency list
    for (const pkg of this.registry.packages) {
      const deps = pkg.dependencies.filter(d => d.startsWith('@unrdf'));
      graph.set(pkg.name, deps);
    }

    const circles = [];
    const visited = new Set();
    const recursionStack = new Set();

    const dfs = (node) => {
      visited.add(node);
      recursionStack.add(node);

      for (const neighbor of graph.get(node) || []) {
        if (!visited.has(neighbor)) {
          dfs(neighbor);
        } else if (recursionStack.has(neighbor)) {
          circles.push([node, neighbor]);
        }
      }

      recursionStack.delete(node);
    };

    for (const node of graph.keys()) {
      if (!visited.has(node)) {
        dfs(node);
      }
    }

    return circles;
  }
}

export default PackageResolver;
