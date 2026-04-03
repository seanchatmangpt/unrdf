/**
 * Working Dependency Injection System
 * For cross-package module resolution
 */

export class DIContainer {
  constructor(registry) {
    this.registry = registry;
    this.bindings = new Map();
    this.singletons = new Map();
    this.factories = new Map();
  }

  /**
   * Register package factory
   */
  registerPackage(name, factory) {
    if (!this.registry.packages.find(p => p.name === name)) {
      throw new Error(`Unknown package: ${name}`);
    }
    this.factories.set(name, factory);
  }

  /**
   * Resolve package with all dependencies
   */
  async resolvePackage(name) {
    if (this.singletons.has(name)) {
      return this.singletons.get(name);
    }

    const pkg = this.registry.packages.find(p => p.name === name);
    if (!pkg) throw new Error(`Package not found: ${name}`);

    const deps = {};
    for (const dep of pkg.dependencies.filter(d => d.startsWith('@unrdf'))) {
      deps[dep] = await this.resolvePackage(dep);
    }

    let instance;
    if (this.factories.has(name)) {
      instance = await this.factories.get(name)(deps);
    } else {
      instance = { package: pkg, dependencies: deps };
    }

    this.singletons.set(name, instance);
    return instance;
  }

  /**
   * Reset singletons
   */
  reset() {
    this.singletons.clear();
  }

  /**
   * Get dependency graph for debugging
   */
  getDependencyGraph() {
    const graph = {};
    for (const pkg of this.registry.packages) {
      graph[pkg.name] = {
        version: pkg.version,
        tier: pkg.tier,
        dependencies: pkg.dependencies.filter(d => d.startsWith('@unrdf'))
      };
    }
    return graph;
  }
}

export default DIContainer;
