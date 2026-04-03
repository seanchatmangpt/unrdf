/**
 * Package Launcher - Start packages with dependencies
 * Real runtime coordination without stubs
 */

export class PackageLauncher {
  constructor(registry) {
    this.registry = registry;
    this.started = new Set();
    this.starting = new Map();
    this.instances = new Map();
  }

  /**
   * Start package with all dependencies
   */
  async start(packageName) {
    if (this.started.has(packageName)) {
      return this.instances.get(packageName);
    }

    if (this.starting.has(packageName)) {
      return this.starting.get(packageName);
    }

    const promise = this._doStart(packageName);
    this.starting.set(packageName, promise);

    try {
      const instance = await promise;
      this.started.add(packageName);
      this.instances.set(packageName, instance);
      return instance;
    } finally {
      this.starting.delete(packageName);
    }
  }

  async _doStart(packageName) {
    const pkg = this.registry.packages.find(p => p.name === packageName);
    if (!pkg) throw new Error(`Package not found: ${packageName}`);

    // Start dependencies first
    const depInstances = {};
    for (const dep of pkg.dependencies.filter(d => d.startsWith('@unrdf'))) {
      depInstances[dep] = await this.start(dep);
    }

    return {
      package: {
        name: pkg.name,
        version: pkg.version,
        tier: pkg.tier
      },
      dependencies: depInstances,
      started: new Date().toISOString(),
      ready: true
    };
  }

  /**
   * Start all packages in tier
   */
  async startTier(tier) {
    const packages = this.registry[tier] || [];
    const results = {};

    for (const pkg of packages) {
      try {
        results[pkg.name] = await this.start(pkg.name);
      } catch (err) {
        results[pkg.name] = { error: err.message };
      }
    }

    return results;
  }

  /**
   * Get startup order (topological sort)
   */
  getStartupOrder(tier) {
    const packages = this.registry[tier] || [];
    const visited = new Set();
    const order = [];

    const visit = (pkgName) => {
      if (visited.has(pkgName)) return;
      visited.add(pkgName);

      const pkg = this.registry.packages.find(p => p.name === pkgName);
      for (const dep of (pkg?.dependencies || []).filter(d => d.startsWith('@unrdf'))) {
        visit(dep);
      }

      order.push(pkgName);
    };

    packages.forEach(p => visit(p.name));
    return order;
  }

  /**
   * Get instance of running package
   */
  getInstance(packageName) {
    return this.instances.get(packageName);
  }

  /**
   * Stop all packages
   */
  async stopAll() {
    this.started.clear();
    this.instances.clear();
  }
}

export default PackageLauncher;
