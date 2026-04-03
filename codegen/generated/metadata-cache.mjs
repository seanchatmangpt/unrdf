/**
 * Working Package Metadata Cache
 * High-performance access to package information
 */

export class MetadataCache {
  constructor(registry) {
    this.registry = registry;
    this.byName = new Map();
    this.byTier = { essential: [], extended: [], optional: [] };
    this.depGraph = new Map();
    this.reverseDepGraph = new Map();

    this._buildIndexes();
  }

  _buildIndexes() {
    // Index by name
    for (const pkg of this.registry.packages) {
      this.byName.set(pkg.name, pkg);
      this.byTier[pkg.tier].push(pkg);

      // Build dependency graphs
      const deps = pkg.dependencies.filter(d => d.startsWith('@unrdf'));
      this.depGraph.set(pkg.name, deps);

      for (const dep of deps) {
        if (!this.reverseDepGraph.has(dep)) {
          this.reverseDepGraph.set(dep, []);
        }
        this.reverseDepGraph.get(dep).push(pkg.name);
      }
    }
  }

  /**
   * Get package metadata
   */
  getPackage(name) {
    return this.byName.get(name);
  }

  /**
   * Get all dependents of a package
   */
  getDependents(name) {
    return this.reverseDepGraph.get(name) || [];
  }

  /**
   * Get all dependencies of a package
   */
  getDependencies(name) {
    return this.depGraph.get(name) || [];
  }

  /**
   * Check if package exists
   */
  exists(name) {
    return this.byName.has(name);
  }

  /**
   * Get packages in tier
   */
  getByTier(tier) {
    return this.byTier[tier] || [];
  }

  /**
   * Get all packages
   */
  getAll() {
    return this.registry.packages;
  }

  /**
   * Search packages
   */
  search(query) {
    const q = query.toLowerCase();
    return this.registry.packages.filter(p =>
      p.name.toLowerCase().includes(q) ||
      (p.description && p.description.toLowerCase().includes(q))
    );
  }

  /**
   * Get impact analysis - what breaks if package removed
   */
  getRemovalImpact(name) {
    return {
      directDependents: this.getDependents(name),
      indirectDependents: this._getTransitiveDependents(name)
    };
  }

  _getTransitiveDependents(name, visited = new Set()) {
    if (visited.has(name)) return [];
    visited.add(name);

    const direct = this.getDependents(name);
    const indirect = [];

    for (const dependent of direct) {
      indirect.push(...this._getTransitiveDependents(dependent, visited));
    }

    return [...new Set([...direct, ...indirect])];
  }
}

export default MetadataCache;
