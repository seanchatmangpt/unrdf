import { getPackageSystem } from './unrdf-package-system.mjs';
import { getResolver } from './unrdf-dependency-resolver.mjs';

class ModuleCache {
  constructor() {
    this.cache = new Map();
    this.accessLog = [];
    this.stats = {
      hits: 0,
      misses: 0,
      evictions: 0,
    };
    this.maxSize = 100;
  }

  get(key) {
    if (this.cache.has(key)) {
      this.stats.hits++;
      this.accessLog.push({ key, action: 'hit', timestamp: Date.now() });
      return this.cache.get(key);
    }

    this.stats.misses++;
    this.accessLog.push({ key, action: 'miss', timestamp: Date.now() });
    return null;
  }

  set(key, value) {
    if (this.cache.size >= this.maxSize && !this.cache.has(key)) {
      const oldestKey = this.cache.keys().next().value;
      this.cache.delete(oldestKey);
      this.stats.evictions++;
    }

    this.cache.set(key, value);
    this.accessLog.push({ key, action: 'set', timestamp: Date.now() });
  }

  clear() {
    this.cache.clear();
  }

  getStats() {
    const total = this.stats.hits + this.stats.misses;
    return {
      ...this.stats,
      hitRate: total > 0 ? ((this.stats.hits / total) * 100).toFixed(2) : 0,
      size: this.cache.size,
      maxSize: this.maxSize,
    };
  }
}

export class ModuleFederation {
  constructor() {
    this.system = null;
    this.cache = new ModuleCache();
    this.loadedModules = new Map();
    this.moduleMetadata = new Map();
    this.boundaries = new Map();
    this.importMap = {};
  }

  async initialize(system) {
    this.system = system;
    await this._buildModuleGraph();
    await this._createBoundaries();
  }

  async _buildModuleGraph() {
    const packages = this.system.registry.getAllPackages();

    for (const pkg of packages) {
      this.moduleMetadata.set(pkg.name, {
        name: pkg.name,
        version: pkg.version,
        tier: pkg.tier,
        loaded: false,
        exports: [],
        dependencies: await this.system.getDependencies(pkg.name),
        loadTime: null,
        error: null,
      });
    }
  }

  async _createBoundaries() {
    const tiers = ['Essential', 'Extended', 'Optional', 'Internal'];

    for (const tier of tiers) {
      const packages = this.system.registry.getPackagesByTier(tier);
      this.boundaries.set(tier, {
        tier,
        packages,
        canImportFrom: this._getAllowedImports(tier),
      });
    }
  }

  _getAllowedImports(tierName) {
    const tierHierarchy = {
      Essential: ['Essential'],
      Extended: ['Essential', 'Extended'],
      Optional: ['Essential', 'Extended', 'Optional'],
      Internal: ['Essential', 'Extended', 'Optional', 'Internal'],
    };

    return tierHierarchy[tierName] || [];
  }

  async loadModule(packageName, options = {}) {
    const { cache = true, validateTier = true, preloadDeps = false } = options;

    if (cache) {
      const cached = this.cache.get(packageName);
      if (cached) return cached;
    }

    const metadata = this.moduleMetadata.get(packageName);
    if (!metadata) {
      throw new Error(`Package not found: ${packageName}`);
    }

    if (validateTier) {
      const validation = await this.system.validator.validatePackage(packageName);
      if (!validation.valid) {
        throw new Error(`Package ${packageName} failed validation: ${validation.violations.map((v) => v.message).join('; ')}`);
      }
    }

    try {
      const startTime = performance.now();

      let module;
      try {
        module = await this.system.registry.load(packageName);
      } catch (error) {
        metadata.error = error.message;
        throw error;
      }

      const loadTime = performance.now() - startTime;
      metadata.loadTime = loadTime;
      metadata.loaded = true;
      metadata.error = null;

      this.loadedModules.set(packageName, {
        module,
        loadTime,
        timestamp: new Date().toISOString(),
      });

      if (cache) {
        this.cache.set(packageName, module);
      }

      if (preloadDeps) {
        await this._preloadDependencies(packageName, cache, validateTier);
      }

      return module;
    } catch (error) {
      metadata.error = error.message;
      throw error;
    }
  }

  async _preloadDependencies(packageName, cache, validateTier) {
    const deps = await this.system.getDependencies(packageName);

    for (const dep of deps) {
      try {
        await this.loadModule(dep, { cache, validateTier, preloadDeps: false });
      } catch (error) {
      }
    }
  }

  async loadByTier(tierName, options = {}) {
    const { parallel = true } = options;
    const packages = this.system.registry.getPackagesByTier(tierName);
    const results = new Map();

    if (parallel) {
      const promises = packages.map((pkg) =>
        this.loadModule(pkg, options)
          .then((module) => results.set(pkg, module))
          .catch((error) => results.set(pkg, { error: error.message }))
      );

      await Promise.all(promises);
    } else {
      for (const pkg of packages) {
        try {
          const module = await this.loadModule(pkg, options);
          results.set(pkg, module);
        } catch (error) {
          results.set(pkg, { error: error.message });
        }
      }
    }

    return results;
  }

  async loadEssential(options = {}) {
    return this.loadByTier('Essential', options);
  }

  async loadDependencySet(packageName, options = {}) {
    const resolution = await this.system.resolver.resolve(packageName);

    if (!resolution.success) {
      throw new Error(`Failed to resolve dependencies: ${resolution.conflicts.map((c) => c.message).join('; ')}`);
    }

    const results = new Map();

    for (const dep of resolution.resolved) {
      try {
        const module = await this.loadModule(dep, options);
        results.set(dep, module);
      } catch (error) {
        results.set(dep, { error: error.message });
      }
    }

    return results;
  }

  getModuleMetadata(packageName) {
    return this.moduleMetadata.get(packageName);
  }

  getAllModuleMetadata() {
    return Object.fromEntries(this.moduleMetadata);
  }

  getBoundary(tierName) {
    return this.boundaries.get(tierName);
  }

  getLoadStatus() {
    const status = {
      total: this.moduleMetadata.size,
      loaded: 0,
      failed: 0,
      pending: 0,
      byTier: {},
    };

    const tiers = ['Essential', 'Extended', 'Optional', 'Internal'];
    for (const tier of tiers) {
      status.byTier[tier] = {
        total: 0,
        loaded: 0,
        failed: 0,
      };
    }

    for (const [, metadata] of this.moduleMetadata) {
      const tier = metadata.tier;

      if (status.byTier[tier]) {
        status.byTier[tier].total++;
      }

      if (metadata.loaded && !metadata.error) {
        status.loaded++;
        if (status.byTier[tier]) status.byTier[tier].loaded++;
      } else if (metadata.error) {
        status.failed++;
        if (status.byTier[tier]) status.byTier[tier].failed++;
      } else {
        status.pending++;
      }
    }

    return status;
  }

  getCacheStats() {
    return {
      cache: this.cache.getStats(),
      loadedModules: this.loadedModules.size,
    };
  }

  async unloadModule(packageName) {
    this.loadedModules.delete(packageName);
    this.cache.cache.delete(packageName);

    const metadata = this.moduleMetadata.get(packageName);
    if (metadata) {
      metadata.loaded = false;
    }
  }

  async clearAllModules() {
    this.loadedModules.clear();
    this.cache.clear();

    for (const metadata of this.moduleMetadata.values()) {
      metadata.loaded = false;
    }
  }

  async validateBoundaries() {
    const violations = [];

    for (const [packageName, metadata] of this.moduleMetadata) {
      const allowedTiers = this._getAllowedImports(metadata.tier);

      for (const dep of metadata.dependencies) {
        const depMetadata = this.moduleMetadata.get(dep);
        if (depMetadata && !allowedTiers.includes(depMetadata.tier)) {
          violations.push({
            package: packageName,
            dependency: dep,
            tier: metadata.tier,
            depTier: depMetadata.tier,
            message: `${metadata.tier} package cannot import from ${depMetadata.tier}`,
          });
        }
      }
    }

    return {
      valid: violations.length === 0,
      violations,
      timestamp: new Date().toISOString(),
    };
  }

  export() {
    return {
      metadata: this.getAllModuleMetadata(),
      status: this.getLoadStatus(),
      cache: this.getCacheStats(),
      boundaries: Object.fromEntries(this.boundaries),
      timestamp: new Date().toISOString(),
    };
  }
}

export const federation = new ModuleFederation();

export async function getModuleFederation(system) {
  await federation.initialize(system);
  return federation;
}

export default federation;
