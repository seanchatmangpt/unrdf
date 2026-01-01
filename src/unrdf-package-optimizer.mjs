export class PackageOptimizer {
  constructor(system, federation, healthMonitor) {
    this.system = system;
    this.federation = federation;
    this.healthMonitor = healthMonitor;
  }

  async analyzePackageSize(packageName) {
    const metadata = this.federation.getModuleMetadata(packageName);
    const loadStatus = this.federation.getLoadStatus();

    return {
      package: packageName,
      tier: metadata.tier,
      loadTime: metadata.loadTime || 'not-loaded',
      dependencies: metadata.dependencies.length,
      reverseDependents: await this._getReverseDependencies(packageName),
    };
  }

  async _getReverseDependencies(packageName) {
    const resolver = this.system.resolver;
    const reverseDeps = await resolver.getReverseDependencies(packageName);
    return reverseDeps.length;
  }

  async detectCircularDependencies() {
    const cycles = [];
    const visited = new Set();

    for (const [packageName] of this.federation.moduleMetadata) {
      if (!visited.has(packageName)) {
        const tree = await this.system.resolver.getFullDependencyTree(packageName);
        const deps = await this.system.resolver.getDirectDependencies(packageName);

        for (const dep of deps) {
          const depTree = await this.system.resolver.getFullDependencyTree(dep);
          if (depTree[packageName]) {
            cycles.push([packageName, dep, packageName]);
            visited.add(packageName);
            visited.add(dep);
          }
        }
      }
    }

    return cycles;
  }

  async findUnusedDependencies() {
    const unused = [];

    for (const [packageName, metadata] of this.federation.moduleMetadata) {
      const reverseDeps = await this.system.resolver.getReverseDependencies(packageName);

      if (reverseDeps.length === 0 && packageName !== '@unrdf/core') {
        unused.push({
          package: packageName,
          tier: metadata.tier,
          reason: 'No packages depend on this',
        });
      }
    }

    return unused;
  }

  async detectHeavyDependencies() {
    const heavy = [];

    for (const [packageName, metadata] of this.federation.moduleMetadata) {
      if (metadata.dependencies.length > 5) {
        heavy.push({
          package: packageName,
          tier: metadata.tier,
          depCount: metadata.dependencies.length,
          dependencies: metadata.dependencies,
          recommendation: 'Consider splitting into smaller packages',
        });
      }
    }

    return heavy.sort((a, b) => b.depCount - a.depCount);
  }

  async detectImportPathOptimizations() {
    const optimizations = [];

    for (const [packageName, metadata] of this.federation.moduleMetadata) {
      const tree = await this.system.resolver.getFullDependencyTree(packageName);
      const depth = Math.max(0, ...Object.values(tree).map((n) => n.level || 0));

      if (depth > 3) {
        optimizations.push({
          package: packageName,
          currentDepth: depth,
          recommendation: 'Deep dependency chain could be optimized',
          suggestion: 'Consider direct imports or facade patterns',
        });
      }
    }

    return optimizations;
  }

  async suggestTierReorganization() {
    const suggestions = [];
    const metadata = this.federation.getAllModuleMetadata();

    for (const [packageName, pkg] of Object.entries(metadata)) {
      for (const dep of pkg.dependencies) {
        const depPkg = metadata[dep];
        if (!depPkg) continue;

        const tierHierarchy = {
          Essential: 1,
          Extended: 2,
          Optional: 3,
          Internal: 4,
        };

        const pkgTierValue = tierHierarchy[pkg.tier] || 0;
        const depTierValue = tierHierarchy[depPkg.tier] || 0;

        if (pkgTierValue < depTierValue) {
          suggestions.push({
            package: packageName,
            currentTier: pkg.tier,
            dependency: dep,
            dependencyTier: depPkg.tier,
            issue: `${pkg.tier} package depends on ${depPkg.tier} package`,
            recommendation: `Move ${packageName} to ${depPkg.tier} tier or move ${dep} to ${pkg.tier} tier`,
          });
        }
      }
    }

    return suggestions;
  }

  async findBuildableComponents() {
    const components = [];
    const metadata = this.federation.getAllModuleMetadata();

    for (const tier of ['Essential', 'Extended', 'Optional']) {
      const tierPackages = this.system.registry.getPackagesByTier(tier);

      components.push({
        tier,
        packages: tierPackages.length,
        canBuildInParallel: true,
        estimatedBuildTime: tierPackages.length * 2,
      });
    }

    return components;
  }

  async generateOptimizationReport() {
    const report = {
      timestamp: new Date().toISOString(),
      analysis: {
        circular: await this.detectCircularDependencies(),
        unused: await this.findUnusedDependencies(),
        heavy: await this.detectHeavyDependencies(),
        deepPaths: await this.detectImportPathOptimizations(),
        tierSuggestions: await this.suggestTierReorganization(),
        buildableComponents: await this.findBuildableComponents(),
      },
      scores: {
        circularityScore: 0,
        depthScore: 0,
        tierScore: 0,
        overallScore: 0,
      },
    };

    report.scores.circularityScore = report.analysis.circular.length === 0 ? 100 : Math.max(0, 100 - report.analysis.circular.length * 10);
    report.scores.depthScore = report.analysis.deepPaths.length === 0 ? 100 : Math.max(0, 100 - report.analysis.deepPaths.length * 5);
    report.scores.tierScore = report.analysis.tierSuggestions.length === 0 ? 100 : Math.max(0, 100 - report.analysis.tierSuggestions.length * 5);
    report.scores.overallScore = (report.scores.circularityScore + report.scores.depthScore + report.scores.tierScore) / 3;

    return report;
  }

  async suggestParallelLoadingGroups() {
    const groups = [];
    const loaded = new Map();

    for (const tier of ['Essential', 'Extended', 'Optional']) {
      const tierPackages = this.system.registry.getPackagesByTier(tier);
      const group = {
        tier,
        packages: [],
        estimatedParallelTime: 0,
      };

      for (const pkg of tierPackages) {
        const metadata = this.federation.getModuleMetadata(pkg);
        if (metadata && metadata.loadTime) {
          group.packages.push({
            name: pkg,
            loadTime: metadata.loadTime,
          });

          group.estimatedParallelTime = Math.max(group.estimatedParallelTime, metadata.loadTime);
        }
      }

      if (group.packages.length > 0) {
        groups.push(group);
      }
    }

    return groups;
  }

  async suggestCachingStrategy() {
    const cacheStats = this.federation.getCacheStats();
    const loadStatus = this.federation.getLoadStatus();

    const strategy = {
      currentCacheHitRate: cacheStats.cache.hitRate,
      totalCached: cacheStats.cache.size,
      maxCacheSize: cacheStats.cache.maxSize,
      recommendations: [],
    };

    if (cacheStats.cache.hitRate < 50) {
      strategy.recommendations.push({
        priority: 'high',
        suggestion: 'Cache hit rate is low - consider increasing cache size or adjusting cache strategy',
        action: 'Increase maxSize or use persistent cache',
      });
    }

    if (loadStatus.loaded > loadStatus.total * 0.9) {
      strategy.recommendations.push({
        priority: 'low',
        suggestion: 'Most modules are loaded - consider aggressive caching',
        action: 'Enable persistent cache to disk',
      });
    }

    return strategy;
  }

  export() {
    return {
      timestamp: new Date().toISOString(),
      optimizer: 'PackageOptimizer',
    };
  }
}

export async function getPackageOptimizer(system, federation, healthMonitor) {
  return new PackageOptimizer(system, federation, healthMonitor);
}

export default PackageOptimizer;
