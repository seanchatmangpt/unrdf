/**
 * @file src/unrdf-package-registry.mjs
 * @description Runtime package registry and discovery system
 * Built from ggen-discovered ontology data
 */

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const projectRoot = path.resolve(__dirname, '..');

class PackageRegistry {
  constructor() {
    this.packages = new Map();
    this.tiers = {
      Essential: [],
      Extended: [],
      Optional: [],
      Internal: [],
    };
    this.dependencyGraph = new Map();
    this.reverseDependencyGraph = new Map();
    this.initialized = false;
  }

  async initialize() {
    if (this.initialized) return;

    const exportsPath = path.join(projectRoot, 'src', 'generated', 'package-exports.mjs');

    try {
      const { ESSENTIAL_PACKAGES, EXTENDED_PACKAGES, OPTIONAL_PACKAGES, INTERNAL_PACKAGES } =
        await import(`file://${exportsPath}`);

      const allPackages = [
        ...ESSENTIAL_PACKAGES,
        ...EXTENDED_PACKAGES,
        ...OPTIONAL_PACKAGES,
        ...INTERNAL_PACKAGES,
      ];

      for (const pkg of allPackages) {
        this.packages.set(pkg.name, {
          name: pkg.name,
          version: pkg.version,
          tier: pkg.tier,
          loaded: false,
          module: null,
        });

        this.tiers[pkg.tier].push(pkg.name);
      }

      this.initialized = true;
    } catch (error) {
      throw new Error(`Failed to initialize PackageRegistry: ${error.message}`);
    }
  }

  async load(packageName) {
    if (!this.initialized) await this.initialize();

    const pkg = this.packages.get(packageName);
    if (!pkg) {
      throw new Error(`Package not found: ${packageName}`);
    }

    if (pkg.loaded) {
      return pkg.module;
    }

    try {
      const modulePath = path.join(projectRoot, 'packages', packageName.replace('@unrdf/', ''), 'src', 'index.mjs');

      if (fs.existsSync(modulePath)) {
        pkg.module = await import(`file://${modulePath}`);
        pkg.loaded = true;
        return pkg.module;
      } else {
        throw new Error(`Package module not found: ${modulePath}`);
      }
    } catch (error) {
      throw new Error(`Failed to load package ${packageName}: ${error.message}`);
    }
  }

  async loadByTier(tier) {
    if (!this.initialized) await this.initialize();

    const packages = this.tiers[tier] || [];
    const results = new Map();

    for (const pkgName of packages) {
      try {
        results.set(pkgName, await this.load(pkgName));
      } catch (error) {
        results.set(pkgName, { error: error.message });
      }
    }

    return results;
  }

  async loadEssential() {
    return this.loadByTier('Essential');
  }

  getPackageInfo(packageName) {
    if (!this.initialized) {
      throw new Error('Registry not initialized. Call initialize() first.');
    }

    return this.packages.get(packageName);
  }

  getPackagesByTier(tier) {
    if (!this.initialized) {
      throw new Error('Registry not initialized. Call initialize() first.');
    }

    return this.tiers[tier] || [];
  }

  getAllPackages() {
    if (!this.initialized) {
      throw new Error('Registry not initialized. Call initialize() first.');
    }

    return Array.from(this.packages.values());
  }

  getPackageCount() {
    if (!this.initialized) {
      throw new Error('Registry not initialized. Call initialize() first.');
    }

    return this.packages.size;
  }

  getTierSummary() {
    if (!this.initialized) {
      throw new Error('Registry not initialized. Call initialize() first.');
    }

    const summary = {};
    for (const [tier, packages] of Object.entries(this.tiers)) {
      summary[tier] = packages.length;
    }
    return summary;
  }

  async resolveDependencies(packageName, visited = new Set()) {
    if (!this.initialized) await this.initialize();

    if (visited.has(packageName)) {
      return [];
    }

    visited.add(packageName);
    const pkg = this.packages.get(packageName);

    if (!pkg) {
      throw new Error(`Package not found: ${packageName}`);
    }

    const resolved = [packageName];

    // For now, dependencies are hardcoded. In the future, parse from ontology
    const dependencies = this._getDependenciesFromPackage(packageName);

    for (const dep of dependencies) {
      const depResolved = await this.resolveDependencies(dep, visited);
      resolved.push(...depResolved);
    }

    return [...new Set(resolved)];
  }

  _getDependenciesFromPackage(packageName) {
    // This will be replaced with ontology-driven dependency resolution
    const depMap = {
      '@unrdf/core': ['@unrdf/oxigraph'],
      '@unrdf/cli': ['@unrdf/core', '@unrdf/hooks'],
      '@unrdf/knowledge-engine': ['@unrdf/core', '@unrdf/oxigraph'],
      '@unrdf/federation': ['@unrdf/core'],
      '@unrdf/hooks': ['@unrdf/core'],
      '@unrdf/kgc-4d': ['@unrdf/core'],
      '@unrdf/streaming': ['@unrdf/core'],
      '@unrdf/v6-core': ['@unrdf/core'],
      '@unrdf/yawl': ['@unrdf/core'],
    };
    return depMap[packageName] || [];
  }

  async validateTierConstraints(packageName) {
    if (!this.initialized) await this.initialize();

    const pkg = this.packages.get(packageName);
    if (!pkg) {
      throw new Error(`Package not found: ${packageName}`);
    }

    // Essential packages can only depend on other Essential or core
    if (pkg.tier === 'Essential') {
      const deps = this._getDependenciesFromPackage(packageName);
      for (const dep of deps) {
        const depPkg = this.packages.get(dep);
        if (depPkg && depPkg.tier !== 'Essential') {
          return {
            valid: false,
            error: `Essential package ${packageName} depends on ${pkg.tier} package ${dep}`,
          };
        }
      }
    }

    // Extended packages can depend on Essential but not Internal
    if (pkg.tier === 'Extended') {
      const deps = this._getDependenciesFromPackage(packageName);
      for (const dep of deps) {
        const depPkg = this.packages.get(dep);
        if (depPkg && depPkg.tier === 'Internal') {
          return {
            valid: false,
            error: `Extended package ${packageName} depends on Internal package ${dep}`,
          };
        }
      }
    }

    // Internal packages can depend on anything
    if (pkg.tier === 'Internal') {
      return { valid: true };
    }

    return { valid: true };
  }

  async getCompatibilityMatrix() {
    if (!this.initialized) await this.initialize();

    const matrix = {};
    const allPackages = this.getAllPackages();

    for (const pkg of allPackages) {
      const deps = this._getDependenciesFromPackage(pkg.name);
      const constraint = await this.validateTierConstraints(pkg.name);

      matrix[pkg.name] = {
        tier: pkg.tier,
        version: pkg.version,
        dependencies: deps,
        constraintValid: constraint.valid,
        constraintError: constraint.error || null,
      };
    }

    return matrix;
  }
}

export const registry = new PackageRegistry();

export async function getRegistry() {
  await registry.initialize();
  return registry;
}

export default registry;
