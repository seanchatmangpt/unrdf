#!/usr/bin/env node

import { PACKAGES, REGISTRY } from '../generated/index.mjs';
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const projectRoot = path.resolve(__dirname, '../..');

// Build working package resolver system
function generatePackageResolver() {
  const code = `/**
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
    if (!pkg) throw new Error(\`Package not found: \${name}\`);

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
      issues.push(\`Missing dependencies: \${missingDeps.join(', ')}\`);
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
      issues.push(\`Potential version conflicts with: \${incompatible.join(', ')}\`);
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
`;

  return code;
}

// Build working dependency injection system
function generateDependencyInjection() {
  const code = `/**
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
      throw new Error(\`Unknown package: \${name}\`);
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
    if (!pkg) throw new Error(\`Package not found: \${name}\`);

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
`;

  return code;
}

// Build working package metadata cache
function generateMetadataCache() {
  const code = `/**
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
`;

  return code;
}

// Main
async function main() {
  console.log('🔧 Generating working utility systems...');

  const outputDir = path.join(projectRoot, 'ggen', 'generated');
  fs.mkdirSync(outputDir, { recursive: true });

  // Generate resolver
  console.log('   📦 Package Resolver...');
  const resolver = generatePackageResolver();
  fs.writeFileSync(path.join(outputDir, 'package-resolver.mjs'), resolver);

  // Generate DI
  console.log('   💉 Dependency Injection...');
  const di = generateDependencyInjection();
  fs.writeFileSync(path.join(outputDir, 'di-container.mjs'), di);

  // Generate cache
  console.log('   ⚡ Metadata Cache...');
  const cache = generateMetadataCache();
  fs.writeFileSync(path.join(outputDir, 'metadata-cache.mjs'), cache);

  // Generate index that exports all
  console.log('   📋 Unified exports...');
  const exportsCode = `export { PackageResolver, default as PackageResolverDefault } from './package-resolver.mjs';
export { DIContainer, default as DIContainerDefault } from './di-container.mjs';
export { MetadataCache, default as MetadataCacheDefault } from './metadata-cache.mjs';
export { PACKAGES, REGISTRY, getPackage, findByTier, stats } from './index.mjs';

// Convenience exports
export async function createResolver() {
  const { REGISTRY } = await import('./index.mjs');
  const { PackageResolver } = await import('./package-resolver.mjs');
  return new PackageResolver(REGISTRY);
}

export async function createDIContainer() {
  const { REGISTRY } = await import('./index.mjs');
  const { DIContainer } = await import('./di-container.mjs');
  return new DIContainer(REGISTRY);
}

export async function createMetadataCache() {
  const { REGISTRY } = await import('./index.mjs');
  const { MetadataCache } = await import('./metadata-cache.mjs');
  return new MetadataCache(REGISTRY);
}
`;

  fs.writeFileSync(path.join(outputDir, 'utilities.mjs'), exportsCode);

  console.log(`\n✅ Generated working utility systems`);
  console.log(`   - PackageResolver: Async resolution with caching`);
  console.log(`   - DIContainer: Dependency injection for cross-package modules`);
  console.log(`   - MetadataCache: Fast indexed package metadata access`);
  console.log(`   - Utilities: Convenience factory functions`);
}

main().catch(console.error);
