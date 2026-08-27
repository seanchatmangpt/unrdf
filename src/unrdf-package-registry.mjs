/**
 * Runtime package registry backed exclusively by the ggen package projection.
 * Package manifests are observed once; src/generated/package-exports.mjs is the
 * deterministic projection consumed here. No hard-coded package/dependency map.
 */
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';
import { ALL_PACKAGES, getPackagesByTier as projectedPackagesByTier } from './generated/package-exports.mjs';

const projectRoot = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const TIERS = ['Essential', 'Extended', 'Optional', 'Internal'];

export class PackageRegistry {
  constructor() {
    this.packages = new Map();
    this.tiers = Object.fromEntries(TIERS.map(tier => [tier, []]));
    this.initialized = false;
  }

  async initialize() {
    if (this.initialized) return;
    this.packages.clear();
    this.tiers = Object.fromEntries(TIERS.map(tier => [tier, []]));

    for (const projected of ALL_PACKAGES) {
      if (!projected?.name || !projected?.path || !projected?.tier) {
        throw new Error(`PACKAGE_PROJECTION_INVALID:${JSON.stringify(projected)}`);
      }
      if (this.packages.has(projected.name)) throw new Error(`PACKAGE_PROJECTION_DUPLICATE:${projected.name}`);
      const pkg = { ...projected, loaded: false, module: null };
      this.packages.set(pkg.name, pkg);
      (this.tiers[pkg.tier] ||= []).push(pkg.name);
    }
    for (const names of Object.values(this.tiers)) names.sort();
    this.initialized = true;
  }

  async load(packageName) {
    if (!this.initialized) await this.initialize();
    const pkg = this.packages.get(packageName);
    if (!pkg) throw new Error(`Package not found: ${packageName}`);
    if (pkg.loaded) return pkg.module;
    if (!pkg.entry) throw new Error(`PACKAGE_ENTRY_NOT_DECLARED:${packageName}`);

    const modulePath = path.resolve(projectRoot, pkg.path, pkg.entry);
    if (!fs.existsSync(modulePath)) throw new Error(`PACKAGE_ENTRY_MISSING:${packageName}:${modulePath}`);
    pkg.module = await import(pathToFileURL(modulePath).href);
    pkg.loaded = true;
    return pkg.module;
  }

  async loadByTier(tier) {
    if (!this.initialized) await this.initialize();
    const results = new Map();
    for (const name of this.getPackagesByTier(tier)) {
      try { results.set(name, await this.load(name)); }
      catch (error) { results.set(name, { error: error.message }); }
    }
    return results;
  }

  async loadEssential() { return this.loadByTier('Essential'); }

  getPackageInfo(packageName) {
    if (!this.initialized) throw new Error('Registry not initialized. Call initialize() first.');
    return this.packages.get(packageName);
  }

  getPackagesByTier(tier) {
    if (!this.initialized) throw new Error('Registry not initialized. Call initialize() first.');
    const projected = projectedPackagesByTier(tier).map(pkg => pkg.name);
    return projected.length ? projected : [...(this.tiers[tier] || [])];
  }

  getAllPackages() {
    if (!this.initialized) throw new Error('Registry not initialized. Call initialize() first.');
    return [...this.packages.values()];
  }

  getPackageCount() {
    if (!this.initialized) throw new Error('Registry not initialized. Call initialize() first.');
    return this.packages.size;
  }

  getTierSummary() {
    if (!this.initialized) throw new Error('Registry not initialized. Call initialize() first.');
    return Object.fromEntries(Object.entries(this.tiers).map(([tier, names]) => [tier, names.length]));
  }

  getDirectDependencies(packageName) {
    if (!this.initialized) throw new Error('Registry not initialized. Call initialize() first.');
    const pkg = this.packages.get(packageName);
    if (!pkg) throw new Error(`Package not found: ${packageName}`);
    return [...(pkg.dependencies || [])];
  }

  getReverseDependencies(packageName) {
    if (!this.initialized) throw new Error('Registry not initialized. Call initialize() first.');
    const pkg = this.packages.get(packageName);
    if (!pkg) throw new Error(`Package not found: ${packageName}`);
    return [...(pkg.reverseDependencies || [])];
  }

  async resolveDependencies(packageName) {
    if (!this.initialized) await this.initialize();
    const visited = new Set();
    const visit = name => {
      if (visited.has(name)) return;
      const pkg = this.packages.get(name);
      if (!pkg) throw new Error(`Package not found: ${name}`);
      visited.add(name);
      for (const dep of pkg.dependencies || []) visit(dep);
    };
    visit(packageName);
    return [...visited];
  }

  async validateTierConstraints(packageName) {
    if (!this.initialized) await this.initialize();
    const pkg = this.packages.get(packageName);
    if (!pkg) throw new Error(`Package not found: ${packageName}`);
    const missing = (pkg.dependencies || []).filter(dep => !this.packages.has(dep));
    return missing.length ? { valid: false, error: `Unknown internal dependencies: ${missing.join(', ')}` } : { valid: true };
  }

  async getCompatibilityMatrix() {
    if (!this.initialized) await this.initialize();
    return Object.fromEntries(this.getAllPackages().map(pkg => [pkg.name, {
      tier: pkg.tier,
      version: pkg.version,
      dependencies: [...(pkg.dependencies || [])],
      reverseDependencies: [...(pkg.reverseDependencies || [])],
      sccId: pkg.sccId,
      sccSize: pkg.sccSize,
      cyclic: pkg.cyclic,
      constraintValid: (pkg.dependencies || []).every(dep => this.packages.has(dep)),
    }]));
  }
}

export const registry = new PackageRegistry();
export async function getRegistry() { await registry.initialize(); return registry; }
export default registry;
