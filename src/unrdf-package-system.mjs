/**
 * @file src/unrdf-package-system.mjs
 * @description Unified package system API integrating registry, resolver, hooks, and validator
 */

import { getRegistry } from './unrdf-package-registry.mjs';
import { getResolver } from './unrdf-dependency-resolver.mjs';
import { lifecycleHooks } from './unrdf-package-hooks.mjs';
import { getValidator } from './unrdf-package-validator.mjs';

export class UnrdfPackageSystem {
  constructor() {
    this.registry = null;
    this.resolver = null;
    this.validator = null;
    this.hooks = lifecycleHooks;
    this.initialized = false;
  }

  async initialize() {
    if (this.initialized) return;

    await this.hooks.emitDiscoveryStart({ timestamp: new Date().toISOString() });

    this.registry = await getRegistry();
    this.resolver = await getResolver();
    this.validator = await getValidator();

    this.initialized = true;

    await this.hooks.emitDiscoveryComplete({
      packagesFound: this.registry.getPackageCount(),
      timestamp: new Date().toISOString(),
    });
  }

  async discoverAndValidate() {
    if (!this.initialized) await this.initialize();

    const pkgs = this.registry.getAllPackages();

    for (const pkg of pkgs) {
      await this.hooks.emitPackageFound({
        name: pkg.name,
        version: pkg.version,
        tier: pkg.tier,
      });
    }

    const validation = await this.validator.validateAll();

    for (const result of validation.results) {
      if (!result.valid) {
        for (const violation of result.violations) {
          await this.hooks.emitTierViolation({
            package: result.package,
            violation,
          });
        }
      }
    }

    return {
      discoveredPackages: pkgs.length,
      validPackages: validation.validPackages,
      issues: validation.results.filter((r) => !r.valid),
    };
  }

  async resolveAndLoad(packageName) {
    if (!this.initialized) await this.initialize();

    const validation = await this.validator.validatePackage(packageName);
    if (!validation.valid) {
      await this.hooks.emitPackageError({
        package: packageName,
        violations: validation.violations,
      });
      throw new Error(`Package validation failed for ${packageName}: ${validation.violations.map((v) => v.message).join('; ')}`);
    }

    const resolution = await this.resolver.resolve(packageName);
    if (!resolution.success) {
      await this.hooks.emitConflictDetected({
        package: packageName,
        conflicts: resolution.conflicts,
      });
      throw new Error(`Dependency resolution failed: ${resolution.conflicts.map((c) => c.message).join('; ')}`);
    }

    await this.hooks.emitDependencyResolved({
      package: packageName,
      resolved: resolution.resolved,
    });

    const modules = new Map();
    for (const depName of resolution.resolved) {
      try {
        const module = await this.registry.load(depName);
        modules.set(depName, module);
        await this.hooks.emitPackageLoaded({
          name: depName,
          loaded: true,
        });
      } catch (error) {
        await this.hooks.emitPackageError({
          package: depName,
          error: error.message,
        });
        throw error;
      }
    }

    return modules;
  }

  async getFullReport() {
    if (!this.initialized) await this.initialize();

    const tierStructure = this.registry.getTierSummary();
    const validationReport = await this.validator.generateValidationReport();
    const eventStats = this.hooks.getEventStats();

    return {
      timestamp: new Date().toISOString(),
      packages: {
        total: this.registry.getPackageCount(),
        byTier: tierStructure,
      },
      validation: validationReport,
      events: eventStats,
      initialized: this.initialized,
    };
  }

  async getPackageInfo(packageName) {
    if (!this.initialized) await this.initialize();
    return this.registry.getPackageInfo(packageName);
  }

  async getDependencies(packageName) {
    if (!this.initialized) await this.initialize();
    return this.resolver.getDirectDependencies(packageName);
  }

  async getFullDependencyTree(packageName) {
    if (!this.initialized) await this.initialize();
    return this.resolver.getFullDependencyTree(packageName);
  }

  async getCompatibilityMatrix() {
    if (!this.initialized) await this.initialize();
    return this.registry.getCompatibilityMatrix();
  }

  async findSharedDependencies(packageNames) {
    if (!this.initialized) await this.initialize();
    return this.resolver.getSharedDependencies(packageNames);
  }

  async analyzeDependencies(packageName) {
    if (!this.initialized) await this.initialize();
    return this.resolver.analyzeDepthAndBreadth(packageName);
  }

  onPackageDiscovered(callback) {
    this.hooks.onPackageFound(callback);
  }

  onPackageLoaded(callback) {
    this.hooks.onPackageLoaded(callback);
  }

  onValidationViolation(callback) {
    this.hooks.onTierViolation(callback);
  }

  onDependencyResolved(callback) {
    this.hooks.onDependencyResolved(callback);
  }

  getEventHistory(event = null) {
    return this.hooks.getEventHistory(event);
  }
}

export const packageSystem = new UnrdfPackageSystem();

export async function getPackageSystem() {
  await packageSystem.initialize();
  return packageSystem;
}

export default packageSystem;
