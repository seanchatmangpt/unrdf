/**
 * @file src/unrdf-package-validator.mjs
 * @description Package validation system for tier constraints and rules
 */

import { getRegistry } from './unrdf-package-registry.mjs';
import { getResolver } from './unrdf-dependency-resolver.mjs';

const TIER_HIERARCHY = {
  Essential: 1,
  Extended: 2,
  Optional: 3,
  Internal: 4,
};

const TIER_RULES = {
  Essential: {
    canDependOn: ['Essential'],
    description: 'Core packages that everything depends on',
  },
  Extended: {
    canDependOn: ['Essential', 'Extended'],
    description: 'Common packages for typical use cases',
  },
  Optional: {
    canDependOn: ['Essential', 'Extended', 'Optional'],
    description: 'Performance and optional features',
  },
  Internal: {
    canDependOn: ['Essential', 'Extended', 'Optional', 'Internal'],
    description: 'Internal testing and validation only',
  },
};

export class PackageValidator {
  constructor() {
    this.registry = null;
    this.resolver = null;
    this.violations = [];
  }

  async initialize() {
    this.registry = await getRegistry();
    this.resolver = await getResolver();
  }

  async validatePackage(packageName) {
    if (!this.registry) await this.initialize();

    const results = {
      package: packageName,
      valid: true,
      violations: [],
      warnings: [],
    };

    // Check if package exists
    const pkg = this.registry.getPackageInfo(packageName);
    if (!pkg) {
      results.valid = false;
      results.violations.push({
        code: 'PACKAGE_NOT_FOUND',
        message: `Package ${packageName} not found in registry`,
      });
      return results;
    }

    // Validate tier constraints
    const tierViolations = await this._validateTierConstraints(packageName);
    if (tierViolations.length > 0) {
      results.valid = false;
      results.violations.push(...tierViolations);
    }

    // Validate dependency graph
    const depViolations = await this._validateDependencyGraph(packageName);
    if (depViolations.length > 0) {
      results.valid = false;
      results.violations.push(...depViolations);
    }

    // Check for circular dependencies
    const circular = await this._checkCircularDeps(packageName);
    if (circular) {
      results.violations.push({
        code: 'CIRCULAR_DEPENDENCY',
        message: `Circular dependency detected: ${circular.join(' -> ')}`,
      });
      results.valid = false;
    }

    return results;
  }

  async _validateTierConstraints(packageName) {
    const violations = [];
    const pkg = this.registry.getPackageInfo(packageName);
    const deps = await this.resolver.getDirectDependencies(packageName);

    const allowedTiers = TIER_RULES[pkg.tier].canDependOn;

    for (const dep of deps) {
      const depPkg = this.registry.getPackageInfo(dep);
      if (!depPkg) {
        violations.push({
          code: 'INVALID_DEPENDENCY',
          message: `${packageName} depends on unknown package ${dep}`,
        });
        continue;
      }

      if (!allowedTiers.includes(depPkg.tier)) {
        violations.push({
          code: 'TIER_CONSTRAINT_VIOLATION',
          message: `${pkg.tier} package ${packageName} cannot depend on ${depPkg.tier} package ${dep}`,
          tier: pkg.tier,
          depTier: depPkg.tier,
        });
      }
    }

    return violations;
  }

  async _validateDependencyGraph(packageName) {
    const violations = [];

    const result = await this.resolver.resolve(packageName, { checkConflicts: true });

    if (!result.success) {
      for (const conflict of result.conflicts) {
        violations.push({
          code: 'DEPENDENCY_CONFLICT',
          message: conflict.message,
          type: conflict.type,
        });
      }
    }

    return violations;
  }

  async _checkCircularDeps(packageName, visited = new Set(), path = []) {
    const deps = await this.resolver.getDirectDependencies(packageName);

    for (const dep of deps) {
      if (path.includes(dep)) {
        return [...path, dep];
      }

      if (!visited.has(dep)) {
        visited.add(dep);
        const circular = await this._checkCircularDeps(dep, visited, [...path, packageName]);
        if (circular) {
          return circular;
        }
      }
    }

    return null;
  }

  async validateAll() {
    if (!this.registry) await this.initialize();

    const results = [];
    const packages = this.registry.getAllPackages();

    for (const pkg of packages) {
      const result = await this.validatePackage(pkg.name);
      results.push(result);
    }

    return {
      totalPackages: packages.length,
      validPackages: results.filter((r) => r.valid).length,
      invalidPackages: results.filter((r) => !r.valid).length,
      results,
    };
  }

  async validateTierStructure() {
    if (!this.registry) await this.initialize();

    const summary = this.registry.getTierSummary();
    const structure = {};

    for (const [tier, count] of Object.entries(summary)) {
      structure[tier] = {
        count,
        rules: TIER_RULES[tier],
        packages: this.registry.getPackagesByTier(tier),
      };
    }

    return structure;
  }

  async enforceConsistency() {
    if (!this.registry) await this.initialize();

    const results = await this.validateAll();
    const issues = results.results.filter((r) => !r.valid);

    return {
      consistent: issues.length === 0,
      issueCount: issues.length,
      issues,
    };
  }

  async generateValidationReport() {
    if (!this.registry) await this.initialize();

    const allValidation = await this.validateAll();
    const tierStructure = await this.validateTierStructure();
    const consistency = await this.enforceConsistency();

    return {
      timestamp: new Date().toISOString(),
      summary: {
        totalPackages: allValidation.totalPackages,
        validPackages: allValidation.validPackages,
        invalidPackages: allValidation.invalidPackages,
        consistencyScore: (allValidation.validPackages / allValidation.totalPackages) * 100,
      },
      tierStructure,
      validationResults: allValidation.results,
      consistency,
    };
  }
}

export const validator = new PackageValidator();

export async function getValidator() {
  await validator.initialize();
  return validator;
}

export default validator;
