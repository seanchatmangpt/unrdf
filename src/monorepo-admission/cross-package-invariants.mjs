/**
 * @fileoverview Cross-Package Invariants (Q_monorepo) - Checkable properties across packages
 *
 * Invariants that MUST hold true across the entire monorepo:
 * - Q_version_consistency: Version alignment across dependent packages
 * - Q_api_stability: Breaking changes require corresponding downstream updates
 * - Q_dependency_acyclic: No circular dependencies in package graph
 * - Q_license_compliance: All packages have compatible licenses
 * - Q_documentation_coverage: Exported APIs have documentation
 * - Q_test_coverage: Packages meet minimum test coverage thresholds
 *
 * @module monorepo-admission/cross-package-invariants
 */

import { z } from 'zod';
import {
  PackagePartition,
  PROTECTED_PACKAGES,
  COMPATIBLE_LICENSES,
  PartitionCategory
} from './package-partition.mjs';

/**
 * Invariant result schema
 */
export const InvariantResultSchema = z.object({
  invariantName: z.string(),
  passed: z.boolean(),
  reason: z.string(),
  violations: z.array(z.any()).optional(),
  warnings: z.array(z.any()).optional(),
  metadata: z.record(z.any()).optional(),
  severity: z.enum(['error', 'warning', 'info']).default('error')
});

/**
 * Default thresholds for invariant checks
 */
export const DEFAULT_THRESHOLDS = {
  minTestCoverage: 70,           // Minimum test coverage percentage
  minDocCoverage: 80,            // Minimum doc coverage for public APIs
  maxVersionDrift: 1,            // Max minor version drift from core
  maxDependencyDepth: 10         // Max depth in dependency graph
};

/**
 * Q_version_consistency: Verify version alignment across packages
 *
 * Rules:
 * 1. If core is v5.x.y, dependent packages must be >=4.9.0
 * 2. Workspace packages should share major version
 * 3. Protected packages must be tightly aligned
 *
 * @param {Map<string, PackagePartition>} partitions - Package partitions
 * @param {Object} delta - Proposed changes
 * @param {Object} options - Check options
 * @returns {Object} Invariant result
 */
export function Q_version_consistency(partitions, delta, options = {}) {
  const violations = [];
  const warnings = [];
  const maxDrift = options.maxVersionDrift ?? DEFAULT_THRESHOLDS.maxVersionDrift;

  // Find core package version as reference
  const corePartition = partitions.get('@unrdf/core');
  if (!corePartition) {
    return {
      invariantName: 'Q_version_consistency',
      passed: true,
      reason: 'Core package not found, skipping version consistency check',
      severity: 'info'
    };
  }

  const coreVersion = corePartition.parseVersion();

  // Check all packages for version alignment
  for (const [name, partition] of partitions) {
    if (name === '@unrdf/core') continue;

    try {
      const pkgVersion = partition.parseVersion();

      // Protected packages must have same major.minor
      if (partition.isProtectedPartition()) {
        if (pkgVersion.major !== coreVersion.major ||
            Math.abs(pkgVersion.minor - coreVersion.minor) > 0) {
          violations.push({
            package: name,
            expected: `${coreVersion.major}.${coreVersion.minor}.x`,
            actual: partition.version,
            reason: 'Protected package must match core major.minor version'
          });
        }
      } else {
        // Other packages: major version must match, minor can drift by maxDrift
        if (pkgVersion.major !== coreVersion.major) {
          violations.push({
            package: name,
            expected: `${coreVersion.major}.x.x`,
            actual: partition.version,
            reason: 'Major version must match core'
          });
        } else if (Math.abs(pkgVersion.minor - coreVersion.minor) > maxDrift) {
          warnings.push({
            package: name,
            expected: `${coreVersion.major}.${coreVersion.minor - maxDrift}-${coreVersion.minor + maxDrift}.x`,
            actual: partition.version,
            reason: 'Minor version drift exceeds threshold'
          });
        }
      }

      // Check workspace dependency versions
      for (const dep of partition.getWorkspaceDependencies()) {
        const depPartition = partitions.get(dep.name);
        if (depPartition) {
          const depVersion = depPartition.parseVersion();

          // Workspace deps should be at same major version
          if (depVersion.major !== pkgVersion.major) {
            violations.push({
              package: name,
              dependency: dep.name,
              packageVersion: partition.version,
              dependencyVersion: depPartition.version,
              reason: 'Workspace dependency major version mismatch'
            });
          }
        }
      }
    } catch (error) {
      warnings.push({
        package: name,
        error: error.message,
        reason: 'Failed to parse version'
      });
    }
  }

  if (violations.length > 0) {
    return {
      invariantName: 'Q_version_consistency',
      passed: false,
      reason: `Version consistency violated: ${violations.length} package(s) have incompatible versions`,
      violations,
      warnings,
      severity: 'error',
      metadata: {
        coreVersion: corePartition.version,
        checkedPackages: partitions.size
      }
    };
  }

  return {
    invariantName: 'Q_version_consistency',
    passed: true,
    reason: `All ${partitions.size} packages have consistent versions`,
    warnings: warnings.length > 0 ? warnings : undefined,
    metadata: {
      coreVersion: corePartition.version,
      checkedPackages: partitions.size
    }
  };
}

/**
 * Q_api_stability: Verify breaking changes have corresponding downstream updates
 *
 * Rules:
 * 1. Breaking changes to core require updates to all direct dependents
 * 2. Export removals require migration in all consumers
 * 3. Signature changes require version bumps in downstream
 *
 * @param {Map<string, PackagePartition>} partitions - Package partitions
 * @param {Object} delta - Proposed changes
 * @param {Object} options - Check options
 * @returns {Object} Invariant result
 */
export function Q_api_stability(partitions, delta, options = {}) {
  const violations = [];
  const warnings = [];

  if (!delta || !delta.hasBreakingChanges?.()) {
    return {
      invariantName: 'Q_api_stability',
      passed: true,
      reason: 'No breaking changes proposed',
      severity: 'info'
    };
  }

  const breakingChanges = delta.getBreakingChanges?.() || [];
  const affectedPackages = delta.getAffectedPackages?.() || new Set();

  // Build reverse dependency map
  const dependents = new Map();
  for (const [name, partition] of partitions) {
    for (const dep of partition.getWorkspaceDependencies()) {
      if (!dependents.has(dep.name)) {
        dependents.set(dep.name, new Set());
      }
      dependents.get(dep.name).add(name);
    }
  }

  // Check each breaking change
  for (const change of breakingChanges) {
    const packageName = change.packageName;
    const packageDependents = dependents.get(packageName) || new Set();

    // If package has dependents, they must also be in the changeset
    for (const dependent of packageDependents) {
      if (!affectedPackages.has(dependent)) {
        violations.push({
          breakingPackage: packageName,
          changeType: change.changeType,
          missingDependent: dependent,
          reason: 'Breaking change requires corresponding update in dependent package'
        });
      }
    }

    // Protected packages need additional scrutiny
    if (PROTECTED_PACKAGES.has(packageName)) {
      if (packageDependents.size > 5 && breakingChanges.length < packageDependents.size / 2) {
        warnings.push({
          package: packageName,
          dependentCount: packageDependents.size,
          changeCount: breakingChanges.length,
          reason: 'Protected package breaking change may require more downstream updates'
        });
      }
    }
  }

  if (violations.length > 0) {
    return {
      invariantName: 'Q_api_stability',
      passed: false,
      reason: `API stability violated: ${violations.length} dependent package(s) not updated`,
      violations,
      warnings,
      severity: 'error',
      metadata: {
        breakingChangeCount: breakingChanges.length,
        affectedPackageCount: affectedPackages.size
      }
    };
  }

  return {
    invariantName: 'Q_api_stability',
    passed: true,
    reason: 'All breaking changes have corresponding downstream updates',
    warnings: warnings.length > 0 ? warnings : undefined,
    metadata: {
      breakingChangeCount: breakingChanges.length
    }
  };
}

/**
 * Q_dependency_acyclic: Verify no circular dependencies in package graph
 *
 * Uses Kahn's algorithm for topological sort to detect cycles.
 *
 * @param {Map<string, PackagePartition>} partitions - Package partitions
 * @param {Object} delta - Proposed changes (for new dependency additions)
 * @param {Object} options - Check options
 * @returns {Object} Invariant result
 */
export function Q_dependency_acyclic(partitions, delta, options = {}) {
  const violations = [];

  // Build adjacency list (package -> dependencies)
  const graph = new Map();
  const inDegree = new Map();

  for (const [name] of partitions) {
    graph.set(name, new Set());
    inDegree.set(name, 0);
  }

  // Add edges for existing dependencies
  for (const [name, partition] of partitions) {
    for (const dep of partition.getWorkspaceDependencies()) {
      if (partitions.has(dep.name)) {
        graph.get(name).add(dep.name);
      }
    }
  }

  // Add edges for proposed new dependencies
  if (delta && delta.changes) {
    for (const change of delta.changes) {
      if (change.changeType === 'dependency_add' && change.details?.dependency) {
        const depName = change.details.dependency;
        if (partitions.has(depName)) {
          if (!graph.has(change.packageName)) {
            graph.set(change.packageName, new Set());
          }
          graph.get(change.packageName).add(depName);
        }
      }
    }
  }

  // Calculate in-degrees
  for (const [_, deps] of graph) {
    for (const dep of deps) {
      inDegree.set(dep, (inDegree.get(dep) || 0) + 1);
    }
  }

  // Kahn's algorithm
  const queue = [];
  const sorted = [];

  for (const [name, degree] of inDegree) {
    if (degree === 0) {
      queue.push(name);
    }
  }

  while (queue.length > 0) {
    const node = queue.shift();
    sorted.push(node);

    for (const dep of graph.get(node) || []) {
      inDegree.set(dep, inDegree.get(dep) - 1);
      if (inDegree.get(dep) === 0) {
        queue.push(dep);
      }
    }
  }

  // If not all nodes are sorted, there's a cycle
  if (sorted.length !== partitions.size) {
    // Find cycles using DFS
    const cycles = findCycles(graph);

    for (const cycle of cycles) {
      violations.push({
        cycle: cycle,
        packages: cycle.join(' -> '),
        reason: 'Circular dependency detected'
      });
    }

    return {
      invariantName: 'Q_dependency_acyclic',
      passed: false,
      reason: `Dependency cycle detected: ${violations.length} cycle(s) found`,
      violations,
      severity: 'error',
      metadata: {
        totalPackages: partitions.size,
        sortablePackages: sorted.length,
        cycleCount: violations.length
      }
    };
  }

  return {
    invariantName: 'Q_dependency_acyclic',
    passed: true,
    reason: `No circular dependencies detected in ${partitions.size} packages`,
    metadata: {
      topologicalOrder: sorted
    }
  };
}

/**
 * Find cycles in directed graph using DFS
 * @param {Map<string, Set<string>>} graph - Adjacency list
 * @returns {Array<Array<string>>} Detected cycles
 */
function findCycles(graph) {
  const cycles = [];
  const visited = new Set();
  const recursionStack = new Set();
  const path = [];

  function dfs(node) {
    visited.add(node);
    recursionStack.add(node);
    path.push(node);

    for (const neighbor of graph.get(node) || []) {
      if (!visited.has(neighbor)) {
        dfs(neighbor);
      } else if (recursionStack.has(neighbor)) {
        // Found cycle
        const cycleStart = path.indexOf(neighbor);
        if (cycleStart !== -1) {
          cycles.push([...path.slice(cycleStart), neighbor]);
        }
      }
    }

    path.pop();
    recursionStack.delete(node);
  }

  for (const [node] of graph) {
    if (!visited.has(node)) {
      dfs(node);
    }
  }

  return cycles;
}

/**
 * Q_license_compliance: Verify all packages have compatible licenses
 *
 * @param {Map<string, PackagePartition>} partitions - Package partitions
 * @param {Object} delta - Proposed changes
 * @param {Object} options - Check options
 * @returns {Object} Invariant result
 */
export function Q_license_compliance(partitions, delta, options = {}) {
  const violations = [];
  const warnings = [];
  const baseLicense = options.baseLicense || 'MIT';

  for (const [name, partition] of partitions) {
    const license = partition.license;

    if (!COMPATIBLE_LICENSES.has(license)) {
      violations.push({
        package: name,
        license: license,
        baseLicense: baseLicense,
        reason: `License '${license}' is not compatible with base license '${baseLicense}'`
      });
    }

    // Check if changing license
    if (delta) {
      const licenseChanges = delta.changes?.filter(
        c => c.packageName === name && c.changeType === 'license_change'
      ) || [];

      for (const change of licenseChanges) {
        const newLicense = change.details?.newLicense;
        if (newLicense && !COMPATIBLE_LICENSES.has(newLicense)) {
          violations.push({
            package: name,
            currentLicense: license,
            proposedLicense: newLicense,
            reason: 'Proposed license change is incompatible'
          });
        }
      }
    }
  }

  // Check external dependency licenses if provided
  if (options.checkExternalDeps) {
    for (const [name, partition] of partitions) {
      for (const dep of partition.getExternalDependencies()) {
        if (dep.license && !COMPATIBLE_LICENSES.has(dep.license)) {
          warnings.push({
            package: name,
            dependency: dep.name,
            dependencyLicense: dep.license,
            reason: 'External dependency has potentially incompatible license'
          });
        }
      }
    }
  }

  if (violations.length > 0) {
    return {
      invariantName: 'Q_license_compliance',
      passed: false,
      reason: `License compliance violated: ${violations.length} package(s) have incompatible licenses`,
      violations,
      warnings,
      severity: 'error',
      metadata: {
        baseLicense,
        checkedPackages: partitions.size
      }
    };
  }

  return {
    invariantName: 'Q_license_compliance',
    passed: true,
    reason: `All ${partitions.size} packages have compatible licenses`,
    warnings: warnings.length > 0 ? warnings : undefined,
    metadata: {
      baseLicense
    }
  };
}

/**
 * Q_documentation_coverage: Verify exported APIs have documentation
 *
 * @param {Map<string, PackagePartition>} partitions - Package partitions
 * @param {Object} delta - Proposed changes
 * @param {Object} options - Check options
 * @returns {Object} Invariant result
 */
export function Q_documentation_coverage(partitions, delta, options = {}) {
  const violations = [];
  const warnings = [];
  const minCoverage = options.minDocCoverage ?? DEFAULT_THRESHOLDS.minDocCoverage;

  for (const [name, partition] of partitions) {
    // Skip testing packages
    if (partition.category === PartitionCategory.TESTING) {
      continue;
    }

    const docCoverage = partition.docCoverage;

    if (docCoverage === undefined) {
      warnings.push({
        package: name,
        reason: 'Documentation coverage not measured'
      });
      continue;
    }

    if (docCoverage < minCoverage) {
      // Protected packages are stricter
      if (partition.isProtectedPartition()) {
        violations.push({
          package: name,
          coverage: docCoverage,
          required: minCoverage,
          reason: `Protected package documentation coverage ${docCoverage}% below required ${minCoverage}%`
        });
      } else {
        warnings.push({
          package: name,
          coverage: docCoverage,
          required: minCoverage,
          reason: `Documentation coverage ${docCoverage}% below recommended ${minCoverage}%`
        });
      }
    }
  }

  if (violations.length > 0) {
    return {
      invariantName: 'Q_documentation_coverage',
      passed: false,
      reason: `Documentation coverage violated: ${violations.length} protected package(s) below threshold`,
      violations,
      warnings,
      severity: 'error',
      metadata: {
        minCoverage,
        checkedPackages: partitions.size
      }
    };
  }

  return {
    invariantName: 'Q_documentation_coverage',
    passed: true,
    reason: 'Documentation coverage requirements met',
    warnings: warnings.length > 0 ? warnings : undefined,
    metadata: {
      minCoverage
    }
  };
}

/**
 * Q_test_coverage: Verify packages meet test coverage thresholds
 *
 * @param {Map<string, PackagePartition>} partitions - Package partitions
 * @param {Object} delta - Proposed changes
 * @param {Object} options - Check options
 * @returns {Object} Invariant result
 */
export function Q_test_coverage(partitions, delta, options = {}) {
  const violations = [];
  const warnings = [];
  const minCoverage = options.minTestCoverage ?? DEFAULT_THRESHOLDS.minTestCoverage;

  for (const [name, partition] of partitions) {
    // Skip documentation packages
    if (partition.category === PartitionCategory.DOCUMENTATION) {
      continue;
    }

    const testCoverage = partition.testCoverage;

    if (testCoverage === undefined) {
      warnings.push({
        package: name,
        reason: 'Test coverage not measured'
      });
      continue;
    }

    if (testCoverage < minCoverage) {
      // Core packages are stricter
      if (partition.category === PartitionCategory.CORE ||
          partition.isProtectedPartition()) {
        violations.push({
          package: name,
          coverage: testCoverage,
          required: minCoverage,
          reason: `Core package test coverage ${testCoverage}% below required ${minCoverage}%`
        });
      } else {
        warnings.push({
          package: name,
          coverage: testCoverage,
          required: minCoverage,
          reason: `Test coverage ${testCoverage}% below recommended ${minCoverage}%`
        });
      }
    }
  }

  if (violations.length > 0) {
    return {
      invariantName: 'Q_test_coverage',
      passed: false,
      reason: `Test coverage violated: ${violations.length} core package(s) below threshold`,
      violations,
      warnings,
      severity: 'error',
      metadata: {
        minCoverage,
        checkedPackages: partitions.size
      }
    };
  }

  return {
    invariantName: 'Q_test_coverage',
    passed: true,
    reason: 'Test coverage requirements met',
    warnings: warnings.length > 0 ? warnings : undefined,
    metadata: {
      minCoverage
    }
  };
}

/**
 * Check all cross-package invariants
 * @param {Map<string, PackagePartition>} partitions - Package partitions
 * @param {Object} delta - Proposed changes
 * @param {Object} options - Check options
 * @returns {Object} Combined invariant results
 */
export function checkAllCrossPackageInvariants(partitions, delta, options = {}) {
  const invariants = [
    { fn: Q_version_consistency, enabled: options.enableVersionConsistency !== false },
    { fn: Q_api_stability, enabled: options.enableApiStability !== false },
    { fn: Q_dependency_acyclic, enabled: options.enableDependencyAcyclic !== false },
    { fn: Q_license_compliance, enabled: options.enableLicenseCompliance !== false },
    { fn: Q_documentation_coverage, enabled: options.enableDocCoverage !== false },
    { fn: Q_test_coverage, enabled: options.enableTestCoverage !== false }
  ];

  const results = [];
  const failures = [];

  for (const { fn, enabled } of invariants) {
    if (!enabled) continue;

    const result = fn(partitions, delta, options);
    results.push(result);

    if (!result.passed && result.severity === 'error') {
      failures.push({
        invariant: result.invariantName,
        reason: result.reason,
        violations: result.violations
      });
    }
  }

  return {
    passed: failures.length === 0,
    results,
    failures,
    summary: failures.length > 0
      ? `${failures.length} invariant(s) failed: ${failures.map(f => f.invariant).join(', ')}`
      : `All ${results.length} cross-package invariants passed`
  };
}
