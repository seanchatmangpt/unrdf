/**
 * @fileoverview Monorepo Universe - Orchestrator for all 43 package partitions
 *
 * The MonorepoUniverse treats the entire UNRDF monorepo as a unified universe O
 * where each package is an admissible partition. The universe maintains:
 *
 * - Package partitions organized by category
 * - Cross-package dependency graph
 * - Version alignment state
 * - Admission history and receipts
 *
 * Universe Structure:
 * O = O_core + O_hooks + O_yawl + O_ml + O_infrastructure +
 *     O_observability + O_documentation + O_testing + O_utility
 *
 * @module monorepo-admission/monorepo-universe
 */

import { z } from 'zod';
import { createHash } from 'node:crypto';
import * as fs from 'node:fs/promises';
import * as path from 'node:path';

import {
  PackagePartition,
  PackageDelta,
  PartitionCategory,
  PACKAGE_CATEGORY_MAP,
  PROTECTED_PACKAGES,
  getCategoryForPackage,
  getPackagesInCategory
} from './package-partition.mjs';

import { checkAllCrossPackageInvariants } from './cross-package-invariants.mjs';
import { checkAllCrossPackageGuards } from './cross-package-guards.mjs';

/**
 * Universe state schema
 */
export const UniverseStateSchema = z.object({
  version: z.string(),
  contentHash: z.string(),
  partitionCount: z.number(),
  categoryDistribution: z.record(z.number()),
  lastAdmissionTime: z.string().datetime().nullable(),
  admissionCount: z.number()
});

/**
 * Universe configuration schema
 */
export const UniverseConfigSchema = z.object({
  rootPath: z.string(),
  strictMode: z.boolean().default(true),
  enabledInvariants: z.array(z.string()).default([
    'Q_version_consistency',
    'Q_api_stability',
    'Q_dependency_acyclic',
    'Q_license_compliance',
    'Q_documentation_coverage',
    'Q_test_coverage'
  ]),
  enabledGuards: z.array(z.string()).default([
    'H_core_signature_break',
    'H_internal_api_external_export',
    'H_circular_dependency',
    'H_license_incompatible',
    'H_protected_package_delete',
    'H_cross_category_break'
  ]),
  thresholds: z.object({
    minTestCoverage: z.number().default(70),
    minDocCoverage: z.number().default(80),
    maxVersionDrift: z.number().default(1)
  }).default({})
});

/**
 * MonorepoUniverse - Orchestrates governance across all package partitions
 *
 * @class MonorepoUniverse
 * @example
 * const universe = new MonorepoUniverse({ rootPath: '/path/to/unrdf' });
 * await universe.loadPartitions();
 *
 * const delta = new PackageDelta({
 *   agent: 'developer',
 *   changes: [{ packageName: '@unrdf/core', changeType: 'version_bump', ... }]
 * });
 *
 * const result = await universe.admitDelta(delta);
 * if (result.admitted) {
 *   // Changes are safe
 * }
 */
export class MonorepoUniverse {
  /**
   * Create a new MonorepoUniverse
   * @param {Object} config - Universe configuration
   */
  constructor(config) {
    const validated = UniverseConfigSchema.parse(config);
    this.config = validated;
    this.rootPath = validated.rootPath;

    /** @type {Map<string, PackagePartition>} Package partitions by name */
    this.partitions = new Map();

    /** @type {Map<string, Set<string>>} Dependency graph (pkg -> deps) */
    this.dependencyGraph = new Map();

    /** @type {Map<string, Set<string>>} Reverse dependency graph (pkg -> dependents) */
    this.reverseDependencyGraph = new Map();

    /** @type {Array<Object>} Admission history */
    this.admissionHistory = [];

    /** @type {string|null} Current universe content hash */
    this._contentHash = null;

    /** @type {Object} Statistics */
    this.stats = {
      totalAdmissions: 0,
      allowedAdmissions: 0,
      deniedAdmissions: 0,
      lastAdmissionTime: null
    };
  }

  /**
   * Load all package partitions from the monorepo
   * @returns {Promise<void>}
   */
  async loadPartitions() {
    const packagesDir = path.join(this.rootPath, 'packages');

    try {
      const entries = await fs.readdir(packagesDir, { withFileTypes: true });

      for (const entry of entries) {
        if (!entry.isDirectory()) continue;

        const packageJsonPath = path.join(packagesDir, entry.name, 'package.json');

        try {
          const packageJsonContent = await fs.readFile(packageJsonPath, 'utf-8');
          const packageJson = JSON.parse(packageJsonContent);

          const partition = PackagePartition.fromPackageJson(packageJson, {
            // Add coverage data if available (would come from coverage reports)
            testCoverage: await this._getTestCoverage(entry.name),
            docCoverage: await this._getDocCoverage(entry.name)
          });

          this.partitions.set(partition.name, partition);
        } catch (error) {
          // Skip packages without valid package.json
          console.warn(`Skipping ${entry.name}: ${error.message}`);
        }
      }

      // Build dependency graphs
      this._buildDependencyGraphs();

      // Compute universe hash
      this._contentHash = this._computeUniverseHash();

    } catch (error) {
      throw new Error(`Failed to load partitions: ${error.message}`);
    }
  }

  /**
   * Load partitions from provided data (for testing)
   * @param {Array<Object>} packageConfigs - Package configurations
   */
  loadPartitionsFromData(packageConfigs) {
    this.partitions.clear();

    for (const config of packageConfigs) {
      const partition = new PackagePartition(config);
      this.partitions.set(partition.name, partition);
    }

    this._buildDependencyGraphs();
    this._contentHash = this._computeUniverseHash();
  }

  /**
   * Build dependency graphs from partitions
   * @private
   */
  _buildDependencyGraphs() {
    this.dependencyGraph.clear();
    this.reverseDependencyGraph.clear();

    // Initialize graphs
    for (const [name] of this.partitions) {
      this.dependencyGraph.set(name, new Set());
      this.reverseDependencyGraph.set(name, new Set());
    }

    // Populate graphs
    for (const [name, partition] of this.partitions) {
      for (const dep of partition.getWorkspaceDependencies()) {
        if (this.partitions.has(dep.name)) {
          this.dependencyGraph.get(name).add(dep.name);
          this.reverseDependencyGraph.get(dep.name).add(name);
        }
      }
    }
  }

  /**
   * Get test coverage for a package (placeholder)
   * @param {string} packageName - Package directory name
   * @returns {Promise<number|undefined>} Coverage percentage
   * @private
   */
  async _getTestCoverage(packageName) {
    // In production, this would read from coverage reports
    // For now, return undefined to indicate unmeasured
    return undefined;
  }

  /**
   * Get documentation coverage for a package (placeholder)
   * @param {string} packageName - Package directory name
   * @returns {Promise<number|undefined>} Coverage percentage
   * @private
   */
  async _getDocCoverage(packageName) {
    // In production, this would analyze JSDoc coverage
    return undefined;
  }

  /**
   * Compute universe content hash
   * @returns {string} SHA-256 hash
   * @private
   */
  _computeUniverseHash() {
    const partitionHashes = [];

    for (const [name, partition] of [...this.partitions].sort((a, b) =>
      a[0].localeCompare(b[0])
    )) {
      partitionHashes.push({
        name,
        hash: partition.getHash()
      });
    }

    return createHash('sha256')
      .update(JSON.stringify(partitionHashes))
      .digest('hex');
  }

  /**
   * Get partition by name
   * @param {string} name - Package name
   * @returns {PackagePartition|undefined} Partition or undefined
   */
  getPartition(name) {
    return this.partitions.get(name);
  }

  /**
   * Get all partitions
   * @returns {Map<string, PackagePartition>} All partitions
   */
  getAllPartitions() {
    return this.partitions;
  }

  /**
   * Get partitions by category
   * @param {string} category - Category identifier
   * @returns {Array<PackagePartition>} Partitions in category
   */
  getPartitionsByCategory(category) {
    return [...this.partitions.values()].filter(p => p.category === category);
  }

  /**
   * Get direct dependencies of a package
   * @param {string} packageName - Package name
   * @returns {Set<string>} Dependency names
   */
  getDependencies(packageName) {
    return this.dependencyGraph.get(packageName) || new Set();
  }

  /**
   * Get direct dependents of a package (reverse deps)
   * @param {string} packageName - Package name
   * @returns {Set<string>} Dependent names
   */
  getDependents(packageName) {
    return this.reverseDependencyGraph.get(packageName) || new Set();
  }

  /**
   * Get transitive dependents (all packages that depend on this, recursively)
   * @param {string} packageName - Package name
   * @returns {Set<string>} All transitive dependents
   */
  getTransitiveDependents(packageName) {
    const result = new Set();
    const queue = [packageName];

    while (queue.length > 0) {
      const current = queue.shift();
      const dependents = this.getDependents(current);

      for (const dep of dependents) {
        if (!result.has(dep)) {
          result.add(dep);
          queue.push(dep);
        }
      }
    }

    return result;
  }

  /**
   * Get topological order of packages (for build order)
   * @returns {Array<string>} Packages in topological order
   */
  getTopologicalOrder() {
    const inDegree = new Map();
    const queue = [];
    const result = [];

    // Initialize in-degrees
    for (const [name] of this.partitions) {
      inDegree.set(name, 0);
    }

    // Calculate in-degrees
    for (const [name, deps] of this.dependencyGraph) {
      for (const dep of deps) {
        inDegree.set(dep, (inDegree.get(dep) || 0) + 1);
      }
    }

    // Find nodes with no dependencies
    for (const [name, degree] of inDegree) {
      if (degree === 0) {
        queue.push(name);
      }
    }

    // Process queue
    while (queue.length > 0) {
      const node = queue.shift();
      result.push(node);

      for (const dep of this.getDependencies(node)) {
        inDegree.set(dep, inDegree.get(dep) - 1);
        if (inDegree.get(dep) === 0) {
          queue.push(dep);
        }
      }
    }

    return result;
  }

  /**
   * Admit a delta (proposed changes) to the universe
   * @param {PackageDelta} delta - Proposed changes
   * @param {Object} options - Admission options
   * @returns {Promise<Object>} Admission result
   */
  async admitDelta(delta, options = {}) {
    const timestamp = new Date().toISOString();
    const admissionId = crypto.randomUUID();

    this.stats.totalAdmissions++;

    // Step 1: Run cross-package guards
    const guardResults = checkAllCrossPackageGuards(
      this.partitions,
      delta,
      {
        enableCoreSignatureBreak: this.config.enabledGuards.includes('H_core_signature_break'),
        enableInternalApiExport: this.config.enabledGuards.includes('H_internal_api_external_export'),
        enableCircularDependency: this.config.enabledGuards.includes('H_circular_dependency'),
        enableLicenseIncompatible: this.config.enabledGuards.includes('H_license_incompatible'),
        enableProtectedPackageDelete: this.config.enabledGuards.includes('H_protected_package_delete'),
        enableCrossCategoryBreak: this.config.enabledGuards.includes('H_cross_category_break')
      }
    );

    if (!guardResults.allowed) {
      this.stats.deniedAdmissions++;

      const result = {
        admissionId,
        timestamp,
        admitted: false,
        decision: 'DENY',
        phase: 'GUARD_CHECK',
        reason: guardResults.summary,
        guardResults,
        invariantResults: null,
        deltaHash: delta.getHash(),
        universeHash: this._contentHash
      };

      this._logAdmission(result);
      return result;
    }

    // Step 2: Run cross-package invariants
    const invariantResults = checkAllCrossPackageInvariants(
      this.partitions,
      delta,
      {
        enableVersionConsistency: this.config.enabledInvariants.includes('Q_version_consistency'),
        enableApiStability: this.config.enabledInvariants.includes('Q_api_stability'),
        enableDependencyAcyclic: this.config.enabledInvariants.includes('Q_dependency_acyclic'),
        enableLicenseCompliance: this.config.enabledInvariants.includes('Q_license_compliance'),
        enableDocCoverage: this.config.enabledInvariants.includes('Q_documentation_coverage'),
        enableTestCoverage: this.config.enabledInvariants.includes('Q_test_coverage'),
        ...this.config.thresholds
      }
    );

    if (!invariantResults.passed) {
      this.stats.deniedAdmissions++;

      const result = {
        admissionId,
        timestamp,
        admitted: false,
        decision: 'DENY',
        phase: 'INVARIANT_CHECK',
        reason: invariantResults.summary,
        guardResults,
        invariantResults,
        deltaHash: delta.getHash(),
        universeHash: this._contentHash
      };

      this._logAdmission(result);
      return result;
    }

    // Step 3: All checks passed - ALLOW
    this.stats.allowedAdmissions++;
    this.stats.lastAdmissionTime = timestamp;

    const result = {
      admissionId,
      timestamp,
      admitted: true,
      decision: 'ALLOW',
      phase: 'COMPLETE',
      reason: `All checks passed: ${guardResults.guards.length} guards, ${invariantResults.results.length} invariants`,
      guardResults,
      invariantResults,
      deltaHash: delta.getHash(),
      universeHash: this._contentHash,
      affectedPackages: Array.from(delta.getAffectedPackages()),
      impactAnalysis: this._analyzeImpact(delta)
    };

    this._logAdmission(result);
    return result;
  }

  /**
   * Analyze impact of admitted changes
   * @param {PackageDelta} delta - Admitted delta
   * @returns {Object} Impact analysis
   * @private
   */
  _analyzeImpact(delta) {
    const affectedPackages = delta.getAffectedPackages();
    const transitivelyAffected = new Set();

    for (const pkg of affectedPackages) {
      const dependents = this.getTransitiveDependents(pkg);
      for (const dep of dependents) {
        transitivelyAffected.add(dep);
      }
    }

    return {
      directlyAffected: Array.from(affectedPackages),
      transitivelyAffected: Array.from(transitivelyAffected),
      totalImpact: affectedPackages.size + transitivelyAffected.size,
      requiresFullBuild: transitivelyAffected.size > 10,
      categoriesAffected: [...new Set(
        [...affectedPackages, ...transitivelyAffected]
          .map(p => getCategoryForPackage(p))
      )]
    };
  }

  /**
   * Log admission to history
   * @param {Object} result - Admission result
   * @private
   */
  _logAdmission(result) {
    this.admissionHistory.push({
      timestamp: result.timestamp,
      admissionId: result.admissionId,
      decision: result.decision,
      reason: result.reason,
      phase: result.phase,
      deltaHash: result.deltaHash
    });

    // Keep history bounded
    if (this.admissionHistory.length > 1000) {
      this.admissionHistory.shift();
    }
  }

  /**
   * Get universe state summary
   * @returns {Object} Universe state
   */
  getState() {
    const categoryDistribution = {};

    for (const category of Object.values(PartitionCategory)) {
      categoryDistribution[category] = this.getPartitionsByCategory(category).length;
    }

    return {
      version: this._getUniverseVersion(),
      contentHash: this._contentHash,
      partitionCount: this.partitions.size,
      categoryDistribution,
      lastAdmissionTime: this.stats.lastAdmissionTime,
      admissionCount: this.stats.totalAdmissions,
      protectedPackages: Array.from(PROTECTED_PACKAGES),
      dependencyDepth: this._getMaxDependencyDepth()
    };
  }

  /**
   * Get universe version (derived from core package)
   * @returns {string} Universe version
   * @private
   */
  _getUniverseVersion() {
    const core = this.partitions.get('@unrdf/core');
    return core ? core.version : '0.0.0';
  }

  /**
   * Get maximum dependency depth
   * @returns {number} Max depth
   * @private
   */
  _getMaxDependencyDepth() {
    let maxDepth = 0;

    const calculateDepth = (pkg, visited = new Set()) => {
      if (visited.has(pkg)) return 0;
      visited.add(pkg);

      const deps = this.getDependencies(pkg);
      if (deps.size === 0) return 0;

      let maxChildDepth = 0;
      for (const dep of deps) {
        maxChildDepth = Math.max(maxChildDepth, calculateDepth(dep, new Set(visited)));
      }

      return maxChildDepth + 1;
    };

    for (const [name] of this.partitions) {
      maxDepth = Math.max(maxDepth, calculateDepth(name));
    }

    return maxDepth;
  }

  /**
   * Get admission statistics
   * @returns {Object} Statistics
   */
  getStats() {
    return {
      ...this.stats,
      allowRate: this.stats.totalAdmissions > 0
        ? `${((this.stats.allowedAdmissions / this.stats.totalAdmissions) * 100).toFixed(2)}%`
        : 'N/A',
      partitionCount: this.partitions.size,
      dependencyEdges: [...this.dependencyGraph.values()].reduce((sum, deps) => sum + deps.size, 0)
    };
  }

  /**
   * Get recent admission history
   * @param {number} limit - Number of entries
   * @returns {Array<Object>} Recent admissions
   */
  getAdmissionHistory(limit = 100) {
    return this.admissionHistory.slice(-limit);
  }

  /**
   * Validate universe consistency (run all invariants with no delta)
   * @returns {Object} Validation result
   */
  validateConsistency() {
    return checkAllCrossPackageInvariants(this.partitions, null, this.config.thresholds);
  }

  /**
   * Convert universe to JSON representation
   * @returns {Object} JSON representation
   */
  toJSON() {
    return {
      state: this.getState(),
      partitions: [...this.partitions.entries()].map(([name, p]) => ({
        name,
        ...p.toJSON()
      })),
      dependencyGraph: Object.fromEntries(
        [...this.dependencyGraph.entries()].map(([k, v]) => [k, [...v]])
      ),
      stats: this.getStats()
    };
  }
}

/**
 * Create a MonorepoUniverse instance
 * @param {Object} config - Configuration
 * @returns {MonorepoUniverse} New universe instance
 */
export function createMonorepoUniverse(config) {
  return new MonorepoUniverse(config);
}
