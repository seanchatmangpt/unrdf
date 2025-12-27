/**
 * @fileoverview Monorepo Admission - Governed Ontology Substrate for Monorepo Governance
 *
 * This module extends the Governed Ontology Substrate (GOS) to govern the entire
 * 42+ package UNRDF monorepo. Each package is treated as an admissible partition
 * within a unified universe O.
 *
 * ## Architecture
 *
 * The monorepo is modeled as a universe of partitions:
 * - O = O_core + O_hooks + O_yawl + O_ml + O_infrastructure + O_observability +
 *       O_documentation + O_testing + O_utility
 *
 * ## Invariants (Q_monorepo)
 *
 * Cross-package invariants that must hold:
 * - Q_version_consistency: Version alignment across dependent packages
 * - Q_api_stability: Breaking changes have corresponding downstream updates
 * - Q_dependency_acyclic: No circular dependencies
 * - Q_license_compliance: Compatible licenses throughout
 * - Q_documentation_coverage: Public APIs are documented
 * - Q_test_coverage: Minimum test coverage thresholds
 *
 * ## Guards (H_monorepo)
 *
 * Forbidden operations that are blocked:
 * - H_core_signature_break: Core API changes without major version bump
 * - H_internal_api_external_export: Internal symbols exposed publicly
 * - H_circular_dependency: Dependencies creating cycles
 * - H_license_incompatible: Incompatible license additions
 * - H_protected_package_delete: Deprecation of protected packages
 * - H_cross_category_break: Cross-category dependency violations
 *
 * ## Usage
 *
 * ```javascript
 * import {
 *   MonorepoUniverse,
 *   MonorepoAdmissionEngine,
 *   PackageDelta
 * } from './monorepo-admission/index.mjs';
 *
 * // Create universe and load packages
 * const universe = new MonorepoUniverse({ rootPath: '/path/to/unrdf' });
 * await universe.loadPartitions();
 *
 * // Create admission engine
 * const engine = new MonorepoAdmissionEngine(universe, { strictMode: true });
 *
 * // Propose changes
 * const delta = new PackageDelta({
 *   agent: 'developer',
 *   changes: [
 *     { packageName: '@unrdf/core', changeType: 'version_bump', details: { newVersion: '5.0.2' } }
 *   ]
 * });
 *
 * // Admit delta (atomic decision)
 * const result = await engine.admit(delta);
 * if (result.admitted) {
 *   console.log('Changes admitted. Receipt:', result.receipt.receiptId);
 * } else {
 *   console.error('Denied:', result.reason);
 * }
 * ```
 *
 * @module monorepo-admission
 */

// Package partition model
export {
  PackagePartition,
  PackageChange,
  PackageDelta,
  PartitionCategory,
  PACKAGE_CATEGORY_MAP,
  PROTECTED_PACKAGES,
  COMPATIBLE_LICENSES,
  getCategoryForPackage,
  isProtectedPackage,
  getPackagesInCategory,
  PackagePartitionConfigSchema,
  PackageChangeSchema,
  PackageDeltaSchema,
  SemVerSchema,
  DependencySchema,
  ExportSchema
} from './package-partition.mjs';

// Cross-package invariants
export {
  Q_version_consistency,
  Q_api_stability,
  Q_dependency_acyclic,
  Q_license_compliance,
  Q_documentation_coverage,
  Q_test_coverage,
  checkAllCrossPackageInvariants,
  DEFAULT_THRESHOLDS,
  InvariantResultSchema
} from './cross-package-invariants.mjs';

// Cross-package guards
export {
  H_core_signature_break,
  H_internal_api_external_export,
  H_circular_dependency,
  H_license_incompatible,
  H_protected_package_delete,
  H_cross_category_break,
  checkAllCrossPackageGuards,
  CORE_PUBLIC_API,
  INTERNAL_API_PATTERNS,
  CATEGORY_DEPENDENCY_RULES,
  GuardResultSchema
} from './cross-package-guards.mjs';

// Monorepo universe
export {
  MonorepoUniverse,
  createMonorepoUniverse,
  UniverseStateSchema,
  UniverseConfigSchema
} from './monorepo-universe.mjs';

// Admission engine
export {
  MonorepoAdmissionEngine,
  createMonorepoAdmissionEngine,
  AdmissionDecisionSchema,
  AdmissionReceiptSchema,
  EngineConfigSchema
} from './monorepo-admission-engine.mjs';
