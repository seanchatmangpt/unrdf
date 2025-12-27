/**
 * @fileoverview Cross-Package Guards (H_monorepo) - Forbidden operations across packages
 *
 * Guards that PREVENT dangerous cross-package operations:
 * - H_core_signature_break: Cannot change core public API without major version bump
 * - H_internal_api_external_export: Cannot expose @private symbols in public API
 * - H_circular_dependency: Cannot add dependency that creates cycle
 * - H_license_incompatible: Cannot depend on incompatible license
 * - H_protected_package_delete: Cannot delete or deprecate protected packages
 * - H_cross_category_break: Cannot break cross-category contracts
 *
 * @module monorepo-admission/cross-package-guards
 */

import { z } from 'zod';
import {
  PROTECTED_PACKAGES,
  COMPATIBLE_LICENSES,
  PartitionCategory,
  getCategoryForPackage
} from './package-partition.mjs';

/**
 * Guard result schema
 */
export const GuardResultSchema = z.object({
  allowed: z.boolean(),
  guardName: z.string(),
  reason: z.string(),
  violations: z.array(z.any()).optional(),
  severity: z.enum(['block', 'warn', 'info']).default('block')
});

/**
 * Core package public API signatures that cannot change without major version
 */
export const CORE_PUBLIC_API = {
  '@unrdf/core': [
    'createStore',
    'dataFactory',
    'quad',
    'namedNode',
    'blankNode',
    'literal',
    'variable',
    'defaultGraph',
    'SPARQLEngine',
    'RDFStore'
  ],
  '@unrdf/oxigraph': [
    'createStore',
    'dataFactory',
    'OxigraphStore'
  ],
  '@unrdf/hooks': [
    'defineHook',
    'createHookExecutor',
    'HookExecutor'
  ],
  '@unrdf/validation': [
    'validateGraph',
    'SHACLValidator',
    'ValidationReport'
  ]
};

/**
 * Internal API patterns that must not be exposed
 */
export const INTERNAL_API_PATTERNS = [
  /^_/,                    // Underscore prefix
  /@private/,              // JSDoc @private
  /@internal/,             // JSDoc @internal
  /\.internal\./,          // .internal. in path
  /\/internal\//           // /internal/ in path
];

/**
 * Category dependency rules (which categories can depend on which)
 */
export const CATEGORY_DEPENDENCY_RULES = {
  [PartitionCategory.CORE]: [],  // Core depends on nothing internal
  [PartitionCategory.HOOKS]: [PartitionCategory.CORE],
  [PartitionCategory.YAWL]: [PartitionCategory.CORE, PartitionCategory.HOOKS],
  [PartitionCategory.ML]: [PartitionCategory.CORE, PartitionCategory.HOOKS, PartitionCategory.YAWL],
  [PartitionCategory.INFRASTRUCTURE]: [PartitionCategory.CORE, PartitionCategory.HOOKS],
  [PartitionCategory.OBSERVABILITY]: [PartitionCategory.CORE],
  [PartitionCategory.DOCUMENTATION]: [], // Documentation is isolated
  [PartitionCategory.TESTING]: ['*'],    // Testing can depend on anything
  [PartitionCategory.UTILITY]: [PartitionCategory.CORE, PartitionCategory.HOOKS]
};

/**
 * H_core_signature_break: Prevent core public API changes without major version bump
 *
 * @param {Map<string, Object>} partitions - Package partitions
 * @param {Object} delta - Proposed changes
 * @param {Object} options - Guard options
 * @returns {Object} Guard result
 */
export function H_core_signature_break(partitions, delta, options = {}) {
  const violations = [];

  if (!delta || !delta.changes) {
    return {
      allowed: true,
      guardName: 'H_core_signature_break',
      reason: 'No changes to evaluate',
      severity: 'info'
    };
  }

  for (const change of delta.changes) {
    const packageName = change.packageName;

    // Only check protected packages
    if (!PROTECTED_PACKAGES.has(packageName)) {
      continue;
    }

    // Check for API breaking changes
    if (change.changeType === 'api_breaking' ||
        change.changeType === 'export_remove' ||
        change.changeType === 'export_modify') {

      const partition = partitions.get(packageName);
      if (!partition) continue;

      // Check if this is a major version bump
      const versionChange = delta.changes.find(
        c => c.packageName === packageName && c.changeType === 'version_bump'
      );

      const isMajorBump = versionChange?.details?.bumpType === 'major';

      if (!isMajorBump) {
        // Check if specific public API is affected
        const publicApi = CORE_PUBLIC_API[packageName] || [];
        const affectedSymbol = change.details?.symbol || change.details?.export;

        if (publicApi.includes(affectedSymbol)) {
          violations.push({
            package: packageName,
            changeType: change.changeType,
            affectedSymbol,
            reason: 'Core public API change requires major version bump',
            details: change.details
          });
        } else if (change.changeType === 'export_remove') {
          // Any export removal from protected package needs major bump
          violations.push({
            package: packageName,
            changeType: change.changeType,
            reason: 'Export removal from protected package requires major version bump',
            details: change.details
          });
        }
      }
    }
  }

  if (violations.length > 0) {
    return {
      allowed: false,
      guardName: 'H_core_signature_break',
      reason: `Blocked: ${violations.length} core API change(s) without major version bump`,
      violations,
      severity: 'block'
    };
  }

  return {
    allowed: true,
    guardName: 'H_core_signature_break',
    reason: 'No unauthorized core API changes detected',
    severity: 'info'
  };
}

/**
 * H_internal_api_external_export: Prevent exposing internal symbols in public API
 *
 * @param {Map<string, Object>} partitions - Package partitions
 * @param {Object} delta - Proposed changes
 * @param {Object} options - Guard options
 * @returns {Object} Guard result
 */
export function H_internal_api_external_export(partitions, delta, options = {}) {
  const violations = [];

  if (!delta || !delta.changes) {
    return {
      allowed: true,
      guardName: 'H_internal_api_external_export',
      reason: 'No changes to evaluate',
      severity: 'info'
    };
  }

  for (const change of delta.changes) {
    if (change.changeType !== 'export_add' && change.changeType !== 'export_modify') {
      continue;
    }

    const exportPath = change.details?.path || '';
    const entryPoint = change.details?.entryPoint || '';
    const symbolName = change.details?.symbol || '';

    // Check if export path or symbol matches internal patterns
    for (const pattern of INTERNAL_API_PATTERNS) {
      if (pattern.test(exportPath) || pattern.test(entryPoint) || pattern.test(symbolName)) {
        violations.push({
          package: change.packageName,
          exportPath,
          symbol: symbolName,
          pattern: pattern.toString(),
          reason: 'Cannot expose internal API symbol in public exports'
        });
        break;
      }
    }

    // Check for @private/@internal in JSDoc (if provided)
    if (change.details?.jsdoc) {
      if (change.details.jsdoc.includes('@private') ||
          change.details.jsdoc.includes('@internal')) {
        violations.push({
          package: change.packageName,
          symbol: symbolName,
          reason: 'Cannot export symbol marked as @private or @internal'
        });
      }
    }
  }

  if (violations.length > 0) {
    return {
      allowed: false,
      guardName: 'H_internal_api_external_export',
      reason: `Blocked: ${violations.length} internal symbol(s) exposed in public API`,
      violations,
      severity: 'block'
    };
  }

  return {
    allowed: true,
    guardName: 'H_internal_api_external_export',
    reason: 'No internal API exposure detected',
    severity: 'info'
  };
}

/**
 * H_circular_dependency: Prevent adding dependencies that create cycles
 *
 * @param {Map<string, Object>} partitions - Package partitions
 * @param {Object} delta - Proposed changes
 * @param {Object} options - Guard options
 * @returns {Object} Guard result
 */
export function H_circular_dependency(partitions, delta, options = {}) {
  const violations = [];

  if (!delta || !delta.changes) {
    return {
      allowed: true,
      guardName: 'H_circular_dependency',
      reason: 'No changes to evaluate',
      severity: 'info'
    };
  }

  // Filter for dependency additions
  const depAdditions = delta.changes.filter(c => c.changeType === 'dependency_add');

  if (depAdditions.length === 0) {
    return {
      allowed: true,
      guardName: 'H_circular_dependency',
      reason: 'No dependency additions to check',
      severity: 'info'
    };
  }

  // Build current dependency graph
  const graph = new Map();
  for (const [name, partition] of partitions) {
    graph.set(name, new Set(
      partition.getWorkspaceDependencies?.()?.map(d => d.name) || []
    ));
  }

  // Check each proposed addition for cycles
  for (const addition of depAdditions) {
    const fromPackage = addition.packageName;
    const toPackage = addition.details?.dependency;

    if (!toPackage || !partitions.has(toPackage)) {
      continue; // External dependency, skip
    }

    // Would this create a cycle?
    const wouldCycle = checkWouldCreateCycle(graph, fromPackage, toPackage);

    if (wouldCycle) {
      violations.push({
        from: fromPackage,
        to: toPackage,
        reason: 'Adding this dependency would create a circular dependency',
        cycle: wouldCycle.path
      });
    }
  }

  if (violations.length > 0) {
    return {
      allowed: false,
      guardName: 'H_circular_dependency',
      reason: `Blocked: ${violations.length} dependency addition(s) would create cycle(s)`,
      violations,
      severity: 'block'
    };
  }

  return {
    allowed: true,
    guardName: 'H_circular_dependency',
    reason: 'No circular dependencies would be created',
    severity: 'info'
  };
}

/**
 * Check if adding an edge would create a cycle
 * @param {Map<string, Set<string>>} graph - Current dependency graph
 * @param {string} from - Source package
 * @param {string} to - Target package
 * @returns {Object|null} Cycle info or null
 */
function checkWouldCreateCycle(graph, from, to) {
  // Check if there's already a path from 'to' back to 'from'
  const visited = new Set();
  const path = [to];

  function dfs(node) {
    if (node === from) {
      return true; // Found cycle
    }

    if (visited.has(node)) {
      return false;
    }

    visited.add(node);
    path.push(node);

    for (const dep of graph.get(node) || []) {
      if (dfs(dep)) {
        return true;
      }
    }

    path.pop();
    return false;
  }

  for (const dep of graph.get(to) || []) {
    if (dfs(dep)) {
      return { path: [from, ...path, from] };
    }
  }

  // Also check if 'to' directly depends on 'from'
  if (graph.get(to)?.has(from)) {
    return { path: [from, to, from] };
  }

  return null;
}

/**
 * H_license_incompatible: Prevent adding dependencies with incompatible licenses
 *
 * @param {Map<string, Object>} partitions - Package partitions
 * @param {Object} delta - Proposed changes
 * @param {Object} options - Guard options
 * @returns {Object} Guard result
 */
export function H_license_incompatible(partitions, delta, options = {}) {
  const violations = [];

  if (!delta || !delta.changes) {
    return {
      allowed: true,
      guardName: 'H_license_incompatible',
      reason: 'No changes to evaluate',
      severity: 'info'
    };
  }

  // Check license changes
  const licenseChanges = delta.changes.filter(c => c.changeType === 'license_change');

  for (const change of licenseChanges) {
    const newLicense = change.details?.newLicense;

    if (newLicense && !COMPATIBLE_LICENSES.has(newLicense)) {
      violations.push({
        package: change.packageName,
        proposedLicense: newLicense,
        compatibleLicenses: Array.from(COMPATIBLE_LICENSES),
        reason: 'Proposed license is not compatible with MIT base license'
      });
    }
  }

  // Check dependency additions for license compatibility
  const depAdditions = delta.changes.filter(c => c.changeType === 'dependency_add');

  for (const addition of depAdditions) {
    const depLicense = addition.details?.license;

    if (depLicense && !COMPATIBLE_LICENSES.has(depLicense)) {
      violations.push({
        package: addition.packageName,
        dependency: addition.details?.dependency,
        dependencyLicense: depLicense,
        reason: 'Dependency has incompatible license'
      });
    }
  }

  if (violations.length > 0) {
    return {
      allowed: false,
      guardName: 'H_license_incompatible',
      reason: `Blocked: ${violations.length} license incompatibility issue(s)`,
      violations,
      severity: 'block'
    };
  }

  return {
    allowed: true,
    guardName: 'H_license_incompatible',
    reason: 'No license incompatibilities detected',
    severity: 'info'
  };
}

/**
 * H_protected_package_delete: Prevent deletion/deprecation of protected packages
 *
 * @param {Map<string, Object>} partitions - Package partitions
 * @param {Object} delta - Proposed changes
 * @param {Object} options - Guard options
 * @returns {Object} Guard result
 */
export function H_protected_package_delete(partitions, delta, options = {}) {
  const violations = [];

  if (!delta || !delta.changes) {
    return {
      allowed: true,
      guardName: 'H_protected_package_delete',
      reason: 'No changes to evaluate',
      severity: 'info'
    };
  }

  for (const change of delta.changes) {
    if (change.changeType !== 'deprecation') {
      continue;
    }

    const packageName = change.packageName;

    if (PROTECTED_PACKAGES.has(packageName)) {
      violations.push({
        package: packageName,
        reason: 'Cannot deprecate protected package',
        protectedPackages: Array.from(PROTECTED_PACKAGES)
      });
    }
  }

  if (violations.length > 0) {
    return {
      allowed: false,
      guardName: 'H_protected_package_delete',
      reason: `Blocked: Cannot deprecate ${violations.length} protected package(s)`,
      violations,
      severity: 'block'
    };
  }

  return {
    allowed: true,
    guardName: 'H_protected_package_delete',
    reason: 'No protected package deprecation attempted',
    severity: 'info'
  };
}

/**
 * H_cross_category_break: Prevent breaking cross-category dependency contracts
 *
 * @param {Map<string, Object>} partitions - Package partitions
 * @param {Object} delta - Proposed changes
 * @param {Object} options - Guard options
 * @returns {Object} Guard result
 */
export function H_cross_category_break(partitions, delta, options = {}) {
  const violations = [];
  const warnings = [];

  if (!delta || !delta.changes) {
    return {
      allowed: true,
      guardName: 'H_cross_category_break',
      reason: 'No changes to evaluate',
      severity: 'info'
    };
  }

  // Check dependency additions against category rules
  const depAdditions = delta.changes.filter(c => c.changeType === 'dependency_add');

  for (const addition of depAdditions) {
    const fromPackage = addition.packageName;
    const toPackage = addition.details?.dependency;

    if (!toPackage) continue;

    const fromCategory = getCategoryForPackage(fromPackage);
    const toCategory = getCategoryForPackage(toPackage);

    // Get allowed categories for 'from'
    const allowedCategories = CATEGORY_DEPENDENCY_RULES[fromCategory] || [];

    // Testing can depend on anything
    if (allowedCategories.includes('*')) {
      continue;
    }

    // Check if dependency category is allowed
    if (toCategory && !allowedCategories.includes(toCategory)) {
      // Exception: Same category is always allowed
      if (fromCategory !== toCategory) {
        violations.push({
          from: fromPackage,
          fromCategory,
          to: toPackage,
          toCategory,
          allowedCategories,
          reason: `Category ${fromCategory} cannot depend on ${toCategory}`
        });
      }
    }
  }

  if (violations.length > 0) {
    return {
      allowed: false,
      guardName: 'H_cross_category_break',
      reason: `Blocked: ${violations.length} cross-category dependency violation(s)`,
      violations,
      severity: 'block'
    };
  }

  return {
    allowed: true,
    guardName: 'H_cross_category_break',
    reason: 'Cross-category dependencies are valid',
    severity: 'info'
  };
}

/**
 * Check all cross-package guards
 * @param {Map<string, Object>} partitions - Package partitions
 * @param {Object} delta - Proposed changes
 * @param {Object} options - Guard options
 * @returns {Object} Combined guard results
 */
export function checkAllCrossPackageGuards(partitions, delta, options = {}) {
  const guards = [
    { fn: H_core_signature_break, enabled: options.enableCoreSignatureBreak !== false },
    { fn: H_internal_api_external_export, enabled: options.enableInternalApiExport !== false },
    { fn: H_circular_dependency, enabled: options.enableCircularDependency !== false },
    { fn: H_license_incompatible, enabled: options.enableLicenseIncompatible !== false },
    { fn: H_protected_package_delete, enabled: options.enableProtectedPackageDelete !== false },
    { fn: H_cross_category_break, enabled: options.enableCrossCategoryBreak !== false }
  ];

  const results = [];
  const blocked = [];

  for (const { fn, enabled } of guards) {
    if (!enabled) continue;

    const result = fn(partitions, delta, options);
    results.push(result);

    if (!result.allowed && result.severity === 'block') {
      blocked.push({
        guard: result.guardName,
        reason: result.reason,
        violations: result.violations
      });
    }
  }

  return {
    allowed: blocked.length === 0,
    guards: results,
    blockedCount: blocked.length,
    blockedBy: blocked.map(b => b.guard),
    summary: blocked.length > 0
      ? `Blocked by ${blocked.length} guard(s): ${blocked.map(b => b.guard).join(', ')}`
      : `All ${results.length} guards passed`
  };
}
