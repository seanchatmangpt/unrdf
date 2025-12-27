/**
 * @fileoverview Check modules index - Exports all production readiness checks
 *
 * @module validation/checks
 */

export { codeQualityCheck, CODE_QUALITY_THRESHOLDS } from './code-quality-check.mjs';
export { testCheck, TEST_THRESHOLDS } from './test-check.mjs';
export { dependencyCheck, DEPENDENCY_THRESHOLDS } from './dependency-check.mjs';
export { securityCheck, SEVERITY } from './security-check.mjs';
export { documentationCheck, DOCUMENTATION_THRESHOLDS } from './documentation-check.mjs';
export { performanceCheck, PERFORMANCE_THRESHOLDS } from './performance-check.mjs';
export { accessibilityCheck } from './accessibility-check.mjs';
export { compatibilityCheck, COMPATIBILITY_THRESHOLDS } from './compatibility-check.mjs';

/**
 * All check functions mapped by name
 */
export const checks = {
  codeQuality: (await import('./code-quality-check.mjs')).codeQualityCheck,
  testing: (await import('./test-check.mjs')).testCheck,
  dependencies: (await import('./dependency-check.mjs')).dependencyCheck,
  security: (await import('./security-check.mjs')).securityCheck,
  documentation: (await import('./documentation-check.mjs')).documentationCheck,
  performance: (await import('./performance-check.mjs')).performanceCheck,
  accessibility: (await import('./accessibility-check.mjs')).accessibilityCheck,
  compatibility: (await import('./compatibility-check.mjs')).compatibilityCheck
};

/**
 * Run all checks on a package
 *
 * @param {string} packagePath - Package path
 * @param {Object} options - Check options
 * @returns {Promise<Object>} All check results
 */
export async function runAllChecks(packagePath, options = {}) {
  const results = {};

  for (const [name, check] of Object.entries(checks)) {
    if (options.skip?.includes(name)) {
      results[name] = { status: 'skipped', score: null };
      continue;
    }

    try {
      results[name] = await check(packagePath, options);
    } catch (error) {
      results[name] = {
        status: 'error',
        score: 0,
        error: error.message
      };
    }
  }

  return results;
}
