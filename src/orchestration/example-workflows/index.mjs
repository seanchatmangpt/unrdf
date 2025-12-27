/**
 * @fileoverview Example Workflows Index
 *
 * Collection of example multi-package workflow scenarios demonstrating
 * atomic orchestration patterns.
 *
 * @module orchestration/example-workflows
 */

export {
  runCoreBreakingChange,
  EXAMPLE_PACKAGES,
  EXPECTED_SUCCESS_OUTPUT,
  EXPECTED_FAILURE_OUTPUT
} from './core-breaking-change.mjs';

export {
  runFeatureSpanningPackages,
  runWithSimulatedFailure,
  FEATURE_PACKAGES,
  CHANGED_PACKAGES,
  FEATURE_DESCRIPTION
} from './feature-spanning-packages.mjs';

export {
  runDependencyUpgrade,
  findPackagesUsingDependency,
  analyzeUpgradeCompatibility,
  generateUpgradePlan,
  PACKAGES_WITH_ZOD,
  EXAMPLE_UPGRADE_PLAN
} from './dependency-upgrade.mjs';

/**
 * All available example workflows
 */
export const AVAILABLE_EXAMPLES = {
  'core-breaking-change': {
    name: 'Core Breaking Change',
    description: 'Breaking change to @unrdf/core affecting 10+ downstream packages',
    file: 'core-breaking-change.mjs',
    runner: 'runCoreBreakingChange'
  },
  'feature-spanning-packages': {
    name: 'Feature Spanning Packages',
    description: 'New feature requiring changes to 3 packages atomically',
    file: 'feature-spanning-packages.mjs',
    runner: 'runFeatureSpanningPackages'
  },
  'dependency-upgrade': {
    name: 'Dependency Upgrade',
    description: 'Upgrading shared dependency with version pinning',
    file: 'dependency-upgrade.mjs',
    runner: 'runDependencyUpgrade'
  }
};

/**
 * Run an example workflow by name
 *
 * @param {string} name - Example name
 * @param {Object} [options] - Execution options
 * @returns {Promise<Object>} Workflow result
 */
export async function runExample(name, options = {}) {
  const examples = {
    'core-breaking-change': () => import('./core-breaking-change.mjs').then(m => m.runCoreBreakingChange(options)),
    'feature-spanning-packages': () => import('./feature-spanning-packages.mjs').then(m => m.runFeatureSpanningPackages(options)),
    'dependency-upgrade': () => import('./dependency-upgrade.mjs').then(m => m.runDependencyUpgrade(options))
  };

  const runner = examples[name];
  if (!runner) {
    throw new Error(`Unknown example: ${name}. Available: ${Object.keys(examples).join(', ')}`);
  }

  return runner();
}

/**
 * List all available examples
 *
 * @returns {string[]} Example names
 */
export function listExamples() {
  return Object.keys(AVAILABLE_EXAMPLES);
}

/**
 * Get example description
 *
 * @param {string} name - Example name
 * @returns {Object|undefined} Example description
 */
export function getExampleInfo(name) {
  return AVAILABLE_EXAMPLES[name];
}
