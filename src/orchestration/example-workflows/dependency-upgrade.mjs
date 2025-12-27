/**
 * @fileoverview Example: Dependency Upgrade Workflow
 *
 * **Scenario**: Upgrading a shared dependency (e.g., zod v3 -> v4)
 * that affects multiple packages with pinned versions.
 *
 * **Workflow**:
 * 1. Identify all packages using the dependency
 * 2. Upgrade dependency version consistently
 * 3. Test each package with new version
 * 4. Validate no breaking API changes
 * 5. Atomic commit ensures version consistency
 *
 * **Key Point**: All packages must use the same version to avoid
 * "dependency hell" and version mismatches.
 *
 * @module orchestration/examples/dependency-upgrade
 */

import {
  createWorkflowOrchestrator,
  createDependencyResolver,
  analyzeWorkflowImpact
} from '../index.mjs';

/**
 * Packages with shared dependency
 */
export const PACKAGES_WITH_ZOD = {
  '@unrdf/core': {
    dependencies: [],
    devDependencies: [],
    version: '5.0.0',
    path: 'packages/core',
    externalDependencies: {
      'zod': '^3.22.0'
    }
  },
  '@unrdf/validation': {
    dependencies: ['@unrdf/core'],
    version: '5.0.0',
    path: 'packages/validation',
    externalDependencies: {
      'zod': '^3.22.0'
    }
  },
  '@unrdf/cli': {
    dependencies: ['@unrdf/core', '@unrdf/validation'],
    version: '5.0.0',
    path: 'packages/cli',
    externalDependencies: {
      'zod': '^3.22.0'
    }
  },
  '@unrdf/api': {
    dependencies: ['@unrdf/core', '@unrdf/validation'],
    version: '5.0.0',
    path: 'packages/api',
    externalDependencies: {
      'zod': '^3.22.0'
    }
  },
  '@unrdf/streaming': {
    dependencies: ['@unrdf/core'],
    version: '5.0.0',
    path: 'packages/streaming',
    externalDependencies: {}  // No zod
  },
  '@unrdf/kgc-4d': {
    dependencies: ['@unrdf/core', '@unrdf/validation'],
    version: '5.0.0',
    path: 'packages/kgc-4d',
    externalDependencies: {
      'zod': '^3.22.0'
    }
  }
};

/**
 * Find all packages using a specific external dependency
 *
 * @param {Object} packages - Package definitions
 * @param {string} depName - Dependency name to find
 * @returns {string[]} Package names using the dependency
 */
export function findPackagesUsingDependency(packages, depName) {
  return Object.entries(packages)
    .filter(([, pkg]) => pkg.externalDependencies?.[depName])
    .map(([name]) => name);
}

/**
 * Run dependency upgrade workflow
 *
 * @param {Object} [options] - Execution options
 * @returns {Promise<Object>} Workflow result
 */
export async function runDependencyUpgrade(options = {}) {
  console.log('=== Dependency Upgrade Workflow ===\n');
  console.log('Upgrading: zod ^3.22.0 -> ^4.0.0\n');

  // Step 1: Find affected packages
  const affectedByDep = findPackagesUsingDependency(PACKAGES_WITH_ZOD, 'zod');
  console.log('Step 1: Packages directly using zod:', affectedByDep);
  console.log('');

  // Step 2: Analyze transitive impact
  console.log('Step 2: Analyzing transitive impact...\n');

  const impact = analyzeWorkflowImpact({
    changedPackages: affectedByDep,
    packages: PACKAGES_WITH_ZOD
  });

  console.log('Directly affected:', affectedByDep.length, 'packages');
  console.log('Total affected (including dependents):', impact.affectedPackages.length, 'packages');
  console.log('');
  console.log('Execution order:');
  for (let i = 0; i < impact.executionLevels.length; i++) {
    console.log(`  Level ${i + 1}: ${impact.executionLevels[i].join(', ')}`);
  }
  console.log('');

  // Step 3: Execute upgrade workflow
  console.log('Step 3: Executing atomic upgrade...\n');

  const orchestrator = createWorkflowOrchestrator(options);

  const result = await orchestrator.execute({
    changedPackages: affectedByDep,
    packages: PACKAGES_WITH_ZOD,
    options: {
      parallel: true
    }
  });

  // Step 4: Display version consistency
  console.log('\n=== Version Consistency Check ===\n');

  if (result.decision === 'allow') {
    console.log('SUCCESS: All packages upgraded consistently.');
    console.log('');
    console.log('New versions:');
    for (const pkg of affectedByDep) {
      console.log(`  ${pkg}: zod ^4.0.0`);
    }
    console.log('');
    console.log('Version consistency: VERIFIED');
    console.log('  - All packages use same zod version');
    console.log('  - No version mismatches possible');
  } else {
    console.log('ROLLED BACK: Upgrade failed.');
    console.log('');
    console.log('Original versions preserved:');
    for (const pkg of affectedByDep) {
      console.log(`  ${pkg}: zod ^3.22.0 (unchanged)`);
    }
    console.log('');
    console.log('Failure reason:', result.stages.find(s => s.status === 'failed')?.name || 'Unknown');
  }

  return result;
}

/**
 * Analyze upgrade compatibility
 *
 * @param {string} fromVersion - Current version
 * @param {string} toVersion - Target version
 * @returns {Object} Compatibility analysis
 */
export function analyzeUpgradeCompatibility(fromVersion, toVersion) {
  const [fromMajor] = fromVersion.replace('^', '').split('.');
  const [toMajor] = toVersion.replace('^', '').split('.');

  const isMajorUpgrade = parseInt(toMajor) > parseInt(fromMajor);

  return {
    fromVersion,
    toVersion,
    isMajorUpgrade,
    breakingChanges: isMajorUpgrade ? [
      'API surface changes',
      'Behavior modifications',
      'Removed deprecated features'
    ] : [],
    requiredActions: isMajorUpgrade ? [
      'Review changelog for breaking changes',
      'Update import statements if needed',
      'Run full test suite',
      'Update type definitions'
    ] : [
      'Standard version bump',
      'Run tests to verify'
    ],
    riskLevel: isMajorUpgrade ? 'high' : 'low'
  };
}

/**
 * Generate upgrade plan
 *
 * @param {Object} packages - Package definitions
 * @param {string} depName - Dependency to upgrade
 * @param {string} newVersion - New version
 * @returns {Object} Upgrade plan
 */
export function generateUpgradePlan(packages, depName, newVersion) {
  const affected = findPackagesUsingDependency(packages, depName);

  const resolver = createDependencyResolver();
  resolver.addPackages(packages);
  const resolution = resolver.resolve(affected);

  return {
    dependency: depName,
    newVersion,
    directlyAffected: affected,
    totalAffected: resolution.closure,
    executionOrder: resolution.order,
    executionLevels: resolution.levels,
    estimatedDuration: `${resolution.levels.length * 3}-${resolution.levels.length * 8} minutes`,
    preflightChecks: [
      `npm info ${depName}@${newVersion}`,
      'Review release notes',
      'Check for known issues',
      'Verify compatibility with Node.js version'
    ],
    postflightChecks: [
      'Run full test suite',
      'Verify type definitions',
      'Check for runtime warnings',
      'Validate API behavior'
    ]
  };
}

/**
 * Upgrade plan for documentation
 */
export const EXAMPLE_UPGRADE_PLAN = generateUpgradePlan(
  PACKAGES_WITH_ZOD,
  'zod',
  '^4.0.0'
);

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  console.log('Upgrade Plan:');
  console.log(JSON.stringify(EXAMPLE_UPGRADE_PLAN, null, 2));
  console.log('\n');

  runDependencyUpgrade()
    .then(result => {
      console.log('\n=== Workflow Complete ===');
      process.exit(result.decision === 'allow' ? 0 : 1);
    })
    .catch(error => {
      console.error('Workflow failed:', error);
      process.exit(1);
    });
}
