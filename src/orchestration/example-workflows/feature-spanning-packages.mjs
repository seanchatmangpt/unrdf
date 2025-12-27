/**
 * @fileoverview Example: Feature Spanning Multiple Packages
 *
 * **Scenario**: A new feature that requires changes to 3 packages:
 * - @unrdf/core: New API surface
 * - @unrdf/streaming: Feature implementation
 * - @unrdf/cli: Feature exposure to users
 *
 * **Workflow**:
 * 1. Admit all 3 packages atomically
 * 2. Test changes in dependency order
 * 3. Build all 3 packages
 * 4. Validate cross-package integration
 * 5. Atomic commit or full rollback
 *
 * **Key Point**: Either ALL changes land or NONE do.
 *
 * @module orchestration/examples/feature-spanning-packages
 */

import {
  createWorkflowOrchestrator,
  analyzeWorkflowImpact
} from '../index.mjs';

/**
 * Packages involved in the feature
 */
export const FEATURE_PACKAGES = {
  '@unrdf/core': {
    dependencies: [],
    version: '5.1.0',
    path: 'packages/core'
  },
  '@unrdf/types': {
    dependencies: ['@unrdf/core'],
    version: '5.1.0',
    path: 'packages/types'
  },
  '@unrdf/utils': {
    dependencies: ['@unrdf/core', '@unrdf/types'],
    version: '5.1.0',
    path: 'packages/utils'
  },
  '@unrdf/streaming': {
    dependencies: ['@unrdf/core', '@unrdf/utils'],
    version: '5.1.0',
    path: 'packages/streaming'
  },
  '@unrdf/cli': {
    dependencies: ['@unrdf/core', '@unrdf/utils', '@unrdf/streaming'],
    version: '5.1.0',
    path: 'packages/cli'
  }
};

/**
 * Changed packages for this feature
 */
export const CHANGED_PACKAGES = [
  '@unrdf/core',
  '@unrdf/streaming',
  '@unrdf/cli'
];

/**
 * Run feature spanning packages workflow
 *
 * @param {Object} [options] - Execution options
 * @returns {Promise<Object>} Workflow result
 */
export async function runFeatureSpanningPackages(options = {}) {
  console.log('=== Feature Spanning Packages Workflow ===\n');

  // Step 1: Analyze impact
  console.log('Step 1: Analyzing impact...\n');

  const impact = analyzeWorkflowImpact({
    changedPackages: CHANGED_PACKAGES,
    packages: FEATURE_PACKAGES
  });

  console.log('Changed packages:', impact.changedPackages);
  console.log('  - These packages contain the new feature');
  console.log('');
  console.log('Affected packages:', impact.affectedPackages);
  console.log('  - These packages need to be tested/rebuilt');
  console.log('');
  console.log('Execution order:', impact.executionOrder);
  console.log('  - Topologically sorted for safe execution');
  console.log('');
  console.log('Execution levels:', impact.executionLevels);
  console.log('  - Packages at same level can run in parallel');
  console.log('');

  // Step 2: Execute workflow
  console.log('Step 2: Executing atomic workflow...\n');

  const orchestrator = createWorkflowOrchestrator(options);

  const result = await orchestrator.execute({
    changedPackages: CHANGED_PACKAGES,
    packages: FEATURE_PACKAGES,
    options: {
      parallel: true
    }
  });

  // Step 3: Display atomicity guarantee
  console.log('\n=== Atomicity Results ===\n');

  if (result.decision === 'allow') {
    console.log('SUCCESS: All 3 packages were updated atomically.');
    console.log('');
    console.log('Applied changes:');
    for (const pkg of result.changedPackages) {
      console.log(`  - ${pkg}: Updated`);
    }
    console.log('');
    console.log('Rebuilt packages:');
    for (const pkg of result.affectedPackages) {
      if (!result.changedPackages.includes(pkg)) {
        console.log(`  - ${pkg}: Rebuilt (dependency of changed package)`);
      }
    }
  } else {
    console.log('ROLLED BACK: No changes were applied.');
    console.log('');
    console.log('Rollback reason:', result.stages.find(s => s.status === 'failed')?.name || 'Unknown');
    console.log('');
    console.log('State restored to:');
    for (const pkg of result.affectedPackages) {
      console.log(`  - ${pkg}: Original state`);
    }
  }

  return result;
}

/**
 * Simulate partial failure (for testing rollback)
 *
 * @returns {Promise<Object>} Workflow result with forced failure
 */
export async function runWithSimulatedFailure() {
  console.log('=== Simulated Failure Workflow ===\n');
  console.log('This demonstrates rollback behavior when one package fails.\n');

  // Custom executor that fails on @unrdf/streaming
  const failingTestRunner = {
    async run(packageName) {
      if (packageName === '@unrdf/streaming') {
        return {
          passed: false,
          total: 50,
          passed: 48,
          failed: 2,
          errorMessage: 'Simulated test failure in streaming package'
        };
      }
      return {
        passed: true,
        total: 50,
        passed: 50,
        failed: 0
      };
    }
  };

  const orchestrator = createWorkflowOrchestrator({
    testRunner: failingTestRunner
  });

  const result = await orchestrator.execute({
    changedPackages: CHANGED_PACKAGES,
    packages: FEATURE_PACKAGES
  });

  console.log('Result:', result.status);
  console.log('Decision:', result.decision);

  if (result.rollbackResult) {
    console.log('\nRollback details:');
    console.log('  Operations rolled back:', result.rollbackResult.rolledBackOperations);
    console.log('  Checkpoints restored:', result.rollbackResult.restoredCheckpoints);
  }

  return result;
}

/**
 * Feature change description for documentation
 */
export const FEATURE_DESCRIPTION = {
  name: 'Streaming SPARQL Support',
  packages: {
    '@unrdf/core': {
      changes: [
        'Add StreamingQuery interface',
        'Export new QueryStream type',
        'Add streaming configuration options'
      ]
    },
    '@unrdf/streaming': {
      changes: [
        'Implement StreamingQuery interface',
        'Add backpressure handling',
        'Implement result batching'
      ]
    },
    '@unrdf/cli': {
      changes: [
        'Add --stream flag to query command',
        'Add streaming progress indicator',
        'Update help documentation'
      ]
    }
  },
  atomicityRequirement: `
    All three packages must be updated together because:
    1. @unrdf/cli depends on the streaming feature being available
    2. @unrdf/streaming depends on the new core interface
    3. A partial update would leave the system in an inconsistent state
  `
};

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  const args = process.argv.slice(2);
  const simulate = args.includes('--fail');

  const runner = simulate ? runWithSimulatedFailure : runFeatureSpanningPackages;

  runner()
    .then(result => {
      console.log('\n=== Workflow Complete ===');
      process.exit(result.decision === 'allow' ? 0 : 1);
    })
    .catch(error => {
      console.error('Workflow failed:', error);
      process.exit(1);
    });
}
