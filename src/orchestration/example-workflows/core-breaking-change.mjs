/**
 * @fileoverview Example: Core Breaking Change Workflow
 *
 * **Scenario**: A breaking change to @unrdf/core that affects 10+ downstream packages.
 *
 * **Workflow**:
 * 1. Admit core change through GOS
 * 2. Test all downstream packages against new core
 * 3. Build all affected packages
 * 4. Validate production readiness
 * 5. Run cross-package integration tests
 * 6. Atomic commit or full rollback
 *
 * **Expected Output**:
 * - If all pass: Workflow receipt showing "allow" for all packages
 * - If any fail: Full rollback with denial receipt
 *
 * @module orchestration/examples/core-breaking-change
 */

import {
  createWorkflowOrchestrator,
  analyzeWorkflowImpact
} from '../index.mjs';

/**
 * Example monorepo package structure
 */
export const EXAMPLE_PACKAGES = {
  '@unrdf/core': {
    dependencies: [],
    version: '5.0.0',
    path: 'packages/core'
  },
  '@unrdf/types': {
    dependencies: ['@unrdf/core'],
    version: '5.0.0',
    path: 'packages/types'
  },
  '@unrdf/utils': {
    dependencies: ['@unrdf/core', '@unrdf/types'],
    version: '5.0.0',
    path: 'packages/utils'
  },
  '@unrdf/validation': {
    dependencies: ['@unrdf/core', '@unrdf/types'],
    version: '5.0.0',
    path: 'packages/validation'
  },
  '@unrdf/oxigraph': {
    dependencies: ['@unrdf/core', '@unrdf/types'],
    version: '5.0.0',
    path: 'packages/oxigraph'
  },
  '@unrdf/streaming': {
    dependencies: ['@unrdf/core', '@unrdf/utils'],
    version: '5.0.0',
    path: 'packages/streaming'
  },
  '@unrdf/cli': {
    dependencies: ['@unrdf/core', '@unrdf/utils', '@unrdf/validation'],
    version: '5.0.0',
    path: 'packages/cli'
  },
  '@unrdf/kgc-4d': {
    dependencies: ['@unrdf/core', '@unrdf/oxigraph', '@unrdf/streaming'],
    version: '5.0.0',
    path: 'packages/kgc-4d'
  },
  '@unrdf/api': {
    dependencies: ['@unrdf/core', '@unrdf/validation', '@unrdf/kgc-4d'],
    version: '5.0.0',
    path: 'packages/api'
  },
  '@unrdf/web': {
    dependencies: ['@unrdf/api', '@unrdf/streaming'],
    version: '5.0.0',
    path: 'packages/web'
  },
  '@unrdf/integration-tests': {
    dependencies: [
      '@unrdf/core',
      '@unrdf/cli',
      '@unrdf/api',
      '@unrdf/web'
    ],
    devDependencies: ['@unrdf/utils'],
    version: '5.0.0',
    path: 'packages/integration-tests'
  }
};

/**
 * Run core breaking change workflow
 *
 * @param {Object} [options] - Execution options
 * @returns {Promise<Object>} Workflow result
 */
export async function runCoreBreakingChange(options = {}) {
  console.log('=== Core Breaking Change Workflow ===\n');

  // Step 1: Analyze impact
  console.log('Step 1: Analyzing impact...\n');

  const impact = analyzeWorkflowImpact({
    changedPackages: ['@unrdf/core'],
    packages: EXAMPLE_PACKAGES
  });

  console.log('Changed packages:', impact.changedPackages);
  console.log('Affected packages:', impact.affectedPackages);
  console.log('Execution order:', impact.executionOrder);
  console.log('Risk level:', impact.recommendation.riskLevel);
  console.log('Recommendation:', impact.recommendation.recommendation);
  console.log('');

  // Step 2: Execute workflow
  console.log('Step 2: Executing workflow...\n');

  const orchestrator = createWorkflowOrchestrator(options);

  const result = await orchestrator.execute({
    changedPackages: ['@unrdf/core'],
    packages: EXAMPLE_PACKAGES,
    options: {
      parallel: true
    }
  });

  // Step 3: Display results
  console.log('\n=== Workflow Results ===\n');
  console.log('Workflow ID:', result.workflowId);
  console.log('Status:', result.status);
  console.log('Decision:', result.decision);
  console.log('Duration:', result.duration, 'ms');
  console.log('');

  // Stage summary
  console.log('Stage Summary:');
  for (const stage of result.stages) {
    const status = stage.status === 'completed' ? 'PASS' : 'FAIL';
    console.log(`  ${stage.name}: ${status} (${stage.duration}ms)`);
    for (const pkg of stage.packages) {
      const pkgStatus = pkg.status === 'completed' ? 'OK' : 'FAILED';
      console.log(`    - ${pkg.name}: ${pkgStatus}`);
    }
  }

  // Receipt summary
  if (result.receipt) {
    console.log('\nReceipt Summary:');
    console.log('  Merkle Root:', result.receipt.merkleRoot);
    console.log('  Stages:', result.receipt.stageReceipts.length);

    if (result.receipt.rollbackTrail?.length > 0) {
      console.log('  Rollback Trail:', result.receipt.rollbackTrail.length, 'entries');
    }
  }

  return result;
}

/**
 * Expected successful output for reference
 */
export const EXPECTED_SUCCESS_OUTPUT = {
  workflowId: 'uuid-here',
  status: 'completed',
  decision: 'allow',
  changedPackages: ['@unrdf/core'],
  affectedPackages: [
    '@unrdf/core',
    '@unrdf/types',
    '@unrdf/utils',
    '@unrdf/validation',
    '@unrdf/oxigraph',
    '@unrdf/streaming',
    '@unrdf/cli',
    '@unrdf/kgc-4d',
    '@unrdf/api',
    '@unrdf/web',
    '@unrdf/integration-tests'
  ],
  stages: [
    { name: 'admission', status: 'completed' },
    { name: 'testing', status: 'completed' },
    { name: 'building', status: 'completed' },
    { name: 'validation', status: 'completed' },
    { name: 'integration', status: 'completed' },
    { name: 'commit', status: 'completed' }
  ],
  receipt: {
    decision: 'allow',
    merkleRoot: 'computed-hash'
  }
};

/**
 * Expected failure output (for testing rollback)
 */
export const EXPECTED_FAILURE_OUTPUT = {
  workflowId: 'uuid-here',
  status: 'rolled_back',
  decision: 'deny',
  rollbackResult: {
    success: true,
    rolledBackOperations: 5,
    restoredCheckpoints: 5
  },
  receipt: {
    decision: 'deny',
    rollbackTrail: [
      { stage: 'testing', checkpoint: 'cp-id' }
    ]
  }
};

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runCoreBreakingChange()
    .then(result => {
      console.log('\n=== Workflow Complete ===');
      process.exit(result.decision === 'allow' ? 0 : 1);
    })
    .catch(error => {
      console.error('Workflow failed:', error);
      process.exit(1);
    });
}
