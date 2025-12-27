#!/usr/bin/env node
/**
 * @fileoverview Validation Script for Orchestration Module
 *
 * Validates that all orchestration components work correctly.
 */

import {
  DependencyResolver,
  StageExecutor,
  StageType,
  RollbackManager,
  ReceiptAggregator,
  WorkflowOrchestrator,
  executeWorkflow,
  analyzeWorkflowImpact,
  createOrchestrationPipeline
} from './index.mjs';

const tests = [];
let passed = 0;
let failed = 0;

function test(name, fn) {
  tests.push({ name, fn });
}

function expect(value) {
  return {
    toBe(expected) {
      if (value !== expected) {
        throw new Error(`Expected ${expected}, got ${value}`);
      }
    },
    toBeDefined() {
      if (value === undefined) {
        throw new Error('Expected value to be defined');
      }
    },
    toContain(item) {
      if (!value.includes(item)) {
        throw new Error(`Expected ${JSON.stringify(value)} to contain ${item}`);
      }
    },
    toBeGreaterThan(num) {
      if (value <= num) {
        throw new Error(`Expected ${value} > ${num}`);
      }
    },
    toBeLessThan(num) {
      if (value >= num) {
        throw new Error(`Expected ${value} < ${num}`);
      }
    }
  };
}

// ========== Tests ==========

test('DependencyResolver: adds packages', () => {
  const resolver = new DependencyResolver();
  resolver.addPackage('@unrdf/core', { dependencies: [] });
  resolver.addPackage('@unrdf/utils', { dependencies: ['@unrdf/core'] });
  expect(resolver.packages.size).toBe(2);
});

test('DependencyResolver: resolves dependencies', () => {
  const resolver = new DependencyResolver();
  resolver.addPackages({
    '@unrdf/core': { dependencies: [] },
    '@unrdf/types': { dependencies: ['@unrdf/core'] },
    '@unrdf/utils': { dependencies: ['@unrdf/core', '@unrdf/types'] }
  });

  const result = resolver.resolve(['@unrdf/core']);

  expect(result.success).toBe(true);
  expect(result.closure.length).toBe(3);
  expect(result.order[0]).toBe('@unrdf/core');
});

test('DependencyResolver: detects cycles', () => {
  const resolver = new DependencyResolver();
  resolver.addPackages({
    'a': { dependencies: ['b'] },
    'b': { dependencies: ['c'] },
    'c': { dependencies: ['a'] }
  });

  const cycle = resolver.detectCycle();
  expect(cycle.hasCycle).toBe(true);
});

test('DependencyResolver: computes levels', () => {
  const resolver = new DependencyResolver();
  resolver.addPackages({
    'core': { dependencies: [] },
    'utils': { dependencies: ['core'] },
    'cli': { dependencies: ['utils'] }
  });

  const result = resolver.resolve(['core']);
  expect(result.levels.length).toBe(3);
});

test('StageExecutor: creates default stages', () => {
  const executor = StageExecutor.createDefault();
  expect(executor.stages.has('admission')).toBe(true);
  expect(executor.stages.has('testing')).toBe(true);
  expect(executor.stages.has('building')).toBe(true);
});

test('StageExecutor: executes stage', async () => {
  const executor = new StageExecutor();
  executor.registerStage({
    name: 'test-admission',
    type: StageType.ADMISSION,
    timeout: 5000
  });

  const result = await executor.executeStage('test-admission', {
    packageName: '@unrdf/core'
  });

  expect(result.success).toBe(true);
});

test('RollbackManager: begins transaction', () => {
  const manager = new RollbackManager();
  const txId = manager.beginTransaction(['pkg1']);
  expect(txId).toBeDefined();
  expect(manager.hasActiveTransaction()).toBe(true);
});

test('RollbackManager: creates checkpoints', async () => {
  const manager = new RollbackManager();
  manager.beginTransaction(['pkg1']);

  const cpId = await manager.checkpoint('pkg1', 'stage1', { value: 1 });
  expect(cpId).toBeDefined();

  const cp = manager.getCheckpoint(cpId);
  expect(cp.state.value).toBe(1);
});

test('RollbackManager: commits transaction', async () => {
  const manager = new RollbackManager();
  const txId = manager.beginTransaction(['pkg1']);
  await manager.checkpoint('pkg1', 'stage1', { value: 1 });

  const result = manager.commit(txId);
  expect(result.status).toBe('committed');
});

test('RollbackManager: rollback transaction', async () => {
  const manager = new RollbackManager();
  const txId = manager.beginTransaction(['pkg1']);
  await manager.checkpoint('pkg1', 'stage1', { value: 1 });

  const result = await manager.rollback(txId);
  expect(result.success).toBe(true);
  expect(result.restoredCheckpoints).toBe(1);
});

test('ReceiptAggregator: adds package receipt', async () => {
  const aggregator = new ReceiptAggregator();
  const receipt = await aggregator.addPackageReceipt(
    'admission', '@unrdf/core', 'allow', { duration: 100 }
  );

  expect(receipt.hash).toBeDefined();
  expect(receipt.decision).toBe('allow');
});

test('ReceiptAggregator: finalizes stage', async () => {
  const aggregator = new ReceiptAggregator();
  await aggregator.addPackageReceipt('testing', 'pkg1', 'allow', { duration: 100 });
  await aggregator.addPackageReceipt('testing', 'pkg2', 'allow', { duration: 150 });

  const stageReceipt = await aggregator.finalizeStage('testing');

  expect(stageReceipt.merkleRoot).toBeDefined();
  expect(stageReceipt.stats.passed).toBe(2);
});

test('ReceiptAggregator: finalizes workflow', async () => {
  const aggregator = new ReceiptAggregator();
  await aggregator.addPackageReceipt('admission', 'pkg1', 'allow', { duration: 100 });
  await aggregator.finalizeStage('admission');

  const receipt = await aggregator.finalizeWorkflow({
    changedPackages: ['pkg1'],
    affectedPackages: ['pkg1'],
    executionOrder: ['pkg1']
  });

  expect(receipt.merkleRoot).toBeDefined();
  expect(receipt.decision).toBe('allow');
});

test('WorkflowOrchestrator: executes workflow', async () => {
  const orchestrator = new WorkflowOrchestrator();
  const result = await orchestrator.execute({
    changedPackages: ['core'],
    packages: {
      'core': { dependencies: [] },
      'utils': { dependencies: ['core'] }
    }
  });

  expect(result.decision).toBe('allow');
  expect(result.affectedPackages).toContain('core');
  expect(result.affectedPackages).toContain('utils');
});

test('WorkflowOrchestrator: returns execution order', async () => {
  const orchestrator = new WorkflowOrchestrator();
  const result = await orchestrator.execute({
    changedPackages: ['core'],
    packages: {
      'core': { dependencies: [] },
      'utils': { dependencies: ['core'] },
      'cli': { dependencies: ['utils'] }
    }
  });

  expect(result.executionOrder[0]).toBe('core');
  const utilsIdx = result.executionOrder.indexOf('utils');
  const cliIdx = result.executionOrder.indexOf('cli');
  expect(utilsIdx).toBeLessThan(cliIdx);
});

test('analyzeWorkflowImpact: analyzes impact', () => {
  const impact = analyzeWorkflowImpact({
    changedPackages: ['core'],
    packages: {
      'core': { dependencies: [] },
      'utils': { dependencies: ['core'] }
    }
  });

  expect(impact.success).toBe(true);
  expect(impact.affectedPackages.length).toBe(2);
});

test('createOrchestrationPipeline: creates pipeline', () => {
  const pipeline = createOrchestrationPipeline();
  expect(pipeline.resolver).toBeDefined();
  expect(pipeline.executor).toBeDefined();
  expect(pipeline.rollbackManager).toBeDefined();
  expect(pipeline.aggregator).toBeDefined();
  expect(pipeline.orchestrator).toBeDefined();
});

test('executeWorkflow: convenience function works', async () => {
  const result = await executeWorkflow({
    changedPackages: ['core'],
    packages: { 'core': { dependencies: [] } }
  });

  expect(result.decision).toBe('allow');
});

test('Diamond dependency: handles correctly', async () => {
  const orchestrator = new WorkflowOrchestrator();
  const result = await orchestrator.execute({
    changedPackages: ['base'],
    packages: {
      'base': { dependencies: [] },
      'left': { dependencies: ['base'] },
      'right': { dependencies: ['base'] },
      'top': { dependencies: ['left', 'right'] }
    }
  });

  expect(result.affectedPackages).toContain('top');

  const baseIdx = result.executionOrder.indexOf('base');
  const topIdx = result.executionOrder.indexOf('top');
  expect(baseIdx).toBeLessThan(topIdx);
});

// ========== Run Tests ==========

async function run() {
  console.log('=== Orchestration Module Validation ===\n');

  for (const { name, fn } of tests) {
    try {
      await fn();
      passed++;
      console.log(`PASS: ${name}`);
    } catch (error) {
      failed++;
      console.log(`FAIL: ${name}`);
      console.log(`      ${error.message}`);
    }
  }

  console.log(`\n=== Results ===`);
  console.log(`Passed: ${passed}/${tests.length}`);
  console.log(`Failed: ${failed}/${tests.length}`);

  if (failed === 0) {
    console.log('\nAll tests passed!');
    process.exit(0);
  } else {
    console.log('\nSome tests failed.');
    process.exit(1);
  }
}

run().catch(error => {
  console.error('Validation failed:', error);
  process.exit(1);
});
