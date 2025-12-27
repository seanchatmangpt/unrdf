/**
 * @fileoverview Orchestration Module Tests
 *
 * Tests for multi-package atomic change orchestration.
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  DependencyResolver,
  createDependencyResolver,
  StageExecutor,
  StageType,
  StageStatus,
  RollbackManager,
  withRollback,
  ReceiptAggregator,
  WorkflowOrchestrator,
  executeWorkflow,
  analyzeWorkflowImpact,
  createOrchestrationPipeline
} from './index.mjs';

// ============================================================================
// Dependency Resolver Tests
// ============================================================================

describe('DependencyResolver', () => {
  let resolver;

  beforeEach(() => {
    resolver = createDependencyResolver();
  });

  describe('addPackage', () => {
    it('should add a package with dependencies', () => {
      resolver.addPackage('@unrdf/core', { dependencies: [] });
      resolver.addPackage('@unrdf/utils', { dependencies: ['@unrdf/core'] });

      expect(resolver.packages.size).toBe(2);
    });

    it('should chain addPackage calls', () => {
      resolver
        .addPackage('a', { dependencies: [] })
        .addPackage('b', { dependencies: ['a'] });

      expect(resolver.packages.size).toBe(2);
    });
  });

  describe('resolve', () => {
    beforeEach(() => {
      resolver.addPackages({
        '@unrdf/core': { dependencies: [] },
        '@unrdf/types': { dependencies: ['@unrdf/core'] },
        '@unrdf/utils': { dependencies: ['@unrdf/core', '@unrdf/types'] },
        '@unrdf/cli': { dependencies: ['@unrdf/core', '@unrdf/utils'] }
      });
    });

    it('should resolve single package change', () => {
      const result = resolver.resolve(['@unrdf/core']);

      expect(result.success).toBe(true);
      expect(result.closure).toContain('@unrdf/core');
      expect(result.closure).toContain('@unrdf/types');
      expect(result.closure).toContain('@unrdf/utils');
      expect(result.closure).toContain('@unrdf/cli');
    });

    it('should return topological order', () => {
      const result = resolver.resolve(['@unrdf/core']);

      expect(result.order[0]).toBe('@unrdf/core');
      // utils depends on types, so types should come before utils
      const typesIndex = result.order.indexOf('@unrdf/types');
      const utilsIndex = result.order.indexOf('@unrdf/utils');
      expect(typesIndex).toBeLessThan(utilsIndex);
    });

    it('should compute execution levels', () => {
      const result = resolver.resolve(['@unrdf/core']);

      expect(result.levels.length).toBeGreaterThan(0);
      expect(result.levels[0]).toContain('@unrdf/core');
    });

    it('should return error for non-existent package', () => {
      const result = resolver.resolve(['@unrdf/nonexistent']);

      expect(result.success).toBe(false);
      expect(result.error).toContain('not found');
    });
  });

  describe('cycle detection', () => {
    it('should detect cycles', () => {
      resolver.addPackages({
        'a': { dependencies: ['b'] },
        'b': { dependencies: ['c'] },
        'c': { dependencies: ['a'] }
      });

      const cycle = resolver.detectCycle();

      expect(cycle.hasCycle).toBe(true);
      expect(cycle.cyclePackages.length).toBeGreaterThan(0);
    });

    it('should not report cycle for valid graph', () => {
      resolver.addPackages({
        'a': { dependencies: [] },
        'b': { dependencies: ['a'] },
        'c': { dependencies: ['b'] }
      });

      const cycle = resolver.detectCycle();

      expect(cycle.hasCycle).toBe(false);
    });
  });

  describe('impact analysis', () => {
    it('should analyze impact of changes', () => {
      resolver.addPackages({
        'core': { dependencies: [] },
        'utils': { dependencies: ['core'] },
        'cli': { dependencies: ['core', 'utils'] }
      });

      const impact = resolver.analyzeImpact(['core']);

      expect(impact.core).toBeDefined();
      expect(impact.core.directDependents).toContain('utils');
      expect(impact.core.transitiveDependents).toContain('cli');
    });
  });
});

// ============================================================================
// Stage Executor Tests
// ============================================================================

describe('StageExecutor', () => {
  let executor;

  beforeEach(() => {
    executor = new StageExecutor();
  });

  describe('registerStage', () => {
    it('should register a stage', () => {
      executor.registerStage({
        name: 'test-stage',
        type: StageType.TESTING,
        timeout: 5000
      });

      expect(executor.stages.has('test-stage')).toBe(true);
    });
  });

  describe('executeStage', () => {
    it('should execute a stage successfully', async () => {
      executor.registerStage({
        name: 'admission',
        type: StageType.ADMISSION,
        timeout: 5000
      });

      const result = await executor.executeStage('admission', {
        packageName: '@unrdf/core'
      });

      expect(result.status).toBe(StageStatus.COMPLETED);
      expect(result.success).toBe(true);
    });

    it('should handle stage timeout', async () => {
      executor.registerStage({
        name: 'slow-stage',
        type: StageType.TESTING,
        timeout: 10,
        executor: async () => {
          await new Promise(r => setTimeout(r, 100));
          return { passed: true };
        }
      });

      const result = await executor.executeStage('slow-stage', {
        packageName: '@unrdf/core'
      });

      expect(result.status).toBe(StageStatus.FAILED);
      expect(result.error).toContain('timed out');
    });

    it('should throw for unknown stage', async () => {
      await expect(executor.executeStage('unknown', {}))
        .rejects.toThrow('Stage not found');
    });
  });

  describe('createDefault', () => {
    it('should create executor with default stages', () => {
      const defaultExecutor = StageExecutor.createDefault();

      expect(defaultExecutor.stages.has('dependency_analysis')).toBe(true);
      expect(defaultExecutor.stages.has('admission')).toBe(true);
      expect(defaultExecutor.stages.has('testing')).toBe(true);
      expect(defaultExecutor.stages.has('building')).toBe(true);
      expect(defaultExecutor.stages.has('validation')).toBe(true);
      expect(defaultExecutor.stages.has('integration')).toBe(true);
      expect(defaultExecutor.stages.has('commit')).toBe(true);
    });
  });
});

// ============================================================================
// Rollback Manager Tests
// ============================================================================

describe('RollbackManager', () => {
  let manager;

  beforeEach(() => {
    manager = new RollbackManager();
  });

  describe('beginTransaction', () => {
    it('should begin a transaction', () => {
      const txId = manager.beginTransaction(['pkg1', 'pkg2']);

      expect(txId).toBeDefined();
      expect(manager.hasActiveTransaction()).toBe(true);
    });

    it('should throw if transaction already active', () => {
      manager.beginTransaction(['pkg1']);

      expect(() => manager.beginTransaction(['pkg2']))
        .toThrow('Transaction already active');
    });
  });

  describe('checkpoint', () => {
    it('should create checkpoint', async () => {
      manager.beginTransaction(['pkg1']);

      const cpId = await manager.checkpoint('pkg1', 'stage1', { value: 1 });

      expect(cpId).toBeDefined();
      const cp = manager.getCheckpoint(cpId);
      expect(cp.packageName).toBe('pkg1');
      expect(cp.state.value).toBe(1);
    });

    it('should throw if no active transaction', async () => {
      await expect(manager.checkpoint('pkg1', 'stage1', {}))
        .rejects.toThrow('No active transaction');
    });
  });

  describe('commit', () => {
    it('should commit transaction', async () => {
      const txId = manager.beginTransaction(['pkg1']);
      await manager.checkpoint('pkg1', 'stage1', { value: 1 });

      const result = manager.commit(txId);

      expect(result.status).toBe('committed');
      expect(manager.hasActiveTransaction()).toBe(false);
    });
  });

  describe('rollback', () => {
    it('should rollback transaction', async () => {
      const txId = manager.beginTransaction(['pkg1']);
      await manager.checkpoint('pkg1', 'stage1', { value: 1 });
      manager.recordOperation('update', 'pkg1', 'stage1');

      const result = await manager.rollback(txId);

      expect(result.success).toBe(true);
      expect(manager.hasActiveTransaction()).toBe(false);
    });

    it('should restore checkpoints', async () => {
      const txId = manager.beginTransaction(['pkg1']);
      await manager.checkpoint('pkg1', 'stage1', { value: 1 });
      await manager.checkpoint('pkg1', 'stage2', { value: 2 });

      const result = await manager.rollback(txId);

      expect(result.restoredCheckpoints).toBe(2);
    });
  });

  describe('withRollback helper', () => {
    it('should auto-rollback on error', async () => {
      const testManager = new RollbackManager();

      await expect(withRollback(testManager, ['pkg1'], async () => {
        throw new Error('Test failure');
      })).rejects.toThrow('Test failure');

      expect(testManager.hasActiveTransaction()).toBe(false);
    });

    it('should commit on success', async () => {
      const testManager = new RollbackManager();

      const result = await withRollback(testManager, ['pkg1'], async () => {
        return 'success';
      });

      expect(result).toBe('success');
      expect(testManager.hasActiveTransaction()).toBe(false);
    });
  });
});

// ============================================================================
// Receipt Aggregator Tests
// ============================================================================

describe('ReceiptAggregator', () => {
  let aggregator;

  beforeEach(() => {
    aggregator = new ReceiptAggregator();
  });

  describe('addPackageReceipt', () => {
    it('should add package receipt', async () => {
      const receipt = await aggregator.addPackageReceipt(
        'admission',
        '@unrdf/core',
        'allow',
        { duration: 100 }
      );

      expect(receipt.packageName).toBe('@unrdf/core');
      expect(receipt.stage).toBe('admission');
      expect(receipt.decision).toBe('allow');
      expect(receipt.hash).toBeDefined();
    });
  });

  describe('finalizeStage', () => {
    it('should finalize stage with merkle root', async () => {
      await aggregator.addPackageReceipt('testing', 'pkg1', 'allow', { duration: 100 });
      await aggregator.addPackageReceipt('testing', 'pkg2', 'allow', { duration: 150 });

      const stageReceipt = await aggregator.finalizeStage('testing');

      expect(stageReceipt.stage).toBe('testing');
      expect(stageReceipt.decision).toBe('allow');
      expect(stageReceipt.merkleRoot).toBeDefined();
      expect(stageReceipt.stats.total).toBe(2);
      expect(stageReceipt.stats.passed).toBe(2);
    });

    it('should deny stage if any package fails', async () => {
      await aggregator.addPackageReceipt('testing', 'pkg1', 'allow', { duration: 100 });
      await aggregator.addPackageReceipt('testing', 'pkg2', 'deny', { duration: 150 });

      const stageReceipt = await aggregator.finalizeStage('testing');

      expect(stageReceipt.decision).toBe('deny');
      expect(stageReceipt.stats.failed).toBe(1);
    });
  });

  describe('finalizeWorkflow', () => {
    it('should create workflow receipt', async () => {
      await aggregator.addPackageReceipt('admission', 'pkg1', 'allow', { duration: 100 });
      await aggregator.finalizeStage('admission');

      await aggregator.addPackageReceipt('testing', 'pkg1', 'allow', { duration: 200 });
      await aggregator.finalizeStage('testing');

      const workflowReceipt = await aggregator.finalizeWorkflow({
        changedPackages: ['pkg1'],
        affectedPackages: ['pkg1'],
        executionOrder: ['pkg1']
      });

      expect(workflowReceipt.decision).toBe('allow');
      expect(workflowReceipt.merkleRoot).toBeDefined();
      expect(workflowReceipt.stageReceipts.length).toBe(2);
    });
  });

  describe('verify', () => {
    it('should verify valid receipt', async () => {
      await aggregator.addPackageReceipt('admission', 'pkg1', 'allow', { duration: 100 });
      await aggregator.finalizeStage('admission');

      const receipt = await aggregator.finalizeWorkflow({
        changedPackages: ['pkg1'],
        affectedPackages: ['pkg1'],
        executionOrder: ['pkg1']
      });

      const verification = await aggregator.verify(receipt);

      expect(verification.valid).toBe(true);
      expect(verification.errors.length).toBe(0);
    });
  });
});

// ============================================================================
// Workflow Orchestrator Tests
// ============================================================================

describe('WorkflowOrchestrator', () => {
  let orchestrator;

  beforeEach(() => {
    orchestrator = new WorkflowOrchestrator();
  });

  describe('execute', () => {
    it('should execute simple workflow', async () => {
      const result = await orchestrator.execute({
        changedPackages: ['core'],
        packages: {
          'core': { dependencies: [] },
          'utils': { dependencies: ['core'] }
        }
      });

      expect(result.workflowId).toBeDefined();
      expect(result.decision).toBe('allow');
      expect(result.affectedPackages).toContain('core');
      expect(result.affectedPackages).toContain('utils');
    });

    it('should return execution order', async () => {
      const result = await orchestrator.execute({
        changedPackages: ['core'],
        packages: {
          'core': { dependencies: [] },
          'utils': { dependencies: ['core'] },
          'cli': { dependencies: ['utils'] }
        }
      });

      expect(result.executionOrder[0]).toBe('core');
      expect(result.executionOrder.indexOf('utils')).toBeLessThan(
        result.executionOrder.indexOf('cli')
      );
    });

    it('should include receipt', async () => {
      const result = await orchestrator.execute({
        changedPackages: ['core'],
        packages: {
          'core': { dependencies: [] }
        }
      });

      expect(result.receipt).toBeDefined();
      expect(result.receipt.merkleRoot).toBeDefined();
    });
  });

  describe('analyzeImpact', () => {
    it('should analyze impact of changes', () => {
      const impact = orchestrator.analyzeImpact({
        changedPackages: ['core'],
        packages: {
          'core': { dependencies: [] },
          'utils': { dependencies: ['core'] },
          'cli': { dependencies: ['core', 'utils'] }
        }
      });

      expect(impact.success).toBe(true);
      expect(impact.affectedPackages.length).toBe(3);
      expect(impact.recommendation).toBeDefined();
    });

    it('should detect high risk changes', () => {
      // Create many dependent packages
      const packages = { 'core': { dependencies: [] } };
      for (let i = 0; i < 15; i++) {
        packages[`pkg${i}`] = { dependencies: ['core'] };
      }

      const impact = orchestrator.analyzeImpact({
        changedPackages: ['core'],
        packages
      });

      expect(['high', 'critical']).toContain(impact.recommendation.riskLevel);
    });
  });

  describe('getStats', () => {
    it('should return statistics', async () => {
      await orchestrator.execute({
        changedPackages: ['core'],
        packages: { 'core': { dependencies: [] } }
      });

      const stats = orchestrator.getStats();

      expect(stats.totalCompleted).toBe(1);
      expect(stats.totalFailed).toBe(0);
    });
  });
});

// ============================================================================
// Integration Tests
// ============================================================================

describe('Orchestration Pipeline', () => {
  it('should create complete pipeline', () => {
    const pipeline = createOrchestrationPipeline();

    expect(pipeline.resolver).toBeDefined();
    expect(pipeline.executor).toBeDefined();
    expect(pipeline.rollbackManager).toBeDefined();
    expect(pipeline.aggregator).toBeDefined();
    expect(pipeline.orchestrator).toBeDefined();
    expect(pipeline.execute).toBeDefined();
    expect(pipeline.analyze).toBeDefined();
  });

  it('should execute via pipeline', async () => {
    const pipeline = createOrchestrationPipeline();

    const result = await pipeline.execute({
      changedPackages: ['core'],
      packages: {
        'core': { dependencies: [] },
        'utils': { dependencies: ['core'] }
      }
    });

    expect(result.decision).toBe('allow');
  });
});

describe('Convenience Functions', () => {
  describe('executeWorkflow', () => {
    it('should execute workflow', async () => {
      const result = await executeWorkflow({
        changedPackages: ['core'],
        packages: {
          'core': { dependencies: [] }
        }
      });

      expect(result.decision).toBe('allow');
    });
  });

  describe('analyzeWorkflowImpact', () => {
    it('should analyze impact', () => {
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
  });
});

// ============================================================================
// Edge Cases
// ============================================================================

describe('Edge Cases', () => {
  describe('Empty inputs', () => {
    it('should handle empty package list', async () => {
      const orchestrator = new WorkflowOrchestrator();

      await expect(orchestrator.execute({
        changedPackages: [],
        packages: {}
      })).rejects.toThrow();
    });
  });

  describe('Complex dependency graphs', () => {
    it('should handle diamond dependencies', async () => {
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

      expect(result.success).not.toBe(false);
      expect(result.affectedPackages).toContain('top');

      // base must come before left and right
      const baseIdx = result.executionOrder.indexOf('base');
      const leftIdx = result.executionOrder.indexOf('left');
      const rightIdx = result.executionOrder.indexOf('right');
      const topIdx = result.executionOrder.indexOf('top');

      expect(baseIdx).toBeLessThan(leftIdx);
      expect(baseIdx).toBeLessThan(rightIdx);
      expect(leftIdx).toBeLessThan(topIdx);
      expect(rightIdx).toBeLessThan(topIdx);
    });
  });
});
