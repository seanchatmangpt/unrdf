/**
 * @fileoverview Comprehensive Test Suite for Resilience Module
 *
 * **Tests**:
 * - Circuit breakers (per-agent isolation, state transitions)
 * - Retry strategy (exponential backoff, jitter, transient detection)
 * - Graceful degradation (QoS levels, batch tolerance)
 * - Error recovery (checkpoints, rollback, retry)
 * - Receipt chain repair (detection, repair strategies)
 * - Structured error logging (categorization, severity)
 *
 * @module resilience/resilience.test
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { AgentCircuitBreakerRegistry, createAgentCircuitBreaker } from './agent-circuit-breaker.mjs';
import { RetryStrategy, RetryExhaustedError, ErrorType } from './retry-strategy.mjs';
import {
  GracefulDegradationManager,
  QoSLevel,
  FallbackStrategies,
} from './graceful-degradation.mjs';
import { ErrorRecoveryManager, RecoveryStrategy } from './error-recovery.mjs';
import {
  ReceiptChainRepairManager,
  IssueType,
  RepairStrategy,
} from './receipt-chain-repair.mjs';
import {
  StructuredErrorLogger,
  ErrorCategory,
  ErrorSeverity,
} from './structured-error-logger.mjs';

describe('AgentCircuitBreakerRegistry', () => {
  let registry;

  beforeEach(() => {
    registry = new AgentCircuitBreakerRegistry();
  });

  it('should create circuit breaker for agent', () => {
    const breaker = registry.getOrCreateForAgent('agent-001', 'coder');

    expect(breaker).toBeDefined();
    expect(breaker.isHealthy()).toBe(true);
  });

  it('should isolate failures per agent', async () => {
    const breaker1 = registry.getOrCreateForAgent('agent-001', 'coder', {
      failureThreshold: 2,
    });
    const breaker2 = registry.getOrCreateForAgent('agent-002', 'tester');

    // Fail agent-001 circuit
    try {
      await breaker1.execute(async () => {
        throw new Error('Failed');
      });
    } catch {}

    try {
      await breaker1.execute(async () => {
        throw new Error('Failed');
      });
    } catch {}

    // agent-001 circuit should be open, agent-002 still closed
    expect(breaker1.isOpen()).toBe(true);
    expect(breaker2.isHealthy()).toBe(true);
  });

  it('should execute with protection and fallback', async () => {
    const result = await registry.executeWithProtection(
      'agent-001',
      'coder',
      async () => ({ success: true }),
      { fallbackStrategy: 'skip' }
    );

    expect(result).toEqual({ success: true });
  });

  it('should get agent health summary', () => {
    registry.getOrCreateForAgent('agent-001', 'coder');
    registry.getOrCreateForAgent('agent-002', 'tester');

    const summary = registry.getAgentHealthSummary();

    expect(summary.totalAgents).toBe(2);
    expect(summary.healthy).toBe(2);
    expect(summary.agents['agent-001']).toBeDefined();
    expect(summary.agents['agent-001'].agentType).toBe('coder');
  });
});

describe('RetryStrategy', () => {
  let retry;

  beforeEach(() => {
    retry = new RetryStrategy({ maxAttempts: 3, initialDelayMs: 10 });
  });

  it('should succeed on first attempt', async () => {
    const result = await retry.execute(async () => 'success');

    expect(result).toBe('success');
    expect(retry.getMetrics().totalAttempts).toBe(1);
  });

  it('should retry on transient failures', async () => {
    let attempts = 0;

    const result = await retry.execute(async () => {
      attempts++;
      if (attempts < 3) {
        throw new Error('ETIMEDOUT: Connection timeout');
      }
      return 'success';
    });

    expect(result).toBe('success');
    expect(attempts).toBe(3);
  });

  it('should not retry permanent failures', async () => {
    await expect(
      retry.execute(async () => {
        throw new Error('Validation failed: bad input');
      })
    ).rejects.toThrow('Validation failed');

    expect(retry.getMetrics().permanentFailures).toBe(1);
  });

  it('should throw RetryExhaustedError after max attempts', async () => {
    await expect(
      retry.execute(async () => {
        throw new Error('ETIMEDOUT');
      })
    ).rejects.toThrow(RetryExhaustedError);

    expect(retry.getMetrics().failedRetries).toBe(1);
  });

  it('should calculate exponential backoff correctly', () => {
    const delay1 = retry._calculateBackoff(1);
    const delay2 = retry._calculateBackoff(2);
    const delay3 = retry._calculateBackoff(3);

    // Should increase exponentially (with jitter)
    expect(delay2).toBeGreaterThan(delay1);
    expect(delay3).toBeGreaterThan(delay2);
  });

  it('should classify errors correctly', () => {
    expect(retry._classifyError(new Error('ETIMEDOUT'))).toBe(ErrorType.TRANSIENT);
    expect(retry._classifyError(new Error('404 Not Found'))).toBe(ErrorType.PERMANENT);
    expect(retry._classifyError(new Error('Unknown'))).toBe(ErrorType.UNKNOWN);
  });
});

describe('GracefulDegradationManager', () => {
  let manager;

  beforeEach(() => {
    manager = new GracefulDegradationManager();
  });

  it('should return primary result when successful', async () => {
    const result = await manager.executeWithDegradation(
      async () => ({ data: 'primary' }),
      [FallbackStrategies.skip()]
    );

    expect(result.result).toEqual({ data: 'primary' });
    expect(result.qosLevel).toBe(QoSLevel.PERFECT);
    expect(result.degraded).toBe(false);
  });

  it('should use fallback when primary fails', async () => {
    const result = await manager.executeWithDegradation(
      async () => {
        throw new Error('Primary failed');
      },
      [FallbackStrategies.defaultValue({ data: 'fallback' })]
    );

    expect(result.result).toEqual({ data: 'fallback' });
    expect(result.qosLevel).toBe(QoSLevel.MINIMAL);
    expect(result.degraded).toBe(true);
  });

  it('should try fallbacks in order', async () => {
    const fallbacks = [
      {
        name: 'first',
        qosLevel: QoSLevel.HIGH,
        fallbackFn: async () => {
          throw new Error('First failed');
        },
      },
      {
        name: 'second',
        qosLevel: QoSLevel.MEDIUM,
        fallbackFn: async () => ({ data: 'second' }),
      },
    ];

    const result = await manager.executeWithDegradation(
      async () => {
        throw new Error('Primary failed');
      },
      fallbacks
    );

    expect(result.strategy).toBe('second');
    expect(result.qosLevel).toBe(QoSLevel.MEDIUM);
  });

  it('should handle batch operations with partial failures', async () => {
    const operations = [
      async () => 'success1',
      async () => {
        throw new Error('failed');
      },
      async () => 'success2',
    ];

    const result = await manager.executeBatchWithTolerance(operations, {
      continueOnFailure: true,
      minSuccessCount: 2,
    });

    expect(result.successCount).toBe(2);
    expect(result.failureCount).toBe(1);
    expect(result.degraded).toBe(true);
  });
});

describe('ErrorRecoveryManager', () => {
  let recovery;

  beforeEach(() => {
    recovery = new ErrorRecoveryManager();
  });

  it('should create checkpoint', () => {
    const checkpoint = recovery.createCheckpoint('epoch_001', {
      data: 'test',
      count: 42,
    });

    expect(checkpoint.id).toBeDefined();
    expect(checkpoint.epoch).toBe('epoch_001');
    expect(checkpoint.state).toEqual({ data: 'test', count: 42 });
  });

  it('should recover from checkpoint with rollback', async () => {
    const checkpoint = recovery.createCheckpoint('epoch_001', { value: 100 });

    const result = await recovery.recoverFromCheckpoint(
      checkpoint.id,
      RecoveryStrategy.ROLLBACK
    );

    expect(result.recoveredState).toEqual({ value: 100 });
    expect(result.strategy).toBe(RecoveryStrategy.ROLLBACK);
  });

  it('should execute with automatic checkpoint and recovery', async () => {
    let attempts = 0;

    const result = await recovery.executeWithRecovery(
      'epoch_001',
      { count: 0 },
      async (state) => {
        attempts++;
        if (attempts < 2) {
          throw new Error('Failed');
        }
        return { ...state, success: true };
      },
      { retryOnFailure: true, maxRetries: 3 }
    );

    expect(result.result.success).toBe(true);
    expect(result.recovered).toBe(true);
    expect(result.attempts).toBe(2);
  });

  it('should get latest checkpoint for epoch', () => {
    recovery.createCheckpoint('epoch_001', { v: 1 });
    recovery.createCheckpoint('epoch_001', { v: 2 });
    recovery.createCheckpoint('epoch_002', { v: 3 });

    const latest = recovery.getLatestCheckpointForEpoch('epoch_001');

    expect(latest.state.v).toBe(2);
  });

  it('should cleanup old checkpoints', () => {
    const cfg = { maxCheckpoints: 5 };
    const rec = new ErrorRecoveryManager(cfg);

    for (let i = 0; i < 10; i++) {
      rec.createCheckpoint(`epoch_${i}`, { i });
    }

    expect(rec.checkpoints.size).toBeLessThanOrEqual(5);
  });
});

describe('ReceiptChainRepairManager', () => {
  let repair;

  beforeEach(() => {
    repair = new ReceiptChainRepairManager();
  });

  it('should detect broken hash linkages', async () => {
    const chain = {
      receipts: [
        {
          epoch: 'epoch_001',
          beforeHash: null,
          receiptHash: 'hash1',
          verify: async () => true,
        },
        {
          epoch: 'epoch_002',
          beforeHash: 'wrong_hash', // Should be 'hash1'
          receiptHash: 'hash2',
          verify: async () => true,
        },
      ],
      getAll: function () {
        return this.receipts;
      },
      get length() {
        return this.receipts.length;
      },
    };

    const issues = await repair.detectIssues(chain);

    expect(issues.length).toBeGreaterThan(0);
    expect(issues[0].type).toBe(IssueType.BROKEN_LINK);
  });

  it('should detect epoch regression', async () => {
    const chain = {
      receipts: [
        {
          epoch: 'epoch_002',
          beforeHash: null,
          receiptHash: 'hash1',
          verify: async () => true,
        },
        {
          epoch: 'epoch_001', // Should be > epoch_002
          beforeHash: 'hash1',
          receiptHash: 'hash2',
          verify: async () => true,
        },
      ],
      getAll: function () {
        return this.receipts;
      },
      get length() {
        return this.receipts.length;
      },
    };

    const issues = await repair.detectIssues(chain);

    expect(issues.some((i) => i.type === IssueType.EPOCH_REGRESSION)).toBe(true);
  });

  it('should repair chain with re-hash strategy', async () => {
    const chain = {
      receipts: [
        {
          epoch: 'epoch_001',
          beforeHash: null,
          receiptHash: 'hash1',
          verify: async () => true,
        },
        {
          epoch: 'epoch_002',
          beforeHash: 'wrong_hash',
          receiptHash: 'hash2',
          verify: async () => true,
        },
      ],
      getAll: function () {
        return this.receipts;
      },
      get length() {
        return this.receipts.length;
      },
    };

    const result = await repair.repairChain(chain, RepairStrategy.RE_HASH);

    expect(result.repaired).toBe(true);
    expect(result.issuesFixed).toBeGreaterThan(0);
    expect(chain.receipts[1].beforeHash).toBe('hash1');
  });

  it('should not repair valid chain', async () => {
    const chain = {
      receipts: [
        {
          epoch: 'epoch_001',
          beforeHash: null,
          receiptHash: 'hash1',
          verify: async () => true,
        },
        {
          epoch: 'epoch_002',
          beforeHash: 'hash1',
          receiptHash: 'hash2',
          verify: async () => true,
        },
      ],
      getAll: function () {
        return this.receipts;
      },
      get length() {
        return this.receipts.length;
      },
    };

    const result = await repair.repairChain(chain);

    expect(result.repaired).toBe(false);
    expect(result.issuesFound).toBe(0);
  });
});

describe('StructuredErrorLogger', () => {
  let logger;

  beforeEach(() => {
    logger = new StructuredErrorLogger({ enableConsole: false });
  });

  it('should log error with metadata', () => {
    const error = new Error('Test error');
    const logged = logger.logError(error, {
      category: ErrorCategory.TRANSIENT,
      severity: ErrorSeverity.WARN,
      context: { test: true },
    });

    expect(logged.id).toBeDefined();
    expect(logged.category).toBe(ErrorCategory.TRANSIENT);
    expect(logged.severity).toBe(ErrorSeverity.WARN);
    expect(logged.message).toBe('Test error');
    expect(logged.context.test).toBe(true);
  });

  it('should categorize errors automatically', () => {
    const timeoutError = new Error('ETIMEDOUT: Connection timeout');
    const logged1 = logger.logError(timeoutError);
    expect(logged1.category).toBe(ErrorCategory.TRANSIENT);

    const validationError = new Error('Validation failed');
    const logged2 = logger.logError(validationError);
    expect(logged2.category).toBe(ErrorCategory.PERMANENT);

    const authError = new Error('401 Unauthorized');
    const logged3 = logger.logError(authError);
    expect(logged3.category).toBe(ErrorCategory.SECURITY);
  });

  it('should track error metrics', () => {
    logger.logError(new Error('Error 1'), {
      category: ErrorCategory.TRANSIENT,
      severity: ErrorSeverity.WARN,
    });
    logger.logError(new Error('Error 2'), {
      category: ErrorCategory.PERMANENT,
      severity: ErrorSeverity.ERROR,
    });

    const metrics = logger.getMetrics();

    expect(metrics.totalErrors).toBe(2);
    expect(metrics.byCategory.transient).toBe(1);
    expect(metrics.byCategory.permanent).toBe(1);
    expect(metrics.bySeverity.warn).toBe(1);
    expect(metrics.bySeverity.error).toBe(1);
  });

  it('should get errors by category', () => {
    logger.logError(new Error('Transient 1'), {
      category: ErrorCategory.TRANSIENT,
    });
    logger.logError(new Error('Permanent 1'), {
      category: ErrorCategory.PERMANENT,
    });
    logger.logError(new Error('Transient 2'), {
      category: ErrorCategory.TRANSIENT,
    });

    const transientErrors = logger.getErrorsByCategory(ErrorCategory.TRANSIENT);

    expect(transientErrors.length).toBe(2);
    expect(transientErrors[0].message).toContain('Transient');
  });

  it('should get errors by severity', () => {
    logger.logError(new Error('Warning'), { severity: ErrorSeverity.WARN });
    logger.logError(new Error('Error'), { severity: ErrorSeverity.ERROR });
    logger.logError(new Error('Fatal'), { severity: ErrorSeverity.FATAL });

    const fatalErrors = logger.getErrorsBySeverity(ErrorSeverity.FATAL);

    expect(fatalErrors.length).toBe(1);
    expect(fatalErrors[0].message).toBe('Fatal');
  });

  it('should cleanup old logs when exceeding max size', () => {
    const smallLogger = new StructuredErrorLogger({
      maxLogSize: 5,
      enableConsole: false,
    });

    for (let i = 0; i < 10; i++) {
      smallLogger.logError(new Error(`Error ${i}`));
    }

    expect(smallLogger.errorLog.length).toBeLessThanOrEqual(5);
  });
});

describe('Integration Tests', () => {
  it('should combine circuit breaker with retry', async () => {
    const registry = new AgentCircuitBreakerRegistry();
    const retry = new RetryStrategy({ maxAttempts: 2, initialDelayMs: 10 });

    let attempts = 0;
    const result = await retry.execute(async () => {
      attempts++;
      return await registry.executeWithProtection(
        'agent-001',
        'coder',
        async () => {
          if (attempts < 2) throw new Error('ETIMEDOUT');
          return { success: true };
        }
      );
    });

    expect(result.success).toBe(true);
    expect(attempts).toBe(2);
  });

  it('should combine degradation with error logging', async () => {
    const degradation = new GracefulDegradationManager();
    const logger = new StructuredErrorLogger({ enableConsole: false });

    try {
      await degradation.executeWithDegradation(
        async () => {
          throw new Error('Primary failed');
        },
        [] // No fallbacks
      );
    } catch (error) {
      const logged = logger.logError(error, {
        category: ErrorCategory.TRANSIENT,
      });

      expect(logged.category).toBe(ErrorCategory.TRANSIENT);
    }
  });
});
