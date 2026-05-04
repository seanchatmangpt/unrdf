/**
 * Tests for Saga Pattern - Distributed Transactions
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  SagaOrchestrator,
  createSagaStep,
  createSagaBuilder,
} from '../src/saga-orchestrator.mjs';

describe('Saga Pattern - Distributed Transactions', () => {
  describe('SagaOrchestrator', () => {
    it('should execute saga steps sequentially', async () => {
      const executionOrder = [];

      const step1 = createSagaStep(
        'step1',
        'first',
        async (ctx) => {
          executionOrder.push('step1-execute');
          return { ...ctx, step1: 'done' };
        },
        async () => {
          executionOrder.push('step1-compensate');
        }
      );

      const step2 = createSagaStep(
        'step2',
        'second',
        async (ctx) => {
          executionOrder.push('step2-execute');
          return { ...ctx, step2: 'done' };
        },
        async () => {
          executionOrder.push('step2-compensate');
        }
      );

      const saga = new SagaOrchestrator({
        id: 'test-saga',
        name: 'Test Saga',
        steps: [step1, step2],
      });

      const result = await saga.execute({ initial: 'data' });

      expect(result.success).toBe(true);
      expect(result.result.step1).toBe('done');
      expect(result.result.step2).toBe('done');
      expect(executionOrder).toEqual(['step1-execute', 'step2-execute']);
    });

    it('should execute saga steps in parallel', async () => {
      const executionTimes = {};

      const step1 = createSagaStep(
        'step1',
        'parallel1',
        async () => {
          const start = Date.now();
          await new Promise(resolve => setTimeout(resolve, 50));
          executionTimes.step1 = Date.now() - start;
          return 'step1-result';
        },
        async () => {}
      );

      const step2 = createSagaStep(
        'step2',
        'parallel2',
        async () => {
          const start = Date.now();
          await new Promise(resolve => setTimeout(resolve, 50));
          executionTimes.step2 = Date.now() - start;
          return 'step2-result';
        },
        async () => {}
      );

      const saga = new SagaOrchestrator({
        id: 'parallel-saga',
        name: 'Parallel Saga',
        steps: [step1, step2],
        parallel: true,
      });

      const startTime = Date.now();
      const result = await saga.execute();
      const totalTime = Date.now() - startTime;

      expect(result.success).toBe(true);
      expect(result.result.parallel1).toBe('step1-result');
      expect(result.result.parallel2).toBe('step2-result');
      // Total time should be ~50ms (parallel) not ~100ms (sequential)
      expect(totalTime).toBeLessThan(100);
    });

    it('should compensate on failure (sequential)', async () => {
      const compensated = [];

      const step1 = createSagaStep(
        'step1',
        'first',
        async () => 'step1-done',
        async () => {
          compensated.push('step1');
        }
      );

      const step2 = createSagaStep(
        'step2',
        'second',
        async () => {
          throw new Error('Step 2 failed');
        },
        async () => {
          compensated.push('step2');
        }
      );

      const step3 = createSagaStep(
        'step3',
        'third',
        async () => 'step3-done',
        async () => {
          compensated.push('step3');
        }
      );

      const saga = new SagaOrchestrator({
        id: 'compensate-saga',
        name: 'Compensate Saga',
        steps: [step1, step2, step3],
      });

      const result = await saga.execute();

      expect(result.success).toBe(false);
      expect(result.state.status).toBe('compensated');
      expect(result.state.completedSteps).toHaveLength(1); // Only step1 completed
      expect(compensated).toEqual(['step1']); // Compensate in reverse
    });

    it('should retry failed steps', async () => {
      let attempts = 0;

      const step = createSagaStep(
        'retry-step',
        'retry',
        async () => {
          attempts++;
          if (attempts < 3) {
            throw new Error('Not yet');
          }
          return 'success';
        },
        async () => {},
        { retryable: true, maxRetries: 3 }
      );

      const saga = new SagaOrchestrator({
        id: 'retry-saga',
        name: 'Retry Saga',
        steps: [step],
      });

      const result = await saga.execute();

      expect(result.success).toBe(true);
      expect(attempts).toBe(3);
    });

    it('should not retry non-retryable steps', async () => {
      let attempts = 0;

      const step = createSagaStep(
        'no-retry-step',
        'noretry',
        async () => {
          attempts++;
          throw new Error('Fail immediately');
        },
        async () => {},
        { retryable: false }
      );

      const saga = new SagaOrchestrator({
        id: 'no-retry-saga',
        name: 'No Retry Saga',
        steps: [step],
      });

      const result = await saga.execute();

      expect(result.success).toBe(false);
      expect(attempts).toBe(1);
    });

    it('should track saga execution state', async () => {
      const step = createSagaStep(
        'step1',
        'track',
        async () => 'result',
        async () => {}
      );

      const saga = new SagaOrchestrator({
        id: 'track-saga',
        name: 'Track Saga',
        steps: [step],
      });

      const result = await saga.execute();
      const states = saga.getAllStates();

      expect(states).toHaveLength(1);
      expect(states[0].status).toBe('completed');
      expect(states[0].completedSteps).toHaveLength(1);
      expect(states[0].startTime).toBeDefined();
      expect(states[0].endTime).toBeDefined();
    });

    it('should generate receipt for saga execution', async () => {
      const step = createSagaStep(
        'step1',
        'receipt',
        async () => 'done',
        async () => {}
      );

      const saga = new SagaOrchestrator({
        id: 'receipt-saga',
        name: 'Receipt Saga',
        steps: [step],
      });

      const result = await saga.execute();
      const states = saga.getAllStates();
      const receipt = await saga.generateReceipt(states[0].sagaId);

      expect(receipt.operation).toContain('saga:Receipt Saga');
      expect(receipt.outputs.success).toBe(true);
      expect(receipt.outputs.completedSteps).toBe(1);
    });
  });

  describe('createSagaBuilder', () => {
    it('should build saga with fluent API', () => {
      const saga = createSagaBuilder('builder-saga', 'Builder Saga')
        .step(createSagaStep('s1', 'step1', async () => 1, async () => {}))
        .step(createSagaStep('s2', 'step2', async () => 2, async () => {}))
        .build();

      expect(saga).toBeInstanceOf(SagaOrchestrator);
      expect(saga.config.id).toBe('builder-saga');
      expect(saga.config.steps).toHaveLength(2);
    });

    it('should support parallel execution flag', () => {
      const saga = createSagaBuilder('parallel-builder', 'Parallel')
        .step(createSagaStep('s1', 'step1', async () => 1, async () => {}))
        .inParallel()
        .build();

      expect(saga.config.parallel).toBe(true);
    });

    it('should support continueOnError flag', () => {
      const saga = createSagaBuilder('continue-builder', 'Continue')
        .step(createSagaStep('s1', 'step1', async () => 1, async () => {}))
        .continueOnError()
        .build();

      expect(saga.config.continueOnError).toBe(true);
    });
  });
});
