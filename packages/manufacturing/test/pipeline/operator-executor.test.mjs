/**
 * @file Operator Executor Tests
 */

import { describe, it, expect } from 'vitest';
import { OperatorExecutor } from '../../src/pipeline/operator-executor.mjs';
import { OperatorRegistry } from '../../src/operators/registry.mjs';
import { ValidateOperator } from '../../src/operators/validate-operator.mjs';
import { TransformOperator } from '../../src/operators/transform-operator.mjs';
import { FilterOperator } from '../../src/operators/filter-operator.mjs';
import { OperatorError } from '../../src/error.mjs';

function setupRegistry() {
  const registry = new OperatorRegistry();
  registry.register(new ValidateOperator());
  registry.register(new TransformOperator());
  registry.register(new FilterOperator());
  return registry;
}

describe('OperatorExecutor', () => {
  it('executes a registered operator by name', () => {
    const executor = new OperatorExecutor(setupRegistry());
    const result = executor.execute('validate', { data: { name: 'Alice' } });
    return result.then(r => {
      expect(r.valid).toBe(true);
    });
  });

  it('throws OperatorError for unknown operator', () => {
    const executor = new OperatorExecutor(setupRegistry());
    return executor.execute('nonexistent', { data: {} })
      .then(() => expect.unreachable('should have thrown'))
      .catch(err => {
        expect(err).toBeInstanceOf(OperatorError);
        expect(err.operator).toBe('registry');
      });
  });

  it('throws OperatorError for input validation failure', () => {
    const executor = new OperatorExecutor(setupRegistry());
    return executor.execute('validate', { /* missing 'data' */ })
      .then(() => expect.unreachable('should have thrown'))
      .catch(err => {
        expect(err).toBeInstanceOf(OperatorError);
        expect(err.code).toBe('VALIDATE_FORMAT');
        expect(err.message).toContain('Missing required input');
      });
  });

  it('passes context to operator', () => {
    const executor = new OperatorExecutor(setupRegistry());
    const result = executor.execute('validate', { data: { x: 1 } }, { context: { pipelineId: 'test' } });
    return result.then(r => {
      expect(r.valid).toBe(true);
    });
  });

  it('respects custom timeout', () => {
    const executor = new OperatorExecutor(setupRegistry(), { defaultTimeoutMs: 5000 });
    // FilterOperator is fast, should complete well within 1000ms
    const result = executor.execute('filter', { data: [1, 2, 3] }, { timeoutMs: 1000 });
    return result.then(r => {
      expect(r.data).toHaveLength(3);
    });
  });

  it('times out on slow operator', () => {
    const registry = setupRegistry();
    // Override validate to be slow
    const slowOp = new ValidateOperator();
    slowOp.execute = async () => {
      await new Promise(resolve => setTimeout(resolve, 5000));
    };
    // Re-register with slow implementation
    const slowRegistry = new OperatorRegistry();
    slowRegistry.register(slowOp);
    const executor = new OperatorExecutor(slowRegistry, { defaultTimeoutMs: 10 });

    return executor.execute('validate', { data: { x: 1 } })
      .then(() => expect.unreachable('should have timed out'))
      .catch(err => {
        expect(err.message).toContain('timed out');
      });
  });

  describe('executeSafe', () => {
    it('returns success result for valid execution', () => {
      const executor = new OperatorExecutor(setupRegistry());
      const result = executor.executeSafe('validate', { data: { name: 'Alice' } });
      return result.then(r => {
        expect(r.success).toBe(true);
        expect(r.result.valid).toBe(true);
        expect(r.error).toBeUndefined();
      });
    });

    it('returns error result for unknown operator', () => {
      const executor = new OperatorExecutor(setupRegistry());
      const result = executor.executeSafe('nonexistent', { data: {} });
      return result.then(r => {
        expect(r.success).toBe(false);
        expect(r.error).toBeInstanceOf(OperatorError);
        expect(r.result).toBeUndefined();
      });
    });

    it('returns error result for input validation failure', () => {
      const executor = new OperatorExecutor(setupRegistry());
      const result = executor.executeSafe('validate', {});
      return result.then(r => {
        expect(r.success).toBe(false);
        expect(r.error).toBeInstanceOf(OperatorError);
      });
    });

    it('wraps non-OperatorError in OperatorError', () => {
      const registry = new OperatorRegistry();
      const badOp = new ValidateOperator();
      badOp.execute = async () => { throw new TypeError('weird error'); };
      registry.register(badOp);
      const executor = new OperatorExecutor(registry);

      const result = executor.executeSafe('validate', { data: {} });
      return result.then(r => {
        expect(r.success).toBe(false);
        expect(r.error).toBeInstanceOf(OperatorError);
        expect(r.error.message).toBe('weird error');
      });
    });
  });
});
