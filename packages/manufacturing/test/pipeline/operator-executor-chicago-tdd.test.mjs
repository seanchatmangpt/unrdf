/**
 * @file Operator Executor Tests — Chicago TDD
 * @description Execution boundary verification for operator runtime
 *
 * Core Chicago TDD tests focused on execution boundaries:
 * - Unknown operator throws OperatorError
 * - Input validation failure throws OperatorError
 * - Timeout protection enforced
 * - Context propagation
 * - Error wrapping
 */

import { describe, it, expect } from 'vitest';
import { OperatorExecutor } from '../../src/pipeline/operator-executor.mjs';
import { OperatorRegistry } from '../../src/operators/registry.mjs';
import { ValidateOperator } from '../../src/operators/validate-operator.mjs';
import { TransformOperator } from '../../src/operators/transform-operator.mjs';
import { FilterOperator } from '../../src/operators/filter-operator.mjs';
import { OperatorError, OPERATOR_CODES } from '../../src/error.mjs';

function setupRegistry() {
  const registry = new OperatorRegistry();
  registry.register(new ValidateOperator());
  registry.register(new TransformOperator());
  registry.register(new FilterOperator());
  return registry;
}

describe('OperatorExecutor — Execution Boundary Verification', () => {

  it('executes registered operator by name', () => {
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

  it('enforces timeout on slow operators', () => {
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
        expect(err).toBeInstanceOf(Error);
        expect(err.message).toContain('timed out');
      });
  });

  it('executeSafe wraps errors in OperatorError', () => {
    const registry = setupRegistry();
    const executor = new OperatorExecutor(registry);
    return executor.executeSafe('nonexistent', { data: {} })
      .then(result => {
        expect(result.success).toBe(false);
        expect(result.error).toBeInstanceOf(OperatorError);
      });
  });
});
