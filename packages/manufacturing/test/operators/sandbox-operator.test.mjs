/**
 * @file μ₈ Sandbox Operator Tests
 */

import { describe, it, expect } from 'vitest';
import { SandboxOperator } from '../../src/operators/sandbox-operator.mjs';

describe('SandboxOperator', () => {
  const op = new SandboxOperator();

  it('executes effect and returns result', () => {
    const effect = async (input) => ({ computed: input.value * 2 });
    const result = op.execute({ effect, input: { value: 21 } });
    return result.then(r => {
      expect(r.result.computed).toBe(42);
    });
  });

  it('records audit log with committed status', () => {
    const effect = async () => 'ok';
    const result = op.execute({ effect });
    return result.then(r => {
      expect(r.auditLog).toHaveLength(1);
      expect(r.auditLog[0].status).toBe('committed');
      expect(r.auditLog[0].operator).toBe('sandbox');
      expect(r.auditLog[0].timestamp).toBeDefined();
    });
  });

  it('records rolled_back status when effect throws', () => {
    const effect = async () => { throw new Error('boom'); };
    const result = op.execute({ effect });
    return result.then(r => {
      expect(r.result).toBeUndefined();
      expect(r.auditLog[0].status).toBe('rolled_back');
      expect(r.auditLog[0].error).toBe('boom');
    });
  });

  it('preserves existing audit log entries', () => {
    const existingLog = [{ timestamp: 't0', operator: 'sandbox', status: 'committed' }];
    const effect = async () => 'ok';
    const result = op.execute({ effect, auditLog: existingLog });
    return result.then(r => {
      expect(r.auditLog).toHaveLength(2);
      expect(r.auditLog[0]).toEqual(existingLog[0]);
    });
  });

  it('returns metrics with duration and entries count', () => {
    const effect = async () => 'ok';
    const result = op.execute({ effect });
    return result.then(r => {
      expect(r.metrics.duration_ms).toBeGreaterThanOrEqual(0);
      expect(r.metrics.entries).toBe(1);
    });
  });
});
