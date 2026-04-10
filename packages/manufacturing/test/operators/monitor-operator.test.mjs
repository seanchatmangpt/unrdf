/**
 * @file μ₇ Monitor Operator Tests
 */

import { describe, it, expect } from 'vitest';
import { MonitorOperator } from '../../src/operators/monitor-operator.mjs';

describe('MonitorOperator', () => {
  const op = new MonitorOperator();

  it('passes data through unchanged', () => {
    const data = { name: 'Alice', age: 30 };
    const result = op.execute({ data });
    return result.then(r => {
      expect(r.data).toBe(data); // same reference, not mutated
    });
  });

  it('records type as object for plain object', () => {
    const result = op.execute({ data: { a: 1 } });
    return result.then(r => {
      expect(r.metrics.data.type).toBe('object');
    });
  });

  it('records type as array for arrays', () => {
    const result = op.execute({ data: [1, 2, 3] });
    return result.then(r => {
      expect(r.metrics.data.type).toBe('array');
    });
  });

  it('records size as object key count', () => {
    const result = op.execute({ data: { a: 1, b: 2, c: 3 } });
    return result.then(r => {
      expect(r.metrics.data.size).toBe(3);
    });
  });

  it('records size as array length', () => {
    const result = op.execute({ data: [1, 2, 3, 4, 5] });
    return result.then(r => {
      expect(r.metrics.data.size).toBe(5);
    });
  });

  it('records keys for objects', () => {
    const result = op.execute({ data: { x: 1, y: 2 } });
    return result.then(r => {
      expect(r.metrics.data.keys).toEqual(['x', 'y']);
    });
  });

  it('records empty keys for arrays', () => {
    const result = op.execute({ data: [1, 2] });
    return result.then(r => {
      expect(r.metrics.data.keys).toEqual([]);
    });
  });

  it('includes labels when provided', () => {
    const result = op.execute({
      data: { a: 1 },
      labels: { pipeline: 'test', stage: 'validate' },
    });
    return result.then(r => {
      expect(r.metrics.labels).toEqual({ pipeline: 'test', stage: 'validate' });
    });
  });

  it('includes context pipelineId and stage', () => {
    const result = op.execute(
      { data: { a: 1 } },
      { pipelineId: 'pipe-1', stage: 'breed' },
    );
    return result.then(r => {
      expect(r.metrics.pipelineId).toBe('pipe-1');
      expect(r.metrics.stage).toBe('breed');
    });
  });

  it('records timestamp', () => {
    const before = new Date().toISOString();
    const result = op.execute({ data: {} });
    return result.then(r => {
      expect(r.metrics.timestamp).toBeDefined();
      expect(r.metrics.timestamp >= before).toBe(true);
    });
  });

  it('records duration', () => {
    const result = op.execute({ data: {} });
    return result.then(r => {
      expect(r.metrics.duration_ms).toBeGreaterThanOrEqual(0);
    });
  });
});
