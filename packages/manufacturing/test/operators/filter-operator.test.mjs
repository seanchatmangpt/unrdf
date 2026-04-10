/**
 * @file μ₄ Filter Operator Tests
 */

import { describe, it, expect } from 'vitest';
import { FilterOperator } from '../../src/operators/filter-operator.mjs';

describe('FilterOperator', () => {
  const op = new FilterOperator();

  it('passes all items with default predicate', () => {
    const items = [{ id: 1 }, { id: 2 }, { id: 3 }];
    const result = op.execute({ data: items });
    return result.then(r => {
      expect(r.data).toHaveLength(3);
      expect(r.passed).toBe(3);
      expect(r.skipped).toBe(0);
    });
  });

  it('filters by predicate', () => {
    const items = [{ id: 1 }, { id: 2 }, { id: 3 }];
    const result = op.execute({
      data: items,
      predicate: item => item.id > 1,
    });
    return result.then(r => {
      expect(r.data).toHaveLength(2);
      expect(r.passed).toBe(2);
      expect(r.skipped).toBe(1);
    });
  });

  it('filters out all items', () => {
    const items = [{ id: 1 }, { id: 2 }];
    const result = op.execute({
      data: items,
      predicate: () => false,
    });
    return result.then(r => {
      expect(r.data).toHaveLength(0);
      expect(r.passed).toBe(0);
      expect(r.skipped).toBe(2);
    });
  });

  it('handles empty array', () => {
    const result = op.execute({ data: [] });
    return result.then(r => {
      expect(r.data).toHaveLength(0);
      expect(r.passed).toBe(0);
      expect(r.skipped).toBe(0);
    });
  });

  it('skips items when predicate throws', () => {
    const items = [{ id: 1 }, { bad: true }, { id: 3 }];
    const result = op.execute({
      data: items,
      predicate: item => item.id > 0,
    });
    return result.then(r => {
      expect(r.passed).toBe(2);
      expect(r.skipped).toBe(1);
    });
  });

  it('returns metrics with duration', () => {
    const result = op.execute({ data: [{ id: 1 }] });
    return result.then(r => {
      expect(r.metrics.duration_ms).toBeGreaterThanOrEqual(0);
    });
  });
});
