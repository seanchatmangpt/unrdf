/**
 * @file μ₆ Derive Operator Tests
 */

import { describe, it, expect } from 'vitest';
import { DeriveOperator } from '../../src/operators/derive-operator.mjs';

describe('DeriveOperator', () => {
  const op = new DeriveOperator();

  it('returns data unchanged when no rules', () => {
    const result = op.execute({ data: { x: 1, y: 2 } });
    return result.then(r => {
      expect(r.data).toEqual({ x: 1, y: 2 });
      expect(r.derived).toHaveLength(0);
    });
  });

  it('derives a new field from rule', () => {
    const result = op.execute({
      data: { firstName: 'Alice', lastName: 'Smith' },
      rules: [
        { output: 'fullName', derive: d => `${d.firstName} ${d.lastName}` },
      ],
    });
    return result.then(r => {
      expect(r.data.fullName).toBe('Alice Smith');
      expect(r.derived).toContain('fullName');
    });
  });

  it('derives from rule returning object', () => {
    const result = op.execute({
      data: { width: 10, height: 5 },
      rules: [
        { output: 'computed', derive: d => ({ area: d.width * d.height, perimeter: 2 * (d.width + d.height) }) },
      ],
    });
    return result.then(r => {
      expect(r.data.area).toBe(50);
      expect(r.data.perimeter).toBe(30);
    });
  });

  it('skips rule when derive returns null', () => {
    const result = op.execute({
      data: { value: null },
      rules: [
        { output: 'computed', derive: () => null },
      ],
    });
    return result.then(r => {
      expect(r.data).not.toHaveProperty('computed');
      expect(r.derived).toHaveLength(0);
    });
  });

  it('skips rule when derive returns undefined', () => {
    const result = op.execute({
      data: { x: 1 },
      rules: [
        { output: 'computed', derive: () => undefined },
      ],
    });
    return result.then(r => {
      expect(r.data).not.toHaveProperty('computed');
    });
  });

  it('handles rule that throws gracefully', () => {
    const result = op.execute({
      data: { x: 1 },
      rules: [
        { output: 'bad', derive: () => { throw new Error('rule error'); } },
        { output: 'good', derive: d => d.x * 2 },
      ],
    });
    return result.then(r => {
      expect(r.data).not.toHaveProperty('bad');
      expect(r.data.good).toBe(2);
      expect(r.derived).toContain('good');
    });
  });

  it('applies multiple rules', () => {
    const result = op.execute({
      data: { price: 100, taxRate: 0.1 },
      rules: [
        { output: 'tax', derive: d => d.price * d.taxRate },
        { output: 'total', derive: d => d.price + d.tax },
      ],
    });
    return result.then(r => {
      expect(r.data.tax).toBe(10);
      expect(r.data.total).toBe(110);
      expect(r.derived).toHaveLength(2);
    });
  });

  it('does not mutate original input', () => {
    const original = { x: 1 };
    const result = op.execute({
      data: original,
      rules: [{ output: 'doubled', derive: d => d.x * 2 }],
    });
    return result.then(r => {
      expect(original).toEqual({ x: 1 });
    });
  });

  it('returns metrics with rule count', () => {
    const result = op.execute({
      data: { x: 1 },
      rules: [{ output: 'y', derive: d => d.x }],
    });
    return result.then(r => {
      expect(r.metrics.ruleCount).toBe(1);
      expect(r.metrics.duration_ms).toBeGreaterThanOrEqual(0);
    });
  });
});
