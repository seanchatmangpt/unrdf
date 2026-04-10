/**
 * @file μ₂ Transform Operator Tests
 */

import { describe, it, expect } from 'vitest';
import { TransformOperator } from '../../src/operators/transform-operator.mjs';

describe('TransformOperator', () => {
  const op = new TransformOperator();

  it('returns data unchanged when no transformations', () => {
    const result = op.execute({ data: { name: 'Alice', age: 30 } });
    return result.then(r => {
      expect(r.data).toEqual({ name: 'Alice', age: 30 });
      expect(r.applied).toHaveLength(0);
    });
  });

  it('applies rename transformation', () => {
    const result = op.execute({
      data: { oldName: 'Alice' },
      transformations: [{ type: 'rename', from: 'oldName', to: 'name' }],
    });
    return result.then(r => {
      expect(r.data.name).toBe('Alice');
      expect(r.data).not.toHaveProperty('oldName');
      expect(r.applied).toContain('rename:oldName→name');
    });
  });

  it('applies convert transformation', () => {
    const result = op.execute({
      data: { temperature: '32' },
      transformations: [{ type: 'convert', field: 'temperature', converter: v => Number(v) + 273 }],
    });
    return result.then(r => {
      expect(r.data.temperature).toBe(305);
      expect(r.applied).toContain('convert:temperature');
    });
  });

  it('applies delete transformation', () => {
    const result = op.execute({
      data: { name: 'Alice', temp: 'remove-me' },
      transformations: [{ type: 'delete', field: 'temp' }],
    });
    return result.then(r => {
      expect(r.data).not.toHaveProperty('temp');
      expect(r.data.name).toBe('Alice');
      expect(r.applied).toContain('delete:temp');
    });
  });

  it('applies default transformation', () => {
    const result = op.execute({
      data: { name: 'Alice' },
      transformations: [{ type: 'default', field: 'role', value: 'user' }],
    });
    return result.then(r => {
      expect(r.data.role).toBe('user');
      expect(r.applied).toContain('default:role');
    });
  });

  it('skips default when field exists', () => {
    const result = op.execute({
      data: { role: 'admin' },
      transformations: [{ type: 'default', field: 'role', value: 'user' }],
    });
    return result.then(r => {
      expect(r.data.role).toBe('admin');
      expect(r.applied).toHaveLength(0);
    });
  });

  it('applies map transformation', () => {
    const result = op.execute({
      data: { firstName: 'Alice', lastName: 'Smith' },
      transformations: [{ type: 'map', fields: { name: 'firstName', surname: 'lastName' } }],
    });
    return result.then(r => {
      expect(r.data).toEqual({ name: 'Alice', surname: 'Smith' });
    });
  });

  it('applies custom function transformation', () => {
    const result = op.execute({
      data: { items: [1, 2, 3] },
      transformations: [{ type: 'custom', fn: d => ({ ...d, count: d.items.length }) }],
    });
    return result.then(r => {
      expect(r.data.count).toBe(3);
      expect(r.applied).toContain('custom');
    });
  });

  it('applies multiple transformations in order', () => {
    const result = op.execute({
      data: { oldName: 'Alice', age: '30' },
      transformations: [
        { type: 'rename', from: 'oldName', to: 'name' },
        { type: 'convert', field: 'age', converter: Number },
      ],
    });
    return result.then(r => {
      expect(r.data.name).toBe('Alice');
      expect(r.data.age).toBe(30);
      expect(r.applied).toHaveLength(2);
    });
  });

  it('does not mutate original input data', () => {
    const original = { name: 'Alice' };
    op.execute({
      data: original,
      transformations: [{ type: 'default', field: 'role', value: 'user' }],
    });
    return op.execute({
      data: original,
      transformations: [{ type: 'default', field: 'role', value: 'user' }],
    }).then(r => {
      expect(original).toEqual({ name: 'Alice' });
      expect(original).not.toHaveProperty('role');
    });
  });

  it('returns metrics', () => {
    const result = op.execute({ data: { x: 1 } });
    return result.then(r => {
      expect(r.metrics.duration_ms).toBeGreaterThanOrEqual(0);
    });
  });
});
