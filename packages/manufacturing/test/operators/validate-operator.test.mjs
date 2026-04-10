/**
 * @file μ₁ Validate Operator Tests
 */

import { describe, it, expect } from 'vitest';
import { ValidateOperator } from '../../src/operators/validate-operator.mjs';

describe('ValidateOperator', () => {
  const op = new ValidateOperator();

  it('validates valid data without schema', () => {
    const result = op.execute({ data: { name: 'test' } });
    return result.then(r => {
      expect(r.valid).toBe(true);
      expect(r.errors).toHaveLength(0);
      expect(r.data).toEqual({ name: 'test' });
    });
  });

  it('validates schema: required field present', () => {
    const result = op.execute({
      data: { name: 'Alice' },
      schema: { name: { required: true } },
    });
    return result.then(r => {
      expect(r.valid).toBe(true);
    });
  });

  it('fails schema: required field missing', () => {
    const result = op.execute({
      data: { age: 30 },
      schema: { name: { required: true } },
    });
    return result.then(r => {
      expect(r.valid).toBe(false);
      expect(r.errors[0]).toContain('Required field "name"');
    });
  });

  it('validates schema: type check string', () => {
    const result = op.execute({
      data: { name: 42 },
      schema: { name: { type: 'string' } },
    });
    return result.then(r => {
      expect(r.valid).toBe(false);
      expect(r.errors[0]).toContain('expected string, got number');
    });
  });

  it('validates schema: type check array', () => {
    const result = op.execute({
      data: { items: [1, 2, 3] },
      schema: { items: { type: 'array' } },
    });
    return result.then(r => {
      expect(r.valid).toBe(true);
    });
  });

  it('validates schema: pattern match', () => {
    const result = op.execute({
      data: { email: 'not-an-email' },
      schema: { email: { pattern: '^[\\w.-]+@[\\w.-]+\\.\\w+$' } },
    });
    return result.then(r => {
      expect(r.valid).toBe(false);
      expect(r.errors[0]).toContain('does not match pattern');
    });
  });

  it('validates schema: pattern passes', () => {
    const result = op.execute({
      data: { email: 'user@example.com' },
      schema: { email: { pattern: '^[\\w.-]+@[\\w.-]+\\.\\w+$' } },
    });
    return result.then(r => {
      expect(r.valid).toBe(true);
    });
  });

  it('validates schema: min constraint', () => {
    const result = op.execute({
      data: { age: 5 },
      schema: { age: { min: 18 } },
    });
    return result.then(r => {
      expect(r.valid).toBe(false);
      expect(r.errors[0]).toContain('below minimum');
    });
  });

  it('validates schema: max constraint', () => {
    const result = op.execute({
      data: { score: 150 },
      schema: { score: { max: 100 } },
    });
    return result.then(r => {
      expect(r.valid).toBe(false);
      expect(r.errors[0]).toContain('above maximum');
    });
  });

  it('validates schema: min/max passes', () => {
    const result = op.execute({
      data: { age: 25 },
      schema: { age: { min: 18, max: 65 } },
    });
    return result.then(r => {
      expect(r.valid).toBe(true);
    });
  });

  it('runs rule-based validation', () => {
    const result = op.execute({
      data: { password: 'abc' },
      rules: [
        {
          check: d => d.password.length >= 8,
          message: 'Password must be at least 8 characters',
        },
      ],
    });
    return result.then(r => {
      expect(r.valid).toBe(false);
      expect(r.errors).toContain('Password must be at least 8 characters');
    });
  });

  it('passes rule-based validation', () => {
    const result = op.execute({
      data: { password: 'longpassword' },
      rules: [
        {
          check: d => d.password.length >= 8,
          message: 'Password too short',
        },
      ],
    });
    return result.then(r => {
      expect(r.valid).toBe(true);
    });
  });

  it('returns metrics with duration', () => {
    const result = op.execute({ data: { name: 'test' } });
    return result.then(r => {
      expect(r.metrics).toBeDefined();
      expect(r.metrics.duration_ms).toBeGreaterThanOrEqual(0);
      expect(r.metrics.ruleCount).toBe(0);
    });
  });

  it('collects multiple errors', () => {
    const result = op.execute({
      data: {},
      schema: {
        name: { required: true },
        email: { required: true },
      },
    });
    return result.then(r => {
      expect(r.valid).toBe(false);
      expect(r.errors).toHaveLength(2);
    });
  });
});
