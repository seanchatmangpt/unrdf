/**
 * @file Base Operator Tests
 */

import { describe, it, expect } from 'vitest';
import { BaseOperator } from '../../src/operators/base.mjs';
import { OPERATOR_CODES } from '../../src/error.mjs';

class ConcreteOperator extends BaseOperator {
  constructor() {
    super({
      name: 'test-op',
      description: 'A test operator',
      latencyTargetMs: 10,
      retrySafe: true,
      requires: ['foo'],
    });
  }

  async execute(input, context = {}) {
    return { input, context };
  }
}

describe('BaseOperator', () => {
  it('stores constructor options', () => {
    const op = new ConcreteOperator();
    expect(op.name).toBe('test-op');
    expect(op.description).toBe('A test operator');
    expect(op.latencyTargetMs).toBe(10);
    expect(op.retrySafe).toBe(true);
    expect(op.requires).toEqual(['foo']);
  });

  it('returns metadata', () => {
    const op = new ConcreteOperator();
    const meta = op.metadata;
    expect(meta.name).toBe('test-op');
    expect(meta.description).toBe('A test operator');
    expect(meta.latencyTargetMs).toBe(10);
    expect(meta.retrySafe).toBe(true);
    expect(meta.requires).toEqual(['foo']);
  });

  it('throws on execute if not implemented', () => {
    const op = new BaseOperator({ name: 'abstract', description: 'n/a', latencyTargetMs: 0 });
    expect(op.execute({})).rejects.toThrow('execute() not implemented by abstract');
  });

  describe('validateInput', () => {
    it('passes when all required keys present', () => {
      const op = new ConcreteOperator();
      const result = op.validateInput({ foo: 'bar' });
      expect(result.valid).toBe(true);
      expect(result.errors).toHaveLength(0);
    });

    it('fails when required key missing', () => {
      const op = new ConcreteOperator();
      const result = op.validateInput({ baz: 'qux' });
      expect(result.valid).toBe(false);
      expect(result.errors).toContain('Missing required input: "foo"');
    });

    it('passes when requires is empty', () => {
      const op = new BaseOperator({ name: 'empty', description: 'n/a', latencyTargetMs: 0 });
      const result = op.validateInput({});
      expect(result.valid).toBe(true);
    });
  });

  describe('error', () => {
    it('creates OperatorError with correct fields', () => {
      const op = new ConcreteOperator();
      const err = op.error(OPERATOR_CODES.VALIDATE_FORMAT, 'bad format');
      expect(err.operator).toBe('test-op');
      expect(err.code).toBe('INVALID_FORMAT');
      expect(err.message).toBe('bad format');
    });
  });
});
