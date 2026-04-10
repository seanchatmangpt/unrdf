/**
 * @file Operator Registry Tests
 */

import { describe, it, expect } from 'vitest';
import { OperatorRegistry } from '../../src/operators/registry.mjs';
import { BaseOperator } from '../../src/operators/base.mjs';

class MockOperator extends BaseOperator {
  constructor(name) {
    super({ name, description: `mock ${name}`, latencyTargetMs: 5 });
  }
  async execute(input) { return input; }
}

describe('OperatorRegistry', () => {
  it('registers and retrieves an operator', () => {
    const registry = new OperatorRegistry();
    const op = new MockOperator('validate');
    registry.register(op);
    expect(registry.get('validate')).toBe(op);
  });

  it('throws on duplicate registration', () => {
    const registry = new OperatorRegistry();
    registry.register(new MockOperator('validate'));
    expect(() => registry.register(new MockOperator('validate'))).toThrow('already registered');
  });

  it('throws when operator not found', () => {
    const registry = new OperatorRegistry();
    expect(() => registry.get('nonexistent')).toThrow('not found');
  });

  it('has() returns boolean', () => {
    const registry = new OperatorRegistry();
    registry.register(new MockOperator('validate'));
    expect(registry.has('validate')).toBe(true);
    expect(registry.has('nonexistent')).toBe(false);
  });

  it('lists all operators with metadata', () => {
    const registry = new OperatorRegistry();
    registry.register(new MockOperator('validate'));
    registry.register(new MockOperator('transform'));
    const list = registry.list();
    expect(list).toHaveLength(2);
    expect(list[0].name).toBe('validate');
    expect(list[1].name).toBe('transform');
  });

  it('reports correct size', () => {
    const registry = new OperatorRegistry();
    expect(registry.size).toBe(0);
    registry.register(new MockOperator('validate'));
    expect(registry.size).toBe(1);
    registry.register(new MockOperator('transform'));
    expect(registry.size).toBe(2);
  });
});
