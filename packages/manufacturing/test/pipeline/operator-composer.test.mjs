/**
 * @file Operator Composer Tests
 */

import { describe, it, expect } from 'vitest';
import { OperatorComposer } from '../../src/pipeline/operator-composer.mjs';

describe('OperatorComposer', () => {
  it('starts with no steps', () => {
    const composer = new OperatorComposer();
    expect(composer.steps).toHaveLength(0);
  });

  it('adds a step and returns self (chainable)', () => {
    const composer = new OperatorComposer();
    const result = composer.add('validate', { data: { name: 'test' } });
    expect(result).toBe(composer);
    expect(composer.steps).toHaveLength(1);
  });

  it('generates auto-incrementing IDs', () => {
    const composer = new OperatorComposer();
    composer.add('validate', {});
    composer.add('transform', {});
    expect(composer.steps[0].id).toBe('step-0');
    expect(composer.steps[1].id).toBe('step-1');
  });

  it('accepts custom ID', () => {
    const composer = new OperatorComposer();
    composer.add('validate', {}, { id: 'my-step' });
    expect(composer.getStep('my-step')).toBeDefined();
    expect(composer.getStep('my-step').operatorName).toBe('validate');
  });

  it('stores dependencies', () => {
    const composer = new OperatorComposer();
    composer.add('validate', {}, { id: 'v' });
    composer.add('transform', {}, { id: 't', dependsOn: ['v'] });
    expect(composer.getStep('t').dependsOn).toEqual(['v']);
  });

  it('getStep returns undefined for missing ID', () => {
    const composer = new OperatorComposer();
    expect(composer.getStep('nonexistent')).toBeUndefined();
  });

  describe('getExecutionOrder', () => {
    it('returns single groups for sequential steps', () => {
      const composer = new OperatorComposer();
      composer.add('validate', {}, { id: 'v' });
      composer.add('transform', {}, { id: 't', dependsOn: ['v'] });
      const order = composer.getExecutionOrder();
      expect(order).toEqual([['v'], ['t']]);
    });

    it('groups parallel steps together', () => {
      const composer = new OperatorComposer();
      composer.add('validate', {}, { id: 'v' });
      composer.add('filter', {}, { id: 'f', dependsOn: ['v'], parallel: true });
      composer.add('enrich', {}, { id: 'e', dependsOn: ['v'], parallel: true });
      composer.add('transform', {}, { id: 't', dependsOn: ['f', 'e'] });
      const order = composer.getExecutionOrder();
      expect(order).toEqual([['v'], ['f', 'e'], ['t']]);
    });

    it('handles diamond dependency', () => {
      const composer = new OperatorComposer();
      composer.add('a', {}, { id: 'a' });
      composer.add('b', {}, { id: 'b', dependsOn: ['a'] });
      composer.add('c', {}, { id: 'c', dependsOn: ['a'] });
      composer.add('d', {}, { id: 'd', dependsOn: ['b', 'c'] });
      const order = composer.getExecutionOrder();
      // a must come first, then b and c, then d
      const flat = order.flat();
      expect(flat.indexOf('a')).toBeLessThan(flat.indexOf('b'));
      expect(flat.indexOf('a')).toBeLessThan(flat.indexOf('c'));
      expect(flat.indexOf('b')).toBeLessThan(flat.indexOf('d'));
      expect(flat.indexOf('c')).toBeLessThan(flat.indexOf('d'));
    });

    it('throws on circular dependency', () => {
      const composer = new OperatorComposer();
      composer.add('a', {}, { id: 'a', dependsOn: ['b'] });
      composer.add('b', {}, { id: 'b', dependsOn: ['a'] });
      expect(() => composer.getExecutionOrder()).toThrow('Circular dependency');
    });

    it('handles no dependencies (all run sequentially by default)', () => {
      const composer = new OperatorComposer();
      composer.add('validate', {}, { id: 'v' });
      composer.add('transform', {}, { id: 't' });
      composer.add('filter', {}, { id: 'f' });
      const order = composer.getExecutionOrder();
      expect(order).toHaveLength(3);
      expect(order[0]).toEqual(['v']);
      expect(order[1]).toEqual(['t']);
      expect(order[2]).toEqual(['f']);
    });
  });

  describe('plan', () => {
    it('returns execution plan summary', () => {
      const composer = new OperatorComposer();
      composer.add('validate', {}, { id: 'v' });
      composer.add('transform', {}, { id: 't', dependsOn: ['v'] });
      const plan = composer.plan;
      expect(plan.totalSteps).toBe(2);
      expect(plan.groups).toEqual([['v'], ['t']]);
      expect(plan.hasParallel).toBe(false);
    });

    it('detects parallel groups', () => {
      const composer = new OperatorComposer();
      composer.add('validate', {}, { id: 'v' });
      composer.add('filter', {}, { id: 'f', dependsOn: ['v'], parallel: true });
      composer.add('enrich', {}, { id: 'e', dependsOn: ['v'], parallel: true });
      const plan = composer.plan;
      expect(plan.hasParallel).toBe(true);
    });
  });

  describe('reset', () => {
    it('resets all step statuses', () => {
      const composer = new OperatorComposer();
      composer.add('validate', {}, { id: 'v' });
      // Simulate execution
      composer._steps[0].status = 'completed';
      composer._steps[0].result = { valid: true };
      composer._steps[0].error = null;
      composer.reset();
      expect(composer._steps[0].status).toBe('pending');
      expect(composer._steps[0].result).toBeUndefined();
      expect(composer._steps[0].error).toBeUndefined();
    });
  });
});
