/**
 * @file μ₅ Aggregate Operator Tests
 */

import { describe, it, expect } from 'vitest';
import { AggregateOperator } from '../../src/operators/aggregate-operator.mjs';

describe('AggregateOperator', () => {
  const op = new AggregateOperator();

  it('returns count without groupBy', () => {
    const items = [{ id: 1 }, { id: 2 }, { id: 3 }];
    const result = op.execute({ data: items });
    return result.then(r => {
      expect(r.data).toEqual([{ _all: 3, _count: 3 }]);
      expect(r.groupCount).toBe(1);
    });
  });

  it('groups by field with count (default)', () => {
    const items = [
      { team: 'A', score: 10 },
      { team: 'B', score: 20 },
      { team: 'A', score: 30 },
    ];
    const result = op.execute({ data: items, groupBy: 'team' });
    return result.then(r => {
      expect(r.data).toHaveLength(2);
      expect(r.groupCount).toBe(2);
      const aGroup = r.data.find(g => g.team === 'A');
      const bGroup = r.data.find(g => g.team === 'B');
      expect(aGroup._count).toBe(2);
      expect(bGroup._count).toBe(1);
    });
  });

  it('aggregates with sum', () => {
    const items = [
      { team: 'A', score: 10 },
      { team: 'A', score: 30 },
    ];
    const result = op.execute({ data: items, groupBy: 'team', aggField: 'score', aggFn: 'sum' });
    return result.then(r => {
      const aGroup = r.data.find(g => g.team === 'A');
      expect(aGroup._sum).toBe(40);
    });
  });

  it('aggregates with avg', () => {
    const items = [
      { team: 'A', score: 10 },
      { team: 'A', score: 30 },
    ];
    const result = op.execute({ data: items, groupBy: 'team', aggField: 'score', aggFn: 'avg' });
    return result.then(r => {
      const aGroup = r.data.find(g => g.team === 'A');
      expect(aGroup._avg).toBe(20);
    });
  });

  it('aggregates with min', () => {
    const items = [
      { team: 'A', score: 10 },
      { team: 'A', score: 30 },
    ];
    const result = op.execute({ data: items, groupBy: 'team', aggField: 'score', aggFn: 'min' });
    return result.then(r => {
      const aGroup = r.data.find(g => g.team === 'A');
      expect(aGroup._min).toBe(10);
    });
  });

  it('aggregates with max', () => {
    const items = [
      { team: 'A', score: 10 },
      { team: 'A', score: 30 },
    ];
    const result = op.execute({ data: items, groupBy: 'team', aggField: 'score', aggFn: 'max' });
    return result.then(r => {
      const aGroup = r.data.find(g => g.team === 'A');
      expect(aGroup._max).toBe(30);
    });
  });

  it('aggregates with first', () => {
    const items = [
      { team: 'A', name: 'Alice' },
      { team: 'A', name: 'Bob' },
    ];
    const result = op.execute({ data: items, groupBy: 'team', aggFn: 'first' });
    return result.then(r => {
      const aGroup = r.data.find(g => g.team === 'A');
      expect(aGroup._first.name).toBe('Alice');
    });
  });

  it('aggregates with concat', () => {
    const items = [
      { team: 'A', tag: 'red' },
      { team: 'A', tag: 'blue' },
    ];
    const result = op.execute({ data: items, groupBy: 'team', aggField: 'tag', aggFn: 'concat' });
    return result.then(r => {
      const aGroup = r.data.find(g => g.team === 'A');
      expect(aGroup._concat).toBe('red, blue');
    });
  });

  it('handles items with missing groupBy field', () => {
    const items = [
      { team: 'A', score: 10 },
      { score: 20 },
    ];
    const result = op.execute({ data: items, groupBy: 'team' });
    return result.then(r => {
      expect(r.groupCount).toBe(2);
    });
  });

  it('returns metrics with totalItems', () => {
    const items = [{ id: 1 }, { id: 2 }];
    const result = op.execute({ data: items, groupBy: 'id' });
    return result.then(r => {
      expect(r.metrics.totalItems).toBe(2);
      expect(r.metrics.duration_ms).toBeGreaterThanOrEqual(0);
    });
  });
});
