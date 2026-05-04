/**
 * @fileoverview Test suite for Process Group (pg) pool formation.
 */

import { describe, it, expect } from 'vitest';

/**
 * Validates pool formation structure.
 */
export function testPoolFormation(nodes) {
  if (!Array.isArray(nodes) || nodes.length === 0) throw new Error('Invalid nodes');
  return { nodes, leader: nodes[0], status: 'formed' };
}

/**
 * Validates pool leader status.
 */
export function verifyPoolLeader(pool) {
  if (!pool?.nodes || !pool?.leader) throw new Error('Invalid pool');
  return pool.nodes.includes(pool.leader);
}

describe('Pool Formation', () => {
  it('should form a valid pool with nodes', () => {
    const nodes = ['n1', 'n2'];
    const pool = testPoolFormation(nodes);
    expect(pool.nodes).toEqual(nodes);
    expect(pool.leader).toBe('n1');
  });

  it('should verify correct pool leader', () => {
    const pool = testPoolFormation(['n1', 'n2']);
    expect(verifyPoolLeader(pool)).toBe(true);
  });

  it('should throw on invalid pool data', () => {
    expect(() => testPoolFormation([])).toThrow();
  });
});
