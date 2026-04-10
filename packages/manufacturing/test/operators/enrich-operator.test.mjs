/**
 * @file μ₃ Enrich Operator Tests
 */

import { describe, it, expect } from 'vitest';
import { EnrichOperator } from '../../src/operators/enrich-operator.mjs';

describe('EnrichOperator', () => {
  const op = new EnrichOperator();

  it('returns data unchanged when no enrichments', () => {
    const result = op.execute({ data: { name: 'Alice' } });
    return result.then(r => {
      expect(r.data).toEqual({ name: 'Alice' });
      expect(r.enrichments).toHaveLength(0);
    });
  });

  it('enriches with lookup function', () => {
    const result = op.execute({
      data: { userId: 'u1' },
      enrichments: [
        { field: 'email', lookup: async (d) => `${d.userId}@example.com` },
      ],
    });
    return result.then(r => {
      expect(r.data.email).toBe('u1@example.com');
      expect(r.enrichments).toContain('email');
    });
  });

  it('enriches with static lookup value', () => {
    const result = op.execute({
      data: { country: 'US' },
      enrichments: [
        { field: 'currency', lookup: 'USD' },
      ],
    });
    return result.then(r => {
      expect(r.data.currency).toBe('USD');
    });
  });

  it('uses fallback when lookup returns null', () => {
    const result = op.execute({
      data: { userId: 'u1' },
      enrichments: [
        { field: 'email', lookup: async () => null, fallback: 'unknown@example.com' },
      ],
    });
    return result.then(r => {
      expect(r.data.email).toBe('unknown@example.com');
      expect(r.enrichments[0]).toContain('fallback');
    });
  });

  it('uses fallback when lookup throws', () => {
    const result = op.execute({
      data: { userId: 'u1' },
      enrichments: [
        { field: 'email', lookup: async () => { throw new Error('API down'); }, fallback: 'fallback@example.com' },
      ],
    });
    return result.then(r => {
      expect(r.data.email).toBe('fallback@example.com');
      expect(r.enrichments[0]).toContain('fallback');
    });
  });

  it('skips enrichment when lookup throws and no fallback', () => {
    const result = op.execute({
      data: { userId: 'u1' },
      enrichments: [
        { field: 'email', lookup: async () => { throw new Error('API down'); } },
      ],
    });
    return result.then(r => {
      expect(r.data).not.toHaveProperty('email');
      expect(r.enrichments).toHaveLength(0);
    });
  });

  it('applies multiple enrichments', () => {
    const result = op.execute({
      data: { userId: 'u1' },
      enrichments: [
        { field: 'email', lookup: async () => 'u1@example.com' },
        { field: 'role', lookup: 'admin' },
        { field: 'team', lookup: async () => null, fallback: 'unassigned' },
      ],
    });
    return result.then(r => {
      expect(r.data.email).toBe('u1@example.com');
      expect(r.data.role).toBe('admin');
      expect(r.data.team).toBe('unassigned');
      expect(r.enrichments).toHaveLength(3);
    });
  });

  it('does not mutate original input', () => {
    const original = { userId: 'u1' };
    const result = op.execute({
      data: original,
      enrichments: [{ field: 'email', lookup: async () => 'u1@example.com' }],
    });
    return result.then(r => {
      expect(original).not.toHaveProperty('email');
    });
  });

  it('returns metrics with enrichment count', () => {
    const result = op.execute({
      data: { x: 1 },
      enrichments: [{ field: 'y', lookup: '2' }],
    });
    return result.then(r => {
      expect(r.metrics.enrichmentCount).toBe(1);
      expect(r.metrics.duration_ms).toBeGreaterThanOrEqual(0);
    });
  });
});
