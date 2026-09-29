/**
 * @file Regression test: 'semantic-inference' conditions must dispatch to a defined evaluator.
 * Previously evaluateCondition referenced an undefined evaluateSemanticInference (ReferenceError).
 */
import { describe, it, expect, vi } from 'vitest';

const { executeSemanticQuery } = vi.hoisted(() => ({ executeSemanticQuery: vi.fn() }));
vi.mock('@unrdf/core/utils/semantic-bridge', () => ({ executeSemanticQuery }));

const { evaluateCondition } = await import('../src/hooks/condition-evaluator.mjs');

const graph = { getQuads: () => [], dump: async () => '' };
const condition = { kind: 'semantic-inference', query: 'SELECT ?s WHERE { ?s ?p ?o }' };

describe('semantic-inference condition', () => {
  it('returns true when the reasoner yields results', async () => {
    executeSemanticQuery.mockResolvedValueOnce({ results: [{ s: 'x' }] });
    expect(await evaluateCondition(condition, graph)).toBe(true);
    expect(executeSemanticQuery).toHaveBeenCalledWith(condition.query, {
      ontologyFiles: [],
      rawTriples: [],
    });
  });

  it('returns false when the reasoner yields no results', async () => {
    executeSemanticQuery.mockResolvedValueOnce({ results: [] });
    expect(await evaluateCondition(condition, graph)).toBe(false);
  });

  it('surfaces engine errors instead of a ReferenceError', async () => {
    executeSemanticQuery.mockResolvedValueOnce({ error: 'boom' });
    await expect(evaluateCondition(condition, graph)).rejects.toThrow(
      /Semantic inference failed: boom/
    );
  });

  it('rejects a condition without a query', async () => {
    await expect(evaluateCondition({ kind: 'semantic-inference' }, graph)).rejects.toThrow(
      /requires a query/
    );
  });
});
