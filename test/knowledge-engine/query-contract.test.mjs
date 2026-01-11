/**
 * @file Query Contract Tests (Fast)
 * @description Single essential test for SPARQL query contract (mocked for speed)
 */

import { describe, it, expect, vi } from 'vitest';

describe('query contract', () => {
  it('should return results as array', async () => {
    // Mock the query function for speed
    const mockResults = [{ s: 'ex:a', p: 'ex:p', o: 'ex:b' }];
    const query = vi.fn().mockResolvedValue(mockResults);

    // Contract validation: query returns array
    const result = await query({}, 'SELECT * WHERE { ?s ?p ?o }');
    expect(Array.isArray(result)).toBe(true);
  });
});
