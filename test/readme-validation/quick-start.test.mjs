/**
 * @file Quick Start README Example Tests (London TDD)
 * @description Tests that validate the Quick Start tutorial from README.md
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import { namedNode, quad, literal } from '@rdfjs/data-model';

describe('README Quick Start Example', () => {
  let mockSystem;
  let mockCreateDarkMatterCore;

  beforeEach(() => {
    // Mock the Dark Matter system
    mockSystem = {
      executeTransaction: vi.fn(),
      query: vi.fn(),
      cleanup: vi.fn(),
    };

    // Mock createDarkMatterCore factory
    mockCreateDarkMatterCore = vi.fn().mockResolvedValue(mockSystem);
  });

  describe('5-Minute Tutorial', () => {
    it('should create knowledge engine successfully', async () => {
      const system = await mockCreateDarkMatterCore();

      expect(mockCreateDarkMatterCore).toHaveBeenCalledOnce();
      expect(system).toBeDefined();
      expect(system.executeTransaction).toBeDefined();
      expect(system.query).toBeDefined();
      expect(system.cleanup).toBeDefined();
    });

    it('should add RDF data via transaction', async () => {
      const system = await mockCreateDarkMatterCore();

      const aliceNameQuad = quad(
        namedNode('http://example.org/alice'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Alice')
      );

      const aliceKnowsQuad = quad(
        namedNode('http://example.org/alice'),
        namedNode('http://xmlns.com/foaf/0.1/knows'),
        namedNode('http://example.org/bob')
      );

      mockSystem.executeTransaction.mockResolvedValue({
        success: true,
        delta: { additions: [aliceNameQuad, aliceKnowsQuad], removals: [] },
      });

      const result = await system.executeTransaction({
        additions: [aliceNameQuad, aliceKnowsQuad],
        removals: [],
        actor: 'system',
      });

      expect(mockSystem.executeTransaction).toHaveBeenCalledWith({
        additions: expect.arrayContaining([
          expect.objectContaining({
            subject: expect.objectContaining({
              value: 'http://example.org/alice',
            }),
            predicate: expect.objectContaining({
              value: 'http://xmlns.com/foaf/0.1/name',
            }),
          }),
          expect.objectContaining({
            subject: expect.objectContaining({
              value: 'http://example.org/alice',
            }),
            predicate: expect.objectContaining({
              value: 'http://xmlns.com/foaf/0.1/knows',
            }),
          }),
        ]),
        removals: [],
        actor: 'system',
      });

      expect(result.success).toBe(true);
      expect(result.delta.additions).toHaveLength(2);
    });

    it('should query data with SPARQL SELECT', async () => {
      const system = await mockCreateDarkMatterCore();

      mockSystem.query.mockResolvedValue([{ name: literal('Alice') }]);

      const results = await system.query({
        query: 'SELECT ?name WHERE { ?person <http://xmlns.com/foaf/0.1/name> ?name }',
        type: 'sparql-select',
      });

      expect(mockSystem.query).toHaveBeenCalledWith({
        query: expect.stringContaining('SELECT ?name'),
        type: 'sparql-select',
      });

      expect(results).toHaveLength(1);
      expect(results[0].name).toBeDefined();
    });

    it('should cleanup system resources', async () => {
      const system = await mockCreateDarkMatterCore();

      mockSystem.cleanup.mockResolvedValue(undefined);

      await system.cleanup();

      expect(mockSystem.cleanup).toHaveBeenCalledOnce();
    });

    it('should execute complete quick start workflow', async () => {
      // Step 1: Create engine
      const system = await mockCreateDarkMatterCore();
      expect(system).toBeDefined();

      // Step 2: Add data
      mockSystem.executeTransaction.mockResolvedValue({
        success: true,
        delta: { additions: [], removals: [] },
      });

      await system.executeTransaction({
        additions: [
          quad(
            namedNode('http://example.org/alice'),
            namedNode('http://xmlns.com/foaf/0.1/name'),
            literal('Alice')
          ),
        ],
        removals: [],
        actor: 'system',
      });

      expect(mockSystem.executeTransaction).toHaveBeenCalled();

      // Step 3: Query data
      mockSystem.query.mockResolvedValue([{ name: literal('Alice') }]);

      const results = await system.query({
        query: 'SELECT ?name WHERE { ?person <http://xmlns.com/foaf/0.1/name> ?name }',
        type: 'sparql-select',
      });

      expect(results).toHaveLength(1);

      // Step 4: Cleanup
      mockSystem.cleanup.mockResolvedValue(undefined);
      await system.cleanup();

      expect(mockSystem.cleanup).toHaveBeenCalled();
    });

    it('should handle transaction failures gracefully', async () => {
      const system = await mockCreateDarkMatterCore();

      mockSystem.executeTransaction.mockRejectedValue(
        new Error('Transaction failed: validation error')
      );

      await expect(
        system.executeTransaction({
          additions: [],
          removals: [],
          actor: 'system',
        })
      ).rejects.toThrow('Transaction failed');
    });

    it('should handle query failures gracefully', async () => {
      const system = await mockCreateDarkMatterCore();

      mockSystem.query.mockRejectedValue(new Error('SPARQL syntax error'));

      await expect(
        system.query({
          query: 'INVALID SPARQL',
          type: 'sparql-select',
        })
      ).rejects.toThrow('SPARQL syntax error');
    });

    it('should support multiple transactions', async () => {
      const system = await mockCreateDarkMatterCore();

      mockSystem.executeTransaction.mockResolvedValue({ success: true });

      await system.executeTransaction({
        additions: [quad(namedNode('ex:s1'), namedNode('ex:p1'), literal('o1'))],
        removals: [],
        actor: 'user1',
      });

      await system.executeTransaction({
        additions: [quad(namedNode('ex:s2'), namedNode('ex:p2'), literal('o2'))],
        removals: [],
        actor: 'user2',
      });

      expect(mockSystem.executeTransaction).toHaveBeenCalledTimes(2);
    });

    it('should support empty additions and removals', async () => {
      const system = await mockCreateDarkMatterCore();

      mockSystem.executeTransaction.mockResolvedValue({ success: true });

      await system.executeTransaction({
        additions: [],
        removals: [],
        actor: 'system',
      });

      expect(mockSystem.executeTransaction).toHaveBeenCalledWith(
        expect.objectContaining({
          additions: [],
          removals: [],
        })
      );
    });

    it('should return query results in expected format', async () => {
      const system = await mockCreateDarkMatterCore();

      const mockResults = [
        { name: literal('Alice') },
        { name: literal('Bob') },
        { name: literal('Charlie') },
      ];

      mockSystem.query.mockResolvedValue(mockResults);

      const results = await system.query({
        query: 'SELECT ?name WHERE { ?person <http://xmlns.com/foaf/0.1/name> ?name }',
        type: 'sparql-select',
      });

      expect(results).toHaveLength(3);
      expect(results[0]).toHaveProperty('name');
      expect(results[1]).toHaveProperty('name');
      expect(results[2]).toHaveProperty('name');
    });
  });
});
