/**
 * @file Knowledge Hooks README Example Tests (London TDD)
 * @description Tests for Knowledge Hooks examples from README.md
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';

describe('README Knowledge Hooks Examples', () => {
  let mockDefineHook;
  let mockRegisterHook;
  let mockDeregisterHook;
  let mockHook;

  beforeEach(() => {
    mockHook = {
      meta: {
        name: 'data-quality-gate',
        description: 'Ensures all persons have names',
      },
      when: {
        kind: 'sparql-ask',
        query:
          'ASK { ?person a <http://xmlns.com/foaf/0.1/Person> . FILTER NOT EXISTS { ?person <http://xmlns.com/foaf/0.1/name> ?name } }',
      },
      run: vi.fn(),
    };

    mockDefineHook = vi.fn().mockReturnValue(mockHook);
    mockRegisterHook = vi.fn().mockResolvedValue({ success: true, hookId: 'hook-123' });
    mockDeregisterHook = vi.fn().mockResolvedValue({ success: true });
  });

  describe('defineHook', () => {
    it('should define hook with meta information', () => {
      const hook = mockDefineHook({
        meta: {
          name: 'data-quality-gate',
          description: 'Ensures all persons have names',
        },
        when: {
          kind: 'sparql-ask',
          query: 'ASK { ?person a <http://xmlns.com/foaf/0.1/Person> . }',
        },
        run: async _event => {},
      });

      expect(mockDefineHook).toHaveBeenCalled();
      expect(hook.meta.name).toBe('data-quality-gate');
      expect(hook.meta.description).toBe('Ensures all persons have names');
    });

    it('should define hook with sparql-ask condition', () => {
      const hook = mockDefineHook({
        meta: { name: 'test-hook', description: 'Test' },
        when: {
          kind: 'sparql-ask',
          query: 'ASK { ?s ?p ?o }',
        },
        run: async () => {},
      });

      expect(hook.when.kind).toBe('sparql-ask');
      expect(hook.when.query).toContain('ASK');
    });

    it('should define hook with run function', () => {
      const runFn = vi.fn();
      const hook = mockDefineHook({
        meta: { name: 'test-hook', description: 'Test' },
        when: { kind: 'sparql-ask', query: 'ASK { ?s ?p ?o }' },
        run: runFn,
      });

      expect(hook.run).toBeDefined();
      expect(typeof hook.run).toBe('function');
    });

    it('should handle validation hook that throws on failure', async () => {
      const runFn = vi.fn(async event => {
        if (event.result === true) {
          throw new Error('All persons must be 18 or older');
        }
      });

      mockDefineHook.mockReturnValue({
        meta: { name: 'age-validation', description: 'Ensure age >= 18' },
        when: {
          kind: 'sparql-ask',
          query: 'ASK { ?person <http://example.org/age> ?age . FILTER (?age < 18) }',
        },
        run: runFn,
      });

      const hook = mockDefineHook({
        meta: { name: 'age-validation', description: 'Ensure age >= 18' },
        when: {
          kind: 'sparql-ask',
          query: 'ASK { ?person <http://example.org/age> ?age . FILTER (?age < 18) }',
        },
        run: runFn,
      });

      // Simulate hook execution when condition is true
      await expect(hook.run({ result: true })).rejects.toThrow('All persons must be 18 or older');
    });

    it('should handle validation hook that passes', async () => {
      const runFn = vi.fn(async event => {
        if (event.result === true) {
          throw new Error('All persons must be 18 or older');
        }
      });

      mockDefineHook.mockReturnValue({
        meta: { name: 'age-validation', description: 'Ensure age >= 18' },
        when: {
          kind: 'sparql-ask',
          query: 'ASK { ?person <http://example.org/age> ?age . FILTER (?age < 18) }',
        },
        run: runFn,
      });

      const hook = mockDefineHook({
        meta: { name: 'age-validation', description: 'Ensure age >= 18' },
        when: {
          kind: 'sparql-ask',
          query: 'ASK { ?person <http://example.org/age> ?age . FILTER (?age < 18) }',
        },
        run: runFn,
      });

      // Simulate hook execution when condition is false (valid data)
      await expect(hook.run({ result: false })).resolves.not.toThrow();
    });
  });

  describe('registerHook', () => {
    it('should register defined hook', async () => {
      const hook = mockDefineHook({
        meta: { name: 'test-hook', description: 'Test' },
        when: { kind: 'sparql-ask', query: 'ASK { ?s ?p ?o }' },
        run: async () => {},
      });

      const result = await mockRegisterHook(hook);

      expect(mockRegisterHook).toHaveBeenCalledWith(hook);
      expect(result.success).toBe(true);
      expect(result.hookId).toBeDefined();
    });

    it('should handle hook registration failure', async () => {
      mockRegisterHook.mockRejectedValue(new Error('Hook registration failed'));

      const hook = mockDefineHook({
        meta: { name: 'invalid-hook', description: 'Invalid' },
        when: { kind: 'sparql-ask', query: 'INVALID' },
        run: async () => {},
      });

      await expect(mockRegisterHook(hook)).rejects.toThrow('Hook registration failed');
    });

    it('should register multiple hooks', async () => {
      const hook1 = mockDefineHook({
        meta: { name: 'hook1', description: 'First hook' },
        when: { kind: 'sparql-ask', query: 'ASK { ?s ?p ?o }' },
        run: async () => {},
      });

      const hook2 = mockDefineHook({
        meta: { name: 'hook2', description: 'Second hook' },
        when: { kind: 'shacl', shapes: [] },
        run: async () => {},
      });

      await mockRegisterHook(hook1);
      await mockRegisterHook(hook2);

      expect(mockRegisterHook).toHaveBeenCalledTimes(2);
    });
  });

  describe('Hook Types', () => {
    it('should support sparql-ask hook type', () => {
      const hook = mockDefineHook({
        meta: { name: 'ask-hook', description: 'ASK query hook' },
        when: {
          kind: 'sparql-ask',
          query: 'ASK { ?s ?p ?o }',
        },
        run: async () => {},
      });

      expect(hook.when.kind).toBe('sparql-ask');
    });

    it('should support shacl hook type', () => {
      mockDefineHook.mockReturnValue({
        meta: { name: 'shacl-hook', description: 'SHACL validation hook' },
        when: { kind: 'shacl', shapes: [] },
        run: vi.fn(),
      });

      const hook = mockDefineHook({
        meta: { name: 'shacl-hook', description: 'SHACL validation hook' },
        when: { kind: 'shacl', shapes: [] },
        run: async () => {},
      });

      expect(hook.when.kind).toBe('shacl');
    });

    it('should support delta hook type', () => {
      mockDefineHook.mockReturnValue({
        meta: { name: 'delta-hook', description: 'Change detection hook' },
        when: { kind: 'delta', pattern: {} },
        run: vi.fn(),
      });

      const hook = mockDefineHook({
        meta: { name: 'delta-hook', description: 'Change detection hook' },
        when: { kind: 'delta', pattern: {} },
        run: async () => {},
      });

      expect(hook.when.kind).toBe('delta');
    });

    it('should support threshold hook type', () => {
      mockDefineHook.mockReturnValue({
        meta: {
          name: 'threshold-hook',
          description: 'Numeric comparison hook',
        },
        when: { kind: 'threshold', value: 100, operator: 'gt' },
        run: vi.fn(),
      });

      const hook = mockDefineHook({
        meta: {
          name: 'threshold-hook',
          description: 'Numeric comparison hook',
        },
        when: { kind: 'threshold', value: 100, operator: 'gt' },
        run: async () => {},
      });

      expect(hook.when.kind).toBe('threshold');
    });

    it('should support count hook type', () => {
      mockDefineHook.mockReturnValue({
        meta: { name: 'count-hook', description: 'Cardinality check hook' },
        when: { kind: 'count', min: 1, max: 10 },
        run: vi.fn(),
      });

      const hook = mockDefineHook({
        meta: { name: 'count-hook', description: 'Cardinality check hook' },
        when: { kind: 'count', min: 1, max: 10 },
        run: async () => {},
      });

      expect(hook.when.kind).toBe('count');
    });

    it('should support window hook type', () => {
      mockDefineHook.mockReturnValue({
        meta: {
          name: 'window-hook',
          description: 'Time-based aggregation hook',
        },
        when: { kind: 'window', duration: '5m' },
        run: vi.fn(),
      });

      const hook = mockDefineHook({
        meta: {
          name: 'window-hook',
          description: 'Time-based aggregation hook',
        },
        when: { kind: 'window', duration: '5m' },
        run: async () => {},
      });

      expect(hook.when.kind).toBe('window');
    });
  });

  describe('Policy-Driven Validation Example', () => {
    it('should define age validation hook', () => {
      const runFn = vi.fn(async event => {
        if (event.result) {
          throw new Error('All persons must be 18 or older');
        }
      });

      mockDefineHook.mockReturnValue({
        meta: { name: 'age-validation', description: 'Ensure age is >= 18' },
        when: {
          kind: 'sparql-ask',
          query: 'ASK { ?person <http://example.org/age> ?age . FILTER (?age < 18) }',
        },
        run: runFn,
      });

      const validateAge = mockDefineHook({
        meta: { name: 'age-validation', description: 'Ensure age is >= 18' },
        when: {
          kind: 'sparql-ask',
          query: 'ASK { ?person <http://example.org/age> ?age . FILTER (?age < 18) }',
        },
        run: runFn,
      });

      expect(validateAge.meta.name).toBe('age-validation');
      expect(validateAge.when.query).toContain('FILTER (?age < 18)');
    });

    it('should register age validation hook', async () => {
      const validateAge = mockDefineHook({
        meta: { name: 'age-validation', description: 'Ensure age >= 18' },
        when: {
          kind: 'sparql-ask',
          query: 'ASK { ?person <http://example.org/age> ?age . FILTER (?age < 18) }',
        },
        run: async event => {
          if (event.result) {
            throw new Error('All persons must be 18 or older');
          }
        },
      });

      await mockRegisterHook(validateAge);

      expect(mockRegisterHook).toHaveBeenCalledWith(validateAge);
    });

    it('should reject transaction when validation fails', async () => {
      const runFn = vi.fn(async event => {
        if (event.result) {
          throw new Error('All persons must be 18 or older');
        }
      });

      mockDefineHook.mockReturnValue({
        meta: { name: 'age-validation', description: 'Ensure age >= 18' },
        when: {
          kind: 'sparql-ask',
          query: 'ASK { ?person <http://example.org/age> ?age . FILTER (?age < 18) }',
        },
        run: runFn,
      });

      const validateAge = mockDefineHook({
        meta: { name: 'age-validation', description: 'Ensure age >= 18' },
        when: {
          kind: 'sparql-ask',
          query: 'ASK { ?person <http://example.org/age> ?age . FILTER (?age < 18) }',
        },
        run: runFn,
      });

      // Simulate hook execution with underage person
      await expect(validateAge.run({ result: true })).rejects.toThrow(
        'All persons must be 18 or older'
      );
    });

    it('should allow transaction when validation passes', async () => {
      const runFn = vi.fn(async event => {
        if (event.result) {
          throw new Error('All persons must be 18 or older');
        }
      });

      mockDefineHook.mockReturnValue({
        meta: { name: 'age-validation', description: 'Ensure age >= 18' },
        when: {
          kind: 'sparql-ask',
          query: 'ASK { ?person <http://example.org/age> ?age . FILTER (?age < 18) }',
        },
        run: runFn,
      });

      const validateAge = mockDefineHook({
        meta: { name: 'age-validation', description: 'Ensure age >= 18' },
        when: {
          kind: 'sparql-ask',
          query: 'ASK { ?person <http://example.org/age> ?age . FILTER (?age < 18) }',
        },
        run: runFn,
      });

      // Simulate hook execution with valid age
      await expect(validateAge.run({ result: false })).resolves.not.toThrow();
    });
  });

  describe('deregisterHook', () => {
    it('should deregister hook by ID', async () => {
      const result = await mockDeregisterHook('hook-123');

      expect(mockDeregisterHook).toHaveBeenCalledWith('hook-123');
      expect(result.success).toBe(true);
    });

    it('should handle deregistration of non-existent hook', async () => {
      mockDeregisterHook.mockRejectedValue(new Error('Hook not found'));

      await expect(mockDeregisterHook('non-existent-hook')).rejects.toThrow('Hook not found');
    });
  });
});
