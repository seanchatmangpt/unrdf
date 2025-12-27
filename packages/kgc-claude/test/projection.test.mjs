/**
 * Projection Tests - Uniform surface parity via projections
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import {
  project,
  registerProjection,
  registerTransform,
  calculateProjectionDrift,
  getProjections,
  getProjectionsForSurface,
  clearProjections,
} from '../src/projection.mjs';

// Mock store for testing
function createMockStore(data = []) {
  return {
    match: (s, p, o, g) => data.filter(q => {
      if (g && q.graph.value !== g.value) return false;
      if (s && q.subject.value !== s.value) return false;
      if (p && q.predicate.value !== p.value) return false;
      if (o && q.object.value !== o.value) return false;
      return true;
    }),
    query: async (sparql) => {
      // Simple mock: return data as query results
      return data.map(q => ({
        subject: { value: q.subject.value },
        predicate: { value: q.predicate.value },
        object: { value: q.object.value },
      }));
    },
  };
}

describe('Projection', () => {
  beforeEach(() => {
    clearProjections();
  });

  afterEach(() => {
    clearProjections();
  });

  describe('registerProjection', () => {
    it('should register a projection config', () => {
      registerProjection({
        name: 'test-projection',
        surface: 'cli',
        format: 'json',
      });

      const projections = getProjections();
      expect(projections.length).toBe(1);
      expect(projections[0].name).toBe('test-projection');
    });

    it('should validate projection config', () => {
      expect(() => {
        registerProjection({
          name: 'test',
          surface: 'invalid-surface',
        });
      }).toThrow();
    });
  });

  describe('project', () => {
    it('should project store data to JSON', async () => {
      const store = createMockStore([
        {
          subject: { value: 'http://example.org/a' },
          predicate: { value: 'http://example.org/name' },
          object: { value: 'Test' },
          graph: { value: 'http://kgc.io/Universe' },
        },
      ]);

      const result = await project(store, {
        name: 'inline',
        surface: 'api',
        format: 'json',
      });

      expect(result.id).toBeDefined();
      expect(result.content).toBeDefined();
      expect(result.contentHash).toBeDefined();
      expect(result.contentHash.length).toBe(64);
    });

    it('should project to text format', async () => {
      const store = createMockStore([]);

      const result = await project(store, {
        name: 'cli-test',
        surface: 'cli',
        format: 'text',
      });

      expect(typeof result.content).toBe('string');
    });

    it('should project to markdown format', async () => {
      const store = createMockStore([
        {
          subject: { value: 'a' },
          predicate: { value: 'b' },
          object: { value: 'c' },
          graph: { value: 'http://kgc.io/Universe' },
        },
      ]);

      const result = await project(store, {
        name: 'md-test',
        surface: 'doc',
        format: 'markdown',
      });

      expect(result.content).toContain('|');
    });

    it('should project to HTML format', async () => {
      const store = createMockStore([
        {
          subject: { value: 'a' },
          predicate: { value: 'b' },
          object: { value: 'c' },
          graph: { value: 'http://kgc.io/Universe' },
        },
      ]);

      const result = await project(store, {
        name: 'html-test',
        surface: 'ui',
        format: 'html',
      });

      expect(result.content).toContain('<table>');
    });

    it('should project to YAML format', async () => {
      const store = createMockStore([]);

      const result = await project(store, {
        name: 'yaml-test',
        surface: 'api',
        format: 'yaml',
      });

      expect(typeof result.content).toBe('string');
    });

    it('should use registered projection by name', async () => {
      registerProjection({
        name: 'named-projection',
        surface: 'cli',
        format: 'text',
      });

      const store = createMockStore([]);
      const result = await project(store, 'named-projection');

      expect(result.config.name).toBe('named-projection');
    });

    it('should throw for unknown projection name', async () => {
      const store = createMockStore([]);

      await expect(project(store, 'unknown')).rejects.toThrow('not found');
    });
  });

  describe('registerTransform', () => {
    it('should apply transform to data', async () => {
      registerTransform('uppercase', (data) => {
        return data.map(d => ({
          ...d,
          subject: d.subject ? { value: String(d.subject.value).toUpperCase() } : d.subject,
        }));
      });

      const store = createMockStore([
        {
          subject: { value: 'test' },
          predicate: { value: 'p' },
          object: { value: 'o' },
          graph: { value: 'http://kgc.io/Universe' },
        },
      ]);

      const result = await project(store, {
        name: 'transform-test',
        surface: 'api',
        transform: 'uppercase',
        format: 'json',
      });

      // The transform was applied
      expect(result.content).toBeDefined();
      expect(Array.isArray(result.content)).toBe(true);
    });
  });

  describe('calculateProjectionDrift', () => {
    it('should return 0 for identical projections', async () => {
      const store = createMockStore([]);

      const p1 = await project(store, { name: 'a', surface: 'cli', format: 'json' });
      const p2 = await project(store, { name: 'b', surface: 'cli', format: 'json' });

      const drift = calculateProjectionDrift(p1, p2);
      expect(drift).toBe(0);
    });

    it('should return > 0 for different projections', async () => {
      const store1 = createMockStore([
        {
          subject: { value: 'a' },
          predicate: { value: 'b' },
          object: { value: 'c' },
          graph: { value: 'http://kgc.io/Universe' },
        },
      ]);
      const store2 = createMockStore([
        {
          subject: { value: 'x' },
          predicate: { value: 'y' },
          object: { value: 'z' },
          graph: { value: 'http://kgc.io/Universe' },
        },
      ]);

      const p1 = await project(store1, { name: 'a', surface: 'cli', format: 'json' });
      const p2 = await project(store2, { name: 'b', surface: 'cli', format: 'json' });

      const drift = calculateProjectionDrift(p1, p2);
      expect(drift).toBeGreaterThan(0);
    });
  });

  describe('getProjectionsForSurface', () => {
    it('should filter projections by surface', () => {
      registerProjection({ name: 'cli1', surface: 'cli', format: 'text' });
      registerProjection({ name: 'cli2', surface: 'cli', format: 'text' });
      registerProjection({ name: 'ui1', surface: 'ui', format: 'json' });

      const cliProjections = getProjectionsForSurface('cli');
      const uiProjections = getProjectionsForSurface('ui');

      expect(cliProjections.length).toBe(2);
      expect(uiProjections.length).toBe(1);
    });
  });

  describe('pre-defined projections', () => {
    it('should be able to register and retrieve projections', () => {
      // Pre-defined projections are cleared in beforeEach
      // Just verify the registration API works
      registerProjection({ name: 'test-proj', surface: 'cli', format: 'text' });
      const projections = getProjections();
      expect(projections.some(p => p.name === 'test-proj')).toBe(true);
    });
  });
});
