/**
 * @file Dependency Tracker Tests
 * @module @unrdf/caching/test
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { DependencyTracker, createDependencyTracker, extractQuerySubjects } from '../src/invalidation/dependency-tracker.mjs';

describe('DependencyTracker', () => {
  let tracker;
  let mockCache;

  beforeEach(() => {
    mockCache = {
      deletePattern: async (pattern) => {
        return 0; // Return count of deleted keys
      }
    };

    tracker = new DependencyTracker(mockCache);
  });

  describe('trackQueryDependency', () => {
    it('should track subject dependencies', async () => {
      await tracker.trackQueryDependency('query1', ['http://example.org/s1', 'http://example.org/s2']);

      const deps = tracker.getDependencies('query1');
      expect(deps).toHaveLength(2);
      expect(deps).toContain('http://example.org/s1');
    });

    it('should deduplicate subjects', async () => {
      await tracker.trackQueryDependency('query1', ['http://example.org/s1', 'http://example.org/s1']);

      const deps = tracker.getDependencies('query1');
      expect(deps).toHaveLength(1);
    });
  });

  describe('invalidateSubject', () => {
    it('should invalidate queries depending on subject', async () => {
      await tracker.trackQueryDependency('query1', ['http://example.org/s1']);
      await tracker.trackQueryDependency('query2', ['http://example.org/s2']);

      const invalidated = await tracker.invalidateSubject('http://example.org/s1');

      expect(invalidated).toContain('query1');
      expect(invalidated).not.toContain('query2');
    });
  });

  describe('clear', () => {
    it('should clear all dependencies', async () => {
      await tracker.trackQueryDependency('query1', ['http://example.org/s1']);
      tracker.clear();

      const deps = tracker.getDependencies('query1');
      expect(deps).toBeUndefined();
    });
  });

  describe('getStats', () => {
    it('should return tracking statistics', async () => {
      await tracker.trackQueryDependency('query1', ['http://example.org/s1', 'http://example.org/s2']);
      await tracker.trackQueryDependency('query2', ['http://example.org/s3']);

      const stats = tracker.getStats();

      expect(stats.totalQueries).toBe(2);
      expect(stats.totalSubjects).toBeGreaterThan(0);
    });
  });
});

describe('extractQuerySubjects', () => {
  it('should extract subjects from SPARQL query', () => {
    const query = 'SELECT * WHERE { <http://example.org/s1> ?p ?o . <http://example.org/s2> ?p2 ?o2 }';
    const subjects = extractQuerySubjects(query);

    expect(subjects).toContain('http://example.org/s1');
    expect(subjects).toContain('http://example.org/s2');
  });

  it('should handle variable subjects', () => {
    const query = 'SELECT * WHERE { ?s ?p ?o }';
    const subjects = extractQuerySubjects(query);

    // Variable subjects mean query depends on all data
    expect(subjects).toHaveLength(0);
  });
});

describe('createDependencyTracker', () => {
  it('should create tracker instance', () => {
    const mockCache = { deletePattern: async () => 0 };
    const tracker = createDependencyTracker(mockCache);

    expect(tracker).toBeInstanceOf(DependencyTracker);
  });
});
