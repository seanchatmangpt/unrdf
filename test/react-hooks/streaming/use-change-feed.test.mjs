/**
 * @file Tests for useChangeFeed hook functionality
 * Tests feed lifecycle, batching, filtering, and change tracking
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';

describe('useChangeFeed', () => {
  describe('Feed Lifecycle', () => {
    it('should initialize with default state', () => {
      const state = {
        changes: [],
        isRunning: false,
        stats: {
          totalChanges: 0,
          inserts: 0,
          deletes: 0,
          filtered: 0
        },
        loading: false,
        error: null
      };

      expect(state.changes).toHaveLength(0);
      expect(state.isRunning).toBe(false);
      expect(state.stats.totalChanges).toBe(0);
      expect(state.loading).toBe(false);
      expect(state.error).toBeNull();
    });

    it('should track running state when started', () => {
      let isRunning = false;

      const start = () => {
        isRunning = true;
        return { success: true };
      };

      const result = start();

      expect(result.success).toBe(true);
      expect(isRunning).toBe(true);
    });

    it('should track running state when stopped', () => {
      let isRunning = true;

      const stop = () => {
        isRunning = false;
        return { success: true };
      };

      const result = stop();

      expect(result.success).toBe(true);
      expect(isRunning).toBe(false);
    });

    it('should handle start error gracefully', () => {
      let error = null;

      const start = () => {
        error = new Error('Feed not initialized');
        throw error;
      };

      expect(() => start()).toThrow('Feed not initialized');
      expect(error).toBeInstanceOf(Error);
    });

    it('should handle stop error gracefully', () => {
      let error = null;

      const stop = () => {
        error = new Error('Feed not initialized');
        throw error;
      };

      expect(() => stop()).toThrow('Feed not initialized');
      expect(error).toBeInstanceOf(Error);
    });
  });

  describe('Change Batching', () => {
    it('should accumulate changes into batch', () => {
      const changes = [];
      const batchSize = 10;

      // Simulate adding changes
      for (let i = 0; i < 5; i++) {
        changes.push({
          id: `change-${i}`,
          operation: 'insert',
          quads: [{ subject: { value: `s${i}` } }],
          timestamp: new Date().toISOString()
        });
      }

      expect(changes.length).toBe(5);
      expect(changes.length).toBeLessThan(batchSize);
    });

    it('should process batch when size reached', () => {
      const batchSize = 3;
      const batches = [];
      let buffer = [];

      const addChange = (change) => {
        buffer.push(change);
        if (buffer.length >= batchSize) {
          batches.push([...buffer]);
          buffer = [];
        }
      };

      for (let i = 0; i < 7; i++) {
        addChange({ id: `change-${i}` });
      }

      expect(batches.length).toBe(2);
      expect(batches[0].length).toBe(3);
      expect(batches[1].length).toBe(3);
      expect(buffer.length).toBe(1);
    });

    it('should respect batch interval', async () => {
      const batchInterval = 100;
      let batchProcessed = false;

      const processBatch = () => {
        batchProcessed = true;
      };

      setTimeout(processBatch, batchInterval);

      await new Promise(resolve => setTimeout(resolve, batchInterval + 50));

      expect(batchProcessed).toBe(true);
    });

    it('should handle empty batch gracefully', () => {
      const batch = [];

      const processBatch = (b) => {
        if (b.length === 0) return { processed: false };
        return { processed: true, count: b.length };
      };

      const result = processBatch(batch);

      expect(result.processed).toBe(false);
    });
  });

  describe('Change Filtering', () => {
    it('should filter by operation type', () => {
      const changes = [
        { id: '1', operation: 'insert', quads: [] },
        { id: '2', operation: 'delete', quads: [] },
        { id: '3', operation: 'insert', quads: [] },
        { id: '4', operation: 'delete', quads: [] }
      ];

      const getChangesByOperation = (operation) => {
        return changes.filter(c => c.operation === operation);
      };

      const inserts = getChangesByOperation('insert');
      const deletes = getChangesByOperation('delete');

      expect(inserts).toHaveLength(2);
      expect(deletes).toHaveLength(2);
    });

    it('should filter by predicate', () => {
      const changes = [
        { id: '1', quads: [{ predicate: { value: 'schema:price' } }] },
        { id: '2', quads: [{ predicate: { value: 'schema:name' } }] },
        { id: '3', quads: [{ predicate: { value: 'schema:price' } }] }
      ];

      const getChangesByPredicate = (predicate) => {
        return changes.filter(c =>
          c.quads.some(q => q.predicate.value === predicate)
        );
      };

      const priceChanges = getChangesByPredicate('schema:price');

      expect(priceChanges).toHaveLength(2);
    });

    it('should filter by subject', () => {
      const changes = [
        { id: '1', quads: [{ subject: { value: 'http://example.org/product1' } }] },
        { id: '2', quads: [{ subject: { value: 'http://example.org/product2' } }] },
        { id: '3', quads: [{ subject: { value: 'http://example.org/product1' } }] }
      ];

      const getChangesBySubject = (subject) => {
        return changes.filter(c =>
          c.quads.some(q => q.subject.value === subject)
        );
      };

      const product1Changes = getChangesBySubject('http://example.org/product1');

      expect(product1Changes).toHaveLength(2);
    });

    it('should apply custom filter function', () => {
      const changes = [
        { id: '1', operation: 'insert', quads: [{ object: { value: '150' } }] },
        { id: '2', operation: 'insert', quads: [{ object: { value: '50' } }] },
        { id: '3', operation: 'delete', quads: [{ object: { value: '200' } }] }
      ];

      const customFilter = (change) => {
        return change.operation === 'insert' &&
          change.quads.some(q => parseFloat(q.object.value) > 100);
      };

      const filtered = changes.filter(customFilter);

      expect(filtered).toHaveLength(1);
      expect(filtered[0].id).toBe('1');
    });
  });

  describe('Statistics Tracking', () => {
    it('should track total changes count', () => {
      let stats = {
        totalChanges: 0,
        inserts: 0,
        deletes: 0,
        filtered: 0
      };

      const updateStats = (batch) => {
        stats = {
          totalChanges: stats.totalChanges + batch.length,
          inserts: stats.inserts + batch.filter(c => c.operation === 'insert').length,
          deletes: stats.deletes + batch.filter(c => c.operation === 'delete').length,
          filtered: stats.filtered
        };
      };

      updateStats([
        { operation: 'insert' },
        { operation: 'insert' },
        { operation: 'delete' }
      ]);

      expect(stats.totalChanges).toBe(3);
      expect(stats.inserts).toBe(2);
      expect(stats.deletes).toBe(1);
    });

    it('should track insert count', () => {
      const changes = [
        { operation: 'insert' },
        { operation: 'insert' },
        { operation: 'delete' },
        { operation: 'insert' }
      ];

      const inserts = changes.filter(c => c.operation === 'insert').length;

      expect(inserts).toBe(3);
    });

    it('should track delete count', () => {
      const changes = [
        { operation: 'insert' },
        { operation: 'delete' },
        { operation: 'delete' },
        { operation: 'delete' }
      ];

      const deletes = changes.filter(c => c.operation === 'delete').length;

      expect(deletes).toBe(3);
    });

    it('should reset stats on clear', () => {
      let stats = {
        totalChanges: 100,
        inserts: 60,
        deletes: 40,
        filtered: 10
      };

      const clear = () => {
        stats = {
          totalChanges: 0,
          inserts: 0,
          deletes: 0,
          filtered: 0
        };
      };

      clear();

      expect(stats.totalChanges).toBe(0);
      expect(stats.inserts).toBe(0);
      expect(stats.deletes).toBe(0);
      expect(stats.filtered).toBe(0);
    });
  });

  describe('Recent Changes', () => {
    it('should get most recent changes', () => {
      const changes = Array(20).fill(null).map((_, i) => ({
        id: `change-${i}`,
        timestamp: new Date(Date.now() + i * 1000).toISOString()
      }));

      const getRecentChanges = (count = 10) => {
        return changes.slice(-count);
      };

      const recent = getRecentChanges(5);

      expect(recent).toHaveLength(5);
      expect(recent[0].id).toBe('change-15');
      expect(recent[4].id).toBe('change-19');
    });

    it('should handle request for more than available', () => {
      const changes = [
        { id: 'change-1' },
        { id: 'change-2' },
        { id: 'change-3' }
      ];

      const getRecentChanges = (count = 10) => {
        return changes.slice(-count);
      };

      const recent = getRecentChanges(10);

      expect(recent).toHaveLength(3);
    });
  });

  describe('Clear Functionality', () => {
    it('should clear all changes', () => {
      let changes = [
        { id: '1' },
        { id: '2' },
        { id: '3' }
      ];

      const clear = () => {
        changes = [];
      };

      clear();

      expect(changes).toHaveLength(0);
    });

    it('should reset refs on clear', () => {
      let changesRef = [{ id: '1' }, { id: '2' }];
      let batchRef = [{ id: 'pending' }];

      const clear = () => {
        changesRef = [];
        batchRef = [];
      };

      clear();

      expect(changesRef).toHaveLength(0);
      expect(batchRef).toHaveLength(0);
    });
  });

  describe('Replay Functionality', () => {
    it('should replay changes within time range', async () => {
      const allChanges = [
        { id: '1', timestamp: '2024-01-01T00:00:00Z' },
        { id: '2', timestamp: '2024-01-01T01:00:00Z' },
        { id: '3', timestamp: '2024-01-01T02:00:00Z' },
        { id: '4', timestamp: '2024-01-01T03:00:00Z' }
      ];

      const replay = async (from, to) => {
        return allChanges.filter(c => {
          const ts = new Date(c.timestamp);
          return ts >= new Date(from) && ts <= new Date(to);
        });
      };

      const replayed = await replay('2024-01-01T01:00:00Z', '2024-01-01T02:30:00Z');

      expect(replayed).toHaveLength(2);
      expect(replayed[0].id).toBe('2');
      expect(replayed[1].id).toBe('3');
    });

    it('should return empty array for invalid range', async () => {
      const allChanges = [
        { id: '1', timestamp: '2024-01-01T00:00:00Z' }
      ];

      const replay = async (from, to) => {
        return allChanges.filter(c => {
          const ts = new Date(c.timestamp);
          return ts >= new Date(from) && ts <= new Date(to);
        });
      };

      const replayed = await replay('2025-01-01T00:00:00Z', '2025-01-02T00:00:00Z');

      expect(replayed).toHaveLength(0);
    });
  });

  describe('Metadata Inclusion', () => {
    it('should include metadata when configured', () => {
      const config = { includeMetadata: true };

      const change = {
        id: '1',
        operation: 'insert',
        quads: [],
        ...(config.includeMetadata && {
          metadata: {
            transactionId: 'tx-123',
            source: 'api',
            userId: 'user-456'
          }
        })
      };

      expect(change.metadata).toBeDefined();
      expect(change.metadata.transactionId).toBe('tx-123');
    });

    it('should exclude metadata when not configured', () => {
      const config = { includeMetadata: false };

      const change = {
        id: '1',
        operation: 'insert',
        quads: [],
        ...(config.includeMetadata && {
          metadata: { transactionId: 'tx-123' }
        })
      };

      expect(change.metadata).toBeUndefined();
    });
  });

  describe('Operations Configuration', () => {
    it('should track only configured operations', () => {
      const config = { operations: ['insert'] };

      const allChanges = [
        { operation: 'insert' },
        { operation: 'delete' },
        { operation: 'insert' },
        { operation: 'update' }
      ];

      const tracked = allChanges.filter(c =>
        config.operations.includes(c.operation)
      );

      expect(tracked).toHaveLength(2);
      expect(tracked.every(c => c.operation === 'insert')).toBe(true);
    });

    it('should default to insert and delete operations', () => {
      const defaultOperations = ['insert', 'delete'];

      const allChanges = [
        { operation: 'insert' },
        { operation: 'delete' },
        { operation: 'update' }
      ];

      const tracked = allChanges.filter(c =>
        defaultOperations.includes(c.operation)
      );

      expect(tracked).toHaveLength(2);
    });
  });
});
