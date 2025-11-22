/**
 * @file Tests for Change Feed
 */

import { describe, it, expect, beforeEach, afterEach, _vi } from 'vitest';
import {
  ChangeFeed,
  ChangeType,
  createChangeFeedHook,
} from '../../src/knowledge-engine/streaming/change-feed.mjs';

describe('ChangeFeed', () => {
  let feed;

  beforeEach(() => {
    feed = new ChangeFeed({
      enableHistory: true,
      historySize: 100,
      batchMode: false,
    });
    feed.start();
  });

  afterEach(async () => {
    await feed.cleanup();
  });

  describe('Change Recording', () => {
    it('should record add changes', async () => {
      const delta = {
        additions: [{ subject: 'alice', predicate: 'knows', object: 'bob' }],
        removals: [],
      };

      const eventPromise = new Promise(resolve => {
        feed.on('change', change => {
          expect(change.type).toBe(ChangeType.ADD);
          expect(change.delta.additions).toHaveLength(1);
          resolve();
        });

        feed.recordChange(delta);
      });

      await eventPromise;
    });

    it('should record delete changes', async () => {
      const delta = {
        additions: [],
        removals: [{ subject: 'alice', predicate: 'knows', object: 'bob' }],
      };

      const eventPromise = new Promise(resolve => {
        feed.on('change', change => {
          expect(change.type).toBe(ChangeType.DELETE);
          expect(change.delta.removals).toHaveLength(1);
          resolve();
        });

        feed.recordChange(delta);
      });

      await eventPromise;
    });

    it('should record update changes', async () => {
      const delta = {
        additions: [{ subject: 'alice', predicate: 'age', object: '31' }],
        removals: [{ subject: 'alice', predicate: 'age', object: '30' }],
      };

      const eventPromise = new Promise(resolve => {
        feed.on('change', change => {
          expect(change.type).toBe(ChangeType.UPDATE);
          expect(change.delta.additions).toHaveLength(1);
          expect(change.delta.removals).toHaveLength(1);
          resolve();
        });

        feed.recordChange(delta);
      });

      await eventPromise;
    });

    it('should include metadata in changes', async () => {
      const delta = {
        additions: [{ subject: 'alice', predicate: 'knows', object: 'bob' }],
        removals: [],
      };

      const metadata = {
        actor: 'alice',
        transactionId: 'tx-123',
        source: 'api',
      };

      const eventPromise = new Promise(resolve => {
        feed.on('change', change => {
          expect(change.metadata).toEqual(metadata);
          resolve();
        });

        feed.recordChange(delta, metadata);
      });

      await eventPromise;
    });
  });

  describe('History Management', () => {
    it('should maintain change history', () => {
      const delta1 = { additions: [{ subject: 'a' }], removals: [] };
      const delta2 = { additions: [{ subject: 'b' }], removals: [] };

      feed.recordChange(delta1);
      feed.recordChange(delta2);

      expect(feed.history).toHaveLength(2);
    });

    it('should trim history when exceeding size', () => {
      const smallFeed = new ChangeFeed({
        enableHistory: true,
        historySize: 2,
      });
      smallFeed.start();

      smallFeed.recordChange({ additions: [{ subject: 'a' }], removals: [] });
      smallFeed.recordChange({ additions: [{ subject: 'b' }], removals: [] });
      smallFeed.recordChange({ additions: [{ subject: 'c' }], removals: [] });

      expect(smallFeed.history).toHaveLength(2);
      expect(smallFeed.history[0].delta.additions[0].subject).toBe('b');

      smallFeed.cleanup();
    });

    it('should get change by ID', () => {
      const delta = { additions: [{ subject: 'alice' }], removals: [] };
      const id = feed.recordChange(delta);

      const change = feed.getChange(id);

      expect(change).toBeDefined();
      expect(change.id).toBe(id);
    });

    it('should get history with filters', () => {
      const now = Date.now();

      feed.recordChange({ additions: [{ subject: 'a' }], removals: [] });
      feed.recordChange({ additions: [], removals: [{ subject: 'b' }] });

      const addChanges = feed.getHistory({ type: ChangeType.ADD });
      expect(addChanges).toHaveLength(1);

      const deleteChanges = feed.getHistory({ type: ChangeType.DELETE });
      expect(deleteChanges).toHaveLength(1);

      const recent = feed.getHistory({ since: now });
      expect(recent).toHaveLength(2);
    });

    it('should clear history', () => {
      feed.recordChange({ additions: [{ subject: 'a' }], removals: [] });
      feed.recordChange({ additions: [{ subject: 'b' }], removals: [] });

      expect(feed.history).toHaveLength(2);

      feed.clearHistory();
      expect(feed.history).toHaveLength(0);
    });
  });

  describe('Batch Mode', () => {
    it('should batch changes', async () => {
      const batchFeed = new ChangeFeed({
        batchMode: true,
        batchSize: 3,
        batchInterval: 5000,
      });
      batchFeed.start();

      const eventPromise = new Promise(resolve => {
        batchFeed.on('batch', batch => {
          expect(batch.changes).toHaveLength(3);
          expect(batch.count).toBe(3);
          batchFeed.cleanup();
          resolve();
        });

        batchFeed.recordChange({ additions: [{ subject: 'a' }], removals: [] });
        batchFeed.recordChange({ additions: [{ subject: 'b' }], removals: [] });
        batchFeed.recordChange({ additions: [{ subject: 'c' }], removals: [] });
      });

      await eventPromise;
    });

    it('should flush batch on interval', async () => {
      const batchFeed = new ChangeFeed({
        batchMode: true,
        batchSize: 10,
        batchInterval: 100,
      });
      batchFeed.start();

      const eventPromise = new Promise(resolve => {
        batchFeed.on('batch', batch => {
          expect(batch.changes).toHaveLength(2);
          batchFeed.cleanup();
          resolve();
        });

        batchFeed.recordChange({ additions: [{ subject: 'a' }], removals: [] });
        batchFeed.recordChange({ additions: [{ subject: 'b' }], removals: [] });
      });

      await eventPromise;
    });
  });

  describe('Metrics', () => {
    it('should track changes processed', () => {
      feed.recordChange({ additions: [{ subject: 'a' }], removals: [] });
      feed.recordChange({ additions: [{ subject: 'b' }], removals: [] });

      const metrics = feed.getMetrics();

      expect(metrics.changesProcessed).toBe(2);
      expect(metrics.changesEmitted).toBe(2);
    });

    it('should track history size', () => {
      feed.recordChange({ additions: [{ subject: 'a' }], removals: [] });

      const metrics = feed.getMetrics();

      expect(metrics.historySize).toBe(1);
    });

    it('should track active state', () => {
      const metrics = feed.getMetrics();

      expect(metrics.isActive).toBe(true);

      feed.stop();

      const metricsAfter = feed.getMetrics();
      expect(metricsAfter.isActive).toBe(false);
    });
  });

  describe('Static Methods', () => {
    it('should compact multiple changes', () => {
      const changes = [
        {
          delta: {
            additions: [{ subject: 'a', predicate: 'p1', object: 'o1' }],
            removals: [],
          },
          timestamp: 1000,
        },
        {
          delta: {
            additions: [{ subject: 'b', predicate: 'p2', object: 'o2' }],
            removals: [{ subject: 'c', predicate: 'p3', object: 'o3' }],
          },
          timestamp: 2000,
        },
      ];

      const compacted = ChangeFeed.compactChanges(changes);

      expect(compacted.additions).toHaveLength(2);
      expect(compacted.removals).toHaveLength(1);
      expect(compacted.count).toBe(2);
      expect(compacted.startTime).toBe(1000);
      expect(compacted.endTime).toBe(2000);
    });
  });

  describe('Change Feed Hook', () => {
    it('should create transaction hook', async () => {
      const hook = createChangeFeedHook(feed);

      expect(hook.id).toBe('change-feed-recorder');
      expect(hook.mode).toBe('post');
      expect(typeof hook.condition).toBe('function');
      expect(typeof hook.effect).toBe('function');
    });

    it('should record changes via hook', async () => {
      const hook = createChangeFeedHook(feed);

      const eventPromise = new Promise(resolve => {
        feed.on('change', change => {
          expect(change.metadata.source).toBe('transaction-hook');
          resolve();
        });
      });

      const store = {};
      const delta = { additions: [{ subject: 'alice' }], removals: [] };

      await hook.effect(store, delta);
      await eventPromise;
    });
  });

  describe('Performance', () => {
    it('should handle high-frequency changes', () => {
      const start = Date.now();

      for (let i = 0; i < 1000; i++) {
        feed.recordChange({ additions: [{ subject: `s${i}` }], removals: [] });
      }

      const duration = Date.now() - start;

      expect(feed.metrics.changesProcessed).toBe(1000);
      expect(duration).toBeLessThan(1000); // Should complete in less than 1 second
    });
  });
});
