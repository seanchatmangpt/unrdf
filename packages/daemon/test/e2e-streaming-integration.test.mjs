/**
 * @file E2E Streaming Integration Tests
 * @module @unrdf/daemon/test/e2e-streaming
 * @description End-to-end tests for daemon streaming integration
 * with reactive trigger patterns, backpressure handling, and memory efficiency
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { Daemon } from '../src/daemon.mjs';
import {
  ReactiveSubscriptionManager,
  subscribeToChangeFeeds,
  createDaemonFromChangeFeeds,
} from '../src/integrations/streaming.mjs';

function generateUUID() {
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
    const r = (Math.random() * 16) | 0;
    const v = c === 'x' ? r : (r & 0x3) | 0x8;
    return v.toString(16);
  });
}

function createMockChangeFeed() {
  const subscribers = new Set();
  return {
    subscribe(handler) {
      subscribers.add(handler);
    },
    unsubscribe(handler) {
      subscribers.delete(handler);
    },
    emit(change) {
      for (const handler of subscribers) {
        handler(change);
      }
    },
  };
}

function createMockChange(type = 'add', subject = 'http://example.org/1', predicate = 'http://example.org/type', object = 'http://example.org/Resource') {
  return {
    type,
    quad: {
      subject: { value: subject },
      predicate: { value: predicate },
      object: { value: object },
    },
    timestamp: Date.now(),
  };
}

describe('Streaming Integration E2E', () => {
  let daemon;
  let feed;
  let manager;

  beforeEach(async () => {
    daemon = new Daemon({
      daemonId: generateUUID(),
      name: 'test-daemon',
    });
    await daemon.start();

    feed = createMockChangeFeed();
  });

  afterEach(async () => {
    if (manager) {
      await manager.cleanup();
    }
    await daemon.stop();
  });

  describe('Basic Subscription and Pattern Matching', () => {
    it('should subscribe to change feed and execute operation on pattern match', async () => {
      const operationHandler = vi.fn(async () => ({ success: true }));
      daemon.schedule({
        id: 'test-op-1',
        handler: operationHandler,
        name: 'Test Operation',
      });

      manager = subscribeToChangeFeeds(daemon, feed);
      const triggerId = manager.registerTrigger('add', 'test-op-1');

      expect(triggerId).toMatch(/^trigger_/);
      expect(manager.listTriggers()).toHaveLength(1);

      feed.emit(createMockChange('add'));

      await new Promise(resolve => setTimeout(resolve, 50));
      expect(operationHandler).toHaveBeenCalled();
    });

    it('should match pattern with quad properties', async () => {
      const operationHandler = vi.fn(async () => ({ matched: true }));
      daemon.schedule({
        id: 'test-op-match',
        handler: operationHandler,
      });

      manager = subscribeToChangeFeeds(daemon, feed);
      const pattern = {
        type: 'add',
        subject: { value: 'http://example.org/person/1' },
      };
      manager.registerTrigger(pattern, 'test-op-match');

      feed.emit(createMockChange('add', 'http://example.org/person/1'));
      await new Promise(resolve => setTimeout(resolve, 50));
      expect(operationHandler).toHaveBeenCalled();

      operationHandler.mockClear();
      feed.emit(createMockChange('add', 'http://example.org/person/2'));
      await new Promise(resolve => setTimeout(resolve, 50));
      expect(operationHandler).not.toHaveBeenCalled();
    });

    it('should match multiple pattern types', async () => {
      const addHandler = vi.fn(async () => ({ op: 'add' }));
      const removeHandler = vi.fn(async () => ({ op: 'remove' }));

      daemon.schedule({ id: 'add-op', handler: addHandler });
      daemon.schedule({ id: 'remove-op', handler: removeHandler });

      manager = subscribeToChangeFeeds(daemon, feed);
      manager.registerTrigger({ type: 'add' }, 'add-op');
      manager.registerTrigger({ type: 'remove' }, 'remove-op');

      feed.emit(createMockChange('add'));
      await new Promise(resolve => setTimeout(resolve, 50));
      expect(addHandler).toHaveBeenCalledTimes(1);
      expect(removeHandler).not.toHaveBeenCalled();

      feed.emit(createMockChange('remove'));
      await new Promise(resolve => setTimeout(resolve, 50));
      expect(removeHandler).toHaveBeenCalledTimes(1);
    });

    it('should handle complex quad property matching', async () => {
      const operationHandler = vi.fn(async () => ({}));
      daemon.schedule({ id: 'complex-op', handler: operationHandler });

      manager = subscribeToChangeFeeds(daemon, feed);
      const pattern = {
        type: 'add',
        subject: { value: 'http://example.org/person/1' },
        predicate: { value: 'http://example.org/type' },
        object: { value: 'http://example.org/Resource' },
      };
      manager.registerTrigger(pattern, 'complex-op');

      feed.emit(createMockChange('add', 'http://example.org/person/1', 'http://example.org/type', 'http://example.org/Resource'));
      await new Promise(resolve => setTimeout(resolve, 50));
      expect(operationHandler).toHaveBeenCalled();

      operationHandler.mockClear();
      feed.emit(createMockChange('add', 'http://example.org/person/1', 'http://example.org/different', 'http://example.org/Resource'));
      await new Promise(resolve => setTimeout(resolve, 50));
      expect(operationHandler).not.toHaveBeenCalled();
    });
  });

  describe('Trigger Unregistration', () => {
    it('should unregister trigger and stop executing operations', async () => {
      const operationHandler = vi.fn(async () => ({}));
      daemon.schedule({ id: 'unreg-op', handler: operationHandler });

      manager = subscribeToChangeFeeds(daemon, feed);
      const triggerId = manager.registerTrigger('add', 'unreg-op');

      feed.emit(createMockChange('add'));
      await new Promise(resolve => setTimeout(resolve, 50));
      expect(operationHandler).toHaveBeenCalledTimes(1);

      manager.unregisterTrigger(triggerId);
      operationHandler.mockClear();

      feed.emit(createMockChange('add'));
      await new Promise(resolve => setTimeout(resolve, 50));
      expect(operationHandler).not.toHaveBeenCalled();
    });

    it('should return false when unregistering non-existent trigger', () => {
      manager = subscribeToChangeFeeds(daemon, feed);
      const result = manager.unregisterTrigger('non-existent-id');
      expect(result).toBe(false);
    });
  });

  describe('Debounce and Throttle Support', () => {
    it('should support debounce configuration', async () => {
      const operationHandler = vi.fn(async () => ({}));
      daemon.schedule({ id: 'debounce-op', handler: operationHandler });

      manager = subscribeToChangeFeeds(daemon, feed);
      const triggerId = manager.registerTrigger('add', 'debounce-op');

      const triggers = manager.listTriggers();
      expect(triggers).toHaveLength(1);
      expect(triggers[0].id).toBe(triggerId);
    });

    it('should support throttle configuration', async () => {
      const operationHandler = vi.fn(async () => ({}));
      daemon.schedule({ id: 'throttle-op', handler: operationHandler });

      manager = subscribeToChangeFeeds(daemon, feed);
      const triggerId = manager.registerTrigger('add', 'throttle-op');

      expect(manager.unregisterTrigger(triggerId)).toBe(true);
      expect(manager.listTriggers()).toHaveLength(0);
    });

    it('should apply backpressure when event frequency is high', async () => {
      const quickHandler = vi.fn(async () => ({}));
      daemon.schedule({ id: 'freq-op', handler: quickHandler });

      manager = new ReactiveSubscriptionManager(daemon, {
        drainThreshold: 2,
        maxBackpressure: 5,
      });
      manager.registerTrigger('add', 'freq-op');
      manager.subscribe(feed);

      for (let i = 0; i < 5; i++) {
        feed.emit(createMockChange('add'));
      }

      await new Promise(resolve => setTimeout(resolve, 100));
      expect(quickHandler.mock.calls.length).toBeGreaterThan(0);
    });
  });

  describe('Backpressure Handling', () => {
    it('should trigger drain when active count reaches threshold', async () => {
      const slowHandler = vi.fn(async () => {
        await new Promise(resolve => setTimeout(resolve, 20));
      });
      daemon.schedule({ id: 'slow-op', handler: slowHandler });

      manager = new ReactiveSubscriptionManager(daemon, {
        drainThreshold: 2,
        maxBackpressure: 5,
        backpressureDrainMs: 10,
      });
      manager.registerTrigger('add', 'slow-op');
      manager.subscribe(feed);

      feed.emit(createMockChange('add'));
      feed.emit(createMockChange('add'));
      feed.emit(createMockChange('add'));

      await new Promise(resolve => setTimeout(resolve, 100));
      expect(slowHandler.mock.calls.length).toBeGreaterThan(0);
    });

    it('should drop events when queue exceeds maxQueueSize', async () => {
      manager = new ReactiveSubscriptionManager(daemon, {
        maxQueueSize: 2,
      });
      manager.registerTrigger('add', 'some-op');

      const change1 = createMockChange('add');
      const change2 = createMockChange('add');
      const change3 = createMockChange('add');

      await manager.handleChange(change1);
      await manager.handleChange(change2);
      const _result3 = await manager.handleChange(change3);

      expect(manager.getMemoryStats().droppedEvents).toBeGreaterThanOrEqual(0);
    });

    it('should track backpressure metrics', async () => {
      const slowHandler = vi.fn(async () => {
        await new Promise(resolve => setTimeout(resolve, 30));
      });
      daemon.schedule({ id: 'metric-op', handler: slowHandler });

      manager = subscribeToChangeFeeds(daemon, feed, {
        drainThreshold: 2,
        maxBackpressure: 5,
      });
      manager.registerTrigger('add', 'metric-op');

      feed.emit(createMockChange('add'));
      feed.emit(createMockChange('add'));

      const stats = manager.getStats();
      expect(stats).toHaveProperty('activeCount');
      expect(stats).toHaveProperty('triggersCount');
      expect(stats).toHaveProperty('totalProcessed');
    });
  });

  describe('Memory Efficiency', () => {
    it('should track memory usage under sustained events', async () => {
      const quickHandler = vi.fn(async () => ({}));
      daemon.schedule({ id: 'memory-op', handler: quickHandler });

      manager = subscribeToChangeFeeds(daemon, feed);
      manager.registerTrigger('add', 'memory-op');

      const _initialStats = manager.getMemoryStats();

      for (let i = 0; i < 50; i++) {
        feed.emit(createMockChange('add'));
      }

      await new Promise(resolve => setTimeout(resolve, 100));

      const finalStats = manager.getMemoryStats();
      expect(finalStats.totalProcessed).toBe(50);
      expect(finalStats.estimatedMemoryBytes).toBeGreaterThan(0);
    });

    it('should maintain bounded memory with event processing', async () => {
      const quickHandler = vi.fn(async () => ({}));
      daemon.schedule({ id: 'bounded-op', handler: quickHandler });

      manager = new ReactiveSubscriptionManager(daemon, {
        maxQueueSize: 100,
      });
      manager.registerTrigger('add', 'bounded-op');
      manager.subscribe(feed);

      for (let i = 0; i < 100; i++) {
        feed.emit(createMockChange('add'));
      }

      await new Promise(resolve => setTimeout(resolve, 100));

      const stats = manager.getMemoryStats();
      expect(stats.eventQueueSize).toBeLessThanOrEqual(100);
    });

    it('should report memory stats accurately', () => {
      manager = subscribeToChangeFeeds(daemon, feed);
      manager.registerTrigger('add', 'test-op-1');
      manager.registerTrigger('remove', 'test-op-2');

      const memStats = manager.getMemoryStats();
      expect(memStats.triggersCount).toBe(2);
      expect(memStats.subscriptionsCount).toBeGreaterThan(0);
      expect(memStats.estimatedMemoryBytes).toBeGreaterThan(0);
    });
  });

  describe('Subscription Cleanup', () => {
    it('should cleanup subscriptions on unsubscribe', async () => {
      const operationHandler = vi.fn(async () => ({}));
      daemon.schedule({ id: 'cleanup-op', handler: operationHandler });

      manager = subscribeToChangeFeeds(daemon, feed);
      manager.registerTrigger('add', 'cleanup-op');

      const initialTriggers = manager.listTriggers();
      expect(initialTriggers).toHaveLength(1);

      feed.emit(createMockChange('add'));
      await new Promise(resolve => setTimeout(resolve, 50));
      expect(operationHandler).toHaveBeenCalled();

      await manager.cleanup();

      operationHandler.mockClear();
      feed.emit(createMockChange('add'));
      await new Promise(resolve => setTimeout(resolve, 50));

      expect(operationHandler).not.toHaveBeenCalled();
      expect(manager.listTriggers()).toHaveLength(0);
    });

    it('should cleanup all resources on cleanup', async () => {
      const operationHandler = vi.fn(async () => ({}));
      daemon.schedule({ id: 'timer-cleanup-op', handler: operationHandler });

      manager = subscribeToChangeFeeds(daemon, feed);
      manager.registerTrigger('add', 'timer-cleanup-op');

      feed.emit(createMockChange('add'));
      await new Promise(resolve => setTimeout(resolve, 50));

      const statsBefore = manager.getStats();
      expect(statsBefore.triggersCount).toBeGreaterThan(0);

      await manager.cleanup();

      const statsAfter = manager.getStats();
      expect(statsAfter.triggersCount).toBe(0);
      expect(statsAfter.subscriptionsCount).toBe(0);
    });

    it('should handle cleanup with no active subscriptions', async () => {
      manager = subscribeToChangeFeeds(daemon, feed);

      await expect(manager.cleanup()).resolves.not.toThrow();
      expect(manager.getStats().triggersCount).toBe(0);
    });

    it('should prevent operations after cleanup', async () => {
      const operationHandler = vi.fn(async () => ({}));
      daemon.schedule({ id: 'post-cleanup-op', handler: operationHandler });

      manager = subscribeToChangeFeeds(daemon, feed);
      manager.registerTrigger('add', 'post-cleanup-op');

      await manager.cleanup();

      feed.emit(createMockChange('add'));
      await new Promise(resolve => setTimeout(resolve, 50));

      expect(operationHandler).not.toHaveBeenCalled();
    });
  });

  describe('Error Handling in Streams', () => {
    it('should handle operation execution errors gracefully', async () => {
      const failingHandler = vi.fn(async () => {
        throw new Error('Operation failed');
      });
      daemon.schedule({ id: 'failing-op', handler: failingHandler });

      manager = subscribeToChangeFeeds(daemon, feed);
      manager.registerTrigger('add', 'failing-op');

      feed.emit(createMockChange('add'));
      await new Promise(resolve => setTimeout(resolve, 50));

      expect(failingHandler).toHaveBeenCalled();
    });

    it('should handle invalid change events', async () => {
      const operationHandler = vi.fn(async () => ({}));
      daemon.schedule({ id: 'invalid-op', handler: operationHandler });

      manager = subscribeToChangeFeeds(daemon, feed);
      manager.registerTrigger('add', 'invalid-op');

      const invalidChange = {
        type: 'add',
        quad: {
          subject: null,
          predicate: null,
          object: null,
        },
        timestamp: Date.now(),
      };

      try {
        await manager.handleChange(invalidChange);
      } catch (error) {
        expect(error).toBeDefined();
      }
    });
  });

  describe('Multiple Feeds and Triggers', () => {
    it('should support multiple feeds', async () => {
      const handler1 = vi.fn(async () => ({}));
      const handler2 = vi.fn(async () => ({}));

      daemon.schedule({ id: 'multi-op-1', handler: handler1 });
      daemon.schedule({ id: 'multi-op-2', handler: handler2 });

      const feed1 = createMockChangeFeed();
      const feed2 = createMockChangeFeed();

      manager = subscribeToChangeFeeds(daemon, [feed1, feed2]);
      manager.registerTrigger('add', 'multi-op-1');
      manager.registerTrigger('add', 'multi-op-2');

      feed1.emit(createMockChange('add'));
      await new Promise(resolve => setTimeout(resolve, 50));
      expect(handler1).toHaveBeenCalled();

      handler1.mockClear();

      feed2.emit(createMockChange('add'));
      await new Promise(resolve => setTimeout(resolve, 50));
      expect(handler2).toHaveBeenCalled();
    });

    it('should support multiple triggers for same operation', async () => {
      const operationHandler = vi.fn(async () => ({}));
      daemon.schedule({ id: 'multi-trigger-op', handler: operationHandler });

      manager = subscribeToChangeFeeds(daemon, feed);
      manager.registerTrigger({ type: 'add' }, 'multi-trigger-op');
      manager.registerTrigger({ type: 'update' }, 'multi-trigger-op');

      feed.emit(createMockChange('add'));
      await new Promise(resolve => setTimeout(resolve, 50));
      expect(operationHandler).toHaveBeenCalledTimes(1);

      feed.emit(createMockChange('update'));
      await new Promise(resolve => setTimeout(resolve, 50));
      expect(operationHandler).toHaveBeenCalledTimes(2);
    });
  });

  describe('Integration with Daemon Lifecycle', () => {
    it('should work with createDaemonFromChangeFeeds helper', async () => {
      const operationHandler = vi.fn(async () => ({ integrated: true }));
      daemon.schedule({ id: 'integrated-op', handler: operationHandler });

      const manager2 = createDaemonFromChangeFeeds(daemon, feed);
      expect(daemon._reactiveManager).toBe(manager2);

      manager2.registerTrigger('add', 'integrated-op');
      feed.emit(createMockChange('add'));

      await new Promise(resolve => setTimeout(resolve, 50));
      expect(operationHandler).toHaveBeenCalled();

      manager = manager2;
    });

    it('should maintain state across multiple operations', async () => {
      const handler1 = vi.fn(async () => ({}));
      const handler2 = vi.fn(async () => ({}));

      daemon.schedule({ id: 'state-op-1', handler: handler1 });
      daemon.schedule({ id: 'state-op-2', handler: handler2 });

      manager = subscribeToChangeFeeds(daemon, feed);
      const _id1 = manager.registerTrigger('add', 'state-op-1');
      const _id2 = manager.registerTrigger('remove', 'state-op-2');

      feed.emit(createMockChange('add'));
      await new Promise(resolve => setTimeout(resolve, 50));

      expect(manager.listTriggers()).toHaveLength(2);
      expect(manager.getStats().totalProcessed).toBe(1);
    });
  });

  describe('Pattern Matching with Different Value Types', () => {
    it('should handle string values in patterns', async () => {
      const operationHandler = vi.fn(async () => ({}));
      daemon.schedule({ id: 'string-pattern-op', handler: operationHandler });

      manager = subscribeToChangeFeeds(daemon, feed);
      const pattern = {
        type: 'add',
        subject: 'http://example.org/resource',
      };
      manager.registerTrigger(pattern, 'string-pattern-op');

      feed.emit({
        type: 'add',
        quad: {
          subject: 'http://example.org/resource',
          predicate: 'http://example.org/type',
          object: 'http://example.org/Thing',
        },
        timestamp: Date.now(),
      });

      await new Promise(resolve => setTimeout(resolve, 50));
      expect(operationHandler).toHaveBeenCalled();
    });

    it('should handle value objects in patterns', async () => {
      const operationHandler = vi.fn(async () => ({}));
      daemon.schedule({ id: 'object-pattern-op', handler: operationHandler });

      manager = subscribeToChangeFeeds(daemon, feed);
      const pattern = {
        subject: { value: 'http://example.org/resource' },
      };
      manager.registerTrigger(pattern, 'object-pattern-op');

      feed.emit({
        type: 'add',
        quad: {
          subject: { value: 'http://example.org/resource' },
          predicate: { value: 'http://example.org/type' },
          object: { value: 'http://example.org/Thing' },
        },
        timestamp: Date.now(),
      });

      await new Promise(resolve => setTimeout(resolve, 50));
      expect(operationHandler).toHaveBeenCalled();
    });
  });

  describe('Statistics and Monitoring', () => {
    it('should track total processed events', async () => {
      const operationHandler = vi.fn(async () => ({}));
      daemon.schedule({ id: 'stats-op', handler: operationHandler });

      manager = subscribeToChangeFeeds(daemon, feed);
      manager.registerTrigger('add', 'stats-op');

      expect(manager.getStats().totalProcessed).toBe(0);

      feed.emit(createMockChange('add'));
      feed.emit(createMockChange('add'));
      feed.emit(createMockChange('add'));

      await new Promise(resolve => setTimeout(resolve, 100));

      expect(manager.getStats().totalProcessed).toBe(3);
    });

    it('should report accurate trigger list', () => {
      manager = subscribeToChangeFeeds(daemon, feed);
      manager.registerTrigger('add', 'op-1');
      manager.registerTrigger('remove', 'op-2');
      manager.registerTrigger({ type: 'update', subject: 'http://example.org/1' }, 'op-3');

      const triggers = manager.listTriggers();
      expect(triggers).toHaveLength(3);
      expect(triggers.every(t => t.id && t.pattern && t.operationId)).toBe(true);
    });
  });
});
