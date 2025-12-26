/**
 * @vitest-environment node
 */
import { describe, it, expect, beforeEach, vi } from 'vitest';
import { namedNode, literal } from '@rdfjs/data-model';
import {
  createChangeFeed,
  createSubscriptionManager,
  createStreamProcessor,
  createSyncMessage,
  parseSyncMessage,
  calculateChecksum,
  mergeSyncMessages,
} from '../src/index.mjs';

describe('@unrdf/streaming', () => {
  describe('createChangeFeed', () => {
    let feed;

    beforeEach(() => {
      feed = createChangeFeed();
    });

    it('should create a change feed', () => {
      expect(feed).toBeDefined();
      expect(typeof feed.emitChange).toBe('function');
      expect(typeof feed.addEventListener).toBe('function');
    });

    it('should emit changes', () => {
      return new Promise(resolve => {
        const quad = {
          subject: namedNode('http://example.org/s'),
          predicate: namedNode('http://example.org/p'),
          object: literal('value'),
        };

        feed.addEventListener('change', event => {
          expect(event.detail.type).toBe('add');
          expect(event.detail.quad).toEqual(quad);
          expect(event.detail.timestamp).toBeDefined();
          resolve();
        });

        feed.emitChange({
          type: 'add',
          quad,
        });
      });
    });

    it('should track changes', () => {
      const quad = {
        subject: namedNode('http://example.org/s'),
        predicate: namedNode('http://example.org/p'),
        object: literal('value'),
      };

      feed.emitChange({ type: 'add', quad });
      feed.emitChange({ type: 'remove', quad });

      const changes = feed.getChanges();
      expect(changes).toHaveLength(2);
      expect(changes[0].type).toBe('add');
      expect(changes[1].type).toBe('remove');
    });

    it('should replay changes', () => {
      const quad = {
        subject: namedNode('http://example.org/s'),
        predicate: namedNode('http://example.org/p'),
        object: literal('value'),
      };

      feed.emitChange({ type: 'add', quad });
      feed.emitChange({ type: 'update', quad });

      const replayed = [];
      feed.replay(change => replayed.push(change));

      expect(replayed).toHaveLength(2);
      expect(replayed[0].type).toBe('add');
      expect(replayed[1].type).toBe('update');
    });

    it('should clear changes', () => {
      const quad = {
        subject: namedNode('http://example.org/s'),
        predicate: namedNode('http://example.org/p'),
        object: literal('value'),
      };

      feed.emitChange({ type: 'add', quad });
      expect(feed.getChanges()).toHaveLength(1);

      feed.clearChanges();
      expect(feed.getChanges()).toHaveLength(0);
    });
  });

  describe('createSubscriptionManager', () => {
    let feed;
    let manager;

    beforeEach(() => {
      feed = createChangeFeed();
      manager = createSubscriptionManager(feed);
    });

    it('should create a subscription manager', () => {
      expect(manager).toBeDefined();
      expect(typeof manager.subscribe).toBe('function');
      expect(typeof manager.unsubscribe).toBe('function');
    });

    it('should subscribe to changes', () => {
      return new Promise(resolve => {
        const quad = {
          subject: namedNode('http://example.org/s'),
          predicate: namedNode('http://example.org/p'),
          object: literal('value'),
        };

        manager.subscribe(change => {
          expect(change.type).toBe('add');
          resolve();
        }, {});

        feed.emitChange({ type: 'add', quad });
      });
    });

    it('should filter by subject', () => {
      return new Promise(resolve => {
        const subject1 = namedNode('http://example.org/s1');
        const subject2 = namedNode('http://example.org/s2');
        const predicate = namedNode('http://example.org/p');

        let callCount = 0;

        manager.subscribe(
          change => {
            expect(change.quad.subject).toEqual(subject1);
            callCount++;
            if (callCount === 2) resolve();
          },
          { subject: subject1 }
        );

        feed.emitChange({
          type: 'add',
          quad: { subject: subject1, predicate, object: literal('1') },
        });
        feed.emitChange({
          type: 'add',
          quad: { subject: subject2, predicate, object: literal('2') },
        });
        feed.emitChange({
          type: 'add',
          quad: { subject: subject1, predicate, object: literal('3') },
        });
      });
    });

    it('should unsubscribe', () => {
      const callback = vi.fn();
      const id = manager.subscribe(callback, {});

      const quad = {
        subject: namedNode('http://example.org/s'),
        predicate: namedNode('http://example.org/p'),
        object: literal('value'),
      };

      feed.emitChange({ type: 'add', quad });
      expect(callback).toHaveBeenCalledTimes(1);

      manager.unsubscribe(id);
      feed.emitChange({ type: 'add', quad });
      expect(callback).toHaveBeenCalledTimes(1);
    });

    it('should list subscriptions', () => {
      const sub1 = manager.subscribe(() => {}, {});
      const sub2 = manager.subscribe(() => {}, { subject: namedNode('http://example.org/s') });

      const subs = manager.listSubscriptions();
      expect(subs).toHaveLength(2);
      expect(subs.find(s => s.id === sub1)).toBeDefined();
      expect(subs.find(s => s.id === sub2)).toBeDefined();
    });
  });

  describe('createStreamProcessor', () => {
    let feed;
    let processor;

    beforeEach(() => {
      feed = createChangeFeed();
      processor = createStreamProcessor(feed);
    });

    it('should filter changes', () => {
      return new Promise(resolve => {
        const quad = {
          subject: namedNode('http://example.org/s'),
          predicate: namedNode('http://example.org/p'),
          object: literal('value'),
        };

        let callCount = 0;

        processor
          .filter(c => c.type === 'add')
          .subscribe(change => {
            expect(change.type).toBe('add');
            callCount++;
            if (callCount === 2) resolve();
          });

        feed.emitChange({ type: 'add', quad });
        feed.emitChange({ type: 'remove', quad });
        feed.emitChange({ type: 'add', quad });
      });
    });

    it('should map changes', () => {
      return new Promise(resolve => {
        const quad = {
          subject: namedNode('http://example.org/s'),
          predicate: namedNode('http://example.org/p'),
          object: literal('value'),
        };

        processor
          .map(c => ({ ...c, processed: true }))
          .subscribe(change => {
            expect(change.processed).toBe(true);
            resolve();
          });

        feed.emitChange({ type: 'add', quad });
      });
    });

    it('should batch changes', () => {
      return new Promise(resolve => {
        const quad = {
          subject: namedNode('http://example.org/s'),
          predicate: namedNode('http://example.org/p'),
          object: literal('value'),
        };

        processor.batch(3).subscribe(changes => {
          expect(changes).toHaveLength(3);
          resolve();
        });

        feed.emitChange({ type: 'add', quad });
        feed.emitChange({ type: 'add', quad });
        feed.emitChange({ type: 'add', quad });
      });
    });

    it('should debounce changes', () => {
      return new Promise(resolve => {
        const quad = {
          subject: namedNode('http://example.org/s'),
          predicate: namedNode('http://example.org/p'),
          object: literal('value'),
        };

        const callback = vi.fn();

        processor.debounce(100).subscribe(callback);

        feed.emitChange({ type: 'add', quad });
        feed.emitChange({ type: 'add', quad });
        feed.emitChange({ type: 'add', quad });

        setTimeout(() => {
          expect(callback).toHaveBeenCalledTimes(1);
          resolve();
        }, 150);
      });
    });
  });

  describe('sync protocol', () => {
    it('should create sync message', () => {
      const changes = [
        {
          type: 'add',
          quad: {
            subject: namedNode('http://example.org/s'),
            predicate: namedNode('http://example.org/p'),
            object: literal('value'),
          },
          timestamp: Date.now(),
        },
      ];

      const message = createSyncMessage(changes);

      expect(message.version).toBe('1.0');
      expect(message.changes).toEqual(changes);
      expect(message.checksum).toBeDefined();
      expect(message.timestamp).toBeDefined();
    });

    it('should parse sync message', () => {
      const changes = [
        {
          type: 'add',
          quad: {
            subject: namedNode('http://example.org/s'),
            predicate: namedNode('http://example.org/p'),
            object: literal('value'),
          },
          timestamp: Date.now(),
        },
      ];

      const message = createSyncMessage(changes);
      const parsed = parseSyncMessage(message);

      expect(parsed).toEqual(message);
    });

    it('should detect checksum mismatch', () => {
      const changes = [
        {
          type: 'add',
          quad: {
            subject: namedNode('http://example.org/s'),
            predicate: namedNode('http://example.org/p'),
            object: literal('value'),
          },
          timestamp: Date.now(),
        },
      ];

      const message = createSyncMessage(changes);
      message.checksum = 'invalid';

      expect(() => parseSyncMessage(message)).toThrow('Checksum mismatch');
    });

    it('should calculate checksum', () => {
      const changes = [
        {
          type: 'add',
          quad: {
            subject: namedNode('http://example.org/s'),
            predicate: namedNode('http://example.org/p'),
            object: literal('value'),
          },
          timestamp: 12345,
        },
      ];

      const checksum1 = calculateChecksum(changes);
      const checksum2 = calculateChecksum(changes);

      expect(checksum1).toBe(checksum2);
      expect(typeof checksum1).toBe('string');
      expect(checksum1.length).toBe(64);
    });

    it('should merge sync messages', () => {
      const quad1 = {
        subject: namedNode('http://example.org/s1'),
        predicate: namedNode('http://example.org/p'),
        object: literal('1'),
      };

      const quad2 = {
        subject: namedNode('http://example.org/s2'),
        predicate: namedNode('http://example.org/p'),
        object: literal('2'),
      };

      const message1 = createSyncMessage([{ type: 'add', quad: quad1, timestamp: 1000 }]);

      const message2 = createSyncMessage([{ type: 'add', quad: quad2, timestamp: 2000 }]);

      const merged = mergeSyncMessages([message1, message2]);

      expect(merged.changes).toHaveLength(2);
      expect(merged.changes[0].timestamp).toBe(1000);
      expect(merged.changes[1].timestamp).toBe(2000);
    });
  });

  /* ========================================================================= */
  /* Memory Leak Scenarios (COVERAGE GAP)                                     */
  /* ========================================================================= */

  describe('Memory Leak Prevention', () => {
    it('should handle 1000+ subscriptions without memory leak', () => {
      const feed = createChangeFeed();
      const manager = createSubscriptionManager(feed);
      const subscriptionIds = [];

      // Create 1000 subscriptions
      for (let i = 0; i < 1000; i++) {
        const id = manager.subscribe(() => {}, {});
        subscriptionIds.push(id);
      }

      expect(manager.listSubscriptions()).toHaveLength(1000);

      // Unsubscribe all
      for (const id of subscriptionIds) {
        manager.unsubscribe(id);
      }

      expect(manager.listSubscriptions()).toHaveLength(0);
    });

    it('should properly clean up subscriptions on unsubscribe', () => {
      const feed = createChangeFeed();
      const manager = createSubscriptionManager(feed);
      const callback = vi.fn();

      const id = manager.subscribe(callback, {});

      const quad = {
        subject: namedNode('http://example.org/s'),
        predicate: namedNode('http://example.org/p'),
        object: literal('value'),
      };

      // Emit before unsubscribe
      feed.emitChange({ type: 'add', quad });
      expect(callback).toHaveBeenCalledTimes(1);

      // Unsubscribe
      manager.unsubscribe(id);

      // Emit after unsubscribe - should not trigger callback
      feed.emitChange({ type: 'add', quad });
      expect(callback).toHaveBeenCalledTimes(1);
    });
  });

  /* ========================================================================= */
  /* Backpressure Simulation (COVERAGE GAP)                                   */
  /* ========================================================================= */

  describe('Backpressure Handling', () => {
    it('should handle 100+ events/sec without blocking', () => {
      return new Promise(resolve => {
        const feed = createChangeFeed();
        const processor = createStreamProcessor(feed);
        let eventCount = 0;

        processor.subscribe(() => {
          eventCount++;
          if (eventCount >= 100) {
            expect(eventCount).toBe(100);
            resolve();
          }
        });

        const quad = {
          subject: namedNode('http://example.org/s'),
          predicate: namedNode('http://example.org/p'),
          object: literal('value'),
        };

        // Emit 100 events rapidly
        for (let i = 0; i < 100; i++) {
          feed.emitChange({ type: 'add', quad });
        }
      });
    });

    it('should debounce rapid events correctly', () => {
      return new Promise(resolve => {
        const feed = createChangeFeed();
        const processor = createStreamProcessor(feed);
        const callback = vi.fn();

        processor.debounce(50).subscribe(callback);

        const quad = {
          subject: namedNode('http://example.org/s'),
          predicate: namedNode('http://example.org/p'),
          object: literal('value'),
        };

        // Rapid fire 20 events
        for (let i = 0; i < 20; i++) {
          feed.emitChange({ type: 'add', quad });
        }

        // Should only trigger once after debounce period
        setTimeout(() => {
          expect(callback).toHaveBeenCalledTimes(1);
          resolve();
        }, 100);
      });
    });
  });

  /* ========================================================================= */
  /* Ring Buffer Eviction (COVERAGE GAP)                                      */
  /* ========================================================================= */

  describe('Ring Buffer Eviction', () => {
    it('should evict oldest changes when exceeding max history', () => {
      const feed = createChangeFeed(null, { maxHistorySize: 100 });

      const quad = {
        subject: namedNode('http://example.org/s'),
        predicate: namedNode('http://example.org/p'),
        object: literal('value'),
      };

      // Add 150 changes (exceeds max of 100)
      for (let i = 0; i < 150; i++) {
        feed.emitChange({
          type: 'add',
          quad,
          timestamp: i,
        });
      }

      const changes = feed.getChanges();

      // Should only keep last 100
      expect(changes).toHaveLength(100);

      // Oldest should be timestamp 50 (150 - 100)
      expect(changes[0].timestamp).toBe(50);

      // Newest should be timestamp 149
      expect(changes[99].timestamp).toBe(149);
    });

    it('should maintain ring buffer with getHistory options', () => {
      const feed = createChangeFeed(null, { maxHistorySize: 50 });

      const quad = {
        subject: namedNode('http://example.org/s'),
        predicate: namedNode('http://example.org/p'),
        object: literal('value'),
      };

      // Add 100 changes
      for (let i = 0; i < 100; i++) {
        feed.emitChange({
          type: 'add',
          quad,
          timestamp: i,
        });
      }

      // Query with limit
      const limited = feed.getHistory({ limit: 10 });
      expect(limited).toHaveLength(10);

      // Query with since filter
      const recent = feed.getHistory({ since: 75 });
      expect(recent.every(c => c.timestamp >= 75)).toBe(true);
    });
  });

  /* ========================================================================= */
  /* Subscription Cleanup (COVERAGE GAP)                                      */
  /* ========================================================================= */

  describe('Subscription Cleanup', () => {
    it('should clean up all subscriptions when unsubscribing', () => {
      const feed = createChangeFeed();
      const manager = createSubscriptionManager(feed);

      const callback1 = vi.fn();
      const callback2 = vi.fn();
      const callback3 = vi.fn();

      const id1 = manager.subscribe(callback1, {});
      const id2 = manager.subscribe(callback2, {});
      const id3 = manager.subscribe(callback3, {});

      expect(manager.listSubscriptions()).toHaveLength(3);

      manager.unsubscribe(id1);
      manager.unsubscribe(id2);
      manager.unsubscribe(id3);

      expect(manager.listSubscriptions()).toHaveLength(0);
    });

    it('should handle unsubscribe function from subscribe', () => {
      const feed = createChangeFeed();
      const callback = vi.fn();

      const unsubscribe = feed.subscribe(callback);

      const quad = {
        subject: namedNode('http://example.org/s'),
        predicate: namedNode('http://example.org/p'),
        object: literal('value'),
      };

      feed.emitChange({ type: 'add', quad });
      expect(callback).toHaveBeenCalledTimes(1);

      unsubscribe();

      feed.emitChange({ type: 'add', quad });
      expect(callback).toHaveBeenCalledTimes(1);
    });

    it('should handle removeEventListener cleanup', () => {
      const feed = createChangeFeed();
      const callback = vi.fn();

      const handler = event => callback(event.detail);

      feed.addEventListener('change', handler);

      const quad = {
        subject: namedNode('http://example.org/s'),
        predicate: namedNode('http://example.org/p'),
        object: literal('value'),
      };

      feed.emitChange({ type: 'add', quad });
      expect(callback).toHaveBeenCalledTimes(1);

      feed.removeEventListener('change', handler);

      feed.emitChange({ type: 'add', quad });
      expect(callback).toHaveBeenCalledTimes(1);
    });
  });
});
