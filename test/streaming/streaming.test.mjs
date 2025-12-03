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
} from '../../packages/streaming/src/index.mjs';

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
      feed.replay((change) => replayed.push(change));

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

    it('should unsubscribe', () => {
      const callback = vi.fn();
      const id = manager.subscribe(callback);

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
      const sub1 = manager.subscribe(() => {});
      const sub2 = manager.subscribe(() => {}, { subject: namedNode('http://example.org/s') });

      const subs = manager.listSubscriptions();
      expect(subs).toHaveLength(2);
      expect(subs.find((s) => s.id === sub1)).toBeDefined();
      expect(subs.find((s) => s.id === sub2)).toBeDefined();
    });
  });

  describe('createStreamProcessor', () => {
    let feed;
    let processor;

    beforeEach(() => {
      feed = createChangeFeed();
      processor = createStreamProcessor(feed);
    });

    it('should filter changes with spy', () => {
      const quad = {
        subject: namedNode('http://example.org/s'),
        predicate: namedNode('http://example.org/p'),
        object: literal('value'),
      };

      const callback = vi.fn();
      processor.filter((c) => c.type === 'add').subscribe(callback);

      feed.emitChange({ type: 'add', quad });
      feed.emitChange({ type: 'remove', quad });
      feed.emitChange({ type: 'add', quad });

      expect(callback).toHaveBeenCalledTimes(2);
      expect(callback.mock.calls[0][0].type).toBe('add');
      expect(callback.mock.calls[1][0].type).toBe('add');
    });

    it('should map changes', () => {
      const quad = {
        subject: namedNode('http://example.org/s'),
        predicate: namedNode('http://example.org/p'),
        object: literal('value'),
      };

      const callback = vi.fn();
      processor.map((c) => ({ ...c, processed: true })).subscribe(callback);

      feed.emitChange({ type: 'add', quad });

      expect(callback).toHaveBeenCalledTimes(1);
      expect(callback.mock.calls[0][0].processed).toBe(true);
    });

    it('should batch changes', () => {
      const quad = {
        subject: namedNode('http://example.org/s'),
        predicate: namedNode('http://example.org/p'),
        object: literal('value'),
      };

      const callback = vi.fn();
      processor.batch(3).subscribe(callback);

      feed.emitChange({ type: 'add', quad });
      feed.emitChange({ type: 'add', quad });
      feed.emitChange({ type: 'add', quad });

      expect(callback).toHaveBeenCalledTimes(1);
      expect(callback.mock.calls[0][0]).toHaveLength(3);
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

      const message1 = createSyncMessage([
        { type: 'add', quad: quad1, timestamp: 1000 },
      ]);

      const message2 = createSyncMessage([
        { type: 'add', quad: quad2, timestamp: 2000 },
      ]);

      const merged = mergeSyncMessages([message1, message2]);

      expect(merged.changes).toHaveLength(2);
      expect(merged.changes[0].timestamp).toBe(1000);
      expect(merged.changes[1].timestamp).toBe(2000);
    });
  });
});
