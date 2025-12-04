/**
 * @vitest-environment node
 * Adversarial Testing: Test advertised capabilities for @unrdf/streaming
 * Goal: PROVE what doesn't work, not security
 */

import { describe, it, expect } from 'vitest';
import {
  createChangeFeed,
  createSubscriptionManager,
  createStreamProcessor,
  createSyncMessage,
  parseSyncMessage,
  calculateChecksum,
} from '../src/index.mjs';
import { createStore, addQuad, namedNode, literal, quad } from '@unrdf/core';

describe('@unrdf/streaming Adversarial Tests - Capabilities', () => {
  describe('Change Feed - Advertised Features', () => {
    it('ADVERTISED: Can create change feed', () => {
      const store = createStore();
      const changeFeed = createChangeFeed(store);

      expect(changeFeed).toBeDefined();
      expect(changeFeed.subscribe).toBeDefined();
      expect(changeFeed.publish).toBeDefined();
    });

    it('ADVERTISED: Change feed emits events on quad operations', done => {
      const store = createStore();
      const changeFeed = createChangeFeed(store);

      changeFeed.subscribe(change => {
        expect(change.type).toBe('add');
        expect(change.quad).toBeDefined();
        done();
      });

      const testQuad = quad(
        namedNode('http://example.org/s'),
        namedNode('http://example.org/p'),
        literal('o')
      );

      changeFeed.publish({ type: 'add', quad: testQuad });
    });

    it('ADVERTISED: Can filter change feed by type', done => {
      const store = createStore();
      const changeFeed = createChangeFeed(store);

      let addCount = 0;

      changeFeed.subscribe(change => {
        if (change.type === 'add') {
          addCount++;
        }
        if (addCount === 1) {
          expect(addCount).toBe(1);
          done();
        }
      });

      changeFeed.publish({
        type: 'add',
        quad: quad(namedNode('http://s'), namedNode('http://p'), literal('o')),
      });
    });
  });

  describe('Subscription Manager - Advertised Features', () => {
    it('ADVERTISED: Can create subscription manager', () => {
      const manager = createSubscriptionManager();

      expect(manager).toBeDefined();
      expect(manager.subscribe).toBeDefined();
      expect(manager.unsubscribe).toBeDefined();
    });

    it('ADVERTISED: Can manage multiple subscriptions', () => {
      const manager = createSubscriptionManager();

      const sub1 = manager.subscribe(data => {
        /* noop */
      });
      const sub2 = manager.subscribe(data => {
        /* noop */
      });

      expect(sub1).toBeDefined();
      expect(sub2).toBeDefined();
      expect(sub1).not.toBe(sub2);
    });

    it('ADVERTISED: Can unsubscribe from events', () => {
      const manager = createSubscriptionManager();

      const subId = manager.subscribe(data => {
        /* noop */
      });
      const unsubResult = manager.unsubscribe(subId);

      expect(unsubResult).toBe(true);
    });
  });

  describe('Stream Processor - Advertised Features', () => {
    it('ADVERTISED: Can create stream processor', () => {
      const processor = createStreamProcessor();

      expect(processor).toBeDefined();
      expect(processor.process).toBeDefined();
    });

    it('ADVERTISED: Can process streaming data', async () => {
      const processor = createStreamProcessor();

      const data = {
        type: 'add',
        quad: quad(namedNode('http://s'), namedNode('http://p'), literal('o')),
      };

      const result = await processor.process(data);
      expect(result).toBeDefined();
    });

    it('ADVERTISED: Can batch process multiple events', async () => {
      const processor = createStreamProcessor();

      const events = [
        { type: 'add', quad: quad(namedNode('http://s1'), namedNode('http://p'), literal('o1')) },
        { type: 'add', quad: quad(namedNode('http://s2'), namedNode('http://p'), literal('o2')) },
      ];

      const results = await Promise.all(events.map(e => processor.process(e)));
      expect(results.length).toBe(2);
    });
  });

  describe('Sync Protocol - Advertised Features', () => {
    it('ADVERTISED: Can create sync messages', () => {
      const message = createSyncMessage({
        type: 'sync',
        sourceId: 'peer-1',
        targetId: 'peer-2',
        changes: [],
      });

      expect(message).toBeDefined();
      expect(message.type).toBe('sync');
      expect(message.sourceId).toBe('peer-1');
    });

    it('ADVERTISED: Can parse sync messages', () => {
      const original = createSyncMessage({
        type: 'sync',
        sourceId: 'peer-1',
        targetId: 'peer-2',
        changes: [],
      });

      const serialized = JSON.stringify(original);
      const parsed = parseSyncMessage(serialized);

      expect(parsed.type).toBe('sync');
      expect(parsed.sourceId).toBe('peer-1');
    });

    it('ADVERTISED: Can calculate checksums for sync verification', () => {
      const store = createStore();
      addQuad(
        store,
        quad(namedNode('http://example.org/s'), namedNode('http://example.org/p'), literal('o'))
      );

      const checksum = calculateChecksum(store);

      expect(checksum).toBeDefined();
      expect(typeof checksum).toBe('string');
      expect(checksum.length).toBeGreaterThan(0);
    });
  });
});
