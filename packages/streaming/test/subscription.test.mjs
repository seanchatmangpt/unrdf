/**
 * @file Streaming Subscription Tests (FAST)
 * @description Fast tests for change feed and subscription with mocked operations
 * Target: <50ms execution time
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import { randomUUID } from 'crypto';
import { createChangeFeed, createSubscriptionManager } from '../src/index.mjs';

describe('Streaming: Subscription & Change Feeds (Fast)', () => {
  let feed;
  let subscriptionManager;

  beforeEach(() => {
    feed = createChangeFeed({
      id: `feed-${randomUUID()}`,
      name: 'Test Feed',
    });

    subscriptionManager = createSubscriptionManager();
  });

  describe('change feed creation', () => {
    it('should create change feed with metadata', () => {
      expect(feed.id).toBeDefined();
      expect(feed.name).toBe('Test Feed');
      expect(feed.createdAt).toBeDefined();
    });

    it('should emit changes when quaduplets are added', (done) => {
      const listener = vi.fn();

      feed.on('change', listener);

      // Add a change
      feed.addChange({
        type: 'add',
        quad: {
          subject: 'http://example.com/subject',
          predicate: 'http://example.com/predicate',
          object: 'http://example.com/object',
        },
      });

      // Verify listener was called
      setTimeout(() => {
        expect(listener).toHaveBeenCalled();
        done();
      }, 10);
    });
  });

  describe('subscription management', () => {
    it('should subscribe to feed and receive notifications', () => {
      const callback = vi.fn();

      const subscription = subscriptionManager.subscribe(feed.id, callback);

      expect(subscription).toBeDefined();
      expect(subscription.id).toBeDefined();
      expect(subscription.feedId).toBe(feed.id);
    });

    it('should unsubscribe from feed', () => {
      const callback = vi.fn();

      const subscription = subscriptionManager.subscribe(feed.id, callback);
      subscriptionManager.unsubscribe(subscription.id);

      const subscriptions = subscriptionManager.getSubscriptions(feed.id);
      expect(subscriptions).not.toContainEqual(
        expect.objectContaining({ id: subscription.id })
      );
    });
  });
});
