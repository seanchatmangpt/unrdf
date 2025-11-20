/**
 * @file Tests for WebSocket Subscription Manager
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { SubscriptionManager, SubscriptionPatternType } from '../../src/knowledge-engine/streaming/subscription-manager.mjs';

describe('SubscriptionManager', () => {
  let manager;

  beforeEach(() => {
    manager = new SubscriptionManager({
      reconnectInterval: 1000,
      maxReconnectAttempts: 3,
      heartbeatInterval: 5000
    });
  });

  afterEach(async () => {
    await manager.cleanup();
  });

  describe('Subscription Management', () => {
    it('should create a subscription', () => {
      const config = {
        pattern: SubscriptionPatternType.PROPERTY_CHANGE,
        subject: 'http://example.org/alice',
        predicate: 'http://example.org/name'
      };

      const id = manager.subscribe(config);

      expect(id).toBeDefined();
      expect(typeof id).toBe('string');
      expect(manager.subscriptions.size).toBe(1);
    });

    it('should create subscription with callback', async () => {
      const config = {
        pattern: SubscriptionPatternType.WILDCARD
      };

      const eventPromise = new Promise((resolve) => {
        const callback = (data) => {
          expect(data).toBeDefined();
          resolve();
        };

        const id = manager.subscribe(config, callback);

        // Simulate change message
        manager.emit(`change:${id}`, { test: 'data' });
      });

      await eventPromise;
    });

    it('should unsubscribe', () => {
      const config = {
        pattern: SubscriptionPatternType.WILDCARD
      };

      const id = manager.subscribe(config);
      expect(manager.subscriptions.size).toBe(1);

      const result = manager.unsubscribe(id);
      expect(result).toBe(true);
      expect(manager.subscriptions.size).toBe(0);
    });

    it('should unsubscribe all', () => {
      manager.subscribe({ pattern: SubscriptionPatternType.WILDCARD });
      manager.subscribe({ pattern: SubscriptionPatternType.WILDCARD });
      manager.subscribe({ pattern: SubscriptionPatternType.WILDCARD });

      expect(manager.subscriptions.size).toBe(3);

      manager.unsubscribeAll();
      expect(manager.subscriptions.size).toBe(0);
    });

    it('should enforce subscription limit', () => {
      const limitedManager = new SubscriptionManager({
        maxSubscriptions: 2
      });

      limitedManager.subscribe({ pattern: SubscriptionPatternType.WILDCARD });
      limitedManager.subscribe({ pattern: SubscriptionPatternType.WILDCARD });

      expect(() => {
        limitedManager.subscribe({ pattern: SubscriptionPatternType.WILDCARD });
      }).toThrow('Maximum subscriptions');

      limitedManager.cleanup();
    });
  });

  describe('Subscription Patterns', () => {
    it('should support SPARQL SELECT pattern', () => {
      const config = {
        pattern: SubscriptionPatternType.SPARQL_SELECT,
        query: 'SELECT * WHERE { ?s ?p ?o }'
      };

      const id = manager.subscribe(config);
      const sub = manager.getSubscription(id);

      expect(sub.config.pattern).toBe(SubscriptionPatternType.SPARQL_SELECT);
      expect(sub.config.query).toBeDefined();
    });

    it('should support property change pattern', () => {
      const config = {
        pattern: SubscriptionPatternType.PROPERTY_CHANGE,
        subject: 'http://example.org/alice',
        predicate: 'http://example.org/age'
      };

      const id = manager.subscribe(config);
      const sub = manager.getSubscription(id);

      expect(sub.config.pattern).toBe(SubscriptionPatternType.PROPERTY_CHANGE);
      expect(sub.config.subject).toBe('http://example.org/alice');
    });

    it('should support entity update pattern', () => {
      const config = {
        pattern: SubscriptionPatternType.ENTITY_UPDATE,
        subject: 'http://example.org/bob'
      };

      const id = manager.subscribe(config);
      const sub = manager.getSubscription(id);

      expect(sub.config.pattern).toBe(SubscriptionPatternType.ENTITY_UPDATE);
    });

    it('should support wildcard pattern', () => {
      const config = {
        pattern: SubscriptionPatternType.WILDCARD
      };

      const id = manager.subscribe(config);
      const sub = manager.getSubscription(id);

      expect(sub.config.pattern).toBe(SubscriptionPatternType.WILDCARD);
    });
  });

  describe('Configuration', () => {
    it('should use default debounce', () => {
      const config = {
        pattern: SubscriptionPatternType.WILDCARD
      };

      const id = manager.subscribe(config);
      const sub = manager.getSubscription(id);

      expect(sub.config.debounceMs).toBe(100);
    });

    it('should use custom debounce', () => {
      const config = {
        pattern: SubscriptionPatternType.WILDCARD,
        debounceMs: 500
      };

      const id = manager.subscribe(config);
      const sub = manager.getSubscription(id);

      expect(sub.config.debounceMs).toBe(500);
    });

    it('should use default batch size', () => {
      const config = {
        pattern: SubscriptionPatternType.WILDCARD
      };

      const id = manager.subscribe(config);
      const sub = manager.getSubscription(id);

      expect(sub.config.batchSize).toBe(10);
    });
  });

  describe('Metrics', () => {
    it('should track subscription count', () => {
      manager.subscribe({ pattern: SubscriptionPatternType.WILDCARD });
      manager.subscribe({ pattern: SubscriptionPatternType.WILDCARD });

      const metrics = manager.getMetrics();

      expect(metrics.subscriptionCount).toBe(2);
    });

    it('should track connection state', () => {
      const metrics = manager.getMetrics();

      expect(metrics.isConnected).toBe(false);
      expect(metrics.reconnectAttempts).toBe(0);
    });

    it('should track messages', () => {
      const metrics = manager.getMetrics();

      expect(metrics.messagesReceived).toBe(0);
      expect(metrics.messagesSent).toBe(0);
    });
  });

  describe('Query Methods', () => {
    it('should get subscription by ID', () => {
      const config = {
        pattern: SubscriptionPatternType.WILDCARD
      };

      const id = manager.subscribe(config);
      const sub = manager.getSubscription(id);

      expect(sub).toBeDefined();
      expect(sub.config.pattern).toBe(SubscriptionPatternType.WILDCARD);
    });

    it('should return null for unknown ID', () => {
      const sub = manager.getSubscription('unknown-id');
      expect(sub).toBeNull();
    });

    it('should get all subscriptions', () => {
      manager.subscribe({ pattern: SubscriptionPatternType.WILDCARD });
      manager.subscribe({ pattern: SubscriptionPatternType.WILDCARD });

      const subs = manager.getAllSubscriptions();

      expect(subs).toHaveLength(2);
      expect(subs[0].id).toBeDefined();
    });
  });

  describe('Performance', () => {
    it('should handle multiple subscriptions efficiently', () => {
      const start = Date.now();

      for (let i = 0; i < 100; i++) {
        manager.subscribe({ pattern: SubscriptionPatternType.WILDCARD });
      }

      const duration = Date.now() - start;

      expect(manager.subscriptions.size).toBe(100);
      expect(duration).toBeLessThan(1000); // Should complete in less than 1 second
    });

    it('should track latency metrics', () => {
      const metrics = manager.getMetrics();

      expect(metrics.avgLatency).toBeDefined();
      expect(metrics.p95Latency).toBeDefined();
    });
  });
});
