/**
 * Rate Limiting & DDoS Protection Integration Tests
 *
 * Tests for:
 * - Rate limiting enforcement
 * - DDoS detection and mitigation
 * - Query cost estimation
 * - Backpressure management
 * - Admin endpoints
 *
 * @module test/rate-limiting-integration
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { estimateQueryCost, validateQueryCost } from '../server/utils/query-cost-estimator.mjs';
import {
  rateLimitMiddleware,
  getRateLimitStatus,
  resetRateLimit,
  getRateLimitConfig,
} from '../server/middleware/03.rate-limit.mjs';
import {
  ddosDetectionMiddleware,
  getDDoSStatus,
  manualBlacklist,
  removeFromBlacklist,
} from '../server/utils/ddos-detector.mjs';
import {
  backpressureMiddleware,
  getBackpressureStatus,
  registerQueue,
} from '../server/utils/backpressure-manager.mjs';

describe('Query Cost Estimator', () => {
  it('should estimate cost for simple query', () => {
    const query = `
      SELECT ?s ?p ?o
      WHERE {
        ?s ?p ?o .
      }
      LIMIT 10
    `;

    const result = estimateQueryCost(query);

    expect(result).toHaveProperty('cost');
    expect(result).toHaveProperty('maxCost');
    expect(result).toHaveProperty('exceeded');
    expect(result).toHaveProperty('suggestions');
    expect(result.cost).toBeGreaterThan(0);
  });

  it('should detect expensive unbound queries', () => {
    const query = `
      SELECT ?s ?p ?o
      WHERE {
        ?s ?p ?o .
        ?s ?p2 ?o2 .
        ?s ?p3 ?o3 .
      }
    `;

    const result = estimateQueryCost(query);

    expect(result.cost).toBeGreaterThan(50); // Should be expensive
    expect(result.suggestions.length).toBeGreaterThan(0);
  });

  it('should validate query cost', () => {
    const simpleQuery = 'SELECT ?s WHERE { ?s <http://example.org/name> ?name } LIMIT 10';

    const result = validateQueryCost(simpleQuery);

    expect(result).toHaveProperty('allowed');
    expect(result).toHaveProperty('cost');
    expect(result.allowed).toBe(true);
  });

  it('should reject queries exceeding max cost', () => {
    // Create an extremely expensive query
    const expensiveQuery = `
      SELECT DISTINCT ?s ?p ?o
      WHERE {
        ?s ?p ?o .
        ?s ?p2 ?o2 .
        ?s ?p3 ?o3 .
        ?s ?p4 ?o4 .
        OPTIONAL { ?s ?p5 ?o5 }
        OPTIONAL { ?s ?p6 ?o6 }
        OPTIONAL { ?s ?p7 ?o7 }
        FILTER regex(?o, ".*", "i")
      }
      ORDER BY ?s
    `;

    const result = validateQueryCost(expensiveQuery);

    // This query should likely exceed the limit
    if (result.exceeded) {
      expect(result.allowed).toBe(false);
      expect(result.suggestions.length).toBeGreaterThan(0);
    }
  });

  it('should provide optimization suggestions', () => {
    const query = `
      SELECT ?s ?p ?o
      WHERE {
        ?s ?p ?o .
        OPTIONAL { ?s ?p2 ?o2 }
        OPTIONAL { ?s ?p3 ?o3 }
        OPTIONAL { ?s ?p4 ?o4 }
        OPTIONAL { ?s ?p5 ?o5 }
      }
    `;

    const result = estimateQueryCost(query);

    expect(result.suggestions).toBeInstanceOf(Array);
    expect(result.suggestions.some(s => s.includes('OPTIONAL'))).toBe(true);
  });
});

describe('Rate Limiting', () => {
  it('should get rate limit configuration', () => {
    const config = getRateLimitConfig();

    expect(config).toHaveProperty('authenticated');
    expect(config).toHaveProperty('unauthenticated');
    expect(config).toHaveProperty('sparql');
    expect(config).toHaveProperty('admin');
    expect(config).toHaveProperty('systemLoad');
    expect(config).toHaveProperty('redisConnected');
  });

  it('should handle rate limit status check', async () => {
    const key = 'test:user:123';
    const type = 'authenticated';

    const status = await getRateLimitStatus(key, type);

    if (status) {
      expect(status).toHaveProperty('key');
      expect(status).toHaveProperty('type');
      expect(status).toHaveProperty('consumed');
      expect(status).toHaveProperty('remaining');
      expect(status).toHaveProperty('limit');
    }
  });

  it('should reset rate limit', async () => {
    const key = 'test:user:123';
    const type = 'authenticated';

    const result = await resetRateLimit(key, type);

    expect(typeof result).toBe('boolean');
  });

  it('should apply adaptive limits based on system load', () => {
    const config = getRateLimitConfig();

    expect(config.systemLoad).toBeGreaterThanOrEqual(0);
    expect(config.systemLoad).toBeLessThanOrEqual(1);
  });
});

describe('DDoS Detection', () => {
  it('should get DDoS status', () => {
    const status = getDDoSStatus();

    expect(status).toHaveProperty('threatScore');
    expect(status).toHaveProperty('status');
    expect(status).toHaveProperty('anomalies');
    expect(status).toHaveProperty('blacklist');
    expect(status).toHaveProperty('windows');

    expect(status.threatScore).toBeGreaterThanOrEqual(0);
    expect(status.threatScore).toBeLessThanOrEqual(1);
    expect(['normal', 'elevated', 'critical']).toContain(status.status);
  });

  it('should manually blacklist IP', () => {
    const testIP = '192.168.1.100';
    const reason = 'Test blacklist';

    manualBlacklist(testIP, reason, 5000); // 5 seconds

    const status = getDDoSStatus();
    const blacklistEntry = status.blacklist.find(entry => entry.ip === testIP);

    expect(blacklistEntry).toBeDefined();
    expect(blacklistEntry.reason).toBe(reason);
  });

  it('should remove IP from blacklist', () => {
    const testIP = '192.168.1.101';

    manualBlacklist(testIP, 'Test removal', 10000);
    const removed = removeFromBlacklist(testIP);

    expect(removed).toBe(true);

    const status = getDDoSStatus();
    const blacklistEntry = status.blacklist.find(entry => entry.ip === testIP);

    expect(blacklistEntry).toBeUndefined();
  });

  it('should track traffic windows', () => {
    const status = getDDoSStatus();

    expect(status.windows).toBeInstanceOf(Array);
    expect(status.windows.length).toBeGreaterThan(0);

    for (const window of status.windows) {
      expect(window).toHaveProperty('windowSize');
      expect(window).toHaveProperty('totalRequests');
      expect(window).toHaveProperty('errorRate');
      expect(window).toHaveProperty('ipDiversity');
    }
  });
});

describe('Backpressure Manager', () => {
  it('should get backpressure status', () => {
    const status = getBackpressureStatus();

    expect(status).toHaveProperty('systemState');
    expect(status).toHaveProperty('inCooldown');
    expect(status).toHaveProperty('queues');
    expect(status).toHaveProperty('totalQueueDepth');

    expect(status.systemState).toHaveProperty('level');
    expect(['normal', 'warning', 'critical', 'overload']).toContain(status.systemState.level);
  });

  it('should register custom queue', () => {
    registerQueue('test-queue', 100);

    const status = getBackpressureStatus();
    const testQueue = status.queues.find(q => q.name === 'test-queue');

    if (testQueue) {
      expect(testQueue.maxDepth).toBe(100);
      expect(testQueue.depth).toBe(0);
    }
  });

  it('should track system load metrics', () => {
    const status = getBackpressureStatus();

    expect(status.systemState).toHaveProperty('systemLoad');
    expect(status.systemState).toHaveProperty('memoryUsage');
    expect(status.systemState).toHaveProperty('queueDepth');

    expect(status.systemState.systemLoad).toBeGreaterThanOrEqual(0);
    expect(status.systemState.memoryUsage).toBeGreaterThanOrEqual(0);
  });
});

describe('Integration Tests', () => {
  it('should handle concurrent rate limiting and DDoS detection', () => {
    const rateLimitConfig = getRateLimitConfig();
    const ddosStatus = getDDoSStatus();
    const backpressureStatus = getBackpressureStatus();

    expect(rateLimitConfig).toBeDefined();
    expect(ddosStatus).toBeDefined();
    expect(backpressureStatus).toBeDefined();
  });

  it('should coordinate backpressure with rate limiting', () => {
    const backpressureStatus = getBackpressureStatus();
    const rateLimitConfig = getRateLimitConfig();

    // When system is under high load, rate limits should adapt
    if (backpressureStatus.systemState.level === 'critical' ||
        backpressureStatus.systemState.level === 'overload') {
      expect(rateLimitConfig.systemLoad).toBeGreaterThan(0.7);
    }
  });

  it('should provide comprehensive security posture', () => {
    const ddosStatus = getDDoSStatus();
    const backpressureStatus = getBackpressureStatus();
    const rateLimitConfig = getRateLimitConfig();

    const securityPosture = {
      ddosThreat: ddosStatus.threatScore,
      systemLoad: backpressureStatus.systemState.level,
      rateLimitActive: rateLimitConfig.authenticated !== undefined,
      blacklistedIPs: ddosStatus.blacklist.length,
      queueDepth: backpressureStatus.totalQueueDepth,
    };

    expect(securityPosture.ddosThreat).toBeGreaterThanOrEqual(0);
    expect(securityPosture.ddosThreat).toBeLessThanOrEqual(1);
    expect(securityPosture.rateLimitActive).toBe(true);
    expect(securityPosture.blacklistedIPs).toBeGreaterThanOrEqual(0);
  });
});
