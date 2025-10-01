/**
 * @file End-to-end sidecar integration tests
 * @module test/sidecar/integration
 */

import { describe, it, expect, beforeAll, afterAll, vi } from 'vitest';
import { SidecarClient } from '../../src/sidecar/client.mjs';

describe('Sidecar Integration', () => {
  let client;

  // These tests require an actual KGC sidecar server running
  const SIDECAR_ADDRESS = process.env.KGC_SIDECAR_ADDRESS || 'localhost:50051';
  const RUN_INTEGRATION = process.env.RUN_INTEGRATION_TESTS === 'true';

  beforeAll(async () => {
    if (!RUN_INTEGRATION) {
      console.log('Skipping integration tests. Set RUN_INTEGRATION_TESTS=true to enable.');
      return;
    }

    client = new SidecarClient({
      address: SIDECAR_ADDRESS,
      maxRetries: 3,
      timeout: 5000
    });

    try {
      await client.connect();
    } catch (error) {
      console.error('Failed to connect to sidecar:', error.message);
      throw error;
    }
  });

  afterAll(async () => {
    if (client && client.connected) {
      await client.disconnect();
    }
  });

  describe.skipIf(!RUN_INTEGRATION)('health and connectivity', () => {
    it('should connect to sidecar service', () => {
      expect(client.connected).toBe(true);
    });

    it('should perform health check', async () => {
      const response = await client.healthCheck();

      expect(response).toBeDefined();
      expect(response.status).toBe('SERVING');
      expect(response.uptime_seconds).toBeGreaterThan(0);
    });

    it('should get sidecar metrics', async () => {
      const response = await client.getMetrics();

      expect(response).toBeDefined();
      expect(response.metrics).toBeDefined();
    });
  });

  describe.skipIf(!RUN_INTEGRATION)('transaction operations', () => {
    it('should apply transaction successfully', async () => {
      const response = await client.applyTransaction({
        delta: {
          additions: [
            {
              subject: 'http://example.org/resource1',
              predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
              object: 'http://example.org/Type',
              graph: 'http://example.org/default'
            }
          ],
          deletions: []
        },
        actor: 'integration-test',
        options: {
          strict_mode: false,
          enable_hooks: true
        }
      });

      expect(response).toBeDefined();
      expect(response.success).toBe(true);
      expect(response.receipt).toBeDefined();
      expect(response.receipt.committed).toBe(true);
      expect(response.receipt.transaction_id).toBeDefined();
    });

    it('should validate transaction receipt', async () => {
      const response = await client.applyTransaction({
        delta: {
          additions: [],
          deletions: []
        },
        actor: 'integration-test'
      });

      const { receipt } = response;

      expect(receipt.before_hash).toBeDefined();
      expect(receipt.before_hash.sha3).toBeDefined();
      expect(receipt.before_hash.blake3).toBeDefined();
      expect(receipt.after_hash).toBeDefined();
      expect(receipt.timestamp).toBeGreaterThan(0);
      expect(receipt.actor).toBe('integration-test');
    });
  });

  describe.skipIf(!RUN_INTEGRATION)('validation operations', () => {
    it('should validate graph against policy', async () => {
      const response = await client.validateGraph({
        quads: [
          {
            subject: 'http://example.org/resource1',
            predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
            object: 'http://example.org/Type',
            graph: 'http://example.org/default'
          }
        ],
        policyPack: 'default',
        strictMode: false
      });

      expect(response).toBeDefined();
      expect(response.valid).toBeDefined();
    });
  });

  describe.skipIf(!RUN_INTEGRATION)('policy operations', () => {
    it('should query policy pack', async () => {
      const response = await client.queryPolicy({
        policyPack: 'default',
        queryType: 'info'
      });

      expect(response).toBeDefined();
      expect(response.policy_pack).toBeDefined();
      expect(response.policy_pack.name).toBe('default');
    });
  });

  describe.skipIf(!RUN_INTEGRATION)('resilience patterns', () => {
    it('should retry on transient failures', async () => {
      // Simulate transient failure by forcing connection issues
      const originalExecute = client._execute.bind(client);
      let attemptCount = 0;

      client._execute = async function(...args) {
        attemptCount++;
        if (attemptCount === 1) {
          throw new Error('UNAVAILABLE');
        }
        return originalExecute(...args);
      };

      const response = await client.healthCheck();

      expect(response).toBeDefined();
      expect(attemptCount).toBe(2);

      client._execute = originalExecute;
    });

    it('should handle circuit breaker', async () => {
      const metrics = client.getClientMetrics();

      expect(metrics.circuitBreaker).toBeDefined();
      expect(metrics.circuitBreaker.state).toBe('CLOSED');
    });
  });

  describe.skipIf(!RUN_INTEGRATION)('performance', () => {
    it('should meet latency requirements', async () => {
      const iterations = 10;
      const latencies = [];

      for (let i = 0; i < iterations; i++) {
        const start = Date.now();
        await client.healthCheck();
        const duration = Date.now() - start;
        latencies.push(duration);
      }

      const avgLatency = latencies.reduce((a, b) => a + b, 0) / latencies.length;
      const p99Latency = latencies.sort((a, b) => a - b)[Math.floor(latencies.length * 0.99)];

      expect(avgLatency).toBeLessThan(100); // Average < 100ms
      expect(p99Latency).toBeLessThan(200); // p99 < 200ms
    });

    it('should handle concurrent requests', async () => {
      const concurrency = 10;
      const promises = [];

      for (let i = 0; i < concurrency; i++) {
        promises.push(client.healthCheck());
      }

      const results = await Promise.all(promises);

      expect(results).toHaveLength(concurrency);
      results.forEach(result => {
        expect(result.status).toBe('SERVING');
      });
    });
  });
});
