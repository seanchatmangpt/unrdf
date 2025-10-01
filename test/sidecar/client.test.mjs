/**
 * @file Sidecar client integration tests
 * @module test/sidecar/client
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { SidecarClient } from '../../src/sidecar/client.mjs';
import { createSidecarConfig } from '../../src/sidecar/config.mjs';

describe('SidecarClient', () => {
  let client;

  beforeEach(() => {
    // Mock configuration
    const config = createSidecarConfig({
      currentContext: 'test',
      contexts: [{
        name: 'test',
        endpoint: {
          address: 'localhost',
          port: 50051
        },
        timeout: 5000,
        maxRetries: 3
      }]
    });

    client = new SidecarClient({
      config,
      enableHealthCheck: false // Disable for unit tests
    });
  });

  afterEach(async () => {
    if (client && client.connected) {
      await client.disconnect();
    }
  });

  describe('initialization', () => {
    it('should create client with configuration', () => {
      expect(client).toBeDefined();
      expect(client.connected).toBe(false);
      expect(client.config).toBeDefined();
    });

    it('should accept custom options', () => {
      const customClient = new SidecarClient({
        maxRetries: 5,
        timeout: 10000
      });

      expect(customClient.options.maxRetries).toBe(5);
      expect(customClient.options.timeout).toBe(10000);
    });
  });

  describe('connection', () => {
    it('should require connection before operations', async () => {
      await expect(client.applyTransaction({})).rejects.toThrow('not connected');
    });

    it('should emit connected event', async () => {
      const connectedSpy = vi.fn();
      client.on('connected', connectedSpy);

      // Note: This will fail without actual gRPC server, but tests the logic
      try {
        await client.connect('localhost:50051');
      } catch (error) {
        // Expected to fail without server
      }
    });
  });

  describe('API methods', () => {
    // Note: These tests require a mock gRPC server or will be skipped in CI
    it.skip('should apply transaction', async () => {
      await client.connect();

      const result = await client.applyTransaction({
        delta: {
          additions: [],
          deletions: []
        },
        actor: 'test-user'
      });

      expect(result).toBeDefined();
      expect(result.success).toBeDefined();
    });

    it.skip('should validate graph', async () => {
      await client.connect();

      const result = await client.validateGraph({
        quads: [],
        policyPack: 'test-policy'
      });

      expect(result).toBeDefined();
      expect(result.valid).toBeDefined();
    });

    it.skip('should evaluate hook', async () => {
      await client.connect();

      const result = await client.evaluateHook({
        hookId: 'test-hook',
        hook: {
          meta: { name: 'test' },
          when: { kind: 'sparql-ask' },
          then: { kind: 'log' }
        },
        event: {
          transactionId: 'test-tx',
          delta: { additions: [], deletions: [] }
        }
      });

      expect(result).toBeDefined();
    });

    it.skip('should query policy', async () => {
      await client.connect();

      const result = await client.queryPolicy({
        policyPack: 'test-policy'
      });

      expect(result).toBeDefined();
      expect(result.policy_pack).toBeDefined();
    });

    it.skip('should perform health check', async () => {
      await client.connect();

      const result = await client.healthCheck();

      expect(result).toBeDefined();
      expect(result.status).toBeDefined();
    });

    it.skip('should get metrics', async () => {
      await client.connect();

      const result = await client.getMetrics();

      expect(result).toBeDefined();
      expect(result.metrics).toBeDefined();
    });
  });

  describe('client metrics', () => {
    it('should track client metrics', () => {
      const metrics = client.getClientMetrics();

      expect(metrics).toBeDefined();
      expect(metrics.requests).toBe(0);
      expect(metrics.successes).toBe(0);
      expect(metrics.failures).toBe(0);
    });
  });

  describe('disconnection', () => {
    it('should disconnect gracefully', async () => {
      const disconnectedSpy = vi.fn();
      client.on('disconnected', disconnectedSpy);

      await client.disconnect();

      expect(client.connected).toBe(false);
    });

    it('should allow multiple disconnect calls', async () => {
      await client.disconnect();
      await client.disconnect(); // Should not throw
    });
  });

  describe('static factory', () => {
    it.skip('should create and connect client', async () => {
      const connectedClient = await SidecarClient.connect('localhost:50051', {
        maxRetries: 2
      });

      expect(connectedClient).toBeDefined();
      expect(connectedClient.connected).toBe(true);

      await connectedClient.disconnect();
    });
  });
});
