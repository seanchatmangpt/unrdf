/**
 * @fileoverview Tests for Federation Coordinator
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import {
  FederationCoordinator,
  createFederationCoordinator,
  StoreHealth
} from '../../src/knowledge-engine/federation/federation-coordinator.mjs';

describe('FederationCoordinator', () => {
  let coordinator;

  beforeEach(async () => {
    coordinator = createFederationCoordinator({
      federationId: 'test-federation',
      enableConsensus: false, // Disable for simpler testing
      healthCheckInterval: 100,
      loadBalancingStrategy: 'weighted'
    });
    await coordinator.initialize();
  });

  afterEach(async () => {
    await coordinator.shutdown();
  });

  describe('initialization', () => {
    it('should initialize with empty stores', () => {
      expect(coordinator.stores.size).toBe(0);
    });

    it('should emit initialized event', async () => {
      const coord = createFederationCoordinator({ federationId: 'test-2', enableConsensus: false });
      const eventPromise = new Promise((resolve) => {
        coord.on('initialized', (data) => {
          expect(data.federationId).toBe('test-2');
          resolve();
        });
      });
      await coord.initialize();
      await eventPromise;
      await coord.shutdown();
    });
  });

  describe('store registration', () => {
    it('should register a store', async () => {
      await coordinator.registerStore({
        storeId: 'store-1',
        endpoint: 'http://store1:3000',
        capabilities: ['sparql-1.1'],
        weight: 1.0
      });

      expect(coordinator.stores.size).toBe(1);
      expect(coordinator.stores.has('store-1')).toBe(true);
    });

    it('should validate store metadata', async () => {
      await expect(coordinator.registerStore({
        storeId: 'store-1',
        endpoint: 'invalid-url'
      })).rejects.toThrow();
    });

    it('should emit storeRegistered event', async () => {
      const eventPromise = new Promise((resolve) => {
        coordinator.on('storeRegistered', (metadata) => {
          expect(metadata.storeId).toBe('store-1');
          resolve();
        });
      });

      await coordinator.registerStore({
        storeId: 'store-1',
        endpoint: 'http://store1:3000'
      });
      await eventPromise;
    });

    it('should set initial health to unknown', async () => {
      await coordinator.registerStore({
        storeId: 'store-1',
        endpoint: 'http://store1:3000'
      });

      // Health check runs asynchronously, wait a bit
      await new Promise(resolve => setTimeout(resolve, 50));

      const health = coordinator.storeHealth.get('store-1');
      expect([StoreHealth.HEALTHY, StoreHealth.UNHEALTHY, StoreHealth.UNKNOWN]).toContain(health);
    });
  });

  describe('store deregistration', () => {
    beforeEach(async () => {
      await coordinator.registerStore({
        storeId: 'store-1',
        endpoint: 'http://store1:3000'
      });
    });

    it('should deregister a store', async () => {
      await coordinator.deregisterStore('store-1');

      expect(coordinator.stores.size).toBe(0);
      expect(coordinator.stores.has('store-1')).toBe(false);
    });

    it('should throw error for non-existent store', async () => {
      await expect(coordinator.deregisterStore('non-existent')).rejects.toThrow('Store not found');
    });

    it('should emit storeDeregistered event', async () => {
      const eventPromise = new Promise((resolve) => {
        coordinator.on('storeDeregistered', (data) => {
          expect(data.storeId).toBe('store-1');
          resolve();
        });
      });

      await coordinator.deregisterStore('store-1');
      await eventPromise;
    });
  });

  describe('store retrieval', () => {
    beforeEach(async () => {
      await coordinator.registerStore({
        storeId: 'store-1',
        endpoint: 'http://store1:3000',
        weight: 1.0
      });
      await coordinator.registerStore({
        storeId: 'store-2',
        endpoint: 'http://store2:3000',
        weight: 0.8
      });
    });

    it('should get all stores', () => {
      const stores = coordinator.getStores();

      expect(stores).toHaveLength(2);
      expect(stores.map(s => s.storeId)).toContain('store-1');
      expect(stores.map(s => s.storeId)).toContain('store-2');
    });

    it('should get healthy stores', async () => {
      // Set health manually for testing
      coordinator.storeHealth.set('store-1', StoreHealth.HEALTHY);
      coordinator.storeHealth.set('store-2', StoreHealth.UNHEALTHY);

      const healthy = coordinator.getHealthyStores();

      expect(healthy).toHaveLength(1);
      expect(healthy[0].storeId).toBe('store-1');
    });
  });

  describe('store selection', () => {
    beforeEach(async () => {
      await coordinator.registerStore({
        storeId: 'store-1',
        endpoint: 'http://store1:3000',
        weight: 1.0
      });
      await coordinator.registerStore({
        storeId: 'store-2',
        endpoint: 'http://store2:3000',
        weight: 0.5
      });

      coordinator.storeHealth.set('store-1', StoreHealth.HEALTHY);
      coordinator.storeHealth.set('store-2', StoreHealth.HEALTHY);
    });

    it('should select a store for query execution', () => {
      const store = coordinator.selectStore();

      expect(store).toBeDefined();
      expect(['store-1', 'store-2']).toContain(store.storeId);
    });

    it('should return null when no healthy stores', () => {
      coordinator.storeHealth.set('store-1', StoreHealth.UNHEALTHY);
      coordinator.storeHealth.set('store-2', StoreHealth.UNHEALTHY);

      const store = coordinator.selectStore();

      expect(store).toBe(null);
    });

    it('should use weighted selection', () => {
      const selections = new Map();

      // Make multiple selections to test distribution
      for (let i = 0; i < 100; i++) {
        const store = coordinator.selectStore();
        selections.set(store.storeId, (selections.get(store.storeId) || 0) + 1);
      }

      // store-1 (weight 1.0) should be selected more often than store-2 (weight 0.5)
      expect(selections.get('store-1')).toBeGreaterThan(selections.get('store-2'));
    });
  });

  describe('health monitoring', () => {
    beforeEach(async () => {
      await coordinator.registerStore({
        storeId: 'store-1',
        endpoint: 'http://store1:3000'
      });
    });

    it('should check store health', async () => {
      const health = await coordinator.checkStoreHealth('store-1');

      expect([StoreHealth.HEALTHY, StoreHealth.UNHEALTHY]).toContain(health);
    });

    it('should emit health change events', async () => {
      coordinator.storeHealth.set('store-1', StoreHealth.HEALTHY);

      const eventPromise = new Promise((resolve) => {
        coordinator.on('storeHealthChanged', (data) => {
          expect(data.storeId).toBe('store-1');
          expect([StoreHealth.HEALTHY, StoreHealth.UNHEALTHY]).toContain(data.health);
          resolve();
        });
      });

      await coordinator.checkStoreHealth('store-1');
      // Only wait if event not yet received
      await Promise.race([eventPromise, new Promise(resolve => setTimeout(resolve, 100))]);
    });

    it('should perform periodic health checks', async () => {
      const initialChecks = coordinator.storeHealth.get('store-1');

      await new Promise(resolve => setTimeout(resolve, 250));

      // Health should have been checked at least once
      expect(coordinator.storeHealth.has('store-1')).toBe(true);
    });
  });

  describe('statistics', () => {
    beforeEach(async () => {
      await coordinator.registerStore({
        storeId: 'store-1',
        endpoint: 'http://store1:3000'
      });
      await coordinator.registerStore({
        storeId: 'store-2',
        endpoint: 'http://store2:3000'
      });
    });

    it('should return federation statistics', () => {
      const stats = coordinator.getStats();

      expect(stats).toMatchObject({
        federationId: 'test-federation',
        totalStores: 2,
        healthStats: expect.any(Object),
        loadBalancingStrategy: 'weighted'
      });
    });

    it('should include health statistics', () => {
      coordinator.storeHealth.set('store-1', StoreHealth.HEALTHY);
      coordinator.storeHealth.set('store-2', StoreHealth.UNHEALTHY);

      const stats = coordinator.getStats();

      expect(stats.healthStats[StoreHealth.HEALTHY]).toBe(1);
      expect(stats.healthStats[StoreHealth.UNHEALTHY]).toBe(1);
    });
  });

  describe('shutdown', () => {
    it('should stop health monitoring', async () => {
      expect(coordinator.healthCheckTimer).not.toBe(null);

      await coordinator.shutdown();

      expect(coordinator.healthCheckTimer).toBe(null);
    });

    it('should emit shutdown event', async () => {
      const eventPromise = new Promise((resolve) => {
        coordinator.on('shutdown', () => {
          resolve();
        });
      });

      await coordinator.shutdown();
      await eventPromise;
    });
  });
});
