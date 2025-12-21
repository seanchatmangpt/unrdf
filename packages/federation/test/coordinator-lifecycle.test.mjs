/**
 * @vitest-environment node
 * @fileoverview Memory leak detection tests for Federation Coordinator
 *
 * @description
 * Tests for resource cleanup and memory leak prevention in FederationCoordinator.
 * Focuses on health check timer cleanup and event listener removal.
 *
 * CRITICAL LEAKS DETECTED:
 * 1. Health check timer not cleared on shutdown()
 * 2. EventEmitter listeners accumulate without cleanup
 * 3. Consensus manager event listeners not removed
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { FederationCoordinator, createFederationCoordinator } from '../src/federation/federation-coordinator.mjs';
import { EventEmitter } from 'events';

describe('FederationCoordinator Lifecycle Management', () => {
  let coordinator;

  beforeEach(() => {
    vi.useFakeTimers();
  });

  afterEach(async () => {
    if (coordinator) {
      await coordinator.shutdown();
      coordinator = null;
    }
    vi.restoreAllMocks();
    vi.useRealTimers();
  });

  describe('Health Check Timer Cleanup', () => {
    it('should cleanup health check timer on shutdown', async () => {
      coordinator = createFederationCoordinator({
        healthCheckInterval: 100,
        enableConsensus: false, // Disable consensus for isolated test
      });

      await coordinator.initialize();

      // Verify timer is active
      expect(coordinator.healthCheckTimer).not.toBeNull();
      const timerId = coordinator.healthCheckTimer;

      // Shutdown
      await coordinator.shutdown();

      // Verify timer is cleared
      expect(coordinator.healthCheckTimer).toBeNull();

      // Verify timer is actually cleared (not just set to null)
      // This is the CRITICAL assertion - if timer leaks, it will still fire
      const timerStillActive = vi.getTimerCount() > 0;
      expect(timerStillActive).toBe(false);
    });

    it('should not leak timers when creating multiple coordinators', async () => {
      const coordinators = [];

      // Create 10 coordinators
      for (let i = 0; i < 10; i++) {
        const coord = createFederationCoordinator({
          healthCheckInterval: 100,
          enableConsensus: false,
        });
        await coord.initialize();
        coordinators.push(coord);
      }

      // Initial state: 10 active timers
      expect(vi.getTimerCount()).toBe(10);

      // Shutdown first 5
      for (let i = 0; i < 5; i++) {
        await coordinators[i].shutdown();
      }

      // Should have 5 timers remaining
      expect(vi.getTimerCount()).toBe(5);

      // Shutdown remaining 5
      for (let i = 5; i < 10; i++) {
        await coordinators[i].shutdown();
      }

      // All timers should be cleared
      expect(vi.getTimerCount()).toBe(0);
    });

    it('should handle shutdown without initialization', async () => {
      coordinator = createFederationCoordinator({
        healthCheckInterval: 100,
        enableConsensus: false,
      });

      // Shutdown without initializing
      await expect(coordinator.shutdown()).resolves.not.toThrow();
      expect(coordinator.healthCheckTimer).toBeNull();
    });

    it('should handle multiple shutdown calls idempotently', async () => {
      coordinator = createFederationCoordinator({
        healthCheckInterval: 100,
        enableConsensus: false,
      });

      await coordinator.initialize();
      expect(coordinator.healthCheckTimer).not.toBeNull();

      // First shutdown
      await coordinator.shutdown();
      expect(coordinator.healthCheckTimer).toBeNull();

      // Second shutdown (should not throw)
      await expect(coordinator.shutdown()).resolves.not.toThrow();
      expect(coordinator.healthCheckTimer).toBeNull();
    });

    it('should clear timer even if health check is in progress', async () => {
      coordinator = createFederationCoordinator({
        healthCheckInterval: 100,
        enableConsensus: false,
      });

      await coordinator.initialize();

      // Register a store to trigger health checks
      await coordinator.registerStore({
        storeId: 'test-store',
        endpoint: 'http://localhost:3000/sparql',
      });

      // Advance timer to trigger health check
      vi.advanceTimersByTime(50);

      // Shutdown while health check might be pending
      await coordinator.shutdown();

      // Verify timer is cleared
      expect(coordinator.healthCheckTimer).toBeNull();
      expect(vi.getTimerCount()).toBe(0);
    });
  });

  describe('Event Listener Cleanup', () => {
    it('should remove all event listeners on shutdown', async () => {
      coordinator = createFederationCoordinator({
        healthCheckInterval: 100,
        enableConsensus: false,
      });

      await coordinator.initialize();

      // Add multiple listeners
      const listener1 = vi.fn();
      const listener2 = vi.fn();
      const listener3 = vi.fn();

      coordinator.on('storeRegistered', listener1);
      coordinator.on('storeDeregistered', listener2);
      coordinator.on('storeHealthChanged', listener3);

      // Verify listeners are registered
      expect(coordinator.listenerCount('storeRegistered')).toBe(1);
      expect(coordinator.listenerCount('storeDeregistered')).toBe(1);
      expect(coordinator.listenerCount('storeHealthChanged')).toBe(1);

      // Shutdown
      await coordinator.shutdown();

      // Verify listeners are removed
      // Note: This test will FAIL if shutdown() doesn't call removeAllListeners()
      expect(coordinator.listenerCount('storeRegistered')).toBe(0);
      expect(coordinator.listenerCount('storeDeregistered')).toBe(0);
      expect(coordinator.listenerCount('storeHealthChanged')).toBe(0);
    });

    it('should not leak listeners across multiple coordinator instances', async () => {
      const coordinators = [];
      const listeners = [];

      // Create 100 coordinators with listeners
      for (let i = 0; i < 100; i++) {
        const coord = createFederationCoordinator({
          healthCheckInterval: 100,
          enableConsensus: false,
        });
        await coord.initialize();

        const listener = vi.fn();
        coord.on('storeRegistered', listener);
        coord.on('storeHealthChanged', listener);

        coordinators.push(coord);
        listeners.push(listener);
      }

      // Verify listeners are registered
      for (const coord of coordinators) {
        expect(coord.listenerCount('storeRegistered')).toBe(1);
        expect(coord.listenerCount('storeHealthChanged')).toBe(1);
      }

      // Shutdown all
      for (const coord of coordinators) {
        await coord.shutdown();
      }

      // Verify all listeners are removed
      for (const coord of coordinators) {
        expect(coord.listenerCount('storeRegistered')).toBe(0);
        expect(coord.listenerCount('storeHealthChanged')).toBe(0);
      }
    });

    it('should emit shutdown event before cleanup', async () => {
      coordinator = createFederationCoordinator({
        healthCheckInterval: 100,
        enableConsensus: false,
      });

      await coordinator.initialize();

      const shutdownListener = vi.fn();
      coordinator.on('shutdown', shutdownListener);

      await coordinator.shutdown();

      // Shutdown event should be emitted
      expect(shutdownListener).toHaveBeenCalledTimes(1);
    });
  });

  describe('Consensus Manager Cleanup', () => {
    it('should cleanup consensus manager on shutdown', async () => {
      coordinator = createFederationCoordinator({
        healthCheckInterval: 100,
        enableConsensus: true,
      });

      await coordinator.initialize();

      // Verify consensus is initialized
      expect(coordinator.consensus).not.toBeNull();

      // Mock consensus shutdown
      const consensusShutdown = vi.spyOn(coordinator.consensus, 'shutdown');

      await coordinator.shutdown();

      // Verify consensus.shutdown() was called
      expect(consensusShutdown).toHaveBeenCalledTimes(1);
    });

    it('should remove consensus event listeners on shutdown', async () => {
      coordinator = createFederationCoordinator({
        healthCheckInterval: 100,
        enableConsensus: true,
      });

      await coordinator.initialize();

      // Consensus should have 'commandApplied' listener
      const consensusListenerCount = coordinator.consensus.listenerCount('commandApplied');
      expect(consensusListenerCount).toBeGreaterThan(0);

      await coordinator.shutdown();

      // Note: This test will FAIL if consensus listeners are not removed
      // Current implementation may leak consensus listeners
      const afterShutdownCount = coordinator.consensus.listenerCount('commandApplied');
      expect(afterShutdownCount).toBe(0);
    });
  });

  /**
   * Helper to measure memory usage
   * @returns {number} Heap used in bytes
   */
  function getHeapUsed() {
    if (global.gc) {
      global.gc();
    }
    return process.memoryUsage().heapUsed;
  }

  describe('Memory Profiling Helpers', () => {

    it('should not leak memory when cycling coordinators', async () => {
      // Force garbage collection before test
      const initialMemory = getHeapUsed();

      // Create and destroy 1000 coordinators
      for (let i = 0; i < 1000; i++) {
        const coord = createFederationCoordinator({
          healthCheckInterval: 100,
          enableConsensus: false,
        });
        await coord.initialize();

        // Register stores
        await coord.registerStore({
          storeId: `store-${i}`,
          endpoint: `http://localhost:${3000 + i}/sparql`,
        });

        await coord.shutdown();
      }

      // Force garbage collection
      const finalMemory = getHeapUsed();
      const memoryIncrease = finalMemory - initialMemory;

      // Memory increase should be minimal (<10MB for 1000 cycles)
      // Note: This test will FAIL if coordinators leak memory
      const maxAcceptableIncrease = 10 * 1024 * 1024; // 10MB
      expect(memoryIncrease).toBeLessThan(maxAcceptableIncrease);

      console.log(`Memory increase: ${(memoryIncrease / 1024 / 1024).toFixed(2)} MB`);
    });

    it('should not leak memory with health checks running', async () => {
      coordinator = createFederationCoordinator({
        healthCheckInterval: 10, // Fast health checks
        enableConsensus: false,
      });

      await coordinator.initialize();

      // Register 100 stores
      for (let i = 0; i < 100; i++) {
        await coordinator.registerStore({
          storeId: `store-${i}`,
          endpoint: `http://localhost:${3000 + i}/sparql`,
        });
      }

      const beforeMemory = getHeapUsed();

      // Run 1000 health check cycles
      for (let i = 0; i < 1000; i++) {
        vi.advanceTimersByTime(10);
        await Promise.resolve(); // Allow health checks to complete
      }

      const afterMemory = getHeapUsed();
      const memoryIncrease = afterMemory - beforeMemory;

      // Health checks should not accumulate memory
      // Note: 100 stores * 1000 health checks = significant OTEL span creation
      // Allow 15MB for OTEL overhead, V8 string interning, etc.
      const maxAcceptableIncrease = 15 * 1024 * 1024; // 15MB
      expect(memoryIncrease).toBeLessThan(maxAcceptableIncrease);

      await coordinator.shutdown();
    });
  });

  describe('Store Registration Cleanup', () => {
    it('should cleanup store references on deregister', async () => {
      coordinator = createFederationCoordinator({
        healthCheckInterval: 100,
        enableConsensus: false,
      });

      await coordinator.initialize();

      // Register 100 stores
      for (let i = 0; i < 100; i++) {
        await coordinator.registerStore({
          storeId: `store-${i}`,
          endpoint: `http://localhost:${3000 + i}/sparql`,
        });
      }

      expect(coordinator.stores.size).toBe(100);
      expect(coordinator.storeHealth.size).toBe(100);

      // Deregister all stores
      for (let i = 0; i < 100; i++) {
        await coordinator.deregisterStore(`store-${i}`);
      }

      // All stores should be removed
      expect(coordinator.stores.size).toBe(0);
      expect(coordinator.storeHealth.size).toBe(0);

      await coordinator.shutdown();
    });

    it('should not leak store metadata after deregister', async () => {
      coordinator = createFederationCoordinator({
        healthCheckInterval: 100,
        enableConsensus: false,
      });

      await coordinator.initialize();

      const initialMemory = getHeapUsed();

      // Cycle through 10000 store registrations/deregistrations
      for (let i = 0; i < 10000; i++) {
        await coordinator.registerStore({
          storeId: `store-${i}`,
          endpoint: `http://localhost:${3000 + (i % 100)}/sparql`,
          metadata: {
            data: Buffer.alloc(1024), // 1KB per store
          },
        });

        await coordinator.deregisterStore(`store-${i}`);
      }

      const finalMemory = getHeapUsed();
      const memoryIncrease = finalMemory - initialMemory;

      // Should not accumulate store metadata
      const maxAcceptableIncrease = 5 * 1024 * 1024; // 5MB
      expect(memoryIncrease).toBeLessThan(maxAcceptableIncrease);

      await coordinator.shutdown();
    });
  });

  describe('Edge Cases', () => {
    it('should handle shutdown during store registration', async () => {
      coordinator = createFederationCoordinator({
        healthCheckInterval: 100,
        enableConsensus: false,
      });

      await coordinator.initialize();

      // Start registering stores (don't await)
      const registrations = [];
      for (let i = 0; i < 10; i++) {
        registrations.push(
          coordinator.registerStore({
            storeId: `store-${i}`,
            endpoint: `http://localhost:${3000 + i}/sparql`,
          })
        );
      }

      // Shutdown immediately
      await coordinator.shutdown();

      // Registrations should complete or fail gracefully
      await expect(Promise.allSettled(registrations)).resolves.toBeDefined();
    });

    it('should handle rapid initialize/shutdown cycles', async () => {
      for (let i = 0; i < 100; i++) {
        coordinator = createFederationCoordinator({
          healthCheckInterval: 100,
          enableConsensus: false,
        });

        await coordinator.initialize();
        await coordinator.shutdown();
      }

      // Should complete without errors
      expect(vi.getTimerCount()).toBe(0);
    });
  });

  describe('Destroy Method (Phase 1)', () => {
    it('should cleanup all resources on destroy()', async () => {
      coordinator = createFederationCoordinator({
        healthCheckInterval: 100,
        enableConsensus: false,
      });

      await coordinator.initialize();

      // Register test stores
      await coordinator.registerStore({
        storeId: 'test-store-1',
        endpoint: 'http://localhost:3000/sparql',
      });
      await coordinator.registerStore({
        storeId: 'test-store-2',
        endpoint: 'http://localhost:3001/sparql',
      });

      // Verify stores registered
      expect(coordinator.stores.size).toBe(2);
      expect(coordinator.healthCheckTimer).not.toBeNull();

      // Destroy (equivalent to shutdown for coordinator)
      await coordinator.shutdown();

      // Verify cleanup
      expect(coordinator.healthCheckTimer).toBeNull();
      expect(coordinator.listenerCount('storeRegistered')).toBe(0);
      expect(coordinator.listenerCount('storeDeregistered')).toBe(0);
      expect(coordinator.listenerCount('storeHealthChanged')).toBe(0);
      expect(vi.getTimerCount()).toBe(0);
    });

    it('should cleanup all resources on destroy() with consensus enabled', async () => {
      coordinator = createFederationCoordinator({
        healthCheckInterval: 100,
        enableConsensus: true,
      });

      await coordinator.initialize();

      // Verify consensus is initialized
      expect(coordinator.consensus).not.toBeNull();

      // Destroy
      await coordinator.shutdown();

      // Verify cleanup
      expect(coordinator.healthCheckTimer).toBeNull();
      expect(vi.getTimerCount()).toBe(0);
    });
  });
});
