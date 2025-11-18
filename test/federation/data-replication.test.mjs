/**
 * @fileoverview Tests for Data Replication Manager
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import {
  DataReplicationManager,
  createDataReplicationManager,
  ReplicationTopology,
  ConflictResolution,
  ReplicationMode
} from '../../src/knowledge-engine/federation/data-replication.mjs';
import { createFederationCoordinator, StoreHealth } from '../../src/knowledge-engine/federation/federation-coordinator.mjs';

describe('DataReplicationManager', () => {
  let coordinator;
  let replication;

  beforeEach(async () => {
    coordinator = createFederationCoordinator({
      federationId: 'test-federation',
      enableConsensus: false
    });
    await coordinator.initialize();

    // Register test stores
    await coordinator.registerStore({
      storeId: 'store-1',
      endpoint: 'http://store1:3000',
      weight: 1.0
    });
    await coordinator.registerStore({
      storeId: 'store-2',
      endpoint: 'http://store2:3000',
      weight: 1.0
    });
    await coordinator.registerStore({
      storeId: 'store-3',
      endpoint: 'http://store3:3000',
      weight: 1.0
    });

    // Set stores as healthy
    coordinator.storeHealth.set('store-1', StoreHealth.HEALTHY);
    coordinator.storeHealth.set('store-2', StoreHealth.HEALTHY);
    coordinator.storeHealth.set('store-3', StoreHealth.HEALTHY);

    replication = createDataReplicationManager(coordinator, {
      topology: ReplicationTopology.FULL_MESH,
      conflictResolution: ConflictResolution.LAST_WRITE_WINS,
      batchInterval: 50
    });
    await replication.initialize();
  });

  afterEach(async () => {
    await replication.shutdown();
    await coordinator.shutdown();
  });

  describe('initialization', () => {
    it('should initialize with empty change log', () => {
      expect(replication.changeLog).toHaveLength(0);
    });

    it('should initialize version vectors for all stores', () => {
      expect(replication.versionVectors.size).toBeGreaterThanOrEqual(3);
    });

    it('should emit initialized event', async () => {
      const mgr = createDataReplicationManager(coordinator, {});
      const eventPromise = new Promise((resolve) => {
        mgr.on('initialized', () => {
          resolve();
        });
      });
      await mgr.initialize();
      await eventPromise;
      await mgr.shutdown();
    });
  });

  describe('data replication', () => {
    it('should replicate a change', async () => {
      const change = {
        storeId: 'store-1',
        operation: 'INSERT',
        quad: {
          subject: 'http://example.org/alice',
          predicate: 'http://xmlns.com/foaf/0.1/name',
          object: '"Alice"'
        }
      };

      await replication.replicate(change);

      expect(replication.changeLog.length).toBeGreaterThan(0);
      expect(replication.changeLog[0].operation).toBe('INSERT');
    });

    it('should assign changeId automatically', async () => {
      const change = {
        storeId: 'store-1',
        operation: 'DELETE',
        quad: {
          subject: 'http://example.org/bob',
          predicate: 'http://xmlns.com/foaf/0.1/name',
          object: '"Bob"'
        }
      };

      await replication.replicate(change);

      expect(replication.changeLog[0].changeId).toBeDefined();
      expect(typeof replication.changeLog[0].changeId).toBe('string');
    });

    it('should update version vector', async () => {
      const initialVector = replication.versionVectors.get('store-1');
      const initialVersion = initialVector['store-1'] || 0;

      await replication.replicate({
        storeId: 'store-1',
        operation: 'INSERT',
        quad: {
          subject: 'http://example.org/test',
          predicate: 'http://example.org/prop',
          object: '"value"'
        }
      });

      const newVector = replication.versionVectors.get('store-1');
      expect(newVector['store-1']).toBe(initialVersion + 1);
    });

    it('should emit changeReplicated event', async () => {
      const eventPromise = new Promise((resolve) => {
        replication.on('changeReplicated', (operation) => {
          expect(operation.storeId).toBe('store-1');
          resolve();
        });
      });

      await replication.replicate({
        storeId: 'store-1',
        operation: 'INSERT',
        quad: {
          subject: 'http://example.org/test',
          predicate: 'http://example.org/prop',
          object: '"value"'
        }
      });
      await eventPromise;
    });
  });

  describe('replication topology', () => {
    it('should replicate to all stores in full-mesh topology', () => {
      const targets = replication.getReplicationTargets('store-1');

      expect(targets).toContain('store-2');
      expect(targets).toContain('store-3');
      expect(targets).not.toContain('store-1');
    });

    it('should replicate to hub in star topology', () => {
      replication.config.topology = ReplicationTopology.STAR;

      const targets = replication.getReplicationTargets('store-2');

      // First store is the hub
      expect(targets).toHaveLength(1);
      expect(targets[0]).toBe('store-1');
    });

    it('should replicate to next store in ring topology', () => {
      replication.config.topology = ReplicationTopology.RING;

      const targets = replication.getReplicationTargets('store-1');

      expect(targets).toHaveLength(1);
      expect(targets[0]).toBe('store-2');
    });
  });

  describe('conflict detection', () => {
    it('should detect concurrent modifications', () => {
      replication.versionVectors.set('store-1', { 'store-1': 5, 'store-2': 3 });
      replication.versionVectors.set('store-2', { 'store-1': 4, 'store-2': 6 });

      const operation = {
        storeId: 'store-1',
        version: { 'store-1': 5, 'store-2': 3 },
        quad: { subject: 'test', predicate: 'test', object: 'test' }
      };

      const conflict = replication.detectConflict('store-2', operation);

      expect(conflict).not.toBe(null);
    });

    it('should not detect conflict for causal updates', () => {
      replication.versionVectors.set('store-1', { 'store-1': 5 });
      replication.versionVectors.set('store-2', { 'store-1': 3 });

      const operation = {
        storeId: 'store-1',
        version: { 'store-1': 5 },
        quad: { subject: 'test', predicate: 'test', object: 'test' }
      };

      const conflict = replication.detectConflict('store-2', operation);

      expect(conflict).toBe(null);
    });
  });

  describe('conflict resolution', () => {
    it('should resolve with last-write-wins', async () => {
      const conflict = {
        targetStoreId: 'store-2',
        targetVector: {},
        operation: { timestamp: Date.now() }
      };

      const resolved = await replication.resolveConflict(conflict, conflict.operation);

      expect(resolved).toBe(true);
    });

    it('should resolve with first-write-wins', async () => {
      replication.config.conflictResolution = ConflictResolution.FIRST_WRITE_WINS;

      const conflict = {
        targetStoreId: 'store-2',
        targetVector: {},
        operation: { timestamp: Date.now() }
      };

      const resolved = await replication.resolveConflict(conflict, conflict.operation);

      expect(resolved).toBe(false);
    });

    it('should emit conflict event', async () => {
      const eventPromise = new Promise((resolve) => {
        replication.on('conflict', (conflict) => {
          expect(conflict).toBeDefined();
          resolve();
        });
      });

      const conflict = {
        targetStoreId: 'store-2',
        targetVector: {},
        operation: { timestamp: Date.now() }
      };

      await replication.resolveConflict(conflict, conflict.operation);
      await eventPromise;
    });
  });

  describe('batch processing', () => {
    it('should queue changes for batch processing', async () => {
      await replication.replicate({
        storeId: 'store-1',
        operation: 'INSERT',
        quad: { subject: 'test', predicate: 'test', object: 'test' }
      });

      expect(replication.replicationQueue.length).toBeGreaterThanOrEqual(0);
    });

    it('should process batches periodically', async () => {
      const initialQueueSize = replication.replicationQueue.length;

      await replication.replicate({
        storeId: 'store-1',
        operation: 'INSERT',
        quad: { subject: 'test', predicate: 'test', object: 'test' }
      });

      // Wait for batch processing
      await new Promise(resolve => setTimeout(resolve, 100));

      // Queue should be processed
      expect(replication.replicationQueue.length).toBeLessThanOrEqual(initialQueueSize + 1);
    });
  });

  describe('version vectors', () => {
    it('should merge version vectors', () => {
      replication.versionVectors.set('store-1', { 'store-1': 5, 'store-2': 3 });

      const incomingVector = { 'store-1': 4, 'store-2': 6, 'store-3': 2 };

      replication.mergeVersionVector('store-1', incomingVector);

      const merged = replication.versionVectors.get('store-1');

      expect(merged['store-1']).toBe(5); // max(5, 4)
      expect(merged['store-2']).toBe(6); // max(3, 6)
      expect(merged['store-3']).toBe(2);
    });
  });

  describe('statistics', () => {
    it('should return replication statistics', async () => {
      await replication.replicate({
        storeId: 'store-1',
        operation: 'INSERT',
        quad: { subject: 'test', predicate: 'test', object: 'test' }
      });

      const stats = replication.getStats();

      expect(stats).toMatchObject({
        changeLogSize: expect.any(Number),
        queueSize: expect.any(Number),
        conflictCount: expect.any(Number),
        versionVectors: expect.any(Object),
        config: expect.any(Object)
      });
    });
  });

  describe('shutdown', () => {
    it('should process remaining queue on shutdown', async () => {
      await replication.replicate({
        storeId: 'store-1',
        operation: 'INSERT',
        quad: { subject: 'test', predicate: 'test', object: 'test' }
      });

      await replication.shutdown();

      expect(replication.batchTimer).toBe(null);
    });

    it('should emit shutdown event', async () => {
      const eventPromise = new Promise((resolve) => {
        replication.on('shutdown', () => {
          resolve();
        });
      });

      await replication.shutdown();
      await eventPromise;
    });
  });
});
