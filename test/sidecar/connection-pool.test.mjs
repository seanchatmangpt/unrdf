/**
 * @file Connection pool tests
 * @module test/sidecar/connection-pool
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { ConnectionPool } from '../../src/sidecar/connection-pool.mjs';

describe('ConnectionPool', () => {
  let pool;
  let channelId = 0;

  const createMockChannel = () => ({
    id: channelId++,
    close: vi.fn()
  });

  beforeEach(() => {
    channelId = 0;
    pool = new ConnectionPool(createMockChannel, {
      minConnections: 2,
      maxConnections: 5,
      idleTimeout: 1000,
      acquireTimeout: 500
    });
  });

  afterEach(async () => {
    if (pool) {
      await pool.close();
    }
  });

  describe('initialization', () => {
    it('should create minimum connections', async () => {
      // Wait for initialization
      await new Promise(resolve => setTimeout(resolve, 100));

      const stats = pool.getStats();
      expect(stats.total).toBe(2);
    });

    it('should accept valid configuration', () => {
      const customPool = new ConnectionPool(createMockChannel, {
        minConnections: 3,
        maxConnections: 10,
        idleTimeout: 2000
      });

      expect(customPool.config.minConnections).toBe(3);
      expect(customPool.config.maxConnections).toBe(10);

      customPool.close();
    });
  });

  describe('connection acquisition', () => {
    it('should acquire idle connection', async () => {
      await new Promise(resolve => setTimeout(resolve, 100));

      const connection = await pool.acquire();
      expect(connection).toBeDefined();
      expect(connection.channel).toBeDefined();

      pool.release(connection);
    });

    it('should create new connection when under max limit', async () => {
      await new Promise(resolve => setTimeout(resolve, 100));

      const connections = [];

      for (let i = 0; i < 3; i++) {
        connections.push(await pool.acquire());
      }

      const stats = pool.getStats();
      expect(stats.total).toBe(3);
      expect(stats.active).toBe(3);

      connections.forEach(c => pool.release(c));
    });

    it('should wait for available connection when at max limit', async () => {
      await new Promise(resolve => setTimeout(resolve, 100));

      // Acquire all connections
      const connections = [];
      for (let i = 0; i < 5; i++) {
        connections.push(await pool.acquire());
      }

      // Try to acquire one more - should wait
      const acquirePromise = pool.acquire();

      // Release a connection after delay
      setTimeout(() => {
        pool.release(connections[0]);
      }, 100);

      const connection = await acquirePromise;
      expect(connection).toBeDefined();

      connections.slice(1).forEach(c => pool.release(c));
      pool.release(connection);
    });

    it('should timeout when waiting too long', async () => {
      await new Promise(resolve => setTimeout(resolve, 100));

      // Acquire all connections
      const connections = [];
      for (let i = 0; i < 5; i++) {
        connections.push(await pool.acquire());
      }

      // Try to acquire without releasing - should timeout
      await expect(pool.acquire()).rejects.toThrow('Connection acquire timeout');

      connections.forEach(c => pool.release(c));
    });
  });

  describe('connection release', () => {
    it('should release connection back to pool', async () => {
      await new Promise(resolve => setTimeout(resolve, 100));

      const connection = await pool.acquire();
      pool.release(connection);

      const stats = pool.getStats();
      expect(stats.idle).toBe(2);
      expect(stats.active).toBe(0);
    });

    it('should process wait queue on release', async () => {
      await new Promise(resolve => setTimeout(resolve, 100));

      // Acquire all connections
      const connections = [];
      for (let i = 0; i < 5; i++) {
        connections.push(await pool.acquire());
      }

      // Start waiting for connection
      const acquirePromise = pool.acquire();

      // Release should resolve waiting promise
      pool.release(connections[0]);

      const connection = await acquirePromise;
      expect(connection).toBe(connections[0]);

      connections.slice(1).forEach(c => pool.release(c));
      pool.release(connection);
    });
  });

  describe('execute helper', () => {
    it('should execute function with pooled connection', async () => {
      await new Promise(resolve => setTimeout(resolve, 100));

      const result = await pool.execute(async (channel) => {
        expect(channel).toBeDefined();
        return 'success';
      });

      expect(result).toBe('success');

      const stats = pool.getStats();
      expect(stats.idle).toBe(2);
    });

    it('should release connection on error', async () => {
      await new Promise(resolve => setTimeout(resolve, 100));

      await expect(pool.execute(async () => {
        throw new Error('test error');
      })).rejects.toThrow('test error');

      const stats = pool.getStats();
      expect(stats.idle).toBe(2);
    });

    it('should record errors on connection', async () => {
      await new Promise(resolve => setTimeout(resolve, 100));

      await expect(pool.execute(async () => {
        throw new Error('test error');
      })).rejects.toThrow();

      // Connection should have error count
      const stats = pool.getStats();
      const connection = stats.connections.find(c => c.errorCount > 0);
      expect(connection).toBeDefined();
    });
  });

  describe('health checks', () => {
    it('should remove unhealthy connections', async () => {
      await new Promise(resolve => setTimeout(resolve, 100));

      const connection = await pool.acquire();

      // Mark as unhealthy
      connection.state = 'UNHEALTHY';
      pool.release(connection);

      // Wait for health check
      await new Promise(resolve => setTimeout(resolve, 1100));

      const stats = pool.getStats();
      expect(stats.unhealthy).toBe(0);
    });
  });

  describe('idle connection eviction', () => {
    it('should evict idle connections after timeout', async () => {
      await new Promise(resolve => setTimeout(resolve, 100));

      const stats1 = pool.getStats();
      expect(stats1.total).toBe(2);

      // Wait for eviction
      await new Promise(resolve => setTimeout(resolve, 2000));

      const stats2 = pool.getStats();
      expect(stats2.total).toBe(2); // Should maintain minimum
    });
  });

  describe('pool statistics', () => {
    it('should provide accurate statistics', async () => {
      await new Promise(resolve => setTimeout(resolve, 100));

      const conn1 = await pool.acquire();
      const conn2 = await pool.acquire();

      const stats = pool.getStats();

      expect(stats.total).toBe(2);
      expect(stats.active).toBe(2);
      expect(stats.idle).toBe(0);
      expect(stats.totalUsage).toBe(2);

      pool.release(conn1);
      pool.release(conn2);
    });
  });

  describe('pool closure', () => {
    it('should close all connections', async () => {
      await new Promise(resolve => setTimeout(resolve, 100));

      const connection = await pool.acquire();
      pool.release(connection);

      await pool.close();

      expect(pool.closed).toBe(true);
      expect(pool.connections.size).toBe(0);
    });

    it('should reject acquire after closure', async () => {
      await pool.close();

      await expect(pool.acquire()).rejects.toThrow('Connection pool is closed');
    });

    it('should reject waiting requests on closure', async () => {
      await new Promise(resolve => setTimeout(resolve, 100));

      // Acquire all connections
      for (let i = 0; i < 5; i++) {
        await pool.acquire();
      }

      const acquirePromise = pool.acquire();

      await pool.close();

      await expect(acquirePromise).rejects.toThrow('Connection pool closed');
    });
  });

  describe('events', () => {
    it('should emit connection events', async () => {
      const createdSpy = vi.fn();
      const acquiredSpy = vi.fn();
      const releasedSpy = vi.fn();

      pool.on('connectionCreated', createdSpy);
      pool.on('connectionAcquired', acquiredSpy);
      pool.on('connectionReleased', releasedSpy);

      await new Promise(resolve => setTimeout(resolve, 100));

      const connection = await pool.acquire();
      pool.release(connection);

      expect(createdSpy).toHaveBeenCalled();
      expect(acquiredSpy).toHaveBeenCalled();
      expect(releasedSpy).toHaveBeenCalled();
    });
  });
});
