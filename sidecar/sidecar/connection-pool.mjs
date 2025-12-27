/**
 * @file gRPC connection pool for efficient resource management
 * @module sidecar/connection-pool
 *
 * @description
 * Connection pool for managing multiple gRPC channels with health checking and automatic cleanup.
 */

import { z } from 'zod';
import { EventEmitter } from 'events';

/**
 * Connection pool configuration schema
 */
const PoolConfigSchema = z.object({
  minConnections: z.number().int().min(1).default(2),
  maxConnections: z.number().int().min(1).default(10),
  idleTimeout: z.number().int().positive().default(60000),
  acquireTimeout: z.number().int().positive().default(5000),
  healthCheckInterval: z.number().int().positive().default(30000),
  evictionRunInterval: z.number().int().positive().default(60000)
});

/**
 * Connection state
 */
const ConnectionState = {
  IDLE: 'IDLE',
  ACTIVE: 'ACTIVE',
  CLOSED: 'CLOSED',
  UNHEALTHY: 'UNHEALTHY'
};

/**
 * Connection wrapper
 */
class PooledConnection {
  constructor(channel, id) {
    this.id = id;
    this.channel = channel;
    this.state = ConnectionState.IDLE;
    this.createdAt = Date.now();
    this.lastUsedAt = Date.now();
    this.usageCount = 0;
    this.errorCount = 0;
  }

  /**
   * Mark connection as active
   */
  acquire() {
    this.state = ConnectionState.ACTIVE;
    this.usageCount++;
    this.lastUsedAt = Date.now();
  }

  /**
   * Mark connection as idle
   */
  release() {
    this.state = ConnectionState.IDLE;
    this.lastUsedAt = Date.now();
  }

  /**
   * Check if connection is idle
   */
  isIdle() {
    return this.state === ConnectionState.IDLE;
  }

  /**
   * Check if connection is healthy
   */
  isHealthy() {
    return this.state !== ConnectionState.CLOSED &&
           this.state !== ConnectionState.UNHEALTHY;
  }

  /**
   * Check if connection exceeded idle timeout
   */
  isExpired(idleTimeout) {
    return Date.now() - this.lastUsedAt > idleTimeout;
  }

  /**
   * Record error
   */
  recordError() {
    this.errorCount++;
    if (this.errorCount >= 3) {
      this.state = ConnectionState.UNHEALTHY;
    }
  }

  /**
   * Close connection
   */
  async close() {
    if (this.state !== ConnectionState.CLOSED) {
      this.state = ConnectionState.CLOSED;
      this.channel.close();
    }
  }

  /**
   * Get connection metrics
   */
  getMetrics() {
    return {
      id: this.id,
      state: this.state,
      age: Date.now() - this.createdAt,
      idleTime: Date.now() - this.lastUsedAt,
      usageCount: this.usageCount,
      errorCount: this.errorCount
    };
  }
}

/**
 * gRPC connection pool
 */
export class ConnectionPool extends EventEmitter {
  /**
   * Create a new connection pool
   * @param {Function} createChannel - Function to create gRPC channel
   * @param {Object} [config] - Pool configuration
   */
  constructor(createChannel, config = {}) {
    super();
    this.createChannel = createChannel;
    this.config = PoolConfigSchema.parse(config);
    this.connections = new Map();
    this.nextConnectionId = 0;
    this.waitQueue = [];
    this.closed = false;
    this.healthCheckTimer = null;
    this.evictionTimer = null;

    // Initialize minimum connections
    this._initializePool();
  }

  /**
   * Initialize pool with minimum connections
   * @private
   */
  async _initializePool() {
    const promises = [];

    for (let i = 0; i < this.config.minConnections; i++) {
      promises.push(this._createConnection());
    }

    await Promise.all(promises);

    // Start background tasks
    this._startHealthChecks();
    this._startEvictionTimer();
  }

  /**
   * Create a new connection
   * @returns {Promise<PooledConnection>} Pooled connection
   * @private
   */
  async _createConnection() {
    const id = this.nextConnectionId++;
    const channel = await this.createChannel();
    const connection = new PooledConnection(channel, id);

    this.connections.set(id, connection);
    this.emit('connectionCreated', { id, total: this.connections.size });

    return connection;
  }

  /**
   * Acquire a connection from the pool
   * @returns {Promise<PooledConnection>} Pooled connection
   * @throws {Error} If acquire timeout exceeded
   */
  async acquire() {
    if (this.closed) {
      throw new Error('Connection pool is closed');
    }

    // Try to find idle connection
    const idleConnection = this._findIdleConnection();

    if (idleConnection) {
      idleConnection.acquire();
      this.emit('connectionAcquired', idleConnection.getMetrics());
      return idleConnection;
    }

    // Create new connection if under max limit
    if (this.connections.size < this.config.maxConnections) {
      const connection = await this._createConnection();
      connection.acquire();
      this.emit('connectionAcquired', connection.getMetrics());
      return connection;
    }

    // Wait for available connection
    return this._waitForConnection();
  }

  /**
   * Find idle connection
   * @returns {PooledConnection|null} Idle connection or null
   * @private
   */
  _findIdleConnection() {
    for (const connection of this.connections.values()) {
      if (connection.isIdle() && connection.isHealthy()) {
        return connection;
      }
    }
    return null;
  }

  /**
   * Wait for available connection
   * @returns {Promise<PooledConnection>} Pooled connection
   * @private
   */
  _waitForConnection() {
    return new Promise((resolve, reject) => {
      const timeout = setTimeout(() => {
        const index = this.waitQueue.findIndex(w => w.resolve === resolve);
        if (index >= 0) {
          this.waitQueue.splice(index, 1);
        }
        reject(new Error('Connection acquire timeout'));
      }, this.config.acquireTimeout);

      this.waitQueue.push({ resolve, reject, timeout });
    });
  }

  /**
   * Release a connection back to the pool
   * @param {PooledConnection} connection - Connection to release
   */
  release(connection) {
    if (!connection || connection.state === ConnectionState.CLOSED) {
      return;
    }

    connection.release();
    this.emit('connectionReleased', connection.getMetrics());

    // Process wait queue
    if (this.waitQueue.length > 0) {
      const waiter = this.waitQueue.shift();
      clearTimeout(waiter.timeout);
      connection.acquire();
      waiter.resolve(connection);
    }
  }

  /**
   * Remove connection from pool
   * @param {PooledConnection} connection - Connection to remove
   * @private
   */
  async _removeConnection(connection) {
    this.connections.delete(connection.id);
    await connection.close();
    this.emit('connectionRemoved', { id: connection.id, total: this.connections.size });
  }

  /**
   * Start health checks
   * @private
   */
  _startHealthChecks() {
    this.healthCheckTimer = setInterval(async () => {
      for (const connection of this.connections.values()) {
        if (!connection.isHealthy()) {
          await this._removeConnection(connection);
        }
      }
    }, this.config.healthCheckInterval);
  }

  /**
   * Start eviction timer for idle connections
   * @private
   */
  _startEvictionTimer() {
    this.evictionTimer = setInterval(async () => {
      const idleConnections = Array.from(this.connections.values())
        .filter(c => c.isIdle() && c.isExpired(this.config.idleTimeout));

      // Keep minimum connections
      const connectionsToRemove = idleConnections.slice(
        0,
        Math.max(0, this.connections.size - this.config.minConnections)
      );

      for (const connection of connectionsToRemove) {
        await this._removeConnection(connection);
      }
    }, this.config.evictionRunInterval);
  }

  /**
   * Get pool statistics
   * @returns {Object} Pool statistics
   */
  getStats() {
    const connections = Array.from(this.connections.values());

    return {
      total: connections.length,
      idle: connections.filter(c => c.isIdle()).length,
      active: connections.filter(c => c.state === ConnectionState.ACTIVE).length,
      unhealthy: connections.filter(c => c.state === ConnectionState.UNHEALTHY).length,
      waitQueueSize: this.waitQueue.length,
      totalUsage: connections.reduce((sum, c) => sum + c.usageCount, 0),
      totalErrors: connections.reduce((sum, c) => sum + c.errorCount, 0),
      connections: connections.map(c => c.getMetrics())
    };
  }

  /**
   * Execute function with pooled connection
   * @param {Function} fn - Function to execute
   * @returns {Promise<any>} Function result
   */
  async execute(fn) {
    const connection = await this.acquire();

    try {
      const result = await fn(connection.channel);
      this.release(connection);
      return result;
    } catch (error) {
      connection.recordError();
      this.release(connection);
      throw error;
    }
  }

  /**
   * Close the connection pool
   */
  async close() {
    if (this.closed) {
      return;
    }

    this.closed = true;

    // Clear timers
    if (this.healthCheckTimer) {
      clearInterval(this.healthCheckTimer);
    }
    if (this.evictionTimer) {
      clearInterval(this.evictionTimer);
    }

    // Reject all waiting requests
    for (const waiter of this.waitQueue) {
      clearTimeout(waiter.timeout);
      waiter.reject(new Error('Connection pool closed'));
    }
    this.waitQueue = [];

    // Close all connections
    const closePromises = Array.from(this.connections.values())
      .map(c => this._removeConnection(c));

    await Promise.all(closePromises);

    this.emit('poolClosed');
  }
}

/**
 * Create a connection pool
 * @param {Function} createChannel - Function to create gRPC channel
 * @param {Object} [config] - Pool configuration
 * @returns {ConnectionPool} Connection pool instance
 */
export function createConnectionPool(createChannel, config) {
  return new ConnectionPool(createChannel, config);
}

export default ConnectionPool;
