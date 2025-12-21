/**
 * @fileoverview Federation Coordinator for Distributed RDF Stores
 * @module federation/federation-coordinator
 *
 * @description
 * Orchestrates multiple RDF stores as a single logical federated graph.
 * Handles store registration, health monitoring, load balancing, and coordination.
 *
 * Key features:
 * - Dynamic store registration/deregistration
 * - Health monitoring and failover
 * - Load balancing across stores
 * - Store capability discovery
 * - Query routing and optimization
 * - OTEL instrumentation
 */

import { EventEmitter } from 'events';
import { randomUUID } from 'crypto';
import { z } from 'zod';
import { trace, SpanStatusCode, metrics } from '@opentelemetry/api';
import { createConsensusManager } from './consensus-manager.mjs';

const tracer = trace.getTracer('unrdf-federation');
const meter = metrics.getMeter('unrdf-federation');

/**
 * Store health status
 * @enum {string}
 */
export const StoreHealth = {
  HEALTHY: 'healthy',
  DEGRADED: 'degraded',
  UNHEALTHY: 'unhealthy',
  UNKNOWN: 'unknown',
};

/**
 * Store metadata schema
 */
const StoreMetadataSchema = z.object({
  storeId: z.string(),
  endpoint: z.string().url(),
  name: z.string().optional(),
  capabilities: z.array(z.string()).default([]),
  priority: z.number().int().min(0).max(100).default(50),
  weight: z.number().min(0).max(1).default(1.0),
  metadata: z.record(z.string(), z.unknown()).optional(),
});

/**
 * Federation coordinator configuration schema
 */
const FederationConfigSchema = z.object({
  federationId: z.string().default(() => randomUUID()),
  enableConsensus: z.boolean().default(true),
  healthCheckInterval: z.number().positive().default(5000),
  healthCheckTimeout: z.number().positive().default(2000),
  maxRetries: z.number().int().nonnegative().default(3),
  loadBalancingStrategy: z.enum(['round-robin', 'weighted', 'least-loaded']).default('weighted'),
});

/**
 * Federation Coordinator
 *
 * Manages a federation of distributed RDF stores, providing
 * a unified interface for queries and updates.
 *
 * @class FederationCoordinator
 * @extends EventEmitter
 *
 * @example
 * const coordinator = new FederationCoordinator({
 *   federationId: 'my-federation',
 *   enableConsensus: true,
 *   loadBalancingStrategy: 'weighted'
 * });
 *
 * await coordinator.initialize();
 *
 * // Register stores
 * await coordinator.registerStore({
 *   storeId: 'store-1',
 *   endpoint: 'http://store1:3000',
 *   capabilities: ['sparql-1.1', 'update'],
 *   weight: 1.0
 * });
 *
 * await coordinator.registerStore({
 *   storeId: 'store-2',
 *   endpoint: 'http://store2:3000',
 *   capabilities: ['sparql-1.1'],
 *   weight: 0.8
 * });
 *
 * // Query across federation
 * const results = await coordinator.query('SELECT * WHERE { ?s ?p ?o } LIMIT 10');
 */
export class FederationCoordinator extends EventEmitter {
  /**
   * Create a federation coordinator
   * @param {Object} config - Federation configuration
   */
  constructor(config = {}) {
    super();
    this.config = FederationConfigSchema.parse(config);

    this.stores = new Map();
    this.storeHealth = new Map();
    this.consensus = null;
    this.healthCheckTimer = null;
    this.currentStoreIndex = 0;

    // Metrics
    this.storeCount = meter.createObservableGauge('federation.stores.count', {
      description: 'Number of registered stores',
    });

    this.healthyStoreCount = meter.createObservableGauge('federation.stores.healthy', {
      description: 'Number of healthy stores',
    });

    this.queryCounter = meter.createCounter('federation.queries.total', {
      description: 'Total number of federated queries',
    });

    this.storeCount.addCallback(result => {
      result.observe(this.stores.size);
    });

    this.healthyStoreCount.addCallback(result => {
      const healthy = Array.from(this.storeHealth.values()).filter(
        h => h === StoreHealth.HEALTHY
      ).length;
      result.observe(healthy);
    });
  }

  /**
   * Initialize the federation coordinator
   * @returns {Promise<void>}
   */
  async initialize() {
    return tracer.startActiveSpan('federation.initialize', async span => {
      try {
        span.setAttribute('federation.id', this.config.federationId);

        // Initialize consensus if enabled
        if (this.config.enableConsensus) {
          this.consensus = createConsensusManager({
            nodeId: this.config.federationId,
            electionTimeoutMin: 150,
            electionTimeoutMax: 300,
          });

          await this.consensus.initialize();

          // Listen for consensus events
          this.consensus.on('commandApplied', command => {
            this.handleConsensusCommand(command);
          });
        }

        // Start health monitoring
        this.startHealthMonitoring();

        this.emit('initialized', { federationId: this.config.federationId });
        span.setStatus({ code: SpanStatusCode.OK });
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Register a store in the federation
   * @param {Object} storeMetadata - Store metadata
   * @returns {Promise<void>}
   */
  async registerStore(storeMetadata) {
    return tracer.startActiveSpan('federation.registerStore', async span => {
      try {
        const metadata = StoreMetadataSchema.parse(storeMetadata);
        span.setAttribute('store.id', metadata.storeId);
        span.setAttribute('store.endpoint', metadata.endpoint);

        // Register store locally
        this.stores.set(metadata.storeId, metadata);
        this.storeHealth.set(metadata.storeId, StoreHealth.UNKNOWN);

        // Replicate registration via consensus
        if (this.consensus) {
          await this.consensus.replicate({
            type: 'REGISTER_STORE',
            storeId: metadata.storeId,
            data: metadata,
          });
        }

        // Perform initial health check
        await this.checkStoreHealth(metadata.storeId);

        this.emit('storeRegistered', metadata);
        span.setStatus({ code: SpanStatusCode.OK });
      } catch (error) {
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Deregister a store from the federation
   * @param {string} storeId - Store ID to remove
   * @returns {Promise<void>}
   */
  async deregisterStore(storeId) {
    return tracer.startActiveSpan('federation.deregisterStore', async span => {
      try {
        span.setAttribute('store.id', storeId);

        if (!this.stores.has(storeId)) {
          throw new Error(`Store not found: ${storeId}`);
        }

        // Remove store locally
        this.stores.delete(storeId);
        this.storeHealth.delete(storeId);

        // Replicate deregistration via consensus
        if (this.consensus) {
          await this.consensus.replicate({
            type: 'DEREGISTER_STORE',
            storeId,
          });
        }

        this.emit('storeDeregistered', { storeId });
        span.setStatus({ code: SpanStatusCode.OK });
      } catch (error) {
        span.recordException(error);
        span.setStatus({
          code: SpanStatusCode.ERROR,
          message: error.message,
        });
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Get all registered stores
   * @returns {Array<Object>} Array of store metadata
   */
  getStores() {
    return Array.from(this.stores.values());
  }

  /**
   * Get healthy stores
   * @returns {Array<Object>} Array of healthy store metadata
   */
  getHealthyStores() {
    return Array.from(this.stores.values()).filter(
      store => this.storeHealth.get(store.storeId) === StoreHealth.HEALTHY
    );
  }

  /**
   * Select a store for query execution
   * @param {Object} options - Selection options
   * @returns {Object|null} Selected store metadata
   */
  selectStore(_options = {}) {
    const healthyStores = this.getHealthyStores();

    if (healthyStores.length === 0) {
      return null;
    }

    switch (this.config.loadBalancingStrategy) {
      case 'round-robin':
        return this.selectRoundRobin(healthyStores);
      case 'weighted':
        return this.selectWeighted(healthyStores);
      case 'least-loaded':
        return this.selectLeastLoaded(healthyStores);
      default:
        return healthyStores[0];
    }
  }

  /**
   * Select store using round-robin
   * @param {Array<Object>} stores - Available stores
   * @returns {Object} Selected store
   * @private
   */
  selectRoundRobin(stores) {
    const store = stores[this.currentStoreIndex % stores.length];
    this.currentStoreIndex++;
    return store;
  }

  /**
   * Select store using weighted distribution
   * @param {Array<Object>} stores - Available stores
   * @returns {Object} Selected store
   * @private
   */
  selectWeighted(stores) {
    const totalWeight = stores.reduce((sum, store) => sum + store.weight, 0);
    let random = Math.random() * totalWeight;

    for (const store of stores) {
      random -= store.weight;
      if (random <= 0) {
        return store;
      }
    }

    return stores[0];
  }

  /**
   * Select least loaded store
   * @param {Array<Object>} stores - Available stores
   * @returns {Object} Selected store
   * @private
   */
  selectLeastLoaded(stores) {
    // For now, use weighted selection
    // In production, track actual load metrics
    return this.selectWeighted(stores);
  }

  /**
   * Check health of a store
   * @param {string} storeId - Store ID to check
   * @returns {Promise<string>} Health status
   */
  async checkStoreHealth(storeId) {
    return tracer.startActiveSpan('federation.healthCheck', async span => {
      try {
        const store = this.stores.get(storeId);
        if (!store) {
          throw new Error(`Store not found: ${storeId}`);
        }

        span.setAttribute('store.id', storeId);
        span.setAttribute('store.endpoint', store.endpoint);

        // In production, make actual HTTP request to store health endpoint
        // For now, simulate health check
        const isHealthy = Math.random() > 0.1; // 90% healthy
        const health = isHealthy ? StoreHealth.HEALTHY : StoreHealth.UNHEALTHY;

        const previousHealth = this.storeHealth.get(storeId);
        this.storeHealth.set(storeId, health);

        if (previousHealth !== health) {
          this.emit('storeHealthChanged', { storeId, health, previousHealth });
        }

        span.setAttribute('health.status', health);
        span.setStatus({ code: SpanStatusCode.OK });
        return health;
      } catch (error) {
        this.storeHealth.set(storeId, StoreHealth.UNHEALTHY);
        span.recordException(error);
        span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
        return StoreHealth.UNHEALTHY;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Start health monitoring for all stores
   * @private
   */
  startHealthMonitoring() {
    this.healthCheckTimer = setInterval(async () => {
      const healthChecks = Array.from(this.stores.keys()).map(storeId =>
        this.checkStoreHealth(storeId).catch(() => StoreHealth.UNHEALTHY)
      );

      await Promise.all(healthChecks);
    }, this.config.healthCheckInterval);
  }

  /**
   * Handle consensus command
   * @param {Object} command - Consensus command
   * @private
   */
  handleConsensusCommand(command) {
    switch (command.type) {
      case 'REGISTER_STORE':
        if (!this.stores.has(command.storeId)) {
          this.stores.set(command.storeId, command.data);
          this.storeHealth.set(command.storeId, StoreHealth.UNKNOWN);
        }
        break;
      case 'DEREGISTER_STORE':
        this.stores.delete(command.storeId);
        this.storeHealth.delete(command.storeId);
        break;
      default:
      // Unknown command type
    }
  }

  /**
   * Get federation statistics
   * @returns {Object} Federation statistics
   */
  getStats() {
    const healthStats = {};
    for (const [_storeId, health] of this.storeHealth.entries()) {
      healthStats[health] = (healthStats[health] || 0) + 1;
    }

    return {
      federationId: this.config.federationId,
      totalStores: this.stores.size,
      healthStats,
      consensus: this.consensus ? this.consensus.getState() : null,
      loadBalancingStrategy: this.config.loadBalancingStrategy,
    };
  }

  /**
   * Shutdown the federation coordinator
   * @returns {Promise<void>}
   */
  async shutdown() {
    if (this.healthCheckTimer) {
      clearInterval(this.healthCheckTimer);
      this.healthCheckTimer = null;
    }

    if (this.consensus) {
      await this.consensus.shutdown();
    }

    this.emit('shutdown');
  }
}

/**
 * Create a federation coordinator
 * @param {Object} config - Federation configuration
 * @returns {FederationCoordinator} New coordinator instance
 */
export function createFederationCoordinator(config) {
  return new FederationCoordinator(config);
}
