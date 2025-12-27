/**
 * @file KGC Sidecar Client v2 with Graceful Degradation
 * @module cli/utils/sidecar-client-v2
 *
 * @description
 * Advanced sidecar client with health checks, circuit breaker, connection pooling,
 * and automatic fallback to local-only mode when sidecar unavailable.
 */

import { SidecarClient } from '../../sidecar/client.mjs';
import { z } from 'zod';
import { EventEmitter } from 'events';

/**
 * Connection states
 */
export const ConnectionState = {
  INITIALIZING: 'INITIALIZING',
  HEALTHY: 'HEALTHY',
  DEGRADED: 'DEGRADED',
  UNHEALTHY: 'UNHEALTHY',
  CIRCUIT_OPEN: 'CIRCUIT_OPEN',
};

/**
 * Sidecar client v2 configuration schema
 */
const SidecarClientV2ConfigSchema = z.object({
  enabled: z.boolean().default(true),
  endpoint: z.string().default('localhost:50051'),
  tls: z
    .object({
      enabled: z.boolean().default(false),
      ca: z.string().optional(),
      cert: z.string().optional(),
      key: z.string().optional(),
    })
    .optional(),
  healthCheck: z.object({
    interval: z.number().int().positive().default(5000),
    timeout: z.number().int().positive().default(100),
    retries: z.number().int().min(1).max(10).default(3),
  }),
  circuitBreaker: z.object({
    threshold: z.number().int().positive().default(3),
    resetTimeout: z.number().int().positive().default(30000),
    halfOpenRequests: z.number().int().positive().default(3),
  }),
  fallback: z.object({
    mode: z.enum(['local', 'none']).default('local'),
    autoRecover: z.boolean().default(true),
  }),
  retry: z.object({
    maxAttempts: z.number().int().min(1).max(10).default(3),
    initialDelay: z.number().int().positive().default(100),
    maxDelay: z.number().int().positive().default(1600),
    multiplier: z.number().min(1).default(2),
  }),
  pool: z.object({
    minConnections: z.number().int().min(1).default(2),
    maxConnections: z.number().int().min(1).default(10),
    idleTimeout: z.number().int().positive().default(60000),
  }),
});

/**
 * KGC Sidecar Client v2 with graceful degradation
 */
export class SidecarClientV2 extends EventEmitter {
  /**
   * Create a new sidecar client v2
   * @param {Object} [options] - Client options
   */
  constructor(options = {}) {
    super();

    // Parse and validate configuration
    this.config = SidecarClientV2ConfigSchema.parse(this._loadConfig(options));

    // Connection state
    this.state = ConnectionState.INITIALIZING;

    // gRPC client (lazy initialized)
    this.grpcClient = null;

    // Local fallback manager (lazy initialized)
    this.localManager = null;

    // Circuit breaker state
    this.circuitBreaker = {
      failures: 0,
      state: 'CLOSED',
      lastFailureTime: null,
      resetTimer: null,
    };

    // Health check state
    this.healthCheck = {
      lastCheckTime: null,
      consecutiveFailures: 0,
      consecutiveSuccesses: 0,
      timer: null,
    };

    // Metrics
    this.metrics = {
      totalRequests: 0,
      grpcRequests: 0,
      grpcSuccesses: 0,
      grpcFailures: 0,
      localFallbacks: 0,
      localSuccesses: 0,
      localFailures: 0,
      timeouts: 0,
      connectionFailures: 0,
      circuitOpenCount: 0,
      lastStateTransition: null,
    };

    // Initialize connection
    this._initialize();
  }

  /**
   * Load configuration from environment and options
   * @param {Object} options - User options
   * @returns {Object} Merged configuration
   * @private
   */
  _loadConfig(options) {
    return {
      enabled: process.env.KGC_SIDECAR_ENABLED === 'true' || options.enabled !== false,
      endpoint: process.env.KGC_SIDECAR_ENDPOINT || options.endpoint || 'localhost:50051',
      tls: {
        enabled: process.env.KGC_SIDECAR_TLS_ENABLED === 'true' || false,
        ca: process.env.KGC_SIDECAR_TLS_CA || options.tls?.ca,
        cert: process.env.KGC_SIDECAR_TLS_CERT || options.tls?.cert,
        key: process.env.KGC_SIDECAR_TLS_KEY || options.tls?.key,
      },
      healthCheck: {
        interval: parseInt(process.env.KGC_SIDECAR_HEALTH_CHECK_INTERVAL || '5000'),
        timeout: parseInt(process.env.KGC_SIDECAR_HEALTH_CHECK_TIMEOUT || '100'),
        retries: parseInt(process.env.KGC_SIDECAR_HEALTH_CHECK_RETRIES || '3'),
      },
      circuitBreaker: {
        threshold: parseInt(process.env.KGC_SIDECAR_CIRCUIT_BREAKER_THRESHOLD || '3'),
        resetTimeout: parseInt(process.env.KGC_SIDECAR_CIRCUIT_BREAKER_TIMEOUT || '30000'),
        halfOpenRequests: parseInt(
          process.env.KGC_SIDECAR_CIRCUIT_BREAKER_HALF_OPEN_REQUESTS || '3'
        ),
      },
      fallback: {
        mode: process.env.KGC_SIDECAR_FALLBACK_MODE || 'local',
        autoRecover: process.env.KGC_SIDECAR_FALLBACK_AUTO_RECOVER !== 'false',
      },
      retry: {
        maxAttempts: parseInt(process.env.KGC_SIDECAR_RETRY_MAX_ATTEMPTS || '3'),
        initialDelay: parseInt(process.env.KGC_SIDECAR_RETRY_INITIAL_DELAY || '100'),
        maxDelay: parseInt(process.env.KGC_SIDECAR_RETRY_MAX_DELAY || '1600'),
        multiplier: parseFloat(process.env.KGC_SIDECAR_RETRY_MULTIPLIER || '2'),
      },
      pool: {
        minConnections: parseInt(process.env.KGC_SIDECAR_POOL_MIN_CONNECTIONS || '2'),
        maxConnections: parseInt(process.env.KGC_SIDECAR_POOL_MAX_CONNECTIONS || '10'),
        idleTimeout: parseInt(process.env.KGC_SIDECAR_POOL_IDLE_TIMEOUT || '60000'),
      },
      ...options,
    };
  }

  /**
   * Initialize connection
   * @private
   */
  async _initialize() {
    if (!this.config.enabled) {
      this._transitionTo(ConnectionState.DEGRADED);
      await this._ensureLocalManager();
      return;
    }

    // Perform initial health check
    const health = await this._performHealthCheck(0);

    if (health.status === 'HEALTHY') {
      await this._ensureGrpcClient();
      this._transitionTo(ConnectionState.HEALTHY);

      // Start periodic health monitoring
      if (this.config.fallback.autoRecover) {
        this._startHealthMonitoring();
      }
    } else {
      this._transitionTo(ConnectionState.DEGRADED);
      await this._ensureLocalManager();

      // Start auto-recovery monitoring if enabled
      if (this.config.fallback.autoRecover) {
        this._startHealthMonitoring();
      }
    }
  }

  /**
   * Perform health check with exponential backoff
   * @param {number} attempt - Attempt number
   * @returns {Promise<Object>} Health check result
   * @private
   */
  async _performHealthCheck(attempt = 0) {
    const timeout = Math.min(
      this.config.healthCheck.timeout * Math.pow(this.config.retry.multiplier, attempt),
      this.config.retry.maxDelay
    );

    const startTime = Date.now();

    try {
      // Ensure gRPC client exists
      await this._ensureGrpcClient();

      // Execute health check with timeout
      const result = await Promise.race([
        this.grpcClient.healthCheck(),
        this._timeout(timeout, 'Health check timeout'),
      ]);

      const latency = Date.now() - startTime;

      if (result.status === 'SERVING') {
        this.healthCheck.consecutiveSuccesses++;
        this.healthCheck.consecutiveFailures = 0;

        return {
          status: 'HEALTHY',
          latency,
          metadata: result,
        };
      } else {
        this.healthCheck.consecutiveFailures++;
        this.healthCheck.consecutiveSuccesses = 0;

        return {
          status: 'UNHEALTHY',
          latency,
          error: `Sidecar status: ${result.status}`,
        };
      }
    } catch (error) {
      const latency = Date.now() - startTime;
      this.healthCheck.consecutiveFailures++;
      this.healthCheck.consecutiveSuccesses = 0;

      // Retry with exponential backoff if attempts remaining
      if (attempt < this.config.healthCheck.retries - 1) {
        return this._performHealthCheck(attempt + 1);
      }

      return {
        status: 'UNHEALTHY',
        latency,
        error: error.message,
        code: error.code,
      };
    } finally {
      this.healthCheck.lastCheckTime = Date.now();
    }
  }

  /**
   * Start periodic health monitoring
   * @private
   */
  _startHealthMonitoring() {
    if (this.healthCheck.timer) {
      clearInterval(this.healthCheck.timer);
    }

    this.healthCheck.timer = setInterval(async () => {
      const health = await this._performHealthCheck(0);

      // Transition based on health status
      if (health.status === 'HEALTHY' && this.state === ConnectionState.DEGRADED) {
        // Recovery: transition from DEGRADED to HEALTHY
        await this._ensureGrpcClient();
        this._transitionTo(ConnectionState.HEALTHY);
        this._resetCircuitBreaker();
        this.emit('recovered');
      } else if (health.status === 'UNHEALTHY' && this.state === ConnectionState.HEALTHY) {
        // Degradation: transition from HEALTHY to DEGRADED
        this._transitionTo(ConnectionState.DEGRADED);
        await this._ensureLocalManager();
      }
    }, this.config.healthCheck.interval);
  }

  /**
   * Ensure gRPC client is initialized
   * @returns {Promise<void>}
   * @private
   */
  async _ensureGrpcClient() {
    if (!this.grpcClient) {
      this.grpcClient = new SidecarClient({
        address: this.config.endpoint,
        timeout: this.config.healthCheck.timeout,
        maxRetries: this.config.retry.maxAttempts,
        connectionPool: this.config.pool,
        circuitBreaker: this.config.circuitBreaker,
        enableHealthCheck: false, // We manage health checks ourselves
      });

      await this.grpcClient.connect(this.config.endpoint);
    }
  }

  /**
   * Ensure local manager is initialized
   * @returns {Promise<void>}
   * @private
   */
  async _ensureLocalManager() {
    if (!this.localManager && this.config.fallback.mode === 'local') {
      // Lazy load local knowledge hook manager
      const { KnowledgeHookManager } = await import(
        '../../knowledge-engine/knowledge-hook-manager.mjs'
      );
      this.localManager = new KnowledgeHookManager({
        mode: 'local',
        observability: { enabled: false }, // Local mode doesn't need OTEL
      });
    }
  }

  /**
   * Transition to new state
   * @param {string} newState - New connection state
   * @private
   */
  _transitionTo(newState) {
    if (this.state !== newState) {
      const oldState = this.state;
      this.state = newState;
      this.metrics.lastStateTransition = Date.now();

      this.emit('stateChanged', { from: oldState, to: newState });
    }
  }

  /**
   * Check if circuit breaker should open
   * @returns {boolean} True if should open
   * @private
   */
  _shouldOpenCircuit() {
    return this.circuitBreaker.failures >= this.config.circuitBreaker.threshold;
  }

  /**
   * Open circuit breaker
   * @private
   */
  _openCircuitBreaker() {
    if (this.circuitBreaker.state !== 'OPEN') {
      this.circuitBreaker.state = 'OPEN';
      this.circuitBreaker.lastFailureTime = Date.now();
      this.metrics.circuitOpenCount++;

      this._transitionTo(ConnectionState.CIRCUIT_OPEN);
      this.emit('circuitOpened', { failures: this.circuitBreaker.failures });

      // Schedule reset to HALF_OPEN
      this.circuitBreaker.resetTimer = setTimeout(() => {
        this.circuitBreaker.state = 'HALF_OPEN';
        this.emit('circuitHalfOpen');
      }, this.config.circuitBreaker.resetTimeout);
    }
  }

  /**
   * Reset circuit breaker
   * @private
   */
  _resetCircuitBreaker() {
    if (this.circuitBreaker.resetTimer) {
      clearTimeout(this.circuitBreaker.resetTimer);
      this.circuitBreaker.resetTimer = null;
    }

    this.circuitBreaker.state = 'CLOSED';
    this.circuitBreaker.failures = 0;
    this.circuitBreaker.lastFailureTime = null;

    this.emit('circuitClosed');
  }

  /**
   * Record circuit breaker failure
   * @private
   */
  _recordCircuitFailure() {
    this.circuitBreaker.failures++;

    if (this._shouldOpenCircuit()) {
      this._openCircuitBreaker();
    }
  }

  /**
   * Execute operation with resilience patterns
   * @param {string} operation - Operation name
   * @param {Object} params - Operation parameters
   * @returns {Promise<any>} Operation result
   */
  async execute(operation, params = {}) {
    this.metrics.totalRequests++;

    // If circuit is OPEN, immediately fail-fast to local mode
    if (this.circuitBreaker.state === 'OPEN') {
      this.emit('circuitOpenRejection', { operation });
      return this._executeLocal(operation, params);
    }

    // Try gRPC first if healthy or half-open
    if (this.state === ConnectionState.HEALTHY || this.circuitBreaker.state === 'HALF_OPEN') {
      try {
        const result = await this._executeGrpc(operation, params);

        // Success: reset circuit breaker if in HALF_OPEN
        if (this.circuitBreaker.state === 'HALF_OPEN') {
          this._resetCircuitBreaker();
          this._transitionTo(ConnectionState.HEALTHY);
        }

        return result;
      } catch (error) {
        this.metrics.grpcFailures++;
        this._recordCircuitFailure();

        // Classify error
        const classified = this._classifyError(error);

        // If UNAVAILABLE or timeout, fall back to local
        if (classified.shouldFallback) {
          this.emit('fallbackToLocal', {
            operation,
            reason: classified.reason,
          });
          return this._executeLocal(operation, params);
        }

        // Otherwise, propagate error
        throw error;
      }
    }

    // If degraded, use local mode
    return this._executeLocal(operation, params);
  }

  /**
   * Execute operation via gRPC
   * @param {string} operation - Operation name
   * @param {Object} params - Operation parameters
   * @returns {Promise<any>} Operation result
   * @private
   */
  async _executeGrpc(operation, params) {
    this.metrics.grpcRequests++;

    await this._ensureGrpcClient();

    // Map operation to gRPC method
    const method = this._mapOperationToMethod(operation);

    if (!this.grpcClient[method]) {
      throw new Error(`Operation ${operation} not supported by gRPC client`);
    }

    const startTime = Date.now();

    try {
      const result = await this.grpcClient[method](params);
      const latency = Date.now() - startTime;

      this.metrics.grpcSuccesses++;
      this.emit('grpcSuccess', { operation, latency });

      return result;
    } catch (error) {
      const latency = Date.now() - startTime;
      this.emit('grpcError', { operation, latency, error });
      throw error;
    }
  }

  /**
   * Execute operation via local manager
   * @param {string} operation - Operation name
   * @param {Object} params - Operation parameters
   * @returns {Promise<any>} Operation result
   * @private
   */
  async _executeLocal(operation, params) {
    if (this.config.fallback.mode !== 'local') {
      throw new Error('Local fallback mode is disabled');
    }

    this.metrics.localFallbacks++;

    await this._ensureLocalManager();

    const startTime = Date.now();

    try {
      // Map operation to local method
      const result = await this._executeLocalOperation(operation, params);
      const latency = Date.now() - startTime;

      this.metrics.localSuccesses++;
      this.emit('localSuccess', { operation, latency });

      return result;
    } catch (error) {
      const latency = Date.now() - startTime;
      this.metrics.localFailures++;
      this.emit('localError', { operation, latency, error });
      throw error;
    }
  }

  /**
   * Execute operation on local manager
   * @param {string} operation - Operation name
   * @param {Object} params - Operation parameters
   * @returns {Promise<any>} Operation result
   * @private
   */
  async _executeLocalOperation(operation, params) {
    switch (operation) {
      case 'listHooks':
        return this.localManager.listHooks();

      case 'evaluateHook':
        return this.localManager.evaluateHook(params.hookId, params.event);

      case 'validateGraph':
        return this.localManager.validateGraph(params.quads, params.policyPack);

      case 'applyTransaction':
        return this.localManager.applyTransaction(params.delta, params.options);

      default:
        throw new Error(`Operation ${operation} not supported in local mode`);
    }
  }

  /**
   * Map operation to gRPC method name
   * @param {string} operation - Operation name
   * @returns {string} gRPC method name
   * @private
   */
  _mapOperationToMethod(operation) {
    const methodMap = {
      listHooks: 'queryPolicy',
      evaluateHook: 'evaluateHook',
      validateGraph: 'validateGraph',
      applyTransaction: 'applyTransaction',
      healthCheck: 'healthCheck',
      getMetrics: 'getMetrics',
    };

    return methodMap[operation] || operation;
  }

  /**
   * Classify error for fallback decision
   * @param {Error} error - Error to classify
   * @returns {Object} Classification result
   * @private
   */
  _classifyError(error) {
    const code = error.code;

    // DEADLINE_EXCEEDED: timeout, should fallback
    if (code === 4) {
      this.metrics.timeouts++;
      return {
        shouldFallback: true,
        reason: 'timeout',
        recoverable: true,
      };
    }

    // UNAVAILABLE: connection failed, should fallback
    if (code === 14) {
      this.metrics.connectionFailures++;
      return {
        shouldFallback: true,
        reason: 'unavailable',
        recoverable: true,
      };
    }

    // UNIMPLEMENTED: method not supported, should fallback
    if (code === 12) {
      return {
        shouldFallback: true,
        reason: 'unimplemented',
        recoverable: false,
      };
    }

    // UNAUTHENTICATED: auth required, should NOT fallback
    if (code === 16) {
      return {
        shouldFallback: false,
        reason: 'unauthenticated',
        recoverable: false,
      };
    }

    // Default: don't fallback for unknown errors
    return {
      shouldFallback: false,
      reason: 'unknown',
      recoverable: false,
    };
  }

  /**
   * Timeout promise
   * @param {number} ms - Timeout in milliseconds
   * @param {string} message - Timeout message
   * @returns {Promise<never>}
   * @private
   */
  _timeout(ms, message = 'Operation timeout') {
    return new Promise((_, reject) => {
      setTimeout(() => {
        const error = new Error(message);
        error.code = 4; // DEADLINE_EXCEEDED
        reject(error);
      }, ms);
    });
  }

  /**
   * Get client metrics
   * @returns {Object} Client metrics
   */
  getMetrics() {
    return {
      state: this.state,
      circuitBreaker: {
        state: this.circuitBreaker.state,
        failures: this.circuitBreaker.failures,
        openCount: this.metrics.circuitOpenCount,
      },
      healthCheck: {
        lastCheckTime: this.healthCheck.lastCheckTime,
        consecutiveFailures: this.healthCheck.consecutiveFailures,
        consecutiveSuccesses: this.healthCheck.consecutiveSuccesses,
      },
      requests: {
        total: this.metrics.totalRequests,
        grpc: this.metrics.grpcRequests,
        grpcSuccesses: this.metrics.grpcSuccesses,
        grpcFailures: this.metrics.grpcFailures,
        localFallbacks: this.metrics.localFallbacks,
        localSuccesses: this.metrics.localSuccesses,
        localFailures: this.metrics.localFailures,
      },
      errors: {
        timeouts: this.metrics.timeouts,
        connectionFailures: this.metrics.connectionFailures,
      },
      lastStateTransition: this.metrics.lastStateTransition,
    };
  }

  /**
   * Check if sidecar is available
   * @returns {boolean} True if available
   */
  isAvailable() {
    return this.state === ConnectionState.HEALTHY;
  }

  /**
   * Check if using local fallback
   * @returns {boolean} True if using local fallback
   */
  isLocalMode() {
    return this.state === ConnectionState.DEGRADED || this.state === ConnectionState.CIRCUIT_OPEN;
  }

  /**
   * Force health check
   * @returns {Promise<Object>} Health check result
   */
  async forceHealthCheck() {
    return this._performHealthCheck(0);
  }

  /**
   * Force reset circuit breaker
   */
  forceResetCircuit() {
    this._resetCircuitBreaker();
    if (this.state === ConnectionState.CIRCUIT_OPEN) {
      this._transitionTo(ConnectionState.DEGRADED);
    }
  }

  /**
   * Disconnect and cleanup
   */
  async disconnect() {
    // Stop health monitoring
    if (this.healthCheck.timer) {
      clearInterval(this.healthCheck.timer);
      this.healthCheck.timer = null;
    }

    // Clear circuit breaker timer
    if (this.circuitBreaker.resetTimer) {
      clearTimeout(this.circuitBreaker.resetTimer);
      this.circuitBreaker.resetTimer = null;
    }

    // Disconnect gRPC client
    if (this.grpcClient) {
      await this.grpcClient.disconnect();
      this.grpcClient = null;
    }

    // Cleanup local manager
    this.localManager = null;

    this.removeAllListeners();
  }
}

/**
 * Create sidecar client v2
 * @param {Object} [options] - Client options
 * @returns {SidecarClientV2} Sidecar client v2 instance
 */
export function createSidecarClientV2(options) {
  return new SidecarClientV2(options);
}

/**
 * Singleton instance
 */
let sidecarClientV2 = null;

/**
 * Get or create singleton sidecar client v2
 * @param {Object} [options] - Client options
 * @returns {Promise<SidecarClientV2>} Sidecar client v2 instance
 */
export async function getSidecarClientV2(options = {}) {
  if (!sidecarClientV2) {
    sidecarClientV2 = new SidecarClientV2(options);
  }
  return sidecarClientV2;
}

/**
 * Close singleton sidecar client v2
 * @returns {Promise<void>}
 */
export async function closeSidecarClientV2() {
  if (sidecarClientV2) {
    await sidecarClientV2.disconnect();
    sidecarClientV2 = null;
  }
}

export default SidecarClientV2;
