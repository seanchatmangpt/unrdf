/**
 * @file Main KGC sidecar gRPC client implementation
 * @module sidecar/client
 *
 * @description
 * Enterprise-grade gRPC client for KGC sidecar with connection pooling,
 * circuit breakers, retries, health checks, and full observability.
 */

import grpc from '@grpc/grpc-js';
import protoLoader from '@grpc/proto-loader';
import { join } from 'path';
import { fileURLToPath } from 'url';
import { dirname } from 'path';
import { z } from 'zod';
import { createSidecarConfig } from './config.mjs';
import { createCircuitBreaker } from './circuit-breaker.mjs';
import { createRetryStrategy } from './retry-strategy.mjs';
import { createConnectionPool } from './connection-pool.mjs';
import { createHealthMonitor, HealthStatus, createHealthCheckResult } from './health-check.mjs';
import { createTelemetryInterceptor, createTimeoutInterceptor, composeInterceptors } from './interceptors.mjs';
import { EventEmitter } from 'events';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

/**
 * Sidecar client options schema
 */
const SidecarClientOptionsSchema = z.object({
  address: z.string().optional(),
  config: z.any().optional(),
  maxRetries: z.number().int().min(0).max(10).default(3),
  timeout: z.number().int().positive().default(5000),
  circuitBreaker: z.object({
    threshold: z.number().int().positive().default(5),
    resetTimeout: z.number().int().positive().default(30000)
  }).optional(),
  connectionPool: z.object({
    minConnections: z.number().int().min(1).default(2),
    maxConnections: z.number().int().min(1).default(10)
  }).optional(),
  observability: z.any().optional(),
  enableHealthCheck: z.boolean().default(true),
  protoPath: z.string().optional()
});

/**
 * KGC Sidecar gRPC Client
 */
export class SidecarClient extends EventEmitter {
  /**
   * Create a new sidecar client
   * @param {Object} [options] - Client options
   */
  constructor(options = {}) {
    super();
    this.options = SidecarClientOptionsSchema.parse(options);
    this.config = options.config || createSidecarConfig();
    this.connected = false;
    this.client = null;
    this.pool = null;
    this.circuitBreaker = null;
    this.retryStrategy = null;
    this.healthMonitor = null;
    this.metrics = {
      requests: 0,
      successes: 0,
      failures: 0,
      retries: 0
    };
  }

  /**
   * Connect to sidecar service
   * @param {string} [address] - Sidecar address (overrides config)
   * @param {Object} [options] - Connection options
   * @returns {Promise<void>}
   */
  async connect(address, options = {}) {
    if (this.connected) {
      return;
    }

    // Resolve address
    const sidecarAddress = address || this.options.address || this.config.getAddress();

    // Load proto file
    const protoPath = this.options.protoPath || join(__dirname, '../../proto/kgc-sidecar.proto');
    const packageDefinition = protoLoader.loadSync(protoPath, {
      keepCase: true,
      longs: String,
      enums: String,
      defaults: true,
      oneofs: true
    });

    const protoDescriptor = grpc.loadPackageDefinition(packageDefinition);
    const KGCSidecar = protoDescriptor.kgc.sidecar.v1.KGCSidecar;

    // Create gRPC channel factory
    const createChannel = () => {
      const credentials = this._getCredentials();
      const channelOptions = {
        ...this.config.getConnectionOptions(),
        ...options
      };

      return new KGCSidecar(sidecarAddress, credentials, channelOptions);
    };

    // Initialize connection pool
    this.pool = createConnectionPool(
      createChannel,
      this.options.connectionPool || {}
    );

    // Create first connection
    this.client = await this.pool.acquire();

    // Initialize circuit breaker
    this.circuitBreaker = createCircuitBreaker(
      this.config.getCircuitBreakerConfig()
    );

    // Initialize retry strategy
    this.retryStrategy = createRetryStrategy({
      maxRetries: this.config.getMaxRetries(),
      initialDelay: 100,
      maxDelay: 5000
    });

    // Initialize health monitoring
    if (this.options.enableHealthCheck) {
      this.healthMonitor = createHealthMonitor(
        () => this._performHealthCheck(),
        {
          interval: 10000,
          timeout: 5000,
          unhealthyThreshold: 3,
          healthyThreshold: 2
        }
      );

      this.healthMonitor.on('statusChanged', ({ from, to }) => {
        this.emit('healthStatusChanged', { from, to });
      });

      this.healthMonitor.start();
    }

    this.connected = true;
    this.emit('connected', { address: sidecarAddress });
  }

  /**
   * Get gRPC credentials
   * @returns {Object} gRPC credentials
   * @private
   */
  _getCredentials() {
    const tlsConfig = this.config.getTLSCredentials();

    if (!tlsConfig) {
      return grpc.credentials.createInsecure();
    }

    if (tlsConfig.insecure) {
      return grpc.credentials.createInsecure();
    }

    return grpc.credentials.createSsl(
      tlsConfig.ca,
      tlsConfig.key,
      tlsConfig.cert
    );
  }

  /**
   * Perform health check
   * @returns {Promise<HealthCheckResult>} Health check result
   * @private
   */
  async _performHealthCheck() {
    try {
      const response = await this.healthCheck();

      if (response.status === 'SERVING') {
        return createHealthCheckResult(HealthStatus.HEALTHY, {
          uptime: response.uptime_seconds
        });
      } else {
        return createHealthCheckResult(HealthStatus.UNHEALTHY, {
          status: response.status
        });
      }
    } catch (error) {
      return createHealthCheckResult(HealthStatus.UNHEALTHY, {
        error: error.message
      }, error);
    }
  }

  /**
   * Execute gRPC call with resilience patterns
   * @param {string} method - gRPC method name
   * @param {Object} request - Request payload
   * @param {Object} [options] - Call options
   * @returns {Promise<Object>} Response
   * @private
   */
  async _execute(method, request, options = {}) {
    this._ensureConnected();

    this.metrics.requests++;

    // Execute with circuit breaker and retry
    try {
      const response = await this.circuitBreaker.execute(async () => {
        return await this.retryStrategy.execute(async () => {
          return await this.pool.execute(async (client) => {
            return await this._call(client, method, request, options);
          });
        });
      });

      this.metrics.successes++;
      return response;
    } catch (error) {
      this.metrics.failures++;
      throw error;
    }
  }

  /**
   * Execute gRPC call with trace context propagation
   * @param {Object} client - gRPC client
   * @param {string} method - Method name
   * @param {Object} request - Request payload
   * @param {Object} [options] - Call options
   * @returns {Promise<Object>} Response
   * @private
   */
  async _call(client, method, request, options = {}) {
    const deadline = Date.now() + (options.timeout || this.config.getTimeout());
    const metadata = new grpc.Metadata();

    // Add custom metadata
    if (options.metadata) {
      Object.entries(options.metadata).forEach(([key, value]) => {
        metadata.set(key, value);
      });
    }

    // Extract current OTEL trace context and inject into gRPC metadata
    // This enables distributed tracing across CLI → Sidecar → Hooks
    try {
      const { trace, context } = await import('@opentelemetry/api');
      const currentSpan = trace.getSpan(context.active());

      if (currentSpan) {
        const spanContext = currentSpan.spanContext();
        metadata.set('x-trace-id', spanContext.traceId);
        metadata.set('x-span-id', spanContext.spanId);
        metadata.set('x-trace-flags', spanContext.traceFlags.toString(16).padStart(2, '0'));

        // Add W3C traceparent header for standard propagation
        const traceparent = `00-${spanContext.traceId}-${spanContext.spanId}-${spanContext.traceFlags.toString(16).padStart(2, '0')}`;
        metadata.set('traceparent', traceparent);

        // Add trace state if present
        if (spanContext.traceState) {
          metadata.set('tracestate', spanContext.traceState.serialize());
        }
      } else if (options.traceContext) {
        // Fallback to provided trace context
        metadata.set('x-trace-id', options.traceContext.traceId || '');
        metadata.set('x-span-id', options.traceContext.spanId || '');
        metadata.set('x-trace-flags', options.traceContext.traceFlags || '01');

        // Add W3C traceparent
        const traceparent = `00-${options.traceContext.traceId}-${options.traceContext.spanId}-${options.traceContext.traceFlags || '01'}`;
        metadata.set('traceparent', traceparent);
      }
    } catch (error) {
      // OTEL not available, continue without trace context
      console.warn('[Sidecar Client] OpenTelemetry not available, skipping trace context propagation');
    }

    // Call the method on the client
    return new Promise((resolve, reject) => {
      if (!client[method]) {
        reject(new Error(`Method ${method} not found on gRPC client`));
        return;
      }

      client[method](request, metadata, { deadline }, (error, response) => {
        if (error) {
          reject(this._transformError(error));
        } else {
          resolve(response);
        }
      });
    });
  }

  /**
   * Transform gRPC error
   * @param {Error} error - gRPC error
   * @returns {Error} Transformed error
   * @private
   */
  _transformError(error) {
    const err = new Error(error.details || error.message);
    err.code = error.code;
    err.metadata = error.metadata;
    return err;
  }

  /**
   * Ensure client is connected
   * @throws {Error} If not connected
   * @private
   */
  _ensureConnected() {
    if (!this.connected) {
      throw new Error('Sidecar client not connected. Call connect() first.');
    }
  }

  /**
   * Apply transaction
   * @param {Object} request - Transaction request
   * @param {Object} request.delta - Transaction delta
   * @param {Object} [request.options] - Transaction options
   * @param {string} [request.actor] - Actor ID
   * @returns {Promise<Object>} Transaction response
   */
  async applyTransaction(request) {
    return this._execute('ApplyTransaction', {
      transaction_id: request.transactionId || crypto.randomUUID(),
      delta: request.delta,
      options: request.options || {},
      actor: request.actor || 'system',
      context: request.context || {}
    });
  }

  /**
   * Validate graph against policy pack
   * @param {Object} request - Validation request
   * @param {Array} request.quads - Graph quads
   * @param {string} request.policyPack - Policy pack name
   * @param {boolean} [request.strictMode] - Strict mode flag
   * @returns {Promise<Object>} Validation response
   */
  async validateGraph(request) {
    return this._execute('ValidateGraph', {
      graph_id: request.graphId || crypto.randomUUID(),
      quads: request.quads,
      policy_pack: request.policyPack,
      strict_mode: request.strictMode || false,
      options: request.options || {}
    });
  }

  /**
   * Evaluate knowledge hook
   * @param {Object} request - Hook evaluation request
   * @param {string} request.hookId - Hook ID
   * @param {Object} request.hook - Hook definition
   * @param {Object} request.event - Hook event
   * @returns {Promise<Object>} Hook evaluation response
   */
  async evaluateHook(request) {
    return this._execute('EvaluateHook', {
      hook_id: request.hookId,
      hook: request.hook,
      event: request.event,
      options: request.options || {}
    });
  }

  /**
   * Query policy pack
   * @param {Object} request - Policy query request
   * @param {string} request.policyPack - Policy pack name
   * @param {string} [request.queryType] - Query type
   * @returns {Promise<Object>} Policy pack response
   */
  async queryPolicy(request) {
    return this._execute('QueryPolicy', {
      policy_pack: request.policyPack,
      query_type: request.queryType || 'info',
      filters: request.filters || {}
    });
  }

  /**
   * Health check
   * @param {Object} [request] - Health check request
   * @returns {Promise<Object>} Health check response
   */
  async healthCheck(request = {}) {
    return this._execute('HealthCheck', {
      service: request.service || 'kgc.sidecar.v1.KGCSidecar'
    });
  }

  /**
   * Get metrics
   * @param {Object} [request] - Metrics request
   * @returns {Promise<Object>} Metrics response
   */
  async getMetrics(request = {}) {
    return this._execute('GetMetrics', {
      metric_names: request.metricNames || [],
      since_timestamp: request.sinceTimestamp || 0
    });
  }

  /**
   * Get client metrics
   * @returns {Object} Client metrics
   */
  getClientMetrics() {
    return {
      ...this.metrics,
      circuitBreaker: this.circuitBreaker?.getMetrics() || {},
      retryStrategy: this.retryStrategy?.getMetrics() || {},
      connectionPool: this.pool?.getStats() || {},
      health: this.healthMonitor?.getMetrics() || {}
    };
  }

  /**
   * Disconnect from sidecar
   */
  async disconnect() {
    if (!this.connected) {
      return;
    }

    // Stop health monitoring
    if (this.healthMonitor) {
      this.healthMonitor.cleanup();
    }

    // Close connection pool
    if (this.pool) {
      await this.pool.close();
    }

    // Cleanup circuit breaker
    if (this.circuitBreaker) {
      this.circuitBreaker.cleanup();
    }

    this.connected = false;
    this.client = null;
    this.emit('disconnected');
  }

  /**
   * Static factory method to create and connect client
   * @param {string} [address] - Sidecar address
   * @param {Object} [options] - Client options
   * @returns {Promise<SidecarClient>} Connected client
   */
  static async connect(address, options = {}) {
    const client = new SidecarClient(options);
    await client.connect(address, options);
    return client;
  }
}

/**
 * Create sidecar client
 * @param {Object} [options] - Client options
 * @returns {SidecarClient} Sidecar client instance
 */
export function createSidecarClient(options) {
  return new SidecarClient(options);
}

export default SidecarClient;
