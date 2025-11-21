/**
 * @file WebSocket Subscription Manager for real-time RDF graph updates
 * @module streaming/subscription-manager
 *
 * @description
 * Manages WebSocket subscriptions to RDF graph changes with support for
 * multiple subscription patterns (SPARQL SELECT, property changes, entity updates).
 * Provides automatic reconnection, subscription lifecycle management, and
 * integration with the Knowledge Hooks system.
 */

import { EventEmitter } from 'events';
import { randomUUID } from 'crypto';
import { z } from 'zod';
import { trace, SpanStatusCode } from '@opentelemetry/api';
import { createObservabilityManager } from '../observability.mjs';

const tracer = trace.getTracer('unrdf-streaming');

/**
 * Subscription pattern types
 * @enum {string}
 */
export const SubscriptionPatternType = {
  SPARQL_SELECT: 'sparql-select',
  PROPERTY_CHANGE: 'property-change',
  ENTITY_UPDATE: 'entity-update',
  WILDCARD: 'wildcard'
};

/**
 * Subscription configuration schema
 */
const SubscriptionConfigSchema = z.object({
  id: z.string().optional(),
  pattern: z.enum([
    SubscriptionPatternType.SPARQL_SELECT,
    SubscriptionPatternType.PROPERTY_CHANGE,
    SubscriptionPatternType.ENTITY_UPDATE,
    SubscriptionPatternType.WILDCARD
  ]),
  query: z.string().optional(),
  subject: z.string().optional(),
  predicate: z.string().optional(),
  object: z.string().optional(),
  debounceMs: z.number().min(0).default(100),
  batchSize: z.number().min(1).default(10),
  timeout: z.number().min(0).default(30000)
});

/**
 * WebSocket message schema
 */
const WebSocketMessageSchema = z.object({
  type: z.enum(['subscribe', 'unsubscribe', 'change', 'error', 'ping', 'pong']),
  subscriptionId: z.string().optional(),
  payload: z.any().optional(),
  timestamp: z.number().optional()
});

/**
 * Subscription manager configuration schema
 */
const ManagerConfigSchema = z.object({
  url: z.string().url().optional(),
  reconnectInterval: z.number().min(100).default(5000),
  maxReconnectAttempts: z.number().min(1).default(10),
  heartbeatInterval: z.number().min(1000).default(30000),
  enableCompression: z.boolean().default(true),
  maxSubscriptions: z.number().min(1).default(1000),
  observability: z.object({
    serviceName: z.string().default('unrdf-streaming'),
    enableTracing: z.boolean().default(true),
    enableMetrics: z.boolean().default(true)
  }).optional()
});

/**
 * WebSocket Subscription Manager
 * Manages real-time subscriptions to RDF graph changes
 */
export class SubscriptionManager extends EventEmitter {
  /**
   * Create a new subscription manager
   * @param {Object} config - Manager configuration
   */
  constructor(config = {}) {
    super();
    this.config = ManagerConfigSchema.parse(config);
    this.subscriptions = new Map();
    this.ws = null;
    this.reconnectAttempts = 0;
    this.reconnectTimer = null;
    this.heartbeatTimer = null;
    this.isConnected = false;
    this.isConnecting = false;

    // Observability
    this.observability = createObservabilityManager(
      this.config.observability || {}
    );

    // Performance metrics
    this.metrics = {
      subscriptionCount: 0,
      messagesReceived: 0,
      messagesSent: 0,
      reconnectCount: 0,
      errorCount: 0,
      latency: []
    };

    // Initialize observability
    this.observability.initialize().catch(err => {
      console.warn('[SubscriptionManager] Failed to initialize observability:', err.message);
    });
  }

  /**
   * Connect to WebSocket server
   * @param {string} [url] - WebSocket URL (overrides config)
   * @returns {Promise<void>}
   */
  async connect(url) {
    const wsUrl = url || this.config.url;
    if (!wsUrl) {
      throw new Error('WebSocket URL is required');
    }

    return tracer.startActiveSpan('subscription-manager.connect', async (span) => {
      try {
        span.setAttributes({
          'ws.url': wsUrl,
          'ws.reconnect_attempts': this.reconnectAttempts
        });

        if (this.isConnected || this.isConnecting) {
          console.log('[SubscriptionManager] Already connected or connecting');
          span.setStatus({ code: SpanStatusCode.OK });
          span.end();
          return;
        }

        this.isConnecting = true;

        // Dynamic import for Node.js WebSocket
        let WebSocket;
        if (typeof window === 'undefined') {
          const ws = await import('ws');
          WebSocket = ws.default;
        } else {
          WebSocket = window.WebSocket;
        }

        this.ws = new WebSocket(wsUrl);

        // Set up event handlers
        this.ws.onopen = () => this._handleOpen();
        this.ws.onmessage = (event) => this._handleMessage(event);
        this.ws.onerror = (error) => this._handleError(error);
        this.ws.onclose = () => this._handleClose();

        // Wait for connection
        await new Promise((resolve, reject) => {
          const timeout = setTimeout(() => {
            reject(new Error('Connection timeout'));
          }, this.config.reconnectInterval);

          const openHandler = () => {
            clearTimeout(timeout);
            this.ws.removeEventListener('open', openHandler);
            this.ws.removeEventListener('error', errorHandler);
            resolve();
          };

          const errorHandler = (error) => {
            clearTimeout(timeout);
            this.ws.removeEventListener('open', openHandler);
            this.ws.removeEventListener('error', errorHandler);
            reject(error);
          };

          this.ws.addEventListener('open', openHandler);
          this.ws.addEventListener('error', errorHandler);
        });

        span.setStatus({ code: SpanStatusCode.OK });
        span.end();
      } catch (error) {
        span.recordException(error);
        span.setStatus({
          code: SpanStatusCode.ERROR,
          message: error.message
        });
        span.end();
        this.isConnecting = false;
        throw error;
      }
    });
  }

  /**
   * Handle WebSocket open event
   * @private
   */
  _handleOpen() {
    this.isConnected = true;
    this.isConnecting = false;
    this.reconnectAttempts = 0;

    console.log('[SubscriptionManager] Connected to WebSocket server');
    this.emit('connected');

    // Start heartbeat
    this._startHeartbeat();

    // Resubscribe to all active subscriptions
    for (const [id, subscription] of this.subscriptions) {
      if (subscription.active) {
        this._sendSubscribe(id, subscription.config);
      }
    }
  }

  /**
   * Handle WebSocket message event
   * @private
   */
  _handleMessage(event) {
    return tracer.startActiveSpan('subscription-manager.message', (span) => {
      try {
        const startTime = Date.now();
        const data = typeof event.data === 'string'
          ? JSON.parse(event.data)
          : event.data;

        const message = WebSocketMessageSchema.parse(data);

        span.setAttributes({
          'ws.message.type': message.type,
          'ws.message.subscription_id': message.subscriptionId || 'none'
        });

        this.metrics.messagesReceived++;

        switch (message.type) {
          case 'change':
            this._handleChangeMessage(message);
            break;
          case 'error':
            this._handleErrorMessage(message);
            break;
          case 'pong':
            // Heartbeat response
            break;
          default:
            console.warn('[SubscriptionManager] Unknown message type:', message.type);
        }

        // Track latency
        if (message.timestamp) {
          const latency = Date.now() - message.timestamp;
          this.metrics.latency.push(latency);
          if (this.metrics.latency.length > 1000) {
            this.metrics.latency.shift();
          }
          span.setAttribute('ws.message.latency_ms', latency);
        }

        span.setStatus({ code: SpanStatusCode.OK });
        span.end();
      } catch (error) {
        span.recordException(error);
        span.setStatus({
          code: SpanStatusCode.ERROR,
          message: error.message
        });
        span.end();
        this.metrics.errorCount++;
        this.observability.recordError(error, { context: 'message-handling' });
      }
    });
  }

  /**
   * Handle change notification message
   * @private
   */
  _handleChangeMessage(message) {
    const { subscriptionId, payload } = message;

    if (!subscriptionId || !this.subscriptions.has(subscriptionId)) {
      console.warn('[SubscriptionManager] Change for unknown subscription:', subscriptionId);
      return;
    }

    const subscription = this.subscriptions.get(subscriptionId);

    // Emit change event
    this.emit('change', {
      subscriptionId,
      pattern: subscription.config.pattern,
      data: payload,
      timestamp: message.timestamp || Date.now()
    });

    // Emit subscription-specific event
    this.emit(`change:${subscriptionId}`, payload);
  }

  /**
   * Handle error message
   * @private
   */
  _handleErrorMessage(message) {
    const { subscriptionId, payload } = message;

    this.metrics.errorCount++;

    const error = new Error(payload?.message || 'Unknown error');
    error.subscriptionId = subscriptionId;

    this.emit('error', error);

    if (subscriptionId) {
      this.emit(`error:${subscriptionId}`, error);
    }

    this.observability.recordError(error, {
      subscriptionId,
      context: 'subscription-error'
    });
  }

  /**
   * Handle WebSocket error event
   * @private
   */
  _handleError(error) {
    console.error('[SubscriptionManager] WebSocket error:', error);
    this.metrics.errorCount++;
    this.emit('error', error);
    this.observability.recordError(error, { context: 'websocket-error' });
  }

  /**
   * Handle WebSocket close event
   * @private
   */
  _handleClose() {
    this.isConnected = false;
    this.isConnecting = false;

    console.log('[SubscriptionManager] Disconnected from WebSocket server');
    this.emit('disconnected');

    // Stop heartbeat
    this._stopHeartbeat();

    // Attempt reconnection with exponential backoff and jitter
    if (this.reconnectAttempts < this.config.maxReconnectAttempts) {
      this.reconnectAttempts++;
      this.metrics.reconnectCount++;

      // Exponential backoff: baseDelay * 2^attempt + random jitter
      const baseDelay = this.config.reconnectInterval;
      const exponentialDelay = baseDelay * Math.pow(2, this.reconnectAttempts - 1);
      const jitter = Math.random() * baseDelay * 0.5; // Add up to 50% jitter
      const totalDelay = Math.min(exponentialDelay + jitter, 60000); // Cap at 60 seconds

      console.log(`[SubscriptionManager] Reconnecting in ${Math.round(totalDelay)}ms (attempt ${this.reconnectAttempts}/${this.config.maxReconnectAttempts})`);

      this.reconnectTimer = setTimeout(() => {
        this.connect(this.config.url).catch(err => {
          console.error('[SubscriptionManager] Reconnection failed:', err.message);
        });
      }, totalDelay);
    } else {
      console.error('[SubscriptionManager] Max reconnection attempts reached');
      this.emit('max-reconnects');
      // Emit recoveryAvailable event to signal manual reconnection is possible
      this.emit('recoveryAvailable', {
        attempts: this.reconnectAttempts,
        lastAttemptTime: Date.now(),
        message: 'Manual reconnection available via reconnect() method'
      });
    }
  }

  /**
   * Manually trigger reconnection after max attempts exhausted
   * Resets reconnect counter and initiates fresh connection attempt
   * @returns {Promise<void>}
   */
  async reconnect() {
    // Clear any existing reconnect timer
    if (this.reconnectTimer) {
      clearTimeout(this.reconnectTimer);
      this.reconnectTimer = null;
    }

    // Reset reconnect attempts to allow fresh connection cycle
    this.reconnectAttempts = 0;

    console.log('[SubscriptionManager] Manual reconnection initiated');
    this.emit('reconnecting', { manual: true });

    try {
      await this.connect(this.config.url);
    } catch (error) {
      console.error('[SubscriptionManager] Manual reconnection failed:', error.message);
      throw error;
    }
  }

  /**
   * Start heartbeat timer
   * @private
   */
  _startHeartbeat() {
    this._stopHeartbeat();

    this.heartbeatTimer = setInterval(() => {
      if (this.isConnected && this.ws) {
        this._send({
          type: 'ping',
          timestamp: Date.now()
        });
      }
    }, this.config.heartbeatInterval);
  }

  /**
   * Stop heartbeat timer
   * @private
   */
  _stopHeartbeat() {
    if (this.heartbeatTimer) {
      clearInterval(this.heartbeatTimer);
      this.heartbeatTimer = null;
    }
  }

  /**
   * Subscribe to graph changes
   * @param {Object} config - Subscription configuration
   * @param {Function} [callback] - Change callback
   * @returns {string} Subscription ID
   */
  subscribe(config, callback) {
    return tracer.startActiveSpan('subscription-manager.subscribe', (span) => {
      try {
        const validatedConfig = SubscriptionConfigSchema.parse(config);
        const id = validatedConfig.id || randomUUID();

        span.setAttributes({
          'subscription.id': id,
          'subscription.pattern': validatedConfig.pattern
        });

        // Check subscription limit
        if (this.subscriptions.size >= this.config.maxSubscriptions) {
          throw new Error(`Maximum subscriptions (${this.config.maxSubscriptions}) reached`);
        }

        // Store subscription
        this.subscriptions.set(id, {
          config: validatedConfig,
          active: true,
          createdAt: Date.now()
        });

        this.metrics.subscriptionCount++;

        // Register callback if provided
        if (callback && typeof callback === 'function') {
          this.on(`change:${id}`, callback);
        }

        // Send subscribe message if connected
        if (this.isConnected) {
          this._sendSubscribe(id, validatedConfig);
        }

        span.setStatus({ code: SpanStatusCode.OK });
        span.end();

        return id;
      } catch (error) {
        span.recordException(error);
        span.setStatus({
          code: SpanStatusCode.ERROR,
          message: error.message
        });
        span.end();
        throw error;
      }
    });
  }

  /**
   * Send subscribe message to server
   * @private
   */
  _sendSubscribe(id, config) {
    this._send({
      type: 'subscribe',
      subscriptionId: id,
      payload: {
        pattern: config.pattern,
        query: config.query,
        subject: config.subject,
        predicate: config.predicate,
        object: config.object,
        debounceMs: config.debounceMs,
        batchSize: config.batchSize
      },
      timestamp: Date.now()
    });
  }

  /**
   * Unsubscribe from graph changes
   * @param {string} id - Subscription ID
   * @returns {boolean} Success
   */
  unsubscribe(id) {
    return tracer.startActiveSpan('subscription-manager.unsubscribe', (span) => {
      try {
        span.setAttribute('subscription.id', id);

        if (!this.subscriptions.has(id)) {
          span.setStatus({ code: SpanStatusCode.OK });
          span.end();
          return false;
        }

        // Send unsubscribe message if connected
        if (this.isConnected) {
          this._send({
            type: 'unsubscribe',
            subscriptionId: id,
            timestamp: Date.now()
          });
        }

        // Remove subscription
        this.subscriptions.delete(id);
        this.removeAllListeners(`change:${id}`);
        this.removeAllListeners(`error:${id}`);

        span.setStatus({ code: SpanStatusCode.OK });
        span.end();

        return true;
      } catch (error) {
        span.recordException(error);
        span.setStatus({
          code: SpanStatusCode.ERROR,
          message: error.message
        });
        span.end();
        return false;
      }
    });
  }

  /**
   * Unsubscribe from all subscriptions
   */
  unsubscribeAll() {
    const ids = Array.from(this.subscriptions.keys());
    for (const id of ids) {
      this.unsubscribe(id);
    }
  }

  /**
   * Send message to server
   * @private
   */
  _send(message) {
    if (!this.isConnected || !this.ws) {
      throw new Error('Not connected to WebSocket server');
    }

    const data = JSON.stringify(message);
    this.ws.send(data);
    this.metrics.messagesSent++;
  }

  /**
   * Get subscription by ID
   * @param {string} id - Subscription ID
   * @returns {Object|null} Subscription or null
   */
  getSubscription(id) {
    return this.subscriptions.get(id) || null;
  }

  /**
   * Get all subscriptions
   * @returns {Array<Object>} Array of subscriptions
   */
  getAllSubscriptions() {
    return Array.from(this.subscriptions.entries()).map(([id, sub]) => ({
      id,
      ...sub
    }));
  }

  /**
   * Get performance metrics
   * @returns {Object} Metrics
   */
  getMetrics() {
    const latencies = this.metrics.latency.slice(-100);
    const avgLatency = latencies.length > 0
      ? latencies.reduce((sum, l) => sum + l, 0) / latencies.length
      : 0;

    const p95Latency = latencies.length > 0
      ? latencies.sort((a, b) => a - b)[Math.floor(latencies.length * 0.95)]
      : 0;

    return {
      subscriptionCount: this.subscriptions.size,
      messagesReceived: this.metrics.messagesReceived,
      messagesSent: this.metrics.messagesSent,
      reconnectCount: this.metrics.reconnectCount,
      errorCount: this.metrics.errorCount,
      avgLatency,
      p95Latency,
      isConnected: this.isConnected,
      reconnectAttempts: this.reconnectAttempts
    };
  }

  /**
   * Disconnect from WebSocket server
   * @returns {Promise<void>}
   */
  async disconnect() {
    // Clear reconnect timer
    if (this.reconnectTimer) {
      clearTimeout(this.reconnectTimer);
      this.reconnectTimer = null;
    }

    // Stop heartbeat
    this._stopHeartbeat();

    // Close WebSocket connection
    if (this.ws) {
      this.ws.close();
      this.ws = null;
    }

    this.isConnected = false;
    this.isConnecting = false;
    this.reconnectAttempts = 0;

    // Shutdown observability
    await this.observability.shutdown();
  }

  /**
   * Cleanup resources
   * @returns {Promise<void>}
   */
  async cleanup() {
    this.unsubscribeAll();
    await this.disconnect();
    this.removeAllListeners();
  }
}

/**
 * Create a subscription manager instance
 * @param {Object} config - Manager configuration
 * @returns {SubscriptionManager} Subscription manager
 */
export function createSubscriptionManager(config = {}) {
  return new SubscriptionManager(config);
}
