/**
 * MCP Bridge - Protocol bridging and transport abstraction
 *
 * Σ_bridge (protocol translation):
 *   B ≔ (σ_in, σ_out, π)
 *   σ_in: Protocol_A → MCP         // input transformation
 *   σ_out: MCP → Protocol_B        // output transformation
 *   π: Message → Message           // protocol translation
 *
 * Law (Bidirectional Translation):
 *   π(σ_in(m)) ≈ m  (lossy compression allowed)
 *   σ_out(π⁻¹(m)) ≈ m
 *
 * Transport Types:
 *   - HTTP: REST-style request/response
 *   - WebSocket: Bidirectional streaming
 *   - STDIO: Standard I/O pipes
 *
 * @module @unrdf/kgc-claude/mcp-bridge
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';
import { now } from '@unrdf/kgc-4d';

/**
 * Message schema - universal MCP message format
 */
export const MessageSchema = z.object({
  id: z.string(),
  type: z.enum(['request', 'response', 'notification', 'error']),
  method: z.string().optional(),
  params: z.any().optional(),
  result: z.any().optional(),
  error: z.object({
    code: z.number().int(),
    message: z.string(),
    data: z.any().optional(),
  }).optional(),
  metadata: z.any().optional(),
  t_ns: z.bigint(),
});

/**
 * @typedef {z.infer<typeof MessageSchema>} Message
 */

/**
 * HTTP transport configuration
 */
export const HTTPConfigSchema = z.object({
  baseUrl: z.string().url(),
  headers: z.record(z.string()).default({}),
  timeout: z.number().int().default(30000),
  retries: z.number().int().default(3),
});

/**
 * @typedef {z.infer<typeof HTTPConfigSchema>} HTTPConfig
 */

/**
 * WebSocket transport configuration
 */
export const WebSocketConfigSchema = z.object({
  url: z.string(),
  protocols: z.array(z.string()).default([]),
  reconnect: z.boolean().default(true),
  heartbeat_interval: z.number().int().default(30000),
});

/**
 * @typedef {z.infer<typeof WebSocketConfigSchema>} WebSocketConfig
 */

/**
 * STDIO transport configuration
 */
export const STDIOConfigSchema = z.object({
  encoding: z.string().default('utf-8'),
  delimiter: z.string().default('\n'),
  buffer_size: z.number().int().default(64 * 1024),
});

/**
 * @typedef {z.infer<typeof STDIOConfigSchema>} STDIOConfig
 */

/**
 * Bridge statistics schema
 */
export const BridgeStatsSchema = z.object({
  messages_in: z.number().int(),
  messages_out: z.number().int(),
  bytes_in: z.number().int(),
  bytes_out: z.number().int(),
  errors: z.number().int(),
  connections: z.number().int(),
  uptime_ms: z.number(),
  hash: z.string(),
  t_ns: z.bigint(),
});

/**
 * @typedef {z.infer<typeof BridgeStatsSchema>} BridgeStats
 */

/**
 * Generate UUID v4
 */
function generateUUID() {
  if (typeof crypto !== 'undefined' && crypto.randomUUID) {
    return crypto.randomUUID();
  }
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
    const r = (Math.random() * 16) | 0;
    const v = c === 'x' ? r : (r & 0x3) | 0x8;
    return v.toString(16);
  });
}

/**
 * MCP Bridge - Protocol translation and transport abstraction
 */
export class MCPBridge {
  constructor(transport = 'http', config = {}) {
    this.transport = transport;
    this.config = this._validateConfig(transport, config);

    /** @type {Message[]} */
    this.messageQueue = [];
    /** @type {Map<string, Message>} */
    this.pendingRequests = new Map();

    this.stats = {
      messages_in: 0,
      messages_out: 0,
      bytes_in: 0,
      bytes_out: 0,
      errors: 0,
      connections: 0,
      start_time: performance.now(),
    };

    this.handlers = new Map();
  }

  /**
   * Validate transport configuration
   * @param {string} transport
   * @param {object} config
   * @returns {object}
   * @private
   */
  _validateConfig(transport, config) {
    switch (transport) {
      case 'http':
        return HTTPConfigSchema.parse(config);
      case 'websocket':
        return WebSocketConfigSchema.parse(config);
      case 'stdio':
        return STDIOConfigSchema.parse(config);
      default:
        throw new Error(`Unknown transport: ${transport}`);
    }
  }

  /**
   * Send message through bridge
   * @param {Message} message
   * @returns {Promise<void>}
   */
  async send(message) {
    const validated = MessageSchema.parse(message);
    const serialized = JSON.stringify(validated, (key, value) =>
      typeof value === 'bigint' ? value.toString() : value
    );

    this.stats.messages_out++;
    this.stats.bytes_out += serialized.length;

    if (validated.type === 'request') {
      this.pendingRequests.set(validated.id, validated);
    }

    // Transport-specific send logic
    switch (this.transport) {
      case 'http':
        await this._sendHTTP(validated);
        break;
      case 'websocket':
        await this._sendWebSocket(validated);
        break;
      case 'stdio':
        await this._sendSTDIO(validated);
        break;
    }
  }

  /**
   * Receive message from bridge
   * @param {string} rawMessage
   * @returns {Promise<Message>}
   */
  async receive(rawMessage) {
    this.stats.messages_in++;
    this.stats.bytes_in += rawMessage.length;

    try {
      const parsed = JSON.parse(rawMessage);
      const message = MessageSchema.parse({
        ...parsed,
        t_ns: parsed.t_ns ? BigInt(parsed.t_ns) : now(),
      });

      // Handle response to pending request
      if (message.type === 'response' && this.pendingRequests.has(message.id)) {
        this.pendingRequests.delete(message.id);
      }

      // Trigger handlers
      const handler = this.handlers.get(message.method || message.type);
      if (handler) {
        await handler(message);
      }

      return message;
    } catch (error) {
      this.stats.errors++;
      throw error;
    }
  }

  /**
   * Register message handler
   * @param {string} methodOrType
   * @param {function} handler
   */
  on(methodOrType, handler) {
    this.handlers.set(methodOrType, handler);
  }

  /**
   * Send via HTTP transport
   * @param {Message} message
   * @returns {Promise<void>}
   * @private
   */
  async _sendHTTP(message) {
    // Mock HTTP send (actual implementation would use fetch/axios)
    // In real implementation:
    // const response = await fetch(`${this.config.baseUrl}/mcp`, {
    //   method: 'POST',
    //   headers: { 'Content-Type': 'application/json', ...this.config.headers },
    //   body: JSON.stringify(message),
    // });

    this.messageQueue.push(message);
  }

  /**
   * Send via WebSocket transport
   * @param {Message} message
   * @returns {Promise<void>}
   * @private
   */
  async _sendWebSocket(message) {
    // Mock WebSocket send
    // In real implementation:
    // this.ws.send(JSON.stringify(message));

    this.messageQueue.push(message);
  }

  /**
   * Send via STDIO transport
   * @param {Message} message
   * @returns {Promise<void>}
   * @private
   */
  async _sendSTDIO(message) {
    // Mock STDIO send
    // In real implementation:
    // process.stdout.write(JSON.stringify(message) + this.config.delimiter);

    this.messageQueue.push(message);
  }

  /**
   * Create request message
   * @param {string} method
   * @param {object} params
   * @returns {Message}
   */
  createRequest(method, params = {}) {
    return MessageSchema.parse({
      id: generateUUID(),
      type: 'request',
      method,
      params: Object.keys(params).length > 0 ? params : undefined,
      metadata: undefined,
      t_ns: now(),
    });
  }

  /**
   * Create response message
   * @param {string} requestId
   * @param {any} result
   * @returns {Message}
   */
  createResponse(requestId, result) {
    return MessageSchema.parse({
      id: requestId,
      type: 'response',
      result,
      metadata: undefined,
      t_ns: now(),
    });
  }

  /**
   * Create error message
   * @param {string} requestId
   * @param {number} code
   * @param {string} message
   * @param {any} data
   * @returns {Message}
   */
  createError(requestId, code, message, data = null) {
    return MessageSchema.parse({
      id: requestId,
      type: 'error',
      error: { code, message, data: data || undefined },
      metadata: undefined,
      t_ns: now(),
    });
  }

  /**
   * Create notification message
   * @param {string} method
   * @param {object} params
   * @returns {Message}
   */
  createNotification(method, params = {}) {
    return MessageSchema.parse({
      id: generateUUID(),
      type: 'notification',
      method,
      params: Object.keys(params).length > 0 ? params : undefined,
      metadata: undefined,
      t_ns: now(),
    });
  }

  /**
   * Get bridge statistics
   * @returns {Promise<BridgeStats>}
   */
  async getStats() {
    const statsData = {
      ...this.stats,
      uptime_ms: performance.now() - this.stats.start_time,
    };

    const hash = await blake3(JSON.stringify(statsData));

    return BridgeStatsSchema.parse({
      ...statsData,
      hash,
      t_ns: now(),
    });
  }

  /**
   * Get pending requests
   * @returns {Message[]}
   */
  getPendingRequests() {
    return Array.from(this.pendingRequests.values());
  }

  /**
   * Get message queue
   * @returns {Message[]}
   */
  getMessageQueue() {
    return [...this.messageQueue];
  }

  /**
   * Clear message queue
   */
  clearQueue() {
    this.messageQueue = [];
  }

  /**
   * Reset statistics
   */
  resetStats() {
    this.stats = {
      messages_in: 0,
      messages_out: 0,
      bytes_in: 0,
      bytes_out: 0,
      errors: 0,
      connections: 0,
      start_time: performance.now(),
    };
  }

  /**
   * Close bridge
   */
  close() {
    this.handlers.clear();
    this.pendingRequests.clear();
    this.messageQueue = [];
  }
}

/**
 * Create MCP bridge instance
 * @param {string} transport
 * @param {object} config
 * @returns {MCPBridge}
 */
export function createBridge(transport = 'http', config = {}) {
  return new MCPBridge(transport, config);
}

/**
 * Create HTTP bridge
 * @param {string} baseUrl
 * @param {object} options
 * @returns {MCPBridge}
 */
export function createHTTPBridge(baseUrl, options = {}) {
  return createBridge('http', { baseUrl, ...options });
}

/**
 * Create WebSocket bridge
 * @param {string} url
 * @param {object} options
 * @returns {MCPBridge}
 */
export function createWebSocketBridge(url, options = {}) {
  return createBridge('websocket', { url, ...options });
}

/**
 * Create STDIO bridge
 * @param {object} options
 * @returns {MCPBridge}
 */
export function createSTDIOBridge(options = {}) {
  return createBridge('stdio', options);
}

export default MCPBridge;
