/**
 * @fileoverview WebSocket Transport Layer for Raft Consensus
 * @module consensus/transport/websocket-transport
 *
 * @description
 * Provides reliable message transport between Raft nodes using WebSocket.
 * Features msgpackr for efficient serialization and automatic reconnection.
 *
 * Key features:
 * - Bidirectional WebSocket communication
 * - msgpackr serialization for efficiency
 * - Automatic reconnection with exponential backoff
 * - Message acknowledgment and retries
 * - Connection pooling
 */

import { WebSocketServer, WebSocket } from 'ws';
import { pack, unpack } from 'msgpackr';
import { EventEmitter } from 'events';
import { z } from 'zod';
import { trace, SpanStatusCode } from '@opentelemetry/api';
import https from 'https';
import fs from 'fs';

const tracer = trace.getTracer('unrdf-consensus');

/**
 * Message schema
 */
const MessageSchema = z.object({
  type: z.enum(['request_vote', 'append_entries', 'heartbeat', 'command', 'response', 'auth']),
  from: z.string(),
  to: z.string(),
  term: z.number().int().nonnegative(),
  data: z.any(),
  messageId: z.string(),
  timestamp: z.number(),
});

/**
 * Transport configuration schema
 */
const TransportConfigSchema = z.object({
  nodeId: z.string(),
  port: z.number().int().positive(),
  host: z.string().default('latest.0'),
  reconnectInterval: z.number().positive().default(1000),
  reconnectMaxRetries: z.number().int().positive().default(10),
  messageTimeout: z.number().positive().default(5000),
  tls: z.object({
    enabled: z.boolean().default(false),
    cert: z.string().optional(),
    key: z.string().optional(),
    ca: z.string().optional(),
    rejectUnauthorized: z.boolean().default(true),
  }).default({ enabled: false }),
  auth: z.object({
    enabled: z.boolean().default(false),
    sharedSecret: z.string().optional(),
  }).default({ enabled: false }),
});

/**
 * WebSocket Transport
 *
 * Manages WebSocket connections between Raft nodes,
 * providing reliable message delivery with retries.
 *
 * @class WebSocketTransport
 * @extends EventEmitter
 */
export class WebSocketTransport extends EventEmitter {
  /**
   * Create a WebSocket transport
   * @param {Object} config - Transport configuration
   */
  constructor(config) {
    super();
    this.config = TransportConfigSchema.parse(config);

    this.server = null;
    this.httpsServer = null;
    this.connections = new Map(); // nodeId -> WebSocket
    this.authenticatedConnections = new Set(); // WebSocket instances that passed auth
    this.pendingMessages = new Map(); // messageId -> { resolve, reject, timeout }
    this.peers = new Map(); // nodeId -> { host, port }
    this.reconnectAttempts = new Map(); // nodeId -> attempts
  }

  /**
   * Start the transport server
   * @returns {Promise<void>}
   */
  async start() {
    return tracer.startActiveSpan('transport.start', async span => {
      try {
        span.setAttribute('node.id', this.config.nodeId);
        span.setAttribute('transport.port', this.config.port);
        span.setAttribute('transport.tls', this.config.tls.enabled);

        const serverOptions = {
          host: this.config.host,
          port: this.config.port,
        };

        if (this.config.tls.enabled) {
          const httpsOptions = {
            cert: this.config.tls.cert ? fs.readFileSync(this.config.tls.cert) : undefined,
            key: this.config.tls.key ? fs.readFileSync(this.config.tls.key) : undefined,
            ca: this.config.tls.ca ? fs.readFileSync(this.config.tls.ca) : undefined,
          };

          this.httpsServer = https.createServer(httpsOptions);
          this.httpsServer.listen(this.config.port, this.config.host);
          
          this.server = new WebSocketServer({
            server: this.httpsServer,
          });
        } else {
          this.server = new WebSocketServer(serverOptions);
        }

        this.server.on('connection', (ws, req) => {
          this.handleConnection(ws, req);
        });

        this.server.on('error', error => {
          this.emit('error', error);
        });

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
   * Handle incoming WebSocket connection
   * @param {WebSocket} ws - WebSocket connection
   * @param {Object} _req - HTTP request
   * @private
   */
  handleConnection(ws, _req) {
    let nodeId = null;

    ws.on('message', data => {
      try {
        const message = unpack(data);
        nodeId = message.from;

        // Authentication check
        if (this.config.auth.enabled && !this.authenticatedConnections.has(ws)) {
          if (message.type === 'auth') {
            if (message.data?.sharedSecret === this.config.auth.sharedSecret) {
              this.authenticatedConnections.add(ws);
              
              // Send auth success response
              const responseMessage = {
                type: 'response',
                from: this.config.nodeId,
                to: message.from,
                messageId: message.messageId,
                term: 0,
                data: { success: true },
                timestamp: Date.now(),
              };
              ws.send(pack(responseMessage));
              return;
            } else {
              this.emit('error', { error: 'Authentication failed', nodeId });
              ws.close(4001, 'Authentication failed');
              return;
            }
          } else {
            // Drop any message before authentication
            return;
          }
        }

        // Register connection
        if (!this.connections.has(nodeId)) {
          this.connections.set(nodeId, ws);
          this.reconnectAttempts.delete(nodeId);
          this.emit('peer_connected', { nodeId });
        }

        // Handle message
        if (message.type === 'response') {
          this.handleResponse(message);
        } else {
          this.emit('message', message);
        }
      } catch (error) {
        this.emit('error', { error, source: 'message_parse' });
      }
    });

    ws.on('close', () => {
      this.authenticatedConnections.delete(ws);
      if (nodeId) {
        this.connections.delete(nodeId);
        this.emit('peer_disconnected', { nodeId });
        this.scheduleReconnect(nodeId);
      }
    });

    ws.on('error', error => {
      this.emit('error', { error, nodeId });
    });
  }

  /**
   * Add a peer node
   * @param {string} nodeId - Peer node ID
   * @param {string} host - Peer host
   * @param {number} port - Peer port
   */
  addPeer(nodeId, host, port) {
    this.peers.set(nodeId, { host, port });
    this.connectToPeer(nodeId);
  }

  /**
   * Remove a peer node
   * @param {string} nodeId - Peer node ID
   */
  removePeer(nodeId) {
    this.peers.delete(nodeId);
    const ws = this.connections.get(nodeId);
    if (ws) {
      ws.close();
      this.connections.delete(nodeId);
    }
    this.reconnectAttempts.delete(nodeId);
  }

  /**
   * Authenticate with a peer
   * @param {WebSocket} ws - WebSocket connection
   * @param {string} nodeId - Target node ID
   * @private
   */
  async authenticate(ws, nodeId) {
    return new Promise((resolve, reject) => {
      const messageId = `auth-${this.config.nodeId}-${Date.now()}`;
      const authMessage = {
        type: 'auth',
        from: this.config.nodeId,
        to: nodeId,
        term: 0,
        data: { sharedSecret: this.config.auth.sharedSecret },
        messageId,
        timestamp: Date.now(),
      };

      const timeout = setTimeout(() => {
        reject(new Error('Authentication timeout'));
      }, this.config.messageTimeout);

      const handleAuthResponse = (data) => {
        try {
          const message = unpack(data);
          if (message.type === 'response' && message.messageId === messageId) {
            clearTimeout(timeout);
            ws.removeListener('message', handleAuthResponse);
            if (message.data?.success) {
              resolve();
            } else {
              reject(new Error('Authentication failed'));
            }
          }
        } catch (error) {
          // Ignore parse errors during handshake
        }
      };

      ws.on('message', handleAuthResponse);
      ws.send(pack(authMessage));
    });
  }

  /**
   * Connect to a peer node
   * @param {string} nodeId - Peer node ID
   * @returns {Promise<void>}
   * @private
   */
  async connectToPeer(nodeId) {
    const peer = this.peers.get(nodeId);
    if (!peer) return;

    if (this.connections.has(nodeId)) return;

    try {
      const protocol = this.config.tls.enabled ? 'wss' : 'ws';
      const wsOptions = {
        rejectUnauthorized: this.config.tls.rejectUnauthorized,
      };

      if (this.config.tls.cert) wsOptions.cert = fs.readFileSync(this.config.tls.cert);
      if (this.config.tls.key) wsOptions.key = fs.readFileSync(this.config.tls.key);
      if (this.config.tls.ca) wsOptions.ca = fs.readFileSync(this.config.tls.ca);

      const ws = new WebSocket(`${protocol}://${peer.host}:${peer.port}`, wsOptions);

      ws.on('open', async () => {
        try {
          if (this.config.auth.enabled) {
            await this.authenticate(ws, nodeId);
            this.authenticatedConnections.add(ws);
          }
          this.connections.set(nodeId, ws);
          this.reconnectAttempts.delete(nodeId);
          this.emit('peer_connected', { nodeId });
        } catch (error) {
          this.emit('error', { error, nodeId, source: 'auth' });
          ws.close();
        }
      });

      ws.on('message', data => {
        try {
          const message = unpack(data);
          if (message.type === 'response') {
            this.handleResponse(message);
          } else {
            this.emit('message', message);
          }
        } catch (error) {
          this.emit('error', { error, source: 'message_parse' });
        }
      });

      ws.on('close', () => {
        this.authenticatedConnections.delete(ws);
        this.connections.delete(nodeId);
        this.emit('peer_disconnected', { nodeId });
        this.scheduleReconnect(nodeId);
      });

      ws.on('error', error => {
        this.emit('error', { error, nodeId });
      });
    } catch (error) {
      this.emit('error', { error, nodeId, source: 'connect' });
      this.scheduleReconnect(nodeId);
    }
  }

  /**
   * Schedule reconnection to a peer
   * @param {string} nodeId - Peer node ID
   * @private
   */
  scheduleReconnect(nodeId) {
    if (!this.peers.has(nodeId)) return;

    const attempts = this.reconnectAttempts.get(nodeId) || 0;
    if (attempts >= this.config.reconnectMaxRetries) {
      this.emit('reconnect_failed', { nodeId, attempts });
      return;
    }

    this.reconnectAttempts.set(nodeId, attempts + 1);

    // Exponential backoff
    const delay = this.config.reconnectInterval * Math.pow(2, attempts);

    setTimeout(() => {
      this.connectToPeer(nodeId);
    }, delay);
  }

  /**
   * Send a message to a peer
   * @param {string} to - Target node ID
   * @param {Object} message - Message to send
   * @returns {Promise<Object>} Response from peer
   */
  async send(to, message) {
    return tracer.startActiveSpan('transport.send', async span => {
      try {
        span.setAttributes({
          'message.to': to,
          'message.type': message.type,
          'message.term': message.term,
        });

        const ws = this.connections.get(to);
        if (!ws || ws.readyState !== WebSocket.OPEN) {
          throw new Error(`No connection to peer: ${to}`);
        }

        const messageId = `${this.config.nodeId}-${Date.now()}-${Math.random()}`;
        const fullMessage = MessageSchema.parse({
          ...message,
          from: this.config.nodeId,
          to,
          messageId,
          timestamp: Date.now(),
        });

        const packed = pack(fullMessage);

        return new Promise((resolve, reject) => {
          const timeout = setTimeout(() => {
            this.pendingMessages.delete(messageId);
            reject(new Error(`Message timeout: ${messageId}`));
          }, this.config.messageTimeout);

          this.pendingMessages.set(messageId, { resolve, reject, timeout });

          ws.send(packed, error => {
            if (error) {
              clearTimeout(timeout);
              this.pendingMessages.delete(messageId);
              reject(error);
            }
          });
        });
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
   * Send a response to a message
   * @param {Object} originalMessage - Original message
   * @param {Object} response - Response data
   */
  sendResponse(originalMessage, response) {
    const ws = this.connections.get(originalMessage.from);
    if (!ws || ws.readyState !== WebSocket.OPEN) return;

    const responseMessage = {
      type: 'response',
      from: this.config.nodeId,
      to: originalMessage.from,
      messageId: originalMessage.messageId,
      term: response.term || 0,
      data: response,
      timestamp: Date.now(),
    };

    const packed = pack(responseMessage);
    ws.send(packed);
  }

  /**
   * Handle response message
   * @param {Object} message - Response message
   * @private
   */
  handleResponse(message) {
    const pending = this.pendingMessages.get(message.messageId);
    if (!pending) return;

    clearTimeout(pending.timeout);
    this.pendingMessages.delete(message.messageId);
    pending.resolve(message.data);
  }

  /**
   * Get connected peers
   * @returns {Array<string>} Array of connected peer node IDs
   */
  getConnectedPeers() {
    return Array.from(this.connections.keys());
  }

  /**
   * Check if connected to a peer
   * @param {string} nodeId - Peer node ID
   * @returns {boolean} True if connected
   */
  isConnected(nodeId) {
    const ws = this.connections.get(nodeId);
    return ws && ws.readyState === WebSocket.OPEN;
  }

  /**
   * Shutdown the transport
   * @returns {Promise<void>}
   */
  async shutdown() {
    // Close all peer connections
    for (const ws of this.connections.values()) {
      ws.close();
    }
    this.connections.clear();
    this.authenticatedConnections.clear();

    // Close server
    if (this.server) {
      await new Promise(resolve => {
        this.server.close(resolve);
      });
    }

    // Close HTTPS server if any
    if (this.httpsServer) {
      await new Promise(resolve => {
        this.httpsServer.close(resolve);
      });
    }

    // Clear pending messages
    for (const pending of this.pendingMessages.values()) {
      clearTimeout(pending.timeout);
      pending.reject(new Error('Transport shutdown'));
    }
    this.pendingMessages.clear();

    this.emit('shutdown');
  }
}

/**
 * Create a WebSocket transport
 * @param {Object} config - Transport configuration
 * @returns {WebSocketTransport} New transport instance
 */
export function createWebSocketTransport(config) {
  return new WebSocketTransport(config);
}
