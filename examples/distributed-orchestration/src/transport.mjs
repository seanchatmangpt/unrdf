/**
 * @file WebSocket transport between orchestrator and workers
 * @description
 * `@unrdf/federation` is a SPARQL query federation package (createCoordinator,
 * peer manager, query planner). It has no orchestrator/worker messaging API,
 * so this example ships a small JSON-over-WebSocket transport built on `ws`.
 *
 * Protocol (one JSON object per frame):
 *   worker -> orchestrator: worker_registration, heartbeat, task_completion, task_failure
 *   orchestrator -> worker: task_assignment, task_cancellation, shutdown, workflow_change
 *
 * @module distributed-orchestration/transport
 */

import { EventEmitter } from 'node:events';
import { WebSocketServer, WebSocket } from 'ws';

/** Upper bound on a single inbound frame (bytes). */
const MAX_PAYLOAD = 1024 * 1024;

/**
 * Orchestrator side: accepts worker connections.
 */
export class OrchestratorTransport extends EventEmitter {
  /**
   * @param {{port?: number}} [config] - Port 0 picks a free port
   */
  constructor(config = {}) {
    super();
    this.port = config.port ?? 8080;
    this.wss = null;
    /** @type {Map<string, WebSocket>} nodeId -> socket */
    this.sockets = new Map();
  }

  /**
   * Start listening.
   *
   * @returns {Promise<number>} The bound port
   */
  async start() {
    await new Promise((resolve, reject) => {
      this.wss = new WebSocketServer({ port: this.port, maxPayload: MAX_PAYLOAD });
      this.wss.once('listening', resolve);
      this.wss.once('error', reject);
    });
    this.port = this.wss.address().port;

    this.wss.on('connection', socket => {
      socket.on('message', raw => {
        let message;
        try {
          message = JSON.parse(raw.toString());
        } catch (error) {
          this.emit('error', new Error(`Invalid frame from worker: ${error.message}`));
          return;
        }
        if (message?.type === 'worker_registration' && typeof message.nodeId === 'string') {
          this.sockets.set(message.nodeId, socket);
          socket.nodeId = message.nodeId;
        }
        this.emit('message', message);
      });
      socket.on('close', () => {
        if (socket.nodeId && this.sockets.get(socket.nodeId) === socket) {
          this.sockets.delete(socket.nodeId);
          this.emit('disconnect', socket.nodeId);
        }
      });
      socket.on('error', error => this.emit('error', error));
    });
    return this.port;
  }

  /**
   * Send to one worker.
   *
   * @param {string} nodeId
   * @param {object} message
   * @returns {Promise<boolean>} true when handed to a connected socket
   */
  async sendMessage(nodeId, message) {
    const socket = this.sockets.get(nodeId);
    if (!socket || socket.readyState !== WebSocket.OPEN) {
      return false;
    }
    await new Promise((resolve, reject) =>
      socket.send(JSON.stringify(message), error => (error ? reject(error) : resolve()))
    );
    return true;
  }

  /**
   * Send to every connected worker.
   *
   * @param {object} message
   * @returns {number} Number of sockets written to
   */
  broadcast(message) {
    const frame = JSON.stringify(message);
    let sent = 0;
    for (const socket of this.sockets.values()) {
      if (socket.readyState === WebSocket.OPEN) {
        socket.send(frame);
        sent++;
      }
    }
    return sent;
  }

  /**
   * Stop listening and close all sockets.
   *
   * @returns {Promise<void>}
   */
  async stop() {
    if (!this.wss) return;
    for (const socket of this.wss.clients) {
      socket.terminate();
    }
    this.sockets.clear();
    await new Promise(resolve => this.wss.close(resolve));
    this.wss = null;
  }
}

/**
 * Worker side: connects to the orchestrator.
 */
export class WorkerTransport extends EventEmitter {
  /**
   * @param {{serverUrl: string}} config - http(s):// or ws(s):// URL
   */
  constructor(config) {
    super();
    this.url = String(config.serverUrl).replace(/^http/, 'ws');
    this.socket = null;
  }

  /**
   * @returns {Promise<void>}
   */
  async connect() {
    await new Promise((resolve, reject) => {
      const socket = new WebSocket(this.url, { maxPayload: MAX_PAYLOAD });
      socket.once('open', resolve);
      socket.once('error', reject);
      this.socket = socket;
    });
    this.socket.on('message', raw => {
      try {
        this.emit('message', JSON.parse(raw.toString()));
      } catch (error) {
        this.emit('error', new Error(`Invalid frame from orchestrator: ${error.message}`));
      }
    });
    this.socket.on('error', error => this.emit('error', error));
  }

  /**
   * @param {object} message
   * @returns {Promise<void>}
   */
  async send(message) {
    if (!this.socket || this.socket.readyState !== WebSocket.OPEN) {
      throw new Error('Worker transport is not connected');
    }
    await new Promise((resolve, reject) =>
      this.socket.send(JSON.stringify(message), error => (error ? reject(error) : resolve()))
    );
  }

  /**
   * @returns {Promise<void>}
   */
  async disconnect() {
    if (!this.socket) return;
    const socket = this.socket;
    this.socket = null;
    if (socket.readyState === WebSocket.CLOSED) return;
    await new Promise(resolve => {
      socket.once('close', resolve);
      socket.close();
    });
  }
}
