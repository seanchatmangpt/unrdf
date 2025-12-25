/**
 * @file Visualizer
 * @description Graph visualization and dashboard
 * @module graph-analytics/visualizer
 */

import { WebSocketServer } from 'ws';
import { trace } from '@opentelemetry/api';

const tracer = trace.getTracer('visualizer');

/**
 * Graph Visualizer
 *
 * Features:
 * - Real-time graph visualization
 * - Interactive dashboard
 * - Metric charts
 * - Alert notifications
 *
 * @class
 */
export class Visualizer {
  /**
   * @param {object} config - Configuration
   */
  constructor(config = {}) {
    this.config = {
      port: 4000,
      ...config,
    };

    this.wss = null;
    this.clients = new Set();
    this.analyticsEngine = config.analyticsEngine;
  }

  /**
   * Start visualizer server
   *
   * @returns {Promise<void>}
   */
  async start() {
    return tracer.startActiveSpan('visualizer.start', async (span) => {
      try {
        this.wss = new WebSocketServer({ port: this.config.port });

        this.wss.on('connection', (ws) => {
          this.clients.add(ws);

          // Send current state
          ws.send(
            JSON.stringify({
              type: 'initial_state',
              metrics: this.analyticsEngine?.getMetrics(),
            })
          );

          ws.on('close', () => {
            this.clients.delete(ws);
          });
        });

        // Subscribe to analytics events
        if (this.analyticsEngine) {
          this.analyticsEngine.subscribe((event) => {
            this._broadcast(event);
          });
        }

        console.log(`Visualizer started on ws://localhost:${this.config.port}`);
        span.setStatus({ code: 1 });
      } catch (error) {
        span.setStatus({ code: 2, message: error.message });
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Broadcast event to all clients
   *
   * @private
   * @param {object} event - Event to broadcast
   * @returns {void}
   */
  _broadcast(event) {
    const message = JSON.stringify(event);

    for (const client of this.clients) {
      if (client.readyState === 1) {
        client.send(message);
      }
    }
  }

  /**
   * Stop visualizer
   *
   * @returns {Promise<void>}
   */
  async stop() {
    for (const client of this.clients) {
      client.close();
    }

    if (this.wss) {
      await new Promise((resolve) => this.wss.close(resolve));
    }
  }
}

export default Visualizer;
