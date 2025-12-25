/**
 * @file Analytics Engine
 * @description Real-time graph analytics with pattern detection
 * @module graph-analytics/analytics-engine
 */

import { createStore } from '@unrdf/oxigraph';
import { createChangeFeed } from '@unrdf/streaming';
import { trace } from '@opentelemetry/api';

const tracer = trace.getTracer('graph-analytics');

/**
 * Graph Analytics Engine
 *
 * Features:
 * - Live graph metrics
 * - Pattern detection
 * - Anomaly alerts
 * - Performance tracking
 *
 * @class
 */
export class AnalyticsEngine {
  /**
   * @param {object} config - Configuration
   */
  constructor(config = {}) {
    this.config = {
      updateInterval: 1000,
      anomalyThreshold: 3, // Standard deviations
      ...config,
    };

    this.store = config.store || createStore();
    this.changeFeed = null;
    this.metrics = {
      triples: { total: 0, added: 0, removed: 0 },
      entities: { total: 0, unique: new Set() },
      predicates: { total: 0, distribution: new Map() },
      patterns: [],
      anomalies: [],
    };
    this.patterns = [];
    this.listeners = new Set();
  }

  /**
   * Start analytics engine
   *
   * @returns {Promise<void>}
   */
  async start() {
    return tracer.startActiveSpan('analytics.start', async (span) => {
      try {
        // Initialize change feed
        this.changeFeed = await createChangeFeed({
          store: this.store,
          batchSize: 100,
          throttleMs: this.config.updateInterval,
        });

        // Subscribe to changes
        this.changeFeed.subscribe((changes) => {
          this._processChanges(changes);
        });

        // Calculate initial metrics
        await this._calculateMetrics();

        // Start pattern detection
        this._startPatternDetection();

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
   * Process graph changes
   *
   * @private
   * @param {object[]} changes - Change events
   * @returns {void}
   */
  _processChanges(changes) {
    return tracer.startActiveSpan('analytics.processChanges', (span) => {
      try {
        span.setAttribute('changes.count', changes.length);

        for (const change of changes) {
          if (change.type === 'add') {
            this.metrics.triples.added++;
            this.metrics.triples.total++;

            // Track entity
            this.metrics.entities.unique.add(change.subject);
            this.metrics.entities.unique.add(change.object);

            // Track predicate
            const count = this.metrics.predicates.distribution.get(change.predicate) || 0;
            this.metrics.predicates.distribution.set(change.predicate, count + 1);
          } else if (change.type === 'remove') {
            this.metrics.triples.removed++;
            this.metrics.triples.total--;
          }
        }

        this.metrics.entities.total = this.metrics.entities.unique.size;
        this.metrics.predicates.total = this.metrics.predicates.distribution.size;

        // Detect anomalies
        this._detectAnomalies(changes);

        // Notify listeners
        this._notifyListeners({ type: 'metrics', metrics: this.metrics });

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
   * Calculate graph metrics
   *
   * @private
   * @returns {Promise<void>}
   */
  async _calculateMetrics() {
    return tracer.startActiveSpan('analytics.calculateMetrics', async (span) => {
      try {
        // Query for basic stats
        const query = `
          SELECT (COUNT(*) as ?count)
          WHERE { ?s ?p ?o }
        `;

        const results = await this.store.query(query);
        for await (const binding of results) {
          this.metrics.triples.total = parseInt(binding.get('count').value, 10);
        }

        span.setAttribute('metrics.triples', this.metrics.triples.total);
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
   * Start pattern detection
   *
   * @private
   * @returns {void}
   */
  _startPatternDetection() {
    setInterval(async () => {
      await this._detectPatterns();
    }, this.config.updateInterval * 10); // Run every 10 seconds
  }

  /**
   * Detect patterns in graph
   *
   * @private
   * @returns {Promise<void>}
   */
  async _detectPatterns() {
    return tracer.startActiveSpan('analytics.detectPatterns', async (span) => {
      try {
        const patterns = [];

        // Pattern 1: Hub nodes (high degree)
        const hubPattern = await this._detectHubs();
        if (hubPattern.length > 0) {
          patterns.push({ type: 'hub', nodes: hubPattern });
        }

        // Pattern 2: Chains (linear sequences)
        const chainPattern = await this._detectChains();
        if (chainPattern.length > 0) {
          patterns.push({ type: 'chain', sequences: chainPattern });
        }

        // Pattern 3: Cycles
        const cyclePattern = await this._detectCycles();
        if (cyclePattern.length > 0) {
          patterns.push({ type: 'cycle', cycles: cyclePattern });
        }

        this.metrics.patterns = patterns;
        this._notifyListeners({ type: 'patterns', patterns });

        span.setAttribute('patterns.detected', patterns.length);
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
   * Detect hub nodes
   *
   * @private
   * @returns {Promise<object[]>} Hub nodes
   */
  async _detectHubs() {
    const query = `
      SELECT ?node (COUNT(?edge) as ?degree)
      WHERE {
        { ?node ?p ?o } UNION { ?s ?p ?node }
        BIND(?p AS ?edge)
      }
      GROUP BY ?node
      HAVING (COUNT(?edge) > 10)
      ORDER BY DESC(?degree)
      LIMIT 10
    `;

    const hubs = [];
    try {
      const results = await this.store.query(query);
      for await (const binding of results) {
        hubs.push({
          node: binding.get('node').value,
          degree: parseInt(binding.get('degree').value, 10),
        });
      }
    } catch (error) {
      // Query might not be supported
    }

    return hubs;
  }

  /**
   * Detect chain patterns
   *
   * @private
   * @returns {Promise<object[]>} Chains
   */
  async _detectChains() {
    // Simplified - actual implementation would use graph traversal
    return [];
  }

  /**
   * Detect cycle patterns
   *
   * @private
   * @returns {Promise<object[]>} Cycles
   */
  async _detectCycles() {
    // Simplified - actual implementation would use cycle detection algorithm
    return [];
  }

  /**
   * Detect anomalies
   *
   * @private
   * @param {object[]} changes - Recent changes
   * @returns {void}
   */
  _detectAnomalies(changes) {
    // Calculate change rate
    const changeRate = changes.length;

    // Simple anomaly detection: rate > threshold
    if (changeRate > 100) {
      const anomaly = {
        type: 'high_change_rate',
        rate: changeRate,
        timestamp: Date.now(),
        severity: 'warning',
      };

      this.metrics.anomalies.push(anomaly);

      // Keep only recent anomalies
      if (this.metrics.anomalies.length > 100) {
        this.metrics.anomalies = this.metrics.anomalies.slice(-100);
      }

      this._notifyListeners({ type: 'anomaly', anomaly });
    }
  }

  /**
   * Subscribe to analytics events
   *
   * @param {function} listener - Event listener
   * @returns {void}
   */
  subscribe(listener) {
    this.listeners.add(listener);
  }

  /**
   * Unsubscribe from analytics events
   *
   * @param {function} listener - Event listener
   * @returns {void}
   */
  unsubscribe(listener) {
    this.listeners.delete(listener);
  }

  /**
   * Notify listeners
   *
   * @private
   * @param {object} event - Event data
   * @returns {void}
   */
  _notifyListeners(event) {
    for (const listener of this.listeners) {
      try {
        listener(event);
      } catch (error) {
        console.error('Listener error:', error);
      }
    }
  }

  /**
   * Get current metrics
   *
   * @returns {object} Metrics
   */
  getMetrics() {
    return {
      ...this.metrics,
      entities: {
        ...this.metrics.entities,
        unique: Array.from(this.metrics.entities.unique),
      },
      predicates: {
        ...this.metrics.predicates,
        distribution: Object.fromEntries(this.metrics.predicates.distribution),
      },
    };
  }

  /**
   * Stop analytics engine
   *
   * @returns {Promise<void>}
   */
  async stop() {
    if (this.changeFeed) {
      this.changeFeed.unsubscribe();
    }
    this.listeners.clear();
  }
}

export default AnalyticsEngine;
