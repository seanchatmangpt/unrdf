/**
 * @file Performance Monitor - Real-time streaming performance monitoring
 * @module streaming/performance-monitor
 *
 * @description
 * Monitors throughput, latency, memory usage, and backpressure for streaming operations.
 * Provides real-time metrics collection and performance benchmarking.
 */

import { EventEmitter } from 'events';
import { trace } from '@opentelemetry/api';
import { z } from 'zod';

const tracer = trace.getTracer('@unrdf/streaming');

/**
 * Performance monitor configuration schema
 */
const PerformanceMonitorConfigSchema = z.object({
  sampleInterval: z.number().positive().default(1000), // 1 second
  windowSize: z.number().positive().default(60), // 60 samples = 1 minute
  enableMemoryTracking: z.boolean().default(true),
  enableLatencyTracking: z.boolean().default(true),
  enableThroughputTracking: z.boolean().default(true),
  thresholds: z.object({
    throughputMin: z.number().optional(),
    latencyMax: z.number().optional(),
    memoryMax: z.number().optional(),
  }).optional(),
});

/**
 * Performance Monitor for streaming operations
 *
 * @class PerformanceMonitor
 * @extends EventEmitter
 */
export class PerformanceMonitor extends EventEmitter {
  /**
   * Create a performance monitor
   *
   * @param {Object} [config] - Monitor configuration
   */
  constructor(config = {}) {
    super();
    this.config = PerformanceMonitorConfigSchema.parse(config);

    // Metrics storage
    this.metrics = {
      throughput: [],
      latency: [],
      memory: [],
      backpressure: [],
      errors: [],
    };

    // Current state
    this.state = {
      quadsProcessed: 0,
      bytesProcessed: 0,
      chunksProcessed: 0,
      errorsCount: 0,
      backpressureEvents: 0,
      startTime: null,
      lastSample: null,
    };

    // Monitoring interval
    this.monitoringInterval = null;
    this.isMonitoring = false;
  }

  /**
   * Start monitoring
   */
  start() {
    if (this.isMonitoring) return;

    this.isMonitoring = true;
    this.state.startTime = Date.now();
    this.state.lastSample = Date.now();

    this.monitoringInterval = setInterval(() => {
      this._collectSample();
    }, this.config.sampleInterval);

    this.emit('started');
  }

  /**
   * Stop monitoring
   */
  stop() {
    if (!this.isMonitoring) return;

    this.isMonitoring = false;

    if (this.monitoringInterval) {
      clearInterval(this.monitoringInterval);
      this.monitoringInterval = null;
    }

    // Collect final sample
    this._collectSample();

    this.emit('stopped', this.getReport());
  }

  /**
   * Record a quad processed
   *
   * @param {number} [latency] - Processing latency in ms
   */
  recordQuad(latency) {
    this.state.quadsProcessed++;

    if (latency !== undefined && this.config.enableLatencyTracking) {
      // Store latency for current window
      const now = Date.now();
      this.metrics.latency.push({ timestamp: now, value: latency });
      this._trimWindow(this.metrics.latency);
    }
  }

  /**
   * Record bytes processed
   *
   * @param {number} bytes - Number of bytes
   */
  recordBytes(bytes) {
    this.state.bytesProcessed += bytes;
  }

  /**
   * Record a chunk processed
   */
  recordChunk() {
    this.state.chunksProcessed++;
  }

  /**
   * Record an error
   *
   * @param {Error} error - Error object
   */
  recordError(error) {
    this.state.errorsCount++;
    this.metrics.errors.push({
      timestamp: Date.now(),
      message: error.message,
      stack: error.stack,
    });
    this._trimWindow(this.metrics.errors);

    this.emit('error-recorded', error);
  }

  /**
   * Record a backpressure event
   */
  recordBackpressure() {
    this.state.backpressureEvents++;
    this.metrics.backpressure.push({
      timestamp: Date.now(),
      value: 1,
    });
    this._trimWindow(this.metrics.backpressure);

    this.emit('backpressure');
  }

  /**
   * Collect a performance sample
   *
   * @private
   */
  _collectSample() {
    return tracer.startActiveSpan('performance-monitor.collect-sample', (span) => {
      const now = Date.now();
      const elapsed = now - this.state.lastSample;

      // Calculate throughput (quads/sec)
      if (this.config.enableThroughputTracking) {
        const throughput = (this.state.quadsProcessed / elapsed) * 1000;
        this.metrics.throughput.push({
          timestamp: now,
          value: throughput,
          quads: this.state.quadsProcessed,
          bytes: this.state.bytesProcessed,
        });
        this._trimWindow(this.metrics.throughput);

        // Check threshold
        if (this.config.thresholds?.throughputMin && throughput < this.config.thresholds.throughputMin) {
          this.emit('threshold-violation', {
            metric: 'throughput',
            value: throughput,
            threshold: this.config.thresholds.throughputMin,
          });
        }
      }

      // Collect memory usage
      if (this.config.enableMemoryTracking) {
        const memUsage = process.memoryUsage();
        this.metrics.memory.push({
          timestamp: now,
          rss: memUsage.rss,
          heapUsed: memUsage.heapUsed,
          heapTotal: memUsage.heapTotal,
          external: memUsage.external,
        });
        this._trimWindow(this.metrics.memory);

        // Check threshold
        if (this.config.thresholds?.memoryMax && memUsage.heapUsed > this.config.thresholds.memoryMax) {
          this.emit('threshold-violation', {
            metric: 'memory',
            value: memUsage.heapUsed,
            threshold: this.config.thresholds.memoryMax,
          });
        }
      }

      span.setAttributes({
        'sample.throughput': this.metrics.throughput[this.metrics.throughput.length - 1]?.value || 0,
        'sample.memory': this.metrics.memory[this.metrics.memory.length - 1]?.heapUsed || 0,
      });

      this.state.lastSample = now;
      span.end();
    });
  }

  /**
   * Trim metrics window to configured size
   *
   * @param {Array} metricsArray - Metrics array to trim
   * @private
   */
  _trimWindow(metricsArray) {
    while (metricsArray.length > this.config.windowSize) {
      metricsArray.shift();
    }
  }

  /**
   * Get current metrics snapshot
   *
   * @returns {Object} Current metrics
   */
  getCurrentMetrics() {
    const totalDuration = Date.now() - (this.state.startTime || Date.now());

    return {
      quadsProcessed: this.state.quadsProcessed,
      bytesProcessed: this.state.bytesProcessed,
      chunksProcessed: this.state.chunksProcessed,
      errorsCount: this.state.errorsCount,
      backpressureEvents: this.state.backpressureEvents,
      duration: totalDuration,
      averageThroughput: totalDuration > 0 ? (this.state.quadsProcessed / totalDuration) * 1000 : 0,
    };
  }

  /**
   * Get performance report
   *
   * @returns {Object} Performance report
   */
  getReport() {
    const current = this.getCurrentMetrics();

    // Calculate statistics
    const throughputStats = this._calculateStats(this.metrics.throughput.map(m => m.value));
    const latencyStats = this._calculateStats(this.metrics.latency.map(m => m.value));
    const memoryStats = this._calculateMemoryStats();

    return {
      summary: current,
      throughput: {
        ...throughputStats,
        unit: 'quads/sec',
      },
      latency: {
        ...latencyStats,
        unit: 'ms',
      },
      memory: memoryStats,
      backpressure: {
        events: this.state.backpressureEvents,
        rate: this.state.chunksProcessed > 0
          ? this.state.backpressureEvents / this.state.chunksProcessed
          : 0,
      },
      errors: {
        count: this.state.errorsCount,
        recent: this.metrics.errors.slice(-10),
      },
    };
  }

  /**
   * Calculate statistics for a metric
   *
   * @param {Array<number>} values - Metric values
   * @returns {Object} Statistics
   * @private
   */
  _calculateStats(values) {
    if (values.length === 0) {
      return { mean: 0, min: 0, max: 0, p50: 0, p95: 0, p99: 0 };
    }

    const sorted = [...values].sort((a, b) => a - b);
    const sum = sorted.reduce((acc, val) => acc + val, 0);

    return {
      mean: sum / sorted.length,
      min: sorted[0],
      max: sorted[sorted.length - 1],
      p50: sorted[Math.floor(sorted.length * 0.5)],
      p95: sorted[Math.floor(sorted.length * 0.95)],
      p99: sorted[Math.floor(sorted.length * 0.99)],
    };
  }

  /**
   * Calculate memory statistics
   *
   * @returns {Object} Memory statistics
   * @private
   */
  _calculateMemoryStats() {
    if (this.metrics.memory.length === 0) {
      return { rss: {}, heapUsed: {}, heapTotal: {} };
    }

    const rssValues = this.metrics.memory.map(m => m.rss);
    const heapUsedValues = this.metrics.memory.map(m => m.heapUsed);
    const heapTotalValues = this.metrics.memory.map(m => m.heapTotal);

    return {
      rss: this._calculateStats(rssValues),
      heapUsed: this._calculateStats(heapUsedValues),
      heapTotal: this._calculateStats(heapTotalValues),
      unit: 'bytes',
    };
  }

  /**
   * Reset all metrics
   */
  reset() {
    this.metrics = {
      throughput: [],
      latency: [],
      memory: [],
      backpressure: [],
      errors: [],
    };

    this.state = {
      quadsProcessed: 0,
      bytesProcessed: 0,
      chunksProcessed: 0,
      errorsCount: 0,
      backpressureEvents: 0,
      startTime: null,
      lastSample: null,
    };

    this.emit('reset');
  }
}

/**
 * Create a performance monitor
 *
 * @param {Object} [config] - Monitor configuration
 * @returns {PerformanceMonitor} Performance monitor instance
 *
 * @example
 * const monitor = createPerformanceMonitor({
 *   sampleInterval: 1000,
 *   windowSize: 60,
 * });
 * monitor.start();
 * // ... perform streaming operations ...
 * monitor.stop();
 * console.log(monitor.getReport());
 */
export function createPerformanceMonitor(config = {}) {
  return new PerformanceMonitor(config);
}
