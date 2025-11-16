/**
 * @fileoverview Main Performance Profiler Interface
 * @module profiling/profiler
 */

import { trace, metrics, context } from '@opentelemetry/api';
import { LatencyProfiler } from './latency-profiler.mjs';
import { MemoryProfiler } from './memory-profiler.mjs';
import { CpuProfiler } from './cpu-profiler.mjs';

const tracer = trace.getTracer('unrdf-profiler', '3.1.0');
const meter = metrics.getMeter('unrdf-profiler', '3.1.0');

/**
 * @typedef {Object} ProfilerOptions
 * @property {boolean} [enableLatency=true] - Enable latency profiling
 * @property {boolean} [enableMemory=true] - Enable memory profiling
 * @property {boolean} [enableCpu=false] - Enable CPU profiling (Node.js only)
 * @property {boolean} [enableOtel=true] - Enable OTEL metrics export
 * @property {string[]} [labels=[]] - Additional labels for metrics
 */

/**
 * @typedef {Object} ProfileResult
 * @property {Object} latency - Latency metrics
 * @property {number} latency.duration - Total duration in ms
 * @property {number} latency.p50 - 50th percentile
 * @property {number} latency.p90 - 90th percentile
 * @property {number} latency.p95 - 95th percentile
 * @property {number} latency.p99 - 99th percentile
 * @property {number} latency.p999 - 99.9th percentile
 * @property {Object} memory - Memory metrics
 * @property {number} memory.heapUsedDelta - Heap memory delta in bytes
 * @property {number} memory.heapUsedPeak - Peak heap usage
 * @property {number} memory.externalDelta - External memory delta
 * @property {Object} [cpu] - CPU metrics (if enabled)
 * @property {Object} [cpu.hotFunctions] - Hot function analysis
 * @property {number} [cpu.totalTime] - Total CPU time
 * @property {Object} metadata - Profiling metadata
 * @property {string} metadata.operationName - Operation name
 * @property {number} metadata.timestamp - Start timestamp
 * @property {string[]} metadata.labels - Applied labels
 */

/**
 * Main Performance Profiler
 */
export class Profiler {
  /**
   * Create a new profiler instance
   * @param {ProfilerOptions} options - Profiler configuration
   */
  constructor(options = {}) {
    this.options = {
      enableLatency: true,
      enableMemory: true,
      enableCpu: false,
      enableOtel: true,
      labels: [],
      ...options
    };

    this.latencyProfiler = new LatencyProfiler();
    this.memoryProfiler = new MemoryProfiler();
    this.cpuProfiler = this.options.enableCpu ? new CpuProfiler() : null;

    this.activeProfiles = new Map();
    this.profileHistory = [];
    this.maxHistorySize = 1000;

    // OTEL metrics
    if (this.options.enableOtel) {
      this.setupOtelMetrics();
    }
  }

  /**
   * Setup OTEL metrics instruments
   * @private
   */
  setupOtelMetrics() {
    this.latencyHistogram = meter.createHistogram('profiler.operation.latency', {
      description: 'Operation latency distribution',
      unit: 'ms'
    });

    this.memoryGauge = meter.createObservableGauge('profiler.memory.heap_used', {
      description: 'Heap memory usage',
      unit: 'bytes'
    });

    this.operationCounter = meter.createCounter('profiler.operations.total', {
      description: 'Total profiled operations'
    });

    this.errorCounter = meter.createCounter('profiler.operations.errors', {
      description: 'Failed profiled operations'
    });
  }

  /**
   * Profile an async operation
   * @param {string} operationName - Name of the operation
   * @param {Function} operation - Async function to profile
   * @param {Object} [options={}] - Additional options
   * @returns {Promise<{result: any, profile: ProfileResult}>}
   */
  async profile(operationName, operation, options = {}) {
    const span = tracer.startSpan(`profile.${operationName}`, {
      attributes: {
        'profiler.operation': operationName,
        'profiler.latency_enabled': this.options.enableLatency,
        'profiler.memory_enabled': this.options.enableMemory,
        'profiler.cpu_enabled': this.options.enableCpu
      }
    });

    const profileId = `${operationName}-${Date.now()}-${Math.random().toString(36).slice(2, 9)}`;
    const startTime = Date.now();

    try {
      return await context.with(trace.setSpan(context.active(), span), async () => {
        // Start profilers
        const latencySession = this.options.enableLatency
          ? this.latencyProfiler.start(operationName)
          : null;

        const memorySession = this.options.enableMemory
          ? this.memoryProfiler.start(operationName)
          : null;

        const cpuSession = this.cpuProfiler
          ? await this.cpuProfiler.start(operationName)
          : null;

        this.activeProfiles.set(profileId, {
          operationName,
          startTime,
          latencySession,
          memorySession,
          cpuSession
        });

        let result;
        let error;

        try {
          // Execute the operation
          result = await operation();
          span.setStatus({ code: 1 }); // OK
        } catch (err) {
          error = err;
          span.setStatus({ code: 2, message: err.message }); // ERROR
          span.recordException(err);

          if (this.options.enableOtel) {
            this.errorCounter.add(1, {
              operation: operationName,
              error: err.name
            });
          }

          throw err;
        } finally {
          // Stop profilers and collect results
          const profile = await this.collectProfile(profileId, operationName);

          // Record OTEL metrics
          if (this.options.enableOtel && profile) {
            this.recordOtelMetrics(operationName, profile);
          }

          // Add to history
          if (profile) {
            this.addToHistory(profile);
          }

          // Cleanup
          this.activeProfiles.delete(profileId);
          span.end();

          if (!error) {
            return { result, profile };
          }
        }
      });
    } catch (err) {
      span.setStatus({ code: 2, message: err.message });
      span.end();
      throw err;
    }
  }

  /**
   * Collect profile results from all profilers
   * @private
   * @param {string} profileId - Profile session ID
   * @param {string} operationName - Operation name
   * @returns {Promise<ProfileResult>}
   */
  async collectProfile(profileId, operationName) {
    const session = this.activeProfiles.get(profileId);
    if (!session) {
      return null;
    }

    const profile = {
      metadata: {
        operationName,
        timestamp: session.startTime,
        labels: [...this.options.labels]
      }
    };

    // Collect latency metrics
    if (session.latencySession) {
      profile.latency = this.latencyProfiler.stop(session.latencySession);
    }

    // Collect memory metrics
    if (session.memorySession) {
      profile.memory = this.memoryProfiler.stop(session.memorySession);
    }

    // Collect CPU metrics
    if (session.cpuSession) {
      profile.cpu = await this.cpuProfiler.stop(session.cpuSession);
    }

    return profile;
  }

  /**
   * Record metrics to OTEL
   * @private
   * @param {string} operationName - Operation name
   * @param {ProfileResult} profile - Profile results
   */
  recordOtelMetrics(operationName, profile) {
    const labels = {
      operation: operationName,
      ...Object.fromEntries(this.options.labels.map((l, i) => [`label_${i}`, l]))
    };

    // Record latency histogram
    if (profile.latency) {
      this.latencyHistogram.record(profile.latency.duration, labels);
    }

    // Increment operation counter
    this.operationCounter.add(1, labels);
  }

  /**
   * Add profile to history
   * @private
   * @param {ProfileResult} profile - Profile result
   */
  addToHistory(profile) {
    this.profileHistory.push(profile);

    // Maintain max history size
    if (this.profileHistory.length > this.maxHistorySize) {
      this.profileHistory.shift();
    }
  }

  /**
   * Get profile history
   * @param {Object} [filters={}] - Filter criteria
   * @param {string} [filters.operationName] - Filter by operation name
   * @param {number} [filters.minDuration] - Minimum duration in ms
   * @param {number} [filters.limit=100] - Maximum results
   * @returns {ProfileResult[]}
   */
  getHistory(filters = {}) {
    let results = [...this.profileHistory];

    if (filters.operationName) {
      results = results.filter(p => p.metadata.operationName === filters.operationName);
    }

    if (filters.minDuration !== undefined) {
      results = results.filter(p => p.latency?.duration >= filters.minDuration);
    }

    const limit = filters.limit || 100;
    return results.slice(-limit);
  }

  /**
   * Get aggregate statistics for an operation
   * @param {string} operationName - Operation name
   * @returns {Object} Aggregate statistics
   */
  getStats(operationName) {
    const profiles = this.getHistory({ operationName, limit: 10000 });

    if (profiles.length === 0) {
      return null;
    }

    const latencies = profiles
      .filter(p => p.latency)
      .map(p => p.latency.duration);

    const memoryDeltas = profiles
      .filter(p => p.memory)
      .map(p => p.memory.heapUsedDelta);

    return {
      operationName,
      count: profiles.length,
      latency: latencies.length > 0 ? {
        min: Math.min(...latencies),
        max: Math.max(...latencies),
        mean: latencies.reduce((a, b) => a + b, 0) / latencies.length,
        p50: this.percentile(latencies, 50),
        p90: this.percentile(latencies, 90),
        p95: this.percentile(latencies, 95),
        p99: this.percentile(latencies, 99)
      } : null,
      memory: memoryDeltas.length > 0 ? {
        min: Math.min(...memoryDeltas),
        max: Math.max(...memoryDeltas),
        mean: memoryDeltas.reduce((a, b) => a + b, 0) / memoryDeltas.length
      } : null
    };
  }

  /**
   * Calculate percentile
   * @private
   * @param {number[]} values - Sorted values
   * @param {number} p - Percentile (0-100)
   * @returns {number}
   */
  percentile(values, p) {
    if (values.length === 0) return 0;
    const sorted = [...values].sort((a, b) => a - b);
    const index = Math.ceil((p / 100) * sorted.length) - 1;
    return sorted[Math.max(0, index)];
  }

  /**
   * Clear profile history
   */
  clearHistory() {
    this.profileHistory = [];
  }

  /**
   * Get current profiler status
   * @returns {Object} Status information
   */
  getStatus() {
    return {
      activeProfiles: this.activeProfiles.size,
      historySize: this.profileHistory.length,
      maxHistorySize: this.maxHistorySize,
      options: this.options
    };
  }
}

/**
 * Create a new profiler instance
 * @param {ProfilerOptions} options - Profiler options
 * @returns {Profiler}
 */
export function createProfiler(options = {}) {
  return new Profiler(options);
}

/**
 * Quick profile helper for one-off profiling
 * @param {string} operationName - Operation name
 * @param {Function} operation - Function to profile
 * @param {ProfilerOptions} [options={}] - Profiler options
 * @returns {Promise<{result: any, profile: ProfileResult}>}
 */
export async function quickProfile(operationName, operation, options = {}) {
  const profiler = new Profiler(options);
  return await profiler.profile(operationName, operation);
}
