/**
 * @fileoverview Metrics Collector - Real-time performance metrics collection
 * @module monitoring/metrics-collector
 *
 * @description
 * Collects real-time performance metrics for distributed consensus:
 * - CPU, memory, I/O per agent
 * - Observable throughput (items/sec)
 * - Compression ratio over time
 * - Receipt chain growth rate
 * - Latency distribution (P50, P95, P99)
 */

import { performance } from 'node:perf_hooks';
import { cpuUsage, memoryUsage } from 'node:process';

/**
 * Metrics snapshot at a point in time
 * @typedef {object} MetricsSnapshot
 * @property {number} timestamp - Unix timestamp (ms)
 * @property {object} cpu - CPU metrics
 * @property {object} memory - Memory metrics
 * @property {object} throughput - Throughput metrics
 * @property {object} latency - Latency distribution
 * @property {object} compression - Compression metrics
 * @property {object} receiptChain - Receipt chain metrics
 */

/**
 * Agent-specific metrics
 * @typedef {object} AgentMetrics
 * @property {string} agentId - Agent identifier (α₁, α₂, etc.)
 * @property {number} cpuTime - CPU time used (μs)
 * @property {number} memoryUsed - Memory used (bytes)
 * @property {number} tasksCompleted - Tasks completed
 * @property {number} tasksQueued - Tasks in queue
 * @property {number} avgLatency - Average task latency (ms)
 */

/**
 * Real-time metrics collector with circular buffer
 */
export class MetricsCollector {
  /**
   * Create a new metrics collector
   * @param {object} options - Configuration options
   * @param {number} options.sampleInterval - Sample interval in ms (default: 1000)
   * @param {number} options.bufferSize - Max samples to keep (default: 3600)
   * @param {boolean} options.enableAgentMetrics - Track per-agent metrics (default: true)
   */
  constructor(options = {}) {
    this.sampleInterval = options.sampleInterval || 1000;
    this.bufferSize = options.bufferSize || 3600; // 1 hour at 1s intervals
    this.enableAgentMetrics = options.enableAgentMetrics ?? true;

    /** @type {MetricsSnapshot[]} */
    this.snapshots = [];

    /** @type {Map<string, AgentMetrics>} */
    this.agentMetrics = new Map();

    /** @type {number|null} */
    this.intervalId = null;

    /** @type {number} */
    this.startTime = Date.now();

    /** @type {object} */
    this.lastCpuUsage = cpuUsage();

    /** @type {number} */
    this.totalOperations = 0;

    /** @type {number[]} */
    this.latencyBuffer = [];

    /** @type {number} */
    this.lastReceiptCount = 0;

    /** @type {number} */
    this.compressionRatioSum = 0;

    /** @type {number} */
    this.compressionSamples = 0;
  }

  /**
   * Start collecting metrics
   */
  start() {
    if (this.intervalId !== null) {
      return; // Already running
    }

    this.startTime = Date.now();
    this.intervalId = setInterval(() => {
      this.collectSnapshot();
    }, this.sampleInterval);

    console.log(`[MetricsCollector] Started (interval: ${this.sampleInterval}ms)`);
  }

  /**
   * Stop collecting metrics
   */
  stop() {
    if (this.intervalId !== null) {
      clearInterval(this.intervalId);
      this.intervalId = null;
      console.log('[MetricsCollector] Stopped');
    }
  }

  /**
   * Collect a metrics snapshot
   */
  collectSnapshot() {
    const now = Date.now();
    const currentCpu = cpuUsage(this.lastCpuUsage);
    const currentMemory = memoryUsage();

    // Calculate throughput (operations per second)
    const lastSnapshot = this.snapshots[this.snapshots.length - 1];
    const timeDelta = lastSnapshot ? (now - lastSnapshot.timestamp) / 1000 : 1;
    const opsDelta = lastSnapshot ? this.totalOperations - (lastSnapshot.throughput?.total || 0) : 0;
    const throughput = opsDelta / timeDelta;

    // Calculate latency percentiles
    const latencyStats = this.calculateLatencyPercentiles();

    // Calculate compression ratio
    const avgCompression = this.compressionSamples > 0
      ? this.compressionRatioSum / this.compressionSamples
      : 1.0;

    // Calculate receipt chain growth rate
    const receiptGrowthRate = lastSnapshot
      ? (this.lastReceiptCount - (lastSnapshot.receiptChain?.count || 0)) / timeDelta
      : 0;

    const snapshot = {
      timestamp: now,
      cpu: {
        user: currentCpu.user,
        system: currentCpu.system,
        total: currentCpu.user + currentCpu.system,
        percentageUser: (currentCpu.user / 1000) / this.sampleInterval * 100,
        percentageSystem: (currentCpu.system / 1000) / this.sampleInterval * 100,
      },
      memory: {
        rss: currentMemory.rss,
        heapTotal: currentMemory.heapTotal,
        heapUsed: currentMemory.heapUsed,
        external: currentMemory.external,
        arrayBuffers: currentMemory.arrayBuffers || 0,
      },
      throughput: {
        total: this.totalOperations,
        opsPerSec: throughput,
        delta: opsDelta,
      },
      latency: latencyStats,
      compression: {
        ratio: avgCompression,
        samples: this.compressionSamples,
      },
      receiptChain: {
        count: this.lastReceiptCount,
        growthRate: receiptGrowthRate,
      },
    };

    // Add to circular buffer
    this.snapshots.push(snapshot);
    if (this.snapshots.length > this.bufferSize) {
      this.snapshots.shift();
    }

    // Update CPU baseline
    this.lastCpuUsage = cpuUsage();

    return snapshot;
  }

  /**
   * Calculate latency percentiles from buffer
   * @returns {object} Latency statistics
   */
  calculateLatencyPercentiles() {
    if (this.latencyBuffer.length === 0) {
      return {
        p50: 0,
        p75: 0,
        p90: 0,
        p95: 0,
        p99: 0,
        p999: 0,
        mean: 0,
        min: 0,
        max: 0,
        count: 0,
      };
    }

    const sorted = [...this.latencyBuffer].sort((a, b) => a - b);
    const sum = sorted.reduce((acc, val) => acc + val, 0);

    return {
      p50: this.percentile(sorted, 50),
      p75: this.percentile(sorted, 75),
      p90: this.percentile(sorted, 90),
      p95: this.percentile(sorted, 95),
      p99: this.percentile(sorted, 99),
      p999: this.percentile(sorted, 99.9),
      mean: sum / sorted.length,
      min: sorted[0],
      max: sorted[sorted.length - 1],
      count: sorted.length,
    };
  }

  /**
   * Calculate percentile from sorted array
   * @param {number[]} sorted - Sorted array
   * @param {number} p - Percentile (0-100)
   * @returns {number}
   */
  percentile(sorted, p) {
    if (sorted.length === 0) return 0;
    const index = (p / 100) * (sorted.length - 1);
    const lower = Math.floor(index);
    const upper = Math.ceil(index);
    const weight = index - lower;
    return sorted[lower] * (1 - weight) + sorted[upper] * weight;
  }

  /**
   * Record an operation completion
   * @param {number} latency - Operation latency in ms
   */
  recordOperation(latency) {
    this.totalOperations++;
    this.latencyBuffer.push(latency);

    // Keep buffer size manageable
    if (this.latencyBuffer.length > 10000) {
      this.latencyBuffer.shift();
    }
  }

  /**
   * Record compression ratio
   * @param {number} ratio - Compression ratio (original/compressed)
   */
  recordCompression(ratio) {
    this.compressionRatioSum += ratio;
    this.compressionSamples++;
  }

  /**
   * Update receipt chain count
   * @param {number} count - Current receipt count
   */
  updateReceiptCount(count) {
    this.lastReceiptCount = count;
  }

  /**
   * Update agent metrics
   * @param {string} agentId - Agent identifier
   * @param {Partial<AgentMetrics>} metrics - Metrics to update
   */
  updateAgentMetrics(agentId, metrics) {
    if (!this.enableAgentMetrics) return;

    const current = this.agentMetrics.get(agentId) || {
      agentId,
      cpuTime: 0,
      memoryUsed: 0,
      tasksCompleted: 0,
      tasksQueued: 0,
      avgLatency: 0,
    };

    this.agentMetrics.set(agentId, {
      ...current,
      ...metrics,
    });
  }

  /**
   * Get current metrics snapshot
   * @returns {MetricsSnapshot|null}
   */
  getCurrentSnapshot() {
    return this.snapshots[this.snapshots.length - 1] || null;
  }

  /**
   * Get all snapshots
   * @returns {MetricsSnapshot[]}
   */
  getAllSnapshots() {
    return [...this.snapshots];
  }

  /**
   * Get snapshots in time range
   * @param {number} startTime - Start timestamp (ms)
   * @param {number} endTime - End timestamp (ms)
   * @returns {MetricsSnapshot[]}
   */
  getSnapshotsInRange(startTime, endTime) {
    return this.snapshots.filter(s => s.timestamp >= startTime && s.timestamp <= endTime);
  }

  /**
   * Get agent metrics
   * @param {string} agentId - Agent identifier
   * @returns {AgentMetrics|undefined}
   */
  getAgentMetrics(agentId) {
    return this.agentMetrics.get(agentId);
  }

  /**
   * Get all agent metrics
   * @returns {Map<string, AgentMetrics>}
   */
  getAllAgentMetrics() {
    return new Map(this.agentMetrics);
  }

  /**
   * Get uptime in seconds
   * @returns {number}
   */
  getUptime() {
    return (Date.now() - this.startTime) / 1000;
  }

  /**
   * Calculate average throughput over time window
   * @param {number} windowMs - Time window in ms
   * @returns {number} Average ops/sec
   */
  getAverageThroughput(windowMs = 60000) {
    const now = Date.now();
    const startTime = now - windowMs;
    const snapshots = this.getSnapshotsInRange(startTime, now);

    if (snapshots.length < 2) return 0;

    const first = snapshots[0];
    const last = snapshots[snapshots.length - 1];
    const timeDelta = (last.timestamp - first.timestamp) / 1000;
    const opsDelta = last.throughput.total - first.throughput.total;

    return opsDelta / timeDelta;
  }

  /**
   * Calculate memory growth rate
   * @param {number} windowMs - Time window in ms
   * @returns {number} Growth rate in bytes/sec
   */
  getMemoryGrowthRate(windowMs = 60000) {
    const now = Date.now();
    const startTime = now - windowMs;
    const snapshots = this.getSnapshotsInRange(startTime, now);

    if (snapshots.length < 2) return 0;

    const first = snapshots[0];
    const last = snapshots[snapshots.length - 1];
    const timeDelta = (last.timestamp - first.timestamp) / 1000;
    const memDelta = last.memory.heapUsed - first.memory.heapUsed;

    return memDelta / timeDelta;
  }

  /**
   * Reset all metrics
   */
  reset() {
    this.snapshots = [];
    this.agentMetrics.clear();
    this.totalOperations = 0;
    this.latencyBuffer = [];
    this.lastReceiptCount = 0;
    this.compressionRatioSum = 0;
    this.compressionSamples = 0;
    this.startTime = Date.now();
    this.lastCpuUsage = cpuUsage();
  }

  /**
   * Export metrics to JSON
   * @returns {object}
   */
  toJSON() {
    return {
      startTime: this.startTime,
      uptime: this.getUptime(),
      snapshots: this.snapshots,
      agentMetrics: Array.from(this.agentMetrics.entries()).map(([id, metrics]) => ({
        agentId: id,
        ...metrics,
      })),
      summary: {
        totalOperations: this.totalOperations,
        avgThroughput: this.getAverageThroughput(),
        memoryGrowthRate: this.getMemoryGrowthRate(),
        currentSnapshot: this.getCurrentSnapshot(),
      },
    };
  }
}
