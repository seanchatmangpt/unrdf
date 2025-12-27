/**
 * @fileoverview Memory Profiler with Trend Analysis
 * @module profiling/memory-profiler
 */

/**
 * @typedef {Object} MemorySession
 * @property {string} id - Session ID
 * @property {string} operationName - Operation name
 * @property {number} startTime - Start timestamp
 * @property {Object} startMemory - Initial memory snapshot
 * @property {Object[]} snapshots - Memory snapshots
 */

/**
 * @typedef {Object} MemoryMetrics
 * @property {number} heapUsedDelta - Change in heap used (bytes)
 * @property {number} heapUsedPeak - Peak heap usage during operation
 * @property {number} heapTotalDelta - Change in total heap
 * @property {number} externalDelta - Change in external memory
 * @property {number} arrayBuffersDelta - Change in array buffers
 * @property {number} rss - Resident set size
 * @property {Object} trend - Memory trend analysis
 * @property {string} trend.direction - 'stable', 'growing', 'shrinking'
 * @property {number} trend.growthRate - Bytes per second
 * @property {boolean} leakDetected - Potential memory leak detected
 * @property {Object[]} snapshots - All memory snapshots
 */

/**
 * Memory Profiler with leak detection
 */
export class MemoryProfiler {
  /**
   *
   */
  constructor() {
    this.activeSessions = new Map();
    this.snapshotInterval = 100; // ms between snapshots
    this.leakThreshold = 1024 * 1024; // 1 MB/sec growth = potential leak
  }

  /**
   * Start memory profiling session
   * @param {string} operationName - Operation name
   * @returns {string} Session ID
   */
  start(operationName) {
    const sessionId = `memory-${Date.now()}-${Math.random().toString(36).slice(2, 9)}`;
    const startTime = Date.now();
    const startMemory = this.captureMemorySnapshot();

    const session = {
      id: sessionId,
      operationName,
      startTime,
      startMemory,
      snapshots: [startMemory],
      peakHeapUsed: startMemory.heapUsed,
      intervalId: null,
    };

    // Start periodic snapshots
    session.intervalId = setInterval(() => {
      if (this.activeSessions.has(sessionId)) {
        const snapshot = this.captureMemorySnapshot();
        session.snapshots.push(snapshot);
        session.peakHeapUsed = Math.max(session.peakHeapUsed, snapshot.heapUsed);
      }
    }, this.snapshotInterval);

    this.activeSessions.set(sessionId, session);
    return sessionId;
  }

  /**
   * Capture current memory snapshot
   * @private
   * @returns {Object} Memory snapshot
   */
  captureMemorySnapshot() {
    const mem = process.memoryUsage();
    return {
      timestamp: Date.now(),
      heapUsed: mem.heapUsed,
      heapTotal: mem.heapTotal,
      external: mem.external,
      arrayBuffers: mem.arrayBuffers,
      rss: mem.rss,
    };
  }

  /**
   * Stop profiling and return metrics
   * @param {string} sessionId - Session ID
   * @returns {MemoryMetrics}
   */
  stop(sessionId) {
    const session = this.activeSessions.get(sessionId);
    if (!session) {
      throw new Error(`Session not found: ${sessionId}`);
    }

    // Stop periodic snapshots
    if (session.intervalId) {
      clearInterval(session.intervalId);
    }

    // Capture final snapshot
    const endMemory = this.captureMemorySnapshot();
    session.snapshots.push(endMemory);

    // Calculate metrics
    const metrics = this.calculateMetrics(session, endMemory);

    // Cleanup
    this.activeSessions.delete(sessionId);

    return metrics;
  }

  /**
   * Calculate memory metrics
   * @private
   * @param {MemorySession} session - Session data
   * @param {Object} endMemory - Final memory snapshot
   * @returns {MemoryMetrics}
   */
  calculateMetrics(session, endMemory) {
    const startMemory = session.startMemory;
    const duration = (endMemory.timestamp - startMemory.timestamp) / 1000; // seconds

    const heapUsedDelta = endMemory.heapUsed - startMemory.heapUsed;
    const heapTotalDelta = endMemory.heapTotal - startMemory.heapTotal;
    const externalDelta = endMemory.external - startMemory.external;
    const arrayBuffersDelta = endMemory.arrayBuffers - startMemory.arrayBuffers;

    // Analyze trend
    const trend = this.analyzeTrend(session.snapshots);

    // Detect potential memory leaks
    const leakDetected = trend.growthRate > this.leakThreshold;

    return {
      heapUsedDelta,
      heapUsedPeak: session.peakHeapUsed,
      heapTotalDelta,
      externalDelta,
      arrayBuffersDelta,
      rss: endMemory.rss,
      trend,
      leakDetected,
      snapshots: session.snapshots,
      duration,
      operationName: session.operationName,
    };
  }

  /**
   * Analyze memory trend
   * @private
   * @param {Object[]} snapshots - Memory snapshots
   * @returns {Object} Trend analysis
   */
  analyzeTrend(snapshots) {
    if (snapshots.length < 2) {
      return {
        direction: 'stable',
        growthRate: 0,
        confidence: 0,
      };
    }

    // Calculate linear regression for heap usage over time
    const points = snapshots.map(s => ({
      x: s.timestamp - snapshots[0].timestamp,
      y: s.heapUsed,
    }));

    const { slope, rSquared } = this.linearRegression(points);

    // Convert slope to bytes per second
    const growthRate = slope; // slope is already in bytes/ms, * 1000 for bytes/sec

    let direction;
    if (Math.abs(growthRate) < 1024) {
      // < 1 KB/sec
      direction = 'stable';
    } else if (growthRate > 0) {
      direction = 'growing';
    } else {
      direction = 'shrinking';
    }

    return {
      direction,
      growthRate: growthRate * 1000, // Convert to bytes/sec
      confidence: rSquared,
      sampleCount: snapshots.length,
    };
  }

  /**
   * Calculate linear regression
   * @private
   * @param {Object[]} points - Array of {x, y} points
   * @returns {Object} Slope and R² value
   */
  linearRegression(points) {
    const n = points.length;
    if (n === 0) return { slope: 0, rSquared: 0 };

    const sumX = points.reduce((sum, p) => sum + p.x, 0);
    const sumY = points.reduce((sum, p) => sum + p.y, 0);
    const sumXY = points.reduce((sum, p) => sum + p.x * p.y, 0);
    const sumXX = points.reduce((sum, p) => sum + p.x * p.x, 0);
    const _sumYY = points.reduce((sum, p) => sum + p.y * p.y, 0);

    const slope = (n * sumXY - sumX * sumY) / (n * sumXX - sumX * sumX);
    const intercept = (sumY - slope * sumX) / n;

    // Calculate R²
    const meanY = sumY / n;
    const ssTotal = points.reduce((sum, p) => sum + Math.pow(p.y - meanY, 2), 0);
    const ssResidual = points.reduce((sum, p) => {
      const predicted = slope * p.x + intercept;
      return sum + Math.pow(p.y - predicted, 2);
    }, 0);

    const rSquared = ssTotal > 0 ? 1 - ssResidual / ssTotal : 0;

    return { slope, intercept, rSquared };
  }

  /**
   * Force garbage collection if available
   * @returns {boolean} True if GC was triggered
   */
  forceGC() {
    if (global.gc) {
      global.gc();
      return true;
    }
    return false;
  }

  /**
   * Get current memory usage
   * @returns {Object} Current memory stats
   */
  getCurrentUsage() {
    return this.captureMemorySnapshot();
  }

  /**
   * Profile a function's memory usage
   * @param {string} operationName - Operation name
   * @param {Function} fn - Function to profile
   * @returns {Promise<Object>} Result and metrics
   */
  async profile(operationName, fn) {
    // Force GC before profiling if available
    this.forceGC();
    await this.sleep(50); // Let GC settle

    const sessionId = this.start(operationName);

    let result;
    let error;

    try {
      result = await fn();
    } catch (err) {
      error = err;
    }

    // Force GC after profiling
    this.forceGC();
    await this.sleep(50);

    const metrics = this.stop(sessionId);

    if (error) {
      throw error;
    }

    return { result, metrics };
  }

  /**
   * Sleep helper
   * @private
   * @param {number} ms - Milliseconds to sleep
   * @returns {Promise<void>}
   */
  sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
  }

  /**
   * Check if memory usage exceeds budget
   * @param {MemoryMetrics} metrics - Memory metrics
   * @param {Object} budget - Memory budget
   * @param {number} [budget.maxHeapDelta] - Max heap delta in bytes
   * @param {number} [budget.maxGrowthRate] - Max growth rate in bytes/sec
   * @returns {Object} Budget check results
   */
  checkBudget(metrics, budget) {
    const violations = [];

    if (
      budget.maxHeapDelta !== undefined &&
      Math.abs(metrics.heapUsedDelta) > budget.maxHeapDelta
    ) {
      violations.push({
        metric: 'heapUsedDelta',
        actual: metrics.heapUsedDelta,
        budget: budget.maxHeapDelta,
        exceeded: Math.abs(metrics.heapUsedDelta) - budget.maxHeapDelta,
      });
    }

    if (budget.maxGrowthRate !== undefined && metrics.trend.growthRate > budget.maxGrowthRate) {
      violations.push({
        metric: 'growthRate',
        actual: metrics.trend.growthRate,
        budget: budget.maxGrowthRate,
        exceeded: metrics.trend.growthRate - budget.maxGrowthRate,
      });
    }

    if (metrics.leakDetected) {
      violations.push({
        metric: 'leakDetection',
        actual: metrics.trend.growthRate,
        budget: this.leakThreshold,
        exceeded: metrics.trend.growthRate - this.leakThreshold,
        message: 'Potential memory leak detected',
      });
    }

    return {
      passed: violations.length === 0,
      violations,
    };
  }
}

/**
 * Quick memory measurement
 * @param {string} operationName - Operation name
 * @param {Function} fn - Function to measure
 * @returns {Promise<Object>} Result and memory metrics
 */
export async function measureMemory(operationName, fn) {
  const profiler = new MemoryProfiler();
  return await profiler.profile(operationName, fn);
}
