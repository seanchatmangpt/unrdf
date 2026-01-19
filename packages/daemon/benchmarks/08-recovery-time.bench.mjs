/**
 * @file Recovery Time Benchmark
 * @module @unrdf/daemon/benchmarks/recovery-time
 * @description Measures MTTR (Mean Time To Recovery), failover speed, and state restoration performance
 * Target: Recovery detection latency <100ms
 */

import { Daemon } from '../src/daemon.mjs';
import { analyzeVariance, storeBenchmarkResult } from './suite.mjs';
import { randomUUID } from 'crypto';

/**
 * Generate a valid UUID for daemon ID
 * @returns {string} Valid UUID
 */
function generateDaemonId() {
  return randomUUID();
}

/**
 * Calculate percentile from sorted array
 * @param {Array<number>} values - Sorted array of values
 * @param {number} percentile - Percentile (0-100)
 * @returns {number} Percentile value
 */
function getPercentile(values, percentile) {
  if (values.length === 0) return 0;
  const sorted = [...values].sort((a, b) => a - b);
  const index = Math.ceil((percentile / 100) * sorted.length) - 1;
  return sorted[Math.max(0, index)];
}

/**
 * Get current memory usage in bytes
 * @returns {Object} Memory usage information
 */
function getMemorySnapshot() {
  const usage = process.memoryUsage();
  return {
    heapUsed: usage.heapUsed,
    heapTotal: usage.heapTotal,
    rss: usage.rss,
  };
}

/**
 * Recovery event types
 * @readonly
 * @enum {string}
 */
const RecoveryEvent = {
  FAILURE_DETECTED: 'failure_detected',
  RECOVERY_STARTED: 'recovery_started',
  STATE_RESTORED: 'state_restored',
  HEALTH_VERIFIED: 'health_verified',
  RECOVERY_COMPLETE: 'recovery_complete',
};

/**
 * Recovery Controller for managing daemon recovery scenarios
 */
class RecoveryController {
  /**
   * @param {Daemon} daemon - Primary daemon instance
   */
  constructor(daemon) {
    this.daemon = daemon;
    this.recoveryHistory = [];
    this.stateSnapshots = new Map();
    this.isRecovering = false;
  }

  /**
   * Create a state snapshot
   * @returns {Object} State snapshot with timing
   */
  createSnapshot() {
    const startTime = performance.now();

    const health = this.daemon.getHealth();
    const metrics = this.daemon.getMetrics();
    const operations = this.daemon.listOperations();

    const snapshot = {
      id: randomUUID(),
      timestamp: Date.now(),
      health,
      metrics,
      operationCount: operations.length,
      operations: operations.slice(0, 100), // Limit for memory
    };

    const snapshotTime = performance.now() - startTime;
    this.stateSnapshots.set(snapshot.id, snapshot);

    return {
      snapshotId: snapshot.id,
      snapshotTimeMs: snapshotTime,
      snapshot,
    };
  }

  /**
   * Simulate failure and measure detection time
   * @returns {Promise<Object>} Detection timing
   */
  async simulateFailure() {
    const startTime = performance.now();

    // Stop the daemon to simulate failure
    await this.daemon.stop();

    const failureTime = performance.now() - startTime;

    return {
      failureTimeMs: failureTime,
      timestamp: Date.now(),
    };
  }

  /**
   * Detect failure state
   * @returns {Object} Detection result with timing
   */
  detectFailure() {
    const startTime = performance.now();

    const health = this.daemon.getHealth();
    const isDown = !health.isRunning;

    const detectionTime = performance.now() - startTime;

    return {
      detected: isDown,
      detectionTimeMs: detectionTime,
      health,
    };
  }

  /**
   * Perform recovery and measure time
   * @param {string} [snapshotId] - Optional snapshot to restore from
   * @returns {Promise<Object>} Recovery timing
   */
  async performRecovery(snapshotId) {
    const recoveryStart = performance.now();
    const events = [];

    this.isRecovering = true;

    // Record recovery start
    events.push({
      event: RecoveryEvent.RECOVERY_STARTED,
      time: performance.now() - recoveryStart,
    });

    // Start the daemon
    await this.daemon.start();

    // Restore state if snapshot provided
    if (snapshotId && this.stateSnapshots.has(snapshotId)) {
      const snapshot = this.stateSnapshots.get(snapshotId);

      // Re-schedule operations from snapshot
      const restoreStart = performance.now();
      for (const op of snapshot.operations) {
        this.daemon.schedule({
          id: `restored-${op.id}`,
          name: op.name,
          handler: async () => ({ restored: true }),
        });
      }

      events.push({
        event: RecoveryEvent.STATE_RESTORED,
        time: performance.now() - recoveryStart,
        restoreTimeMs: performance.now() - restoreStart,
        operationsRestored: snapshot.operations.length,
      });
    }

    // Verify health
    const healthCheckStart = performance.now();
    const health = this.daemon.getHealth();
    events.push({
      event: RecoveryEvent.HEALTH_VERIFIED,
      time: performance.now() - recoveryStart,
      healthCheckTimeMs: performance.now() - healthCheckStart,
      healthy: health.isRunning,
    });

    const totalRecoveryTime = performance.now() - recoveryStart;

    events.push({
      event: RecoveryEvent.RECOVERY_COMPLETE,
      time: totalRecoveryTime,
    });

    this.isRecovering = false;

    const record = {
      snapshotId,
      totalRecoveryTimeMs: totalRecoveryTime,
      events,
      timestamp: Date.now(),
    };

    this.recoveryHistory.push(record);

    return record;
  }

  /**
   * Perform full failover cycle
   * @returns {Promise<Object>} Failover timing
   */
  async performFailover() {
    const failoverStart = performance.now();

    // Create snapshot before failure
    const snapshot = this.createSnapshot();

    // Simulate failure
    const failure = await this.simulateFailure();

    // Detect failure
    const detection = this.detectFailure();

    // Perform recovery
    const recovery = await this.performRecovery(snapshot.snapshotId);

    const totalFailoverTime = performance.now() - failoverStart;

    return {
      snapshotTimeMs: snapshot.snapshotTimeMs,
      failureTimeMs: failure.failureTimeMs,
      detectionTimeMs: detection.detectionTimeMs,
      recoveryTimeMs: recovery.totalRecoveryTimeMs,
      totalFailoverTimeMs: totalFailoverTime,
    };
  }

  /**
   * Get MTTR statistics
   * @returns {Object} MTTR statistics
   */
  getMTTRStats() {
    const recoveryTimes = this.recoveryHistory.map(r => r.totalRecoveryTimeMs);
    return {
      recoveryCount: this.recoveryHistory.length,
      recoveryTimes,
    };
  }
}

/**
 * Benchmark: Recovery detection latency
 * @param {Object} options - Benchmark options
 * @param {number} [options.detectionCount=1000] - Number of detections
 * @param {number} [options.runs=5] - Number of runs
 * @returns {Object} Benchmark result with detection latency metrics
 */
export async function benchmarkRecoveryDetection(options = {}) {
  const { detectionCount = 1000, runs = 5 } = options;
  const allLatencies = [];

  for (let runIdx = 0; runIdx < runs; runIdx++) {
    const daemon = new Daemon({
      daemonId: generateDaemonId(),
      name: `recovery-detect-${runIdx}`,
    });
    await daemon.start();

    const recovery = new RecoveryController(daemon);
    const runLatencies = [];

    // Warm-up phase
    const warmupCount = Math.ceil(detectionCount * 0.1);
    for (let i = 0; i < warmupCount; i++) {
      recovery.detectFailure();
    }

    // Actual measurement - detect healthy state
    for (let i = 0; i < detectionCount; i++) {
      const result = recovery.detectFailure();
      runLatencies.push(result.detectionTimeMs);
    }

    allLatencies.push(...runLatencies);
    await daemon.stop();
  }

  const variance = analyzeVariance(allLatencies);
  const p50 = getPercentile(allLatencies, 50);
  const p95 = getPercentile(allLatencies, 95);
  const p99 = getPercentile(allLatencies, 99);

  return storeBenchmarkResult({
    name: 'recovery-detection-latency',
    type: 'latency',
    unit: 'ms',
    value: parseFloat(variance.mean),
    min: parseFloat(variance.min),
    max: parseFloat(variance.max),
    stdDev: parseFloat(variance.stdDev),
    variance: parseFloat(variance.coefficientOfVariation),
    percentiles: {
      p50: p50.toFixed(4),
      p95: p95.toFixed(4),
      p99: p99.toFixed(4),
    },
    targetMet: p95 < 100.0,
    target: '<100ms (P95)',
    sampleCount: allLatencies.length,
    detectionCount,
    runs,
  });
}

/**
 * Benchmark: State snapshot creation performance
 * @param {Object} options - Benchmark options
 * @param {number} [options.snapshotCount=500] - Number of snapshots
 * @param {number} [options.operationsPerSnapshot=100] - Operations in daemon
 * @param {number} [options.runs=3] - Number of runs
 * @returns {Object} Benchmark result with snapshot metrics
 */
export async function benchmarkStateSnapshot(options = {}) {
  const { snapshotCount = 500, operationsPerSnapshot = 100, runs = 3 } = options;
  const allLatencies = [];

  for (let runIdx = 0; runIdx < runs; runIdx++) {
    const daemon = new Daemon({
      daemonId: generateDaemonId(),
      name: `snapshot-bench-${runIdx}`,
    });
    await daemon.start();

    // Schedule operations to have state to snapshot
    for (let i = 0; i < operationsPerSnapshot; i++) {
      daemon.schedule({
        id: `state-op-${i}`,
        name: `Operation ${i}`,
        handler: async () => ({ index: i }),
        metadata: { data: `metadata-${i}` },
      });
    }

    const recovery = new RecoveryController(daemon);
    const runLatencies = [];

    // Warm-up
    for (let i = 0; i < 50; i++) {
      recovery.createSnapshot();
    }

    // Measure snapshot creation
    for (let i = 0; i < snapshotCount; i++) {
      const result = recovery.createSnapshot();
      runLatencies.push(result.snapshotTimeMs);
    }

    allLatencies.push(...runLatencies);
    await daemon.stop();
  }

  const variance = analyzeVariance(allLatencies);
  const p50 = getPercentile(allLatencies, 50);
  const p95 = getPercentile(allLatencies, 95);
  const p99 = getPercentile(allLatencies, 99);

  return storeBenchmarkResult({
    name: 'state-snapshot-latency',
    type: 'latency',
    unit: 'ms',
    value: parseFloat(variance.mean),
    min: parseFloat(variance.min),
    max: parseFloat(variance.max),
    stdDev: parseFloat(variance.stdDev),
    variance: parseFloat(variance.coefficientOfVariation),
    percentiles: {
      p50: p50.toFixed(4),
      p95: p95.toFixed(4),
      p99: p99.toFixed(4),
    },
    sampleCount: allLatencies.length,
    operationsPerSnapshot,
    snapshotCount,
    runs,
  });
}

/**
 * Benchmark: Full recovery cycle performance
 * @param {Object} options - Benchmark options
 * @param {number} [options.recoveryCycles=50] - Number of recovery cycles
 * @param {number} [options.runs=3] - Number of runs
 * @returns {Object} Benchmark result with recovery cycle metrics
 */
export async function benchmarkRecoveryCycle(options = {}) {
  const { recoveryCycles = 50, runs = 3 } = options;
  const recoveryTimes = [];

  for (let runIdx = 0; runIdx < runs; runIdx++) {
    for (let cycle = 0; cycle < recoveryCycles; cycle++) {
      const daemon = new Daemon({
        daemonId: generateDaemonId(),
        name: `recovery-cycle-${runIdx}-${cycle}`,
      });
      await daemon.start();

      // Schedule some operations
      for (let i = 0; i < 20; i++) {
        daemon.schedule({
          id: `op-${i}`,
          handler: async () => ({ index: i }),
        });
      }

      const recovery = new RecoveryController(daemon);

      // Create snapshot
      const snapshot = recovery.createSnapshot();

      // Simulate failure
      await daemon.stop();

      // Measure recovery
      const recoveryStart = performance.now();
      await daemon.start();
      const recoveryTime = performance.now() - recoveryStart;

      recoveryTimes.push(recoveryTime);

      await daemon.stop();
    }
  }

  const variance = analyzeVariance(recoveryTimes);
  const p50 = getPercentile(recoveryTimes, 50);
  const p95 = getPercentile(recoveryTimes, 95);
  const p99 = getPercentile(recoveryTimes, 99);

  return storeBenchmarkResult({
    name: 'recovery-cycle-latency',
    type: 'latency',
    unit: 'ms',
    value: parseFloat(variance.mean),
    min: parseFloat(variance.min),
    max: parseFloat(variance.max),
    stdDev: parseFloat(variance.stdDev),
    variance: parseFloat(variance.coefficientOfVariation),
    percentiles: {
      p50: p50.toFixed(4),
      p95: p95.toFixed(4),
      p99: p99.toFixed(4),
    },
    targetMet: p95 < 100.0,
    target: '<100ms (P95)',
    recoveryCycles,
    runs,
  });
}

/**
 * Benchmark: Full failover cycle performance
 * @param {Object} options - Benchmark options
 * @param {number} [options.failoverCycles=30] - Number of failover cycles
 * @param {number} [options.runs=3] - Number of runs
 * @returns {Object} Benchmark result with failover metrics
 */
export async function benchmarkFailoverCycle(options = {}) {
  const { failoverCycles = 30, runs = 3 } = options;
  const failoverResults = [];

  for (let runIdx = 0; runIdx < runs; runIdx++) {
    for (let cycle = 0; cycle < failoverCycles; cycle++) {
      const daemon = new Daemon({
        daemonId: generateDaemonId(),
        name: `failover-${runIdx}-${cycle}`,
      });
      await daemon.start();

      // Schedule operations
      for (let i = 0; i < 50; i++) {
        daemon.schedule({
          id: `op-${i}`,
          handler: async () => ({ index: i }),
        });
      }

      const recovery = new RecoveryController(daemon);
      const failover = await recovery.performFailover();
      failoverResults.push(failover);

      await daemon.stop();
    }
  }

  const totalTimes = failoverResults.map(f => f.totalFailoverTimeMs);
  const recoveryTimes = failoverResults.map(f => f.recoveryTimeMs);
  const detectionTimes = failoverResults.map(f => f.detectionTimeMs);

  const totalVariance = analyzeVariance(totalTimes);
  const recoveryVariance = analyzeVariance(recoveryTimes);
  const detectionVariance = analyzeVariance(detectionTimes);

  const totalP95 = getPercentile(totalTimes, 95);
  const recoveryP95 = getPercentile(recoveryTimes, 95);
  const detectionP95 = getPercentile(detectionTimes, 95);

  return storeBenchmarkResult({
    name: 'failover-cycle-latency',
    type: 'latency',
    unit: 'ms',
    value: parseFloat(totalVariance.mean),
    total: {
      mean: parseFloat(totalVariance.mean),
      p95: totalP95.toFixed(4),
      min: parseFloat(totalVariance.min),
      max: parseFloat(totalVariance.max),
    },
    recovery: {
      mean: parseFloat(recoveryVariance.mean),
      p95: recoveryP95.toFixed(4),
    },
    detection: {
      mean: parseFloat(detectionVariance.mean),
      p95: detectionP95.toFixed(4),
    },
    targetMet: recoveryP95 < 100.0,
    target: '<100ms (P95) recovery',
    failoverCycles,
    runs,
  });
}

/**
 * Benchmark: MTTR under varying loads
 * @param {Object} options - Benchmark options
 * @param {number} [options.cyclesPerLoad=20] - Cycles per load level
 * @param {number} [options.runs=2] - Number of runs
 * @returns {Object} Benchmark result with load-based MTTR metrics
 */
export async function benchmarkMTTRUnderLoad(options = {}) {
  const { cyclesPerLoad = 20, runs = 2 } = options;

  const loadLevels = [
    { name: 'light', operations: 10 },
    { name: 'medium', operations: 100 },
    { name: 'heavy', operations: 500 },
  ];

  const loadResults = [];

  for (const load of loadLevels) {
    const loadRecoveryTimes = [];

    for (let runIdx = 0; runIdx < runs; runIdx++) {
      for (let cycle = 0; cycle < cyclesPerLoad; cycle++) {
        const daemon = new Daemon({
          daemonId: generateDaemonId(),
          name: `mttr-load-${load.name}-${cycle}`,
        });
        await daemon.start();

        // Create load
        for (let i = 0; i < load.operations; i++) {
          daemon.schedule({
            id: `load-op-${i}`,
            handler: async () => {
              await new Promise(resolve => setTimeout(resolve, 1));
              return { index: i };
            },
          });
        }

        // Measure recovery
        await daemon.stop();
        const recoveryStart = performance.now();
        await daemon.start();
        const recoveryTime = performance.now() - recoveryStart;

        loadRecoveryTimes.push(recoveryTime);
        await daemon.stop();
      }
    }

    const variance = analyzeVariance(loadRecoveryTimes);
    const p95 = getPercentile(loadRecoveryTimes, 95);

    loadResults.push({
      load: load.name,
      operationCount: load.operations,
      mttr: parseFloat(variance.mean),
      p95: p95.toFixed(4),
      variance: parseFloat(variance.coefficientOfVariation),
      targetMet: p95 < 100.0,
    });
  }

  const allMTTR = loadResults.map(r => r.mttr);
  const overallVariance = analyzeVariance(allMTTR);

  return storeBenchmarkResult({
    name: 'mttr-under-load',
    type: 'latency',
    unit: 'ms',
    value: parseFloat(overallVariance.mean),
    loadResults,
    target: '<100ms (P95)',
    cyclesPerLoad,
    runs,
  });
}

/**
 * Benchmark: State restoration performance
 * @param {Object} options - Benchmark options
 * @param {number} [options.restorations=100] - Number of restorations
 * @param {number} [options.runs=3] - Number of runs
 * @returns {Object} Benchmark result with restoration metrics
 */
export async function benchmarkStateRestoration(options = {}) {
  const { restorations = 100, runs = 3 } = options;

  const stateScales = [
    { name: 'minimal', operations: 10 },
    { name: 'moderate', operations: 50 },
    { name: 'extensive', operations: 200 },
  ];

  const scaleResults = [];

  for (const scale of stateScales) {
    const restorationTimes = [];

    for (let runIdx = 0; runIdx < runs; runIdx++) {
      for (let r = 0; r < restorations / runs; r++) {
        const daemon = new Daemon({
          daemonId: generateDaemonId(),
          name: `restore-${scale.name}-${r}`,
        });
        await daemon.start();

        // Build state
        for (let i = 0; i < scale.operations; i++) {
          daemon.schedule({
            id: `op-${i}`,
            name: `Operation ${i}`,
            handler: async () => ({ index: i }),
            metadata: { data: `data-${i}` },
          });
        }

        const recovery = new RecoveryController(daemon);
        const snapshot = recovery.createSnapshot();

        // Simulate failure
        await daemon.stop();

        // Measure restoration
        const result = await recovery.performRecovery(snapshot.snapshotId);
        restorationTimes.push(result.totalRecoveryTimeMs);

        await daemon.stop();
      }
    }

    const variance = analyzeVariance(restorationTimes);
    const p95 = getPercentile(restorationTimes, 95);

    scaleResults.push({
      scale: scale.name,
      operationCount: scale.operations,
      mean: parseFloat(variance.mean),
      p95: p95.toFixed(4),
      variance: parseFloat(variance.coefficientOfVariation),
      targetMet: p95 < 100.0,
    });
  }

  const allMeans = scaleResults.map(r => r.mean);
  const overallVariance = analyzeVariance(allMeans);

  return storeBenchmarkResult({
    name: 'state-restoration-latency',
    type: 'latency',
    unit: 'ms',
    value: parseFloat(overallVariance.mean),
    scaleResults,
    target: '<100ms (P95)',
    restorations,
    runs,
  });
}

/**
 * Benchmark: Recovery memory overhead
 * @param {Object} options - Benchmark options
 * @param {number} [options.snapshotCount=100] - Snapshots to accumulate
 * @param {number} [options.runs=3] - Number of runs
 * @returns {Object} Benchmark result with memory metrics
 */
export async function benchmarkRecoveryMemoryOverhead(options = {}) {
  const { snapshotCount = 100, runs = 3 } = options;
  const memoryMeasurements = [];

  for (let runIdx = 0; runIdx < runs; runIdx++) {
    if (global.gc) global.gc();

    const daemon = new Daemon({
      daemonId: generateDaemonId(),
      name: `recovery-mem-${runIdx}`,
    });
    await daemon.start();

    // Add operations to snapshot
    for (let i = 0; i < 50; i++) {
      daemon.schedule({
        id: `op-${i}`,
        handler: async () => ({ index: i }),
      });
    }

    const beforeMemory = getMemorySnapshot();
    const recovery = new RecoveryController(daemon);

    // Accumulate snapshots
    for (let i = 0; i < snapshotCount; i++) {
      recovery.createSnapshot();
    }

    const afterMemory = getMemorySnapshot();
    const heapDelta = afterMemory.heapUsed - beforeMemory.heapUsed;
    const bytesPerSnapshot = heapDelta / snapshotCount;

    memoryMeasurements.push({
      snapshotCount,
      heapDelta,
      bytesPerSnapshot,
      totalMB: heapDelta / (1024 * 1024),
    });

    await daemon.stop();
  }

  const bytesPerSnapshotValues = memoryMeasurements.map(m => m.bytesPerSnapshot);
  const variance = analyzeVariance(bytesPerSnapshotValues);

  return storeBenchmarkResult({
    name: 'recovery-memory-overhead',
    type: 'memory',
    unit: 'bytes/snapshot',
    value: parseFloat(variance.mean),
    min: parseFloat(variance.min),
    max: parseFloat(variance.max),
    stdDev: parseFloat(variance.stdDev),
    variance: parseFloat(variance.coefficientOfVariation),
    measurements: memoryMeasurements,
    snapshotCount,
    runs,
  });
}

/**
 * Benchmark: Multi-scale recovery simulation
 * Tests small, medium, and large scale recovery scenarios
 * @param {Object} options - Benchmark options
 * @param {number} [options.runs=2] - Number of runs per scale
 * @returns {Object} Benchmark result with multi-scale metrics
 */
export async function benchmarkMultiScaleRecovery(options = {}) {
  const { runs = 2 } = options;

  const scales = [
    { name: 'small', daemons: 1, cyclesPerDaemon: 30 },
    { name: 'medium', daemons: 3, cyclesPerDaemon: 15 },
    { name: 'large', daemons: 5, cyclesPerDaemon: 10 },
  ];

  const scaleResults = [];

  for (const scale of scales) {
    const scaleRecoveryTimes = [];
    const scaleDetectionTimes = [];

    for (let runIdx = 0; runIdx < runs; runIdx++) {
      for (let d = 0; d < scale.daemons; d++) {
        for (let c = 0; c < scale.cyclesPerDaemon; c++) {
          const daemon = new Daemon({
            daemonId: generateDaemonId(),
            name: `multi-recovery-${scale.name}-${d}-${c}`,
          });
          await daemon.start();

          // Add operations
          for (let i = 0; i < 20; i++) {
            daemon.schedule({
              id: `op-${i}`,
              handler: async () => ({ index: i }),
            });
          }

          const recovery = new RecoveryController(daemon);

          // Measure detection
          const detection = recovery.detectFailure();
          scaleDetectionTimes.push(detection.detectionTimeMs);

          // Simulate and measure recovery
          await daemon.stop();
          const recoveryStart = performance.now();
          await daemon.start();
          const recoveryTime = performance.now() - recoveryStart;
          scaleRecoveryTimes.push(recoveryTime);

          await daemon.stop();
        }
      }
    }

    const recoveryVariance = analyzeVariance(scaleRecoveryTimes);
    const detectionVariance = analyzeVariance(scaleDetectionTimes);

    const recoveryP50 = getPercentile(scaleRecoveryTimes, 50);
    const recoveryP95 = getPercentile(scaleRecoveryTimes, 95);
    const recoveryP99 = getPercentile(scaleRecoveryTimes, 99);

    const detectionP95 = getPercentile(scaleDetectionTimes, 95);

    scaleResults.push({
      scale: scale.name,
      daemons: scale.daemons,
      cyclesPerDaemon: scale.cyclesPerDaemon,
      totalCycles: scale.daemons * scale.cyclesPerDaemon * runs,
      recovery: {
        mean: parseFloat(recoveryVariance.mean),
        percentiles: {
          p50: recoveryP50.toFixed(4),
          p95: recoveryP95.toFixed(4),
          p99: recoveryP99.toFixed(4),
        },
        variance: parseFloat(recoveryVariance.coefficientOfVariation),
      },
      detection: {
        mean: parseFloat(detectionVariance.mean),
        p95: detectionP95.toFixed(4),
      },
      targetMet: recoveryP95 < 100.0 && detectionP95 < 100.0,
    });
  }

  const allRecoveryMeans = scaleResults.map(r => r.recovery.mean);
  const overallVariance = analyzeVariance(allRecoveryMeans);

  return storeBenchmarkResult({
    name: 'multi-scale-recovery-simulation',
    type: 'latency',
    unit: 'ms',
    value: parseFloat(overallVariance.mean),
    scaleResults,
    target: '<100ms (P95)',
    runs,
  });
}
