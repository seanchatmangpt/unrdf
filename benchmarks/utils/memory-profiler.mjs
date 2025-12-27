/**
 * GC-Aware Memory Profiler for Benchmarks
 *
 * Measures memory consumption with proper garbage collection handling.
 * Requires --expose-gc flag for accurate measurements, but degrades gracefully.
 */

export class MemoryProfiler {
  /**
   * Force garbage collection if available
   * Requires node --expose-gc flag
   */
  static forceGC() {
    if (global.gc) {
      global.gc();
      global.gc(); // Run twice for thorough collection
    }
  }

  /**
   * Get current memory usage
   * @returns {{ heapUsed: number, heapTotal: number, external: number, rss: number }}
   */
  static getMemoryUsage() {
    const usage = process.memoryUsage();
    return {
      heapUsed: usage.heapUsed,
      heapTotal: usage.heapTotal,
      external: usage.external,
      rss: usage.rss
    };
  }

  /**
   * Measure memory consumed by an operation
   * @param {Function} operation - Async operation to measure
   * @returns {Promise<Object>} Memory measurement results
   */
  static async measureMemory(operation) {
    // Establish clean baseline
    MemoryProfiler.forceGC();
    await new Promise(resolve => setImmediate(resolve));

    const baselineMemory = MemoryProfiler.getMemoryUsage();
    const gcStart = Date.now();

    // Run the operation
    const result = await operation();

    // Measure peak memory
    const peakMemory = MemoryProfiler.getMemoryUsage();

    // Force GC to see retained memory
    MemoryProfiler.forceGC();
    await new Promise(resolve => setImmediate(resolve));

    const afterGCMemory = MemoryProfiler.getMemoryUsage();
    const gcDuration = Date.now() - gcStart;

    return {
      result,
      baselineHeapUsed: baselineMemory.heapUsed,
      peakHeapUsed: peakMemory.heapUsed,
      afterGCHeapUsed: afterGCMemory.heapUsed,
      deltaHeapUsed: peakMemory.heapUsed - baselineMemory.heapUsed,
      retainedHeapUsed: afterGCMemory.heapUsed - baselineMemory.heapUsed,
      gcDuration,
      baselineRss: baselineMemory.rss,
      peakRss: peakMemory.rss,
      deltaRss: peakMemory.rss - baselineMemory.rss
    };
  }

  /**
   * Detect memory leaks by comparing baseline to cleanup
   * @param {Function} baseline - Function to establish baseline
   * @param {Function} cleanup - Function that should release memory
   * @param {number} [threshold=0.05] - Leak threshold (5% by default)
   * @returns {Promise<Object>} Leak detection results
   */
  static async detectLeak(baseline, cleanup, threshold = 0.05) {
    // Get baseline memory
    const baselineResult = await MemoryProfiler.measureMemory(baseline);

    // Run cleanup
    await cleanup();

    // Force GC and check memory
    MemoryProfiler.forceGC();
    await new Promise(resolve => setImmediate(resolve));

    const cleanupMemory = MemoryProfiler.getMemoryUsage();

    // Calculate delta from baseline
    const delta = cleanupMemory.heapUsed - baselineResult.baselineHeapUsed;
    const deltaPercent = delta / baselineResult.baselineHeapUsed;

    return {
      isLeak: deltaPercent > threshold,
      deltaBytes: delta,
      deltaPercent,
      threshold,
      baselineHeapUsed: baselineResult.baselineHeapUsed,
      cleanupHeapUsed: cleanupMemory.heapUsed
    };
  }

  /**
   * Measure memory footprint per item
   * @param {Function} createItems - Function that creates N items
   * @param {number} itemCount - Number of items to create
   * @returns {Promise<Object>} Per-item memory footprint
   */
  static async measureFootprintPerItem(createItems, itemCount) {
    const measurement = await MemoryProfiler.measureMemory(async () => {
      return createItems(itemCount);
    });

    const bytesPerItem = measurement.deltaHeapUsed / itemCount;
    const retainedBytesPerItem = measurement.retainedHeapUsed / itemCount;

    return {
      ...measurement,
      itemCount,
      bytesPerItem,
      retainedBytesPerItem,
      mbPerItem: bytesPerItem / (1024 * 1024),
      retainedMbPerItem: retainedBytesPerItem / (1024 * 1024)
    };
  }

  /**
   * Check if GC is available
   * @returns {boolean} True if --expose-gc flag is set
   */
  static isGCAvailable() {
    return typeof global.gc === 'function';
  }

  /**
   * Convert bytes to human-readable format
   * @param {number} bytes - Bytes to format
   * @returns {string} Formatted string (e.g., "1.5 MB")
   */
  static formatBytes(bytes) {
    if (bytes === 0) return '0 B';

    const k = 1024;
    const sizes = ['B', 'KB', 'MB', 'GB'];
    const i = Math.floor(Math.log(bytes) / Math.log(k));

    return `${(bytes / Math.pow(k, i)).toFixed(2)} ${sizes[i]}`;
  }
}
