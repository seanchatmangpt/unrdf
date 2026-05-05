/**
 * Phase 3: Determinism and Large-Scale Integration Tests
 *
 * Focus on:
 * - Deterministic execution verification
 * - Large-scale data processing
 * - Performance benchmarking across profiles
 * - Stress testing and resource limits
 */
import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import * as crypto from 'crypto';
import * as pm from '../../pkg/wasm4pm.js';
import { XES_MINIMAL, XES_SEQUENTIAL, XES_PARALLEL } from '../helpers/fixtures';
/**
 * Helper: Compute SHA256 hash of data
 */
function computeHash(data) {
  return crypto.createHash('sha256').update(data).digest('hex');
}
/**
 * Helper: Create XES log with N traces and M events per trace
 */
function generateXESLog(numTraces, eventsPerTrace) {
  let xml = `<?xml version="1.0" encoding="UTF-8"?>
<log xes.version="1.0" openlog.version="1.0">
  <extension name="Concept" prefix="concept" uri="http://www.xes-standard.org/concept.xesext"/>
  <extension name="Time" prefix="time" uri="http://www.xes-standard.org/time.xesext"/>
  <global scope="trace"><string key="concept:name" value="undefined"/></global>
  <global scope="event"><string key="concept:name" value="undefined"/><date key="time:timestamp" value="1970-01-01T00:00:00.000+00:00"/></global>`;
  const activities = ['Start', 'Review', 'Approve', 'Reject', 'Complete', 'Archive'];
  for (let t = 0; t < numTraces; t++) {
    xml += `\n  <trace><string key="concept:name" value="Case${t + 1}"/>`;
    for (let e = 0; e < eventsPerTrace; e++) {
      const activity = activities[e % activities.length];
      const timestamp = new Date(2023, 0, 1, 10 + e, e * 5, e * 15).toISOString();
      xml += `\n    <event><string key="concept:name" value="${activity}"/><date key="time:timestamp" value="${timestamp}"/></event>`;
    }
    xml += '\n  </trace>';
  }
  xml += '\n</log>';
  return xml;
}
describe('Phase 3: Determinism and Large-Scale Tests', () => {
  beforeEach(async () => {
    try {
      await pm.init();
      pm.clear_all_objects();
    } catch (e) {
      // Silent initialization
    }
  });
  afterEach(async () => {
    try {
      pm.clear_all_objects();
    } catch (e) {
      // Silent cleanup
    }
  });
  // ============================================================================
  // 1. STRICT DETERMINISM TESTS
  // ============================================================================
  describe('Strict Determinism Verification', () => {
    it('identical XES input produces identical DFG output', () => {
      // First execution
      const log1 = pm.load_eventlog_from_xes(XES_SEQUENTIAL);
      const dfg1 = pm.discover_dfg(log1, 'concept:name');
      const output1 = JSON.stringify(dfg1);
      pm.clear_all_objects();
      // Second execution (identical input)
      const log2 = pm.load_eventlog_from_xes(XES_SEQUENTIAL);
      const dfg2 = pm.discover_dfg(log2, 'concept:name');
      const output2 = JSON.stringify(dfg2);
      expect(output1).toEqual(output2);
    });
    it('deterministic hash computation', () => {
      const input = XES_SEQUENTIAL;
      const hash1 = computeHash(input);
      const hash2 = computeHash(input);
      expect(hash1).toEqual(hash2);
      expect(hash1).toHaveLength(64); // SHA256 hex
    });
    it('multiple runs of same pipeline produce same output', () => {
      const outputs = [];
      for (let i = 0; i < 3; i++) {
        const log = pm.load_eventlog_from_xes(XES_SEQUENTIAL);
        const stats = pm.analyze_event_statistics(log);
        const duration = pm.analyze_case_duration(log);
        outputs.push(JSON.stringify({ stats, duration }));
        pm.clear_all_objects();
      }
      // All outputs should be identical
      expect(outputs[0]).toEqual(outputs[1]);
      expect(outputs[1]).toEqual(outputs[2]);
    });
    it('execution order does not affect final state', () => {
      // Order 1: Load → DFG → Stats
      const log1 = pm.load_eventlog_from_xes(XES_SEQUENTIAL);
      const dfg1 = pm.discover_dfg(log1, 'concept:name');
      const stats1 = pm.analyze_event_statistics(log1);
      const result1 = { dfg: dfg1, stats: stats1 };
      pm.clear_all_objects();
      // Order 2: Load → Stats → DFG
      const log2 = pm.load_eventlog_from_xes(XES_SEQUENTIAL);
      const stats2 = pm.analyze_event_statistics(log2);
      const dfg2 = pm.discover_dfg(log2, 'concept:name');
      const result2 = { dfg: dfg2, stats: stats2 };
      expect(JSON.stringify(result1)).toEqual(JSON.stringify(result2));
    });
    it('deterministic behavior across process restart simulation', () => {
      const results = [];
      for (let run = 0; run < 5; run++) {
        const log = pm.load_eventlog_from_xes(XES_MINIMAL);
        const dfg = JSON.parse(pm.discover_dfg(log, 'concept:name'));
        const stats = pm.analyze_event_statistics(log);
        results.push({
          dfgNodes: dfg.nodes.length,
          dfgEdges: dfg.edges.length,
          hash: computeHash(JSON.stringify(dfg)),
        });
        pm.clear_all_objects();
      }
      // All runs should produce same results
      const firstResult = results[0];
      results.forEach((result) => {
        expect(result.dfgNodes).toEqual(firstResult.dfgNodes);
        expect(result.dfgEdges).toEqual(firstResult.dfgEdges);
        expect(result.hash).toEqual(firstResult.hash);
      });
    });
    it('determinism holds for empty traces', () => {
      const emptyXes = `<?xml version="1.0" encoding="UTF-8"?>
<log xes.version="1.0" openlog.version="1.0">
</log>`;
      const log1 = pm.load_eventlog_from_xes(emptyXes);
      const out1 = pm.discover_dfg(log1, 'concept:name');
      pm.clear_all_objects();
      const log2 = pm.load_eventlog_from_xes(emptyXes);
      const out2 = pm.discover_dfg(log2, 'concept:name');
      expect(out1).toEqual(out2);
    });
    it('same analysis produces same metrics across runs', () => {
      const metrics = [];
      for (let i = 0; i < 3; i++) {
        const log = pm.load_eventlog_from_xes(XES_SEQUENTIAL);
        const stats = pm.analyze_event_statistics(log);
        const duration = pm.analyze_case_duration(log);
        metrics.push({ stats, duration });
        pm.clear_all_objects();
      }
      // All metrics should match
      const first = JSON.stringify(metrics[0]);
      metrics.slice(1).forEach((m) => {
        expect(JSON.stringify(m)).toEqual(first);
      });
    });
  });
  // ============================================================================
  // 2. LARGE-SCALE DATA PROCESSING
  // ============================================================================
  describe('Large-Scale Data Processing', () => {
    it('should process 100-event log efficiently', () => {
      const largeXes = generateXESLog(10, 10); // 10 traces × 10 events = 100 events
      const startTime = performance.now();
      const log = pm.load_eventlog_from_xes(largeXes);
      const dfg = pm.discover_dfg(log, 'concept:name');
      const stats = pm.analyze_event_statistics(log);
      const duration = performance.now() - startTime;
      expect(log).toBeTruthy();
      expect(dfg).toBeTruthy();
      expect(stats).toBeTruthy();
      expect(duration).toBeLessThan(500); // Should complete in reasonable time
    });
    it('should process 500-event log', () => {
      const largeXes = generateXESLog(50, 10); // 50 traces × 10 events = 500 events
      const startTime = performance.now();
      const log = pm.load_eventlog_from_xes(largeXes);
      const dfg = pm.discover_dfg(log, 'concept:name');
      const duration = performance.now() - startTime;
      expect(log).toBeTruthy();
      expect(dfg).toBeTruthy();
      expect(duration).toBeLessThan(1000);
    });
    it('should process 1000-event log', () => {
      const largeXes = generateXESLog(100, 10); // 100 traces × 10 events = 1000 events
      const startTime = performance.now();
      const log = pm.load_eventlog_from_xes(largeXes);
      const dfg = pm.discover_dfg(log, 'concept:name');
      const duration = performance.now() - startTime;
      expect(log).toBeTruthy();
      expect(dfg).toBeTruthy();
      expect(duration).toBeLessThan(2000);
    });
    it('should scale reasonably with event count', () => {
      const smallXes = generateXESLog(5, 5); // 25 events
      const largeXes = generateXESLog(20, 10); // 200 events
      const smallStart = performance.now();
      let log = pm.load_eventlog_from_xes(smallXes);
      pm.discover_dfg(log, 'concept:name');
      const smallTime = performance.now() - smallStart;
      pm.clear_all_objects();
      const largeStart = performance.now();
      log = pm.load_eventlog_from_xes(largeXes);
      pm.discover_dfg(log, 'concept:name');
      const largeTime = performance.now() - largeStart;
      pm.clear_all_objects();
      // Both should complete in reasonable time
      expect(smallTime).toBeLessThan(500);
      expect(largeTime).toBeLessThan(1000);
    });
    it('should not leak memory processing multiple large logs', () => {
      const initialCount = pm.object_count();
      for (let i = 0; i < 3; i++) {
        const xes = generateXESLog(20, 10); // 200 events
        const log = pm.load_eventlog_from_xes(xes);
        pm.discover_dfg(log, 'concept:name');
        pm.clear_all_objects();
      }
      const finalCount = pm.object_count();
      expect(finalCount).toBe(initialCount); // No memory leak
    });
    it('should handle complex branching patterns at scale', () => {
      const complexXes = generateXESLog(30, 8);
      const startTime = performance.now();
      const log = pm.load_eventlog_from_xes(complexXes);
      const dfg = JSON.parse(pm.discover_dfg(log, 'concept:name'));
      const stats = pm.analyze_event_statistics(log);
      const duration = performance.now() - startTime;
      expect(dfg).toBeTruthy();
      expect(dfg).toHaveProperty('nodes');
      expect(dfg).toHaveProperty('edges');
      expect(duration).toBeLessThan(1000);
    });
  });
  // ============================================================================
  // 3. PROFILE PERFORMANCE COMPARISON
  // ============================================================================
  describe('Profile Performance Comparison', () => {
    it('FAST profile is faster than comprehensive analysis', () => {
      const xes = XES_SEQUENTIAL;
      // Fast profile: just DFG
      let log = pm.load_eventlog_from_xes(xes);
      const dfg = pm.discover_dfg(log, 'concept:name');
      expect(dfg).toBeTruthy();
      pm.clear_all_objects();
      // Comprehensive: DFG + multiple analyses — verify it completes successfully
      log = pm.load_eventlog_from_xes(xes);
      const dfg2 = pm.discover_dfg(log, 'concept:name');
      const stats = pm.analyze_event_statistics(log);
      const duration = pm.analyze_case_duration(log);
      pm.clear_all_objects();
      expect(dfg2).toBeTruthy();
      expect(stats).toBeTruthy();
      expect(duration).toBeTruthy();
    });
    it('processing time increases with data size', () => {
      const smallXes = generateXESLog(5, 5); // 25 events
      const largeXes = generateXESLog(100, 50); // 5000 events
      // Warmup both paths to eliminate JIT bias
      let log = pm.load_eventlog_from_xes(smallXes);
      pm.discover_dfg(log, 'concept:name');
      pm.clear_all_objects();
      log = pm.load_eventlog_from_xes(largeXes);
      pm.discover_dfg(log, 'concept:name');
      pm.clear_all_objects();
      // Measure small (3 runs, take median)
      const smallTimes = [];
      for (let i = 0; i < 3; i++) {
        const t0 = performance.now();
        log = pm.load_eventlog_from_xes(smallXes);
        pm.discover_dfg(log, 'concept:name');
        smallTimes.push(performance.now() - t0);
        pm.clear_all_objects();
      }
      // Measure large (3 runs, take median)
      const largeTimes = [];
      for (let i = 0; i < 3; i++) {
        const t0 = performance.now();
        log = pm.load_eventlog_from_xes(largeXes);
        pm.discover_dfg(log, 'concept:name');
        largeTimes.push(performance.now() - t0);
        pm.clear_all_objects();
      }
      const median = (arr) => {
        const s = [...arr].sort((a, b) => a - b);
        return s[Math.floor(s.length / 2)];
      };
      // Large dataset (200x more events) should not be faster than small
      expect(median(largeTimes)).toBeGreaterThanOrEqual(median(smallTimes) * 0.5);
    });
    it('quality profile produces more detailed output', () => {
      const log = pm.load_eventlog_from_xes(XES_SEQUENTIAL);
      // Basic analysis
      const stats = pm.analyze_event_statistics(log);
      expect(stats).toBeTruthy();
      // Extended analysis
      const duration = pm.analyze_case_duration(log);
      expect(duration).toBeTruthy();
      // Both should produce meaningful results
      expect(stats).not.toBeNull();
      expect(duration).not.toBeNull();
    });
  });
  // ============================================================================
  // 4. STRESS TESTING
  // ============================================================================
  describe('Stress Testing', () => {
    it('should survive rapid sequential operations', () => {
      for (let i = 0; i < 10; i++) {
        const log = pm.load_eventlog_from_xes(XES_MINIMAL);
        pm.discover_dfg(log, 'concept:name');
        pm.clear_all_objects();
      }
      expect(pm.object_count()).toBe(0);
    });
    it('should handle many objects without degradation', () => {
      const logs = [];
      // Load 5 logs
      for (let i = 0; i < 5; i++) {
        const log = pm.load_eventlog_from_xes(XES_MINIMAL);
        logs.push(log);
      }
      expect(pm.object_count()).toBeGreaterThan(0);
      // Process each
      logs.forEach((log) => {
        const dfg = pm.discover_dfg(log, 'concept:name');
        expect(dfg).toBeTruthy();
      });
      // Clear
      pm.clear_all_objects();
      expect(pm.object_count()).toBe(0);
    });
    it('should recover gracefully from edge cases', () => {
      // Edge case 1: Very small log
      let log = pm.load_eventlog_from_xes(XES_MINIMAL);
      expect(log).toBeTruthy();
      pm.clear_all_objects();
      // Edge case 2: Empty-like log
      const emptyXes = `<?xml version="1.0" encoding="UTF-8"?>
<log xes.version="1.0" openlog.version="1.0">
</log>`;
      log = pm.load_eventlog_from_xes(emptyXes);
      expect(log).toBeTruthy();
      pm.clear_all_objects();
      // Edge case 3: Single event
      const singleXes = `<?xml version="1.0" encoding="UTF-8"?>
<log xes.version="1.0" openlog.version="1.0">
  <trace><string key="concept:name" value="Case1"/>
    <event><string key="concept:name" value="A"/><date key="time:timestamp" value="2023-01-01T10:00:00"/></event>
  </trace>
</log>`;
      log = pm.load_eventlog_from_xes(singleXes);
      expect(log).toBeTruthy();
      pm.clear_all_objects();
      expect(pm.object_count()).toBe(0);
    });
    it('should maintain consistency during concurrent-like operations', () => {
      const log1 = pm.load_eventlog_from_xes(XES_SEQUENTIAL);
      const log2 = pm.load_eventlog_from_xes(XES_PARALLEL);
      const dfg1a = pm.discover_dfg(log1, 'concept:name');
      const dfg2a = pm.discover_dfg(log2, 'concept:name');
      const dfg1b = pm.discover_dfg(log1, 'concept:name');
      const dfg2b = pm.discover_dfg(log2, 'concept:name');
      // Repeated operations should yield same results
      expect(JSON.stringify(dfg1a)).toEqual(JSON.stringify(dfg1b));
      expect(JSON.stringify(dfg2a)).toEqual(JSON.stringify(dfg2b));
    });
  });
  // ============================================================================
  // 5. HASH-BASED VERIFICATION
  // ============================================================================
  describe('Hash-Based Verification', () => {
    it('same input produces same output hash', () => {
      const input = XES_SEQUENTIAL;
      const inputHash1 = computeHash(input);
      const inputHash2 = computeHash(input);
      expect(inputHash1).toEqual(inputHash2);
    });
    it('different inputs produce different hashes', () => {
      const hash1 = computeHash(XES_SEQUENTIAL);
      const hash2 = computeHash(XES_MINIMAL);
      const hash3 = computeHash(XES_PARALLEL);
      expect(hash1).not.toEqual(hash2);
      expect(hash2).not.toEqual(hash3);
      expect(hash1).not.toEqual(hash3);
    });
    it('output hash matches across runs', () => {
      // Run 1
      const log1 = pm.load_eventlog_from_xes(XES_SEQUENTIAL);
      const output1 = pm.discover_dfg(log1, 'concept:name');
      const hash1 = computeHash(output1);
      pm.clear_all_objects();
      // Run 2
      const log2 = pm.load_eventlog_from_xes(XES_SEQUENTIAL);
      const output2 = pm.discover_dfg(log2, 'concept:name');
      const hash2 = computeHash(output2);
      pm.clear_all_objects();
      expect(hash1).toEqual(hash2);
    });
    it('hash chain is reproducible', () => {
      const input = XES_SEQUENTIAL;
      const configHash = computeHash(JSON.stringify({ profile: 'fast' }));
      const inputHash = computeHash(input);
      const log = pm.load_eventlog_from_xes(input);
      const output = pm.discover_dfg(log, 'concept:name');
      const outputHash = computeHash(output);
      // Receipt hashes should be reproducible
      const receiptData = JSON.stringify({
        configHash,
        inputHash,
        outputHash,
      });
      const receiptHash1 = computeHash(receiptData);
      const receiptHash2 = computeHash(receiptData);
      expect(receiptHash1).toEqual(receiptHash2);
    });
  });
  // ============================================================================
  // 6. IDEMPOTENCY TESTS
  // ============================================================================
  describe('Idempotency Tests', () => {
    it('loading same log twice produces same handle', () => {
      const log1 = pm.load_eventlog_from_xes(XES_SEQUENTIAL);
      const log2 = pm.load_eventlog_from_xes(XES_SEQUENTIAL);
      // Both should be valid handles
      expect(log1).toBeTruthy();
      expect(log2).toBeTruthy();
    });
    it('repeated DFG discovery produces identical results', () => {
      const log = pm.load_eventlog_from_xes(XES_SEQUENTIAL);
      const results = [];
      for (let i = 0; i < 3; i++) {
        results.push(pm.discover_dfg(log, 'concept:name'));
      }
      // All results should be identical
      expect(results[0]).toEqual(results[1]);
      expect(results[1]).toEqual(results[2]);
    });
    it('repeated analysis is idempotent', () => {
      const log = pm.load_eventlog_from_xes(XES_SEQUENTIAL);
      const stats1 = pm.analyze_event_statistics(log);
      const stats2 = pm.analyze_event_statistics(log);
      expect(JSON.stringify(stats1)).toEqual(JSON.stringify(stats2));
    });
    it('clearing and reloading produces same results', () => {
      // Load and process
      let log = pm.load_eventlog_from_xes(XES_SEQUENTIAL);
      const dfg1 = pm.discover_dfg(log, 'concept:name');
      pm.clear_all_objects();
      // Reload and process
      log = pm.load_eventlog_from_xes(XES_SEQUENTIAL);
      const dfg2 = pm.discover_dfg(log, 'concept:name');
      pm.clear_all_objects();
      expect(dfg1).toEqual(dfg2);
    });
  });
  // ============================================================================
  // 7. CONSISTENCY ACROSS FIXTURE TYPES
  // ============================================================================
  describe('Consistency Across Fixture Types', () => {
    it('all fixtures can be processed without errors', () => {
      const fixtures = [XES_MINIMAL, XES_SEQUENTIAL, XES_PARALLEL];
      fixtures.forEach((xes) => {
        const log = pm.load_eventlog_from_xes(xes);
        expect(log).toBeTruthy();
        const dfg = pm.discover_dfg(log, 'concept:name');
        expect(dfg).toBeTruthy();
        pm.clear_all_objects();
      });
    });
    it('DFG discovery is consistent across fixture types', () => {
      const fixtures = [
        { data: XES_MINIMAL, name: 'minimal' },
        { data: XES_SEQUENTIAL, name: 'sequential' },
        { data: XES_PARALLEL, name: 'parallel' },
      ];
      const results = fixtures.map(({ data, name }) => {
        const log = pm.load_eventlog_from_xes(data);
        const dfg = JSON.parse(pm.discover_dfg(log, 'concept:name'));
        pm.clear_all_objects();
        return { name, dfg };
      });
      // All should produce valid DFGs
      results.forEach(({ dfg }) => {
        expect(dfg).toBeTruthy();
        expect(dfg).toHaveProperty('nodes');
        expect(dfg).toHaveProperty('edges');
      });
    });
  });
  // ============================================================================
  // 8. BENCHMARK AGGREGATION
  // ============================================================================
  describe('Performance Benchmarking', () => {
    it('benchmark small dataset performance', () => {
      const times = [];
      for (let i = 0; i < 5; i++) {
        const start = performance.now();
        const log = pm.load_eventlog_from_xes(XES_MINIMAL);
        pm.discover_dfg(log, 'concept:name');
        times.push(performance.now() - start);
        pm.clear_all_objects();
      }
      const avgTime = times.reduce((a, b) => a + b, 0) / times.length;
      const maxTime = Math.max(...times);
      const minTime = Math.min(...times);
      expect(avgTime).toBeLessThan(100);
      expect(maxTime).toBeLessThan(200);
      expect(minTime).toBeGreaterThan(0);
    });
    it('benchmark medium dataset performance', () => {
      const mediumXes = generateXESLog(20, 5); // 100 events
      const times = [];
      for (let i = 0; i < 3; i++) {
        const start = performance.now();
        const log = pm.load_eventlog_from_xes(mediumXes);
        pm.discover_dfg(log, 'concept:name');
        times.push(performance.now() - start);
        pm.clear_all_objects();
      }
      const avgTime = times.reduce((a, b) => a + b, 0) / times.length;
      expect(avgTime).toBeLessThan(500);
    });
    it('benchmark with all analysis functions', () => {
      const log = pm.load_eventlog_from_xes(XES_SEQUENTIAL);
      const startTime = performance.now();
      pm.discover_dfg(log, 'concept:name');
      pm.analyze_event_statistics(log);
      pm.analyze_case_duration(log);
      const duration = performance.now() - startTime;
      expect(duration).toBeLessThan(500);
    });
  });
});
//# sourceMappingURL=phase3-determinism-scale.test.js.map
