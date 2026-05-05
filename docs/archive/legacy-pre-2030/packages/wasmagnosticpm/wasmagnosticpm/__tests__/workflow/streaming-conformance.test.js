/**
 * Integration tests for process_mining_wasm
 * Tests the complete workflow of the WASM module
 */
import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import * as pm from '../../pkg/wasm4pm.js';
import { XES_WORKFLOW } from '../helpers/fixtures';
describe('Streaming Conformance', () => {
  beforeEach(async () => {
    await pm.init();
    pm.clear_all_objects();
  });
  afterEach(() => {
    pm.clear_all_objects();
  });
  it('should detect conforming traces', () => {
    // Build a reference DFG from a log where A→B→C is the only path
    const logHandle = pm.load_eventlog_from_xes(XES_WORKFLOW);
    const dfgJson = pm.discover_dfg(logHandle, 'concept:name');
    const dfgHandle = pm.store_dfg_from_json(dfgJson);
    const sessionHandle = pm.streaming_conformance_begin(dfgHandle);
    expect(sessionHandle).toBeTruthy();
    // Add a conforming trace (A→B→C matches the DFG)
    pm.streaming_conformance_add_event(sessionHandle, 'case-x', 'Activity A');
    pm.streaming_conformance_add_event(sessionHandle, 'case-x', 'Activity B');
    pm.streaming_conformance_add_event(sessionHandle, 'case-x', 'Activity C');
    const result = JSON.parse(pm.streaming_conformance_close_trace(sessionHandle, 'case-x'));
    expect(result.ok).toBe(true);
    expect(result.is_conforming).toBe(true);
    expect(result.fitness).toBe(1.0);
    expect(result.deviations).toHaveLength(0);
    pm.streaming_conformance_finalize(sessionHandle);
  });
  it('should detect non-conforming traces', () => {
    const logHandle = pm.load_eventlog_from_xes(XES_WORKFLOW);
    const dfgHandle = pm.store_dfg_from_json(pm.discover_dfg(logHandle, 'concept:name'));
    const sessionHandle = pm.streaming_conformance_begin(dfgHandle);
    // Add a non-conforming trace (C→A is not in the DFG)
    pm.streaming_conformance_add_event(sessionHandle, 'bad-case', 'Activity C');
    pm.streaming_conformance_add_event(sessionHandle, 'bad-case', 'Activity A');
    const result = JSON.parse(pm.streaming_conformance_close_trace(sessionHandle, 'bad-case'));
    expect(result.ok).toBe(true);
    expect(result.is_conforming).toBe(false);
    expect(result.deviations.length).toBeGreaterThan(0);
    pm.streaming_conformance_finalize(sessionHandle);
  });
  it('should report stats for open session', () => {
    const logHandle = pm.load_eventlog_from_xes(XES_WORKFLOW);
    const dfgHandle = pm.store_dfg_from_json(pm.discover_dfg(logHandle, 'concept:name'));
    const sessionHandle = pm.streaming_conformance_begin(dfgHandle);
    pm.streaming_conformance_add_event(sessionHandle, 'c1', 'Activity A');
    pm.streaming_conformance_add_event(sessionHandle, 'c1', 'Activity B');
    pm.streaming_conformance_close_trace(sessionHandle, 'c1');
    const stats = JSON.parse(pm.streaming_conformance_stats(sessionHandle));
    expect(stats.event_count).toBe(2);
    expect(stats.closed_traces).toBe(1);
    expect(stats.open_traces).toBe(0);
    pm.streaming_conformance_finalize(sessionHandle);
  });
  it('should finalize and return summary', () => {
    const logHandle = pm.load_eventlog_from_xes(XES_WORKFLOW);
    const dfgHandle = pm.store_dfg_from_json(pm.discover_dfg(logHandle, 'concept:name'));
    const sessionHandle = pm.streaming_conformance_begin(dfgHandle);
    pm.streaming_conformance_add_event(sessionHandle, 'c1', 'Activity A');
    pm.streaming_conformance_add_event(sessionHandle, 'c1', 'Activity B');
    pm.streaming_conformance_close_trace(sessionHandle, 'c1');
    const summary = JSON.parse(pm.streaming_conformance_finalize(sessionHandle));
    expect(summary.total_traces).toBe(1);
    expect(typeof summary.avg_fitness).toBe('number');
    expect(Array.isArray(summary.results)).toBe(true);
  });
});
//# sourceMappingURL=streaming-conformance.test.js.map
