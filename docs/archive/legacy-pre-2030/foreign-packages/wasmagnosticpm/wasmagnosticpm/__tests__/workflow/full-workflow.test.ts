/**
 * Integration tests for process_mining_wasm
 * Tests the complete workflow of the WASM module
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import * as pm from '../../pkg/wasm4pm.js';
import { XES_WORKFLOW, OCEL_MINIMAL } from '../helpers/fixtures';

// OCEL sample for testing (separate from minimal for workflow)
const SAMPLE_OCEL = `{
  "event_types": ["CreateOrder", "ProcessOrder"],
  "object_types": ["Order"],
  "events": [
    {
      "id": "e1",
      "event_type": "CreateOrder",
      "timestamp": "2023-01-01T10:00:00",
      "attributes": {},
      "object_ids": ["o1"]
    },
    {
      "id": "e2",
      "event_type": "ProcessOrder",
      "timestamp": "2023-01-01T10:05:00",
      "attributes": {},
      "object_ids": ["o1"]
    }
  ],
  "objects": [
    {
      "id": "o1",
      "object_type": "Order",
      "attributes": {}
    }
  ]
}`;

describe('Process Mining WASM - Integration Tests', () => {
  beforeEach(async () => {
    try {
      await pm.init();
      await pm.clear_all_objects();
    } catch (e) {}
  });

  afterEach(async () => {
    try {
      await pm.clear_all_objects();
    } catch (e) {}
  });

  it('should initialize module and get version', () => {
    const version = pm.get_version();
    expect(version).toBeTruthy();
    expect(typeof version).toBe('string');
  });

  it('should load XES event log and return handle', () => {
    const logHandle = pm.load_eventlog_from_xes(XES_WORKFLOW);
    expect(logHandle).toBeTruthy();
    expect(typeof logHandle).toBe('string');
  });

  it('should track object count in state', () => {
    const initialCount = pm.object_count();
    const logHandle = pm.load_eventlog_from_xes(XES_WORKFLOW);
    const newCount = pm.object_count();
    expect(newCount).toBeGreaterThan(initialCount);
  });

  it('should analyze event statistics', () => {
    const logHandle = pm.load_eventlog_from_xes(XES_WORKFLOW);
    const stats = pm.analyze_event_statistics(logHandle);
    expect(stats).toBeTruthy();
  });

  it('should analyze case duration', () => {
    const logHandle = pm.load_eventlog_from_xes(XES_WORKFLOW);
    const durations = pm.analyze_case_duration(logHandle);
    expect(durations).toBeTruthy();
  });

  it('should discover DFG from EventLog', () => {
    const logHandle = pm.load_eventlog_from_xes(XES_WORKFLOW);
    const dfg = pm.discover_dfg(logHandle, 'concept:name');
    expect(dfg).toBeTruthy();
  });

  it('should export EventLog to XES format', () => {
    const logHandle = pm.load_eventlog_from_xes(XES_WORKFLOW);
    const exportedXes = pm.export_eventlog_to_xes(logHandle);
    expect(exportedXes).toBeTruthy();
    expect(exportedXes).toContain('<?xml');
    expect(exportedXes).toContain('</log>');
  });

  it('should load OCEL from JSON', () => {
    const ocelHandle = pm.load_ocel_from_json(SAMPLE_OCEL);
    expect(ocelHandle).toBeTruthy();
    expect(typeof ocelHandle).toBe('string');
  });

  it('should delete objects by handle', () => {
    const logHandle = pm.load_eventlog_from_xes(XES_WORKFLOW);
    const countBefore = pm.object_count();
    const deleted = pm.delete_object(logHandle);
    expect(deleted).toBe(true);
    const countAfter = pm.object_count();
    expect(countAfter).toBeLessThan(countBefore);
  });

  it('should clear all objects from state', () => {
    pm.load_eventlog_from_xes(XES_WORKFLOW);
    const countBefore = pm.object_count();
    expect(countBefore).toBeGreaterThan(0);

    pm.clear_all_objects();
    const countAfter = pm.object_count();
    expect(countAfter).toBe(0);
  });

  it('should list available discovery algorithms', () => {
    const algos = pm.available_discovery_algorithms();
    expect(algos).toBeTruthy();
  });

  it('should list available analysis functions', () => {
    const funcs = pm.available_analysis_functions();
    expect(funcs).toBeTruthy();
  });
});
