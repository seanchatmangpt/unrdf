/**
 * Type Wrapper Tests
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import * as wasm from '../../pkg/wasm4pm.js';
import { XES_MINIMAL } from '../helpers/fixtures';

describe('Type Wrapper - WasmEventLog', () => {
  beforeEach(async () => {
    try {
      await wasm.init();
      await wasm.clear_all_objects();
    } catch (e) {}
  });

  afterEach(async () => {
    try {
      await wasm.clear_all_objects();
    } catch (e) {}
  });

  it('should create WasmEventLog from handle', () => {
    const handle = wasm.load_eventlog_from_xes(XES_MINIMAL);
    expect(handle).toBeTruthy();

    const wasmLog = new wasm.WasmEventLog(handle);
    expect(wasmLog).toBeTruthy();
  });

  it('should retrieve event count from WasmEventLog', () => {
    const handle = wasm.load_eventlog_from_xes(XES_MINIMAL);
    const wasmLog = new wasm.WasmEventLog(handle);

    expect(() => {
      wasmLog.event_count();
    }).not.toThrow();
  });

  it('should retrieve case count from WasmEventLog', () => {
    const handle = wasm.load_eventlog_from_xes(XES_MINIMAL);
    const wasmLog = new wasm.WasmEventLog(handle);

    expect(() => {
      wasmLog.case_count();
    }).not.toThrow();
  });

  it('should retrieve stats from WasmEventLog', () => {
    const handle = wasm.load_eventlog_from_xes(XES_MINIMAL);
    const wasmLog = new wasm.WasmEventLog(handle);

    expect(() => {
      wasmLog.stats();
    }).not.toThrow();
  });

  it('should fail with invalid handle', () => {
    const wasmLog = new wasm.WasmEventLog('obj_999999');

    expect(() => {
      wasmLog.event_count();
    }).toThrow();
  });
});
