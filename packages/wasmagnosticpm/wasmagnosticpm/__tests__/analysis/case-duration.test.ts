/**
 * Analysis Functions Tests
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import * as wasm from '../../pkg/wasm4pm.js';
import { XES_MINIMAL } from '../helpers/fixtures';

describe('Analysis - Case Duration', () => {
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

  it('should analyze case duration from EventLog', () => {
    const logHandle = wasm.load_eventlog_from_xes(XES_MINIMAL);
    expect(logHandle).toBeTruthy();

    const duration = wasm.analyze_case_duration(logHandle);
    expect(duration).toBeTruthy();
  });

  it('should provide min and max duration', () => {
    const logHandle = wasm.load_eventlog_from_xes(XES_MINIMAL);
    const duration = wasm.analyze_case_duration(logHandle);
    expect(duration).toBeTruthy();
  });

  it('should fail when EventLog handle is invalid', () => {
    expect(() => {
      wasm.analyze_case_duration('obj_999999');
    }).toThrow();
  });
});
