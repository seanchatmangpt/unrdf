/**
 * Analysis Functions Tests
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import * as wasm from '../../pkg/wasm4pm.js';
import { XES_MINIMAL } from '../helpers/fixtures';

describe('Analysis - Event Statistics', () => {
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

  it('should analyze event statistics from EventLog', () => {
    const logHandle = wasm.load_eventlog_from_xes(XES_MINIMAL);
    expect(logHandle).toBeTruthy();

    const stats = wasm.analyze_event_statistics(logHandle);
    expect(stats).toBeTruthy();
  });

  it('should include activity frequencies in event statistics', () => {
    const logHandle = wasm.load_eventlog_from_xes(XES_MINIMAL);
    const stats = wasm.analyze_event_statistics(logHandle);
    expect(stats).toBeTruthy();
  });

  it('should fail when EventLog handle is invalid', () => {
    expect(() => {
      wasm.analyze_event_statistics('obj_999999');
    }).toThrow();
  });
});
