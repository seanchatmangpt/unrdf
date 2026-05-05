/**
 * State Management Tests
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import * as wasm from '../../pkg/wasm4pm.js';
import { XES_MINIMAL, OCEL_MINIMAL } from '../helpers/fixtures';

describe('State Management - Object Storage', () => {
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

  it('should store EventLog and return a handle', () => {
    const handle = wasm.load_eventlog_from_xes(XES_MINIMAL);

    expect(handle).toBeTruthy();
    expect(typeof handle).toBe('string');
    expect(handle).toMatch(/^obj_\d+$/);
  });

  it('should store OCEL and return a handle', () => {
    const handle = wasm.load_ocel_from_json(OCEL_MINIMAL);

    expect(handle).toBeTruthy();
    expect(typeof handle).toBe('string');
    expect(handle).toMatch(/^obj_\d+$/);
  });

  it('should generate unique handles for different objects', () => {
    const handle1 = wasm.load_eventlog_from_xes(XES_MINIMAL);
    const handle2 = wasm.load_eventlog_from_xes(XES_MINIMAL);

    expect(handle1).not.toBe(handle2);
  });

  it('should track object count correctly', () => {
    const initialCount = wasm.object_count();
    expect(typeof initialCount).toBe('number');

    const handle1 = wasm.load_eventlog_from_xes(XES_MINIMAL);
    const count1 = wasm.object_count();
    expect(count1).toBe(initialCount + 1);
  });
});
