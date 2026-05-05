/**
 * State Management Tests
 */

import { describe, it, expect, beforeEach } from 'vitest';
import * as wasm from '../../pkg/wasm4pm.js';
import { XES_MINIMAL } from '../helpers/fixtures';

describe('State Management - Clear All Objects', () => {
  beforeEach(async () => {
    try {
      await wasm.init();
      await wasm.clear_all_objects();
    } catch (e) {}
  });

  it('should clear all objects from state', () => {
    wasm.load_eventlog_from_xes(XES_MINIMAL);
    const countBefore = wasm.object_count();
    expect(countBefore).toBeGreaterThan(0);

    wasm.clear_all_objects();

    const countAfter = wasm.object_count();
    expect(countAfter).toBe(0);
  });

  it('should work when clearing empty state', () => {
    expect(() => {
      wasm.clear_all_objects();
    }).not.toThrow();

    const count = wasm.object_count();
    expect(count).toBe(0);
  });
});
