/**
 * State Management Tests
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import * as wasm from '../../pkg/wasm4pm.js';
import { XES_MINIMAL } from '../helpers/fixtures';

describe('State Management - Object Deletion', () => {
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

  it('should delete an object by handle', () => {
    const handle = wasm.load_eventlog_from_xes(XES_MINIMAL);
    const initialCount = wasm.object_count();

    const deleted = wasm.delete_object(handle);
    expect(deleted).toBe(true);

    const finalCount = wasm.object_count();
    expect(finalCount).toBe(initialCount - 1);
  });

  it('should return false when deleting non-existent object', () => {
    const deleted = wasm.delete_object('obj_999999');
    expect(deleted).toBe(false);
  });

  it('should fail to use deleted object handle', () => {
    const handle = wasm.load_eventlog_from_xes(XES_MINIMAL);
    wasm.delete_object(handle);

    expect(() => {
      wasm.export_eventlog_to_xes(handle);
    }).toThrow();
  });
});
