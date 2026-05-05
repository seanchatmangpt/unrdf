/**
 * I/O Operations Tests
 * Tests for loading and exporting EventLogs and OCELs
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import * as wasm from '../../pkg/wasm4pm.js';
import { OCEL_MINIMAL } from '../helpers/fixtures';

describe('I/O Operations - OCEL JSON', () => {
  beforeEach(async () => {
    try {
      await wasm.init();
      await wasm.clear_all_objects();
    } catch (e) {
      // Ignore
    }
  });

  afterEach(async () => {
    try {
      await wasm.clear_all_objects();
    } catch (e) {
      // Ignore
    }
  });

  it('should load a valid OCEL JSON file', () => {
    const handle = wasm.load_ocel_from_json(OCEL_MINIMAL);
    expect(handle).toBeTruthy();
    expect(typeof handle).toBe('string');
  });

  it('should export OCEL to JSON format', () => {
    const loadHandle = wasm.load_ocel_from_json(OCEL_MINIMAL);
    const jsonContent = wasm.export_ocel_to_json(loadHandle);

    expect(jsonContent).toBeTruthy();
    expect(typeof jsonContent).toBe('string');

    const jsonObj = JSON.parse(jsonContent);
    expect(jsonObj).toBeTruthy();
  });

  it('should fail when OCEL handle is invalid', () => {
    expect(() => {
      wasm.export_ocel_to_json('obj_999999');
    }).toThrow();
  });
});
