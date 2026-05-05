/**
 * Type Wrapper Tests
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import * as wasm from '../../pkg/wasm4pm.js';
import { OCEL_MINIMAL } from '../helpers/fixtures';

describe('Type Wrapper - WasmOCEL', () => {
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

  it('should create WasmOCEL from handle', () => {
    const handle = wasm.load_ocel_from_json(OCEL_MINIMAL);
    expect(handle).toBeTruthy();

    const wasmOCEL = new wasm.WasmOCEL(handle);
    expect(wasmOCEL).toBeTruthy();
  });

  it('should retrieve event count from WasmOCEL', () => {
    const handle = wasm.load_ocel_from_json(OCEL_MINIMAL);
    const wasmOCEL = new wasm.WasmOCEL(handle);

    expect(() => {
      wasmOCEL.event_count();
    }).not.toThrow();
  });

  it('should retrieve object count from WasmOCEL', () => {
    const handle = wasm.load_ocel_from_json(OCEL_MINIMAL);
    const wasmOCEL = new wasm.WasmOCEL(handle);

    expect(() => {
      wasmOCEL.object_count();
    }).not.toThrow();
  });

  it('should retrieve stats from WasmOCEL', () => {
    const handle = wasm.load_ocel_from_json(OCEL_MINIMAL);
    const wasmOCEL = new wasm.WasmOCEL(handle);

    expect(() => {
      wasmOCEL.stats();
    }).not.toThrow();
  });

  it('should fail with invalid handle', () => {
    const wasmOCEL = new wasm.WasmOCEL('obj_999999');

    expect(() => {
      wasmOCEL.event_count();
    }).toThrow();
  });
});
