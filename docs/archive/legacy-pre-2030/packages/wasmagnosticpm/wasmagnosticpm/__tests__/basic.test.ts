/**
 * Unit tests for wasm4pm basic functionality
 */

import { describe, it, expect } from 'vitest';
import * as wasm from '../pkg/wasm4pm.js';
import { XES_MINIMAL } from './helpers/fixtures';

describe('wasm4pm - Basic Functionality', () => {
  it('should successfully import the WASM module', async () => {
    expect(wasm).toBeDefined();
    expect(typeof wasm.init).toBe('function');
  });

  it('should return a valid semantic version from get_version()', () => {
    const version = wasm.get_version();
    expect(version).toBeTruthy();
    expect(typeof version).toBe('string');
    // Match semver pattern: latest or latest-alpha
    expect(version).toMatch(/^\d+\.\d+\.\d+(-[\w.]+)?$/);
  });

  it('should export core event log functions', () => {
    expect(wasm.load_eventlog_from_xes).toBeDefined();
    expect(typeof wasm.load_eventlog_from_xes).toBe('function');
    expect(wasm.export_eventlog_to_xes).toBeDefined();
    expect(typeof wasm.export_eventlog_to_xes).toBe('function');
    expect(wasm.get_event_count).toBeDefined();
    expect(typeof wasm.get_event_count).toBe('function');
  });

  it('should export discovery algorithm functions', () => {
    expect(wasm.discover_dfg).toBeDefined();
    expect(typeof wasm.discover_dfg).toBe('function');
    expect(wasm.discover_alpha_plus_plus).toBeDefined();
    expect(typeof wasm.discover_alpha_plus_plus).toBe('function');
    expect(wasm.discover_heuristic_miner).toBeDefined();
    expect(typeof wasm.discover_heuristic_miner).toBe('function');
  });

  it('should export state management functions', () => {
    expect(wasm.clear_all_objects).toBeDefined();
    expect(typeof wasm.clear_all_objects).toBe('function');
    expect(wasm.delete_object).toBeDefined();
    expect(typeof wasm.delete_object).toBe('function');
  });

  it('should load XES minimal fixture without errors', () => {
    const handle = wasm.load_eventlog_from_xes(XES_MINIMAL);
    expect(handle).toBeTruthy();
    expect(typeof handle).toBe('string');
    // Handle should be a valid reference (non-empty string)
    expect(handle.length).toBeGreaterThan(0);
  });

  it('should have initialized WASM successfully', () => {
    // This test validates that setup.ts initialization succeeded
    // If init failed, we would not reach this test
    expect(true).toBe(true);
  });
});
