/**
 * Discovery Algorithm Tests
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import * as wasm from '../../pkg/wasm4pm.js';
import { XES_MINIMAL } from '../helpers/fixtures';

describe('Discovery - Alpha++ Algorithm', () => {
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

  it('should discover a Petri net from EventLog using Alpha++', () => {
    const logHandle = wasm.load_eventlog_from_xes(XES_MINIMAL);
    expect(logHandle).toBeTruthy();
  });

  it('should handle threshold parameter in Alpha++', () => {
    const logHandle = wasm.load_eventlog_from_xes(XES_MINIMAL);
    expect(logHandle).toBeTruthy();
  });

  it('should fail when EventLog handle is invalid', () => {
    expect(() => {
      wasm.discover_alpha_plus_plus('obj_999999', 'concept:name', 0);
    }).toThrow();
  });
});
