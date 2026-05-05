/**
 * Discovery Algorithm Tests
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import * as wasm from '../../pkg/wasm4pm.js';
import { XES_MINIMAL } from '../helpers/fixtures';

describe('Discovery - DFG (Directly-Follows Graph)', () => {
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

  it('should discover DFG from EventLog', () => {
    const logHandle = wasm.load_eventlog_from_xes(XES_MINIMAL);
    expect(logHandle).toBeTruthy();

    const dfg = wasm.discover_dfg(logHandle, 'concept:name');
    expect(dfg).toBeTruthy();
  });

  it('should fail when EventLog handle is invalid', () => {
    expect(() => {
      wasm.discover_dfg('obj_999999', 'concept:name');
    }).toThrow();
  });
});
