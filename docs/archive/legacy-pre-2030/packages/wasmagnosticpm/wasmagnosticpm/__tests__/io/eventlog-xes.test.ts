/**
 * I/O Operations Tests
 * Tests for loading and exporting EventLogs and OCELs
 */

import { describe, it, expect } from 'vitest';
import * as wasm from '../../pkg/wasm4pm.js';
import { XES_MINIMAL, XES_SEQUENTIAL } from '../helpers/fixtures';

describe('I/O Operations - EventLog XES', () => {
  it('should load a valid XES file from string content', () => {
    expect(() => {
      const handle = wasm.load_eventlog_from_xes(XES_MINIMAL);
      expect(handle).toBeTruthy();
      expect(typeof handle).toBe('string');
    }).not.toThrow();
  });

  it('should export EventLog to XES format', () => {
    const loadHandle = wasm.load_eventlog_from_xes(XES_MINIMAL);
    const xesContent = wasm.export_eventlog_to_xes(loadHandle);

    expect(xesContent).toBeTruthy();
    expect(typeof xesContent).toBe('string');
    expect(xesContent).toContain('<?xml');
    expect(xesContent).toContain('<log');
  });

  it('should fail when EventLog handle is invalid', () => {
    expect(() => {
      wasm.export_eventlog_to_xes('obj_999999');
    }).toThrow();
  });
});
