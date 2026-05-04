/**
 * @fileoverview Poka-Yoke Validation Tests
 * @description
 * Verifies error prevention and state management in AtomVM runtimes.
 */

import { describe, it, expect } from 'vitest';
import { AtomVMRuntime } from '../src/atomvm-runtime.mjs';
import { AtomVMNodeRuntime } from '../src/node-runtime.mjs';
import { TerminalUI } from '../src/terminal-ui.mjs';

describe('AtomVMRuntime Poka-Yoke', () => {
  it('should prevent construction with invalid moduleName', () => {
    const terminal = new TerminalUI();
    expect(() => new AtomVMRuntime(terminal, '')).toThrow(/required/i);
    expect(() => new AtomVMRuntime(terminal, null)).toThrow(/required/i);
  });

  it('should prevent operations when not ready', async () => {
    const runtime = new AtomVMRuntime(new TerminalUI(), 'test');
    await expect(runtime.executeBeam('/test.avm')).rejects.toThrow(/not ready/i);
  });

  it('should track lifecycle state correctly', () => {
    const runtime = new AtomVMRuntime(new TerminalUI(), 'test');
    expect(runtime.state).toBe('Uninitialized');
    
    runtime.destroy();
    expect(runtime.state).toBe('Destroyed');
  });
});

describe('AtomVMNodeRuntime Poka-Yoke', () => {
  it('should prevent execution before load', async () => {
    const runtime = new AtomVMNodeRuntime();
    await expect(runtime.execute('/path.avm')).rejects.toThrow(/not ready/i);
  });

  it('should track lifecycle state correctly', () => {
    const runtime = new AtomVMNodeRuntime();
    expect(runtime.state).toBe('Uninitialized');
    
    runtime.destroy();
    expect(runtime.state).toBe('Destroyed');
  });
});
