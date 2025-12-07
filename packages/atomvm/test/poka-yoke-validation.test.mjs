/**
 * @fileoverview Poka-Yoke Validation Tests
 * @description
 * Tests that verify error prevention mechanisms work correctly.
 * These tests attempt invalid operations and verify they fail appropriately.
 */

import { describe, it, expect } from 'vitest';
import { AtomVMRuntime } from '../src/atomvm-runtime.mjs';
import { AtomVMNodeRuntime } from '../src/node-runtime.mjs';
import { TerminalUI } from '../src/terminal-ui.mjs';

describe('Poka-Yoke: AtomVMRuntime Error Prevention', () => {
  it('should prevent construction with empty moduleName', () => {
    const terminal = new TerminalUI();
    
    // Attempt invalid construction - should throw
    expect(() => new AtomVMRuntime(terminal, '')).toThrow('moduleName is required');
    expect(() => new AtomVMRuntime(terminal, null)).toThrow('moduleName is required');
    expect(() => new AtomVMRuntime(terminal, undefined)).toThrow('moduleName is required');
  });

  it('should prevent executeBeam before loadWASM', async () => {
    const terminal = new TerminalUI();
    const runtime = new AtomVMRuntime(terminal, 'testmodule');
    
    // Attempt invalid operation - should throw
    await expect(runtime.executeBeam('/test.avm')).rejects.toThrow('Runtime not ready');
  });

  it('should prevent runExample before loadWASM', async () => {
    const terminal = new TerminalUI();
    const runtime = new AtomVMRuntime(terminal, 'testmodule');
    
    // Attempt invalid operation - should throw
    await expect(runtime.runExample()).rejects.toThrow('Runtime not ready');
  });

  it('should prevent executeBeam with empty avmPath', async () => {
    const terminal = new TerminalUI();
    const runtime = new AtomVMRuntime(terminal, 'testmodule');
    
    // Mock loaded state (for testing validation)
    runtime.state = 'Ready';
    runtime.atomvmModule = {};
    
    // Attempt invalid input - should throw
    await expect(runtime.executeBeam('')).rejects.toThrow('avmPath is required');
    await expect(runtime.executeBeam(null)).rejects.toThrow('avmPath is required');
  });

  it('should prevent operations after destroy', async () => {
    const terminal = new TerminalUI();
    const runtime = new AtomVMRuntime(terminal, 'testmodule');
    
    runtime.destroy();
    
    // Attempt invalid operation - should throw
    await expect(runtime.loadWASM()).rejects.toThrow('Runtime has been destroyed');
  });

  it('should track state correctly', () => {
    const terminal = new TerminalUI();
    const runtime = new AtomVMRuntime(terminal, 'testmodule');
    
    // Initial state
    expect(runtime.state).toBe('Uninitialized');
    expect(runtime.isReady()).toBe(false);
    expect(runtime.isLoaded()).toBe(false);
    
    // After destroy
    runtime.destroy();
    expect(runtime.state).toBe('Destroyed');
    expect(runtime.isReady()).toBe(false);
    expect(runtime.isLoaded()).toBe(false);
  });
});

describe('Poka-Yoke: AtomVMNodeRuntime Error Prevention', () => {
  it('should prevent execute before load', async () => {
    const runtime = new AtomVMNodeRuntime();
    
    // Attempt invalid operation - should throw
    await expect(runtime.execute('/test.avm')).rejects.toThrow('Runtime not ready');
  });

  it('should prevent execute with empty avmPath', async () => {
    const runtime = new AtomVMNodeRuntime();
    
    // Mock loaded state (for testing validation)
    runtime.state = 'Ready';
    runtime.atomvmPath = '/path/to/atomvm.js';
    
    // Attempt invalid input - should throw
    await expect(runtime.execute('')).rejects.toThrow('avmPath is required');
    await expect(runtime.execute(null)).rejects.toThrow('avmPath is required');
  });

  it('should prevent operations after destroy', async () => {
    const runtime = new AtomVMNodeRuntime();
    
    runtime.destroy();
    
    // Attempt invalid operation - should throw
    await expect(runtime.load()).rejects.toThrow('Runtime has been destroyed');
  });

  it('should track state correctly', () => {
    const runtime = new AtomVMNodeRuntime();
    
    // Initial state
    expect(runtime.state).toBe('Uninitialized');
    expect(runtime.isReady()).toBe(false);
    expect(runtime.isLoaded()).toBe(false);
    
    // After destroy
    runtime.destroy();
    expect(runtime.state).toBe('Destroyed');
    expect(runtime.isReady()).toBe(false);
    expect(runtime.isLoaded()).toBe(false);
  });
});

