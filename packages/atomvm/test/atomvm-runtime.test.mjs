/**
 * @fileoverview AtomVM Runtime Tests
 * @vitest-environment jsdom
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import { AtomVMRuntime } from '../src/atomvm-runtime.mjs';
import { TerminalUI } from '../src/terminal-ui.mjs';

describe('AtomVMRuntime', () => {
  let runtime;
  let terminal;
  let terminalEl;

  beforeEach(() => {
    // Create mock terminal
    terminalEl = document.createElement('div');
    terminalEl.id = 'terminal';
    document.body.appendChild(terminalEl);
    
    terminal = new TerminalUI();
    runtime = new AtomVMRuntime(terminal, 'testmodule');
  });

  it('should create runtime instance', () => {
    expect(runtime).toBeDefined();
    expect(runtime.terminal).toBe(terminal);
    expect(runtime.atomvmModule).toBeNull();
    expect(runtime.isLoaded()).toBe(false);
    expect(runtime.state).toBe('Uninitialized');
    expect(runtime.memory).toBeNull();
    expect(runtime.moduleName).toBe('testmodule');
  });

  describe('loadWASM', () => {
    it('should throw error when SharedArrayBuffer not available', async () => {
      delete global.SharedArrayBuffer;
      
      await expect(runtime.loadWASM()).rejects.toThrow('SharedArrayBuffer not available');
    });

    it('should load AtomVM module when SharedArrayBuffer available', async () => {
      // Use real SharedArrayBuffer if available, otherwise create a mock that works
      if (typeof SharedArrayBuffer === 'undefined') {
        // Create a mock that behaves like SharedArrayBuffer
        const mockBuffer = new ArrayBuffer(1024 * 1024);
        global.SharedArrayBuffer = function(length) {
          const buffer = new ArrayBuffer(length);
          Object.defineProperty(buffer, 'byteLength', {
            value: length,
            writable: false,
          });
          return buffer;
        };
        global.SharedArrayBuffer.prototype = ArrayBuffer.prototype;
      }
      
      // Mock window.Module for Emscripten pattern
      global.window = global.window || {};
      global.window.Module = { ready: true, calledRun: false };
      global.document = global.document || { createElement: () => ({ addEventListener: () => {} }), head: { appendChild: () => {} }, querySelector: () => null };
      
      await runtime.loadWASM();
      
      expect(runtime.isLoaded()).toBe(true);
      expect(runtime.state).toBe('Ready');
      expect(runtime.atomvmModule).toBeDefined();
    });

    it('should log progress messages', async () => {
      global.SharedArrayBuffer = class SharedArrayBuffer {
        constructor() {
          this.byteLength = 1024;
        }
      };
      
      const logSpy = vi.spyOn(terminal, 'log');
      
      await runtime.loadWASM();
      
      expect(logSpy).toHaveBeenCalledWith(
        'Checking SharedArrayBuffer support...',
        'info'
      );
      expect(logSpy).toHaveBeenCalledWith(
        'SharedArrayBuffer confirmed available ✓',
        'success'
      );
    });
  });

  describe('runExample', () => {
    it('should throw error when runtime not initialized', async () => {
      await expect(runtime.runExample()).rejects.toThrow('Runtime not ready');
    });

    it('should run example when initialized', async () => {
      global.SharedArrayBuffer = class SharedArrayBuffer {
        constructor() {
          this.byteLength = 1024;
        }
      };
      
      // Mock window.Module and fetch for Emscripten pattern
      global.window = global.window || {};
      global.window.Module = { ready: true, calledRun: false, callMain: vi.fn(), onExit: null };
      global.document = global.document || { createElement: () => ({ addEventListener: () => {} }), head: { appendChild: () => {} }, querySelector: () => null };
      global.fetch = vi.fn(() => Promise.resolve({ ok: true, status: 200 }));
      
      await runtime.loadWASM();
      
      const logSpy = vi.spyOn(terminal, 'log');
      
      // Mock executeBeam to avoid actual execution
      runtime.executeBeam = vi.fn(() => Promise.resolve({ status: 'ok', exitCode: 0 }));
      
      await runtime.runExample();
      
      expect(logSpy).toHaveBeenCalled();
      expect(runtime.executeBeam).toHaveBeenCalledWith('/testmodule.avm');
    });
  });

  describe('executeBeam', () => {
    it('should throw error when AtomVM module not loaded', async () => {
      const avmPath = '/testmodule.avm';
      
      await expect(runtime.executeBeam(avmPath)).rejects.toThrow('Runtime not ready');
    });
  });

  describe('destroy', () => {
    it('should clean up resources', () => {
      runtime.atomvmModule = {};
      runtime.state = 'Ready';
      runtime.memory = new Uint8Array(10);
      
      runtime.destroy();
      
      expect(runtime.atomvmModule).toBeNull();
      expect(runtime.state).toBe('Destroyed');
      expect(runtime.isLoaded()).toBe(false);
      expect(runtime.memory).toBeNull();
    });
  });
});

