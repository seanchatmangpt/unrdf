/**
 * @fileoverview AtomVM Runtime Tests (Chicago-Style Refactor)
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
    // Standard setup
    terminalEl = document.createElement('div');
    terminalEl.id = 'terminal';
    document.body.appendChild(terminalEl);
    
    terminal = new TerminalUI();
    runtime = new AtomVMRuntime(terminal, 'testmodule');
  });

  describe('Lifecycle and State Transitions', () => {
    it('should initialize in Uninitialized state', () => {
      expect(runtime.state).toBe('Uninitialized');
      expect(runtime.isLoaded()).toBe(false);
    });

    it('should transition to Ready after successful loadWASM', async () => {
      // Sociable Mocking of environment requirements
      if (typeof SharedArrayBuffer === 'undefined') {
        global.SharedArrayBuffer = class SharedArrayBuffer {
          constructor(length) { this.byteLength = length; }
        };
      }

      // Simulate AtomVM-node/browser script loading by setting window.Module
      global.window = global.window || {};
      global.window.Module = { 
        ready: true, 
        calledRun: false,
        _AtomVM_init: vi.fn().mockReturnValue(0)
      };

      await runtime.loadWASM();
      
      expect(runtime.state).toBe('Ready');
      expect(runtime.isLoaded()).toBe(true);
      expect(runtime.atomvmModule).toBeDefined();
    });

    it('should throw error when executeBeam is called before Ready', async () => {
      await expect(runtime.executeBeam('/test.avm')).rejects.toThrow(/Runtime not ready/i);
    });

    it('should reach Destroyed state and clear memory on destroy', () => {
      // Set to high-fidelity fake ready state
      runtime.state = 'Ready';
      runtime.atomvmModule = { _free: vi.fn() };
      runtime.memory = new Uint8Array(10);
      
      runtime.destroy();
      
      expect(runtime.state).toBe('Destroyed');
      expect(runtime.isLoaded()).toBe(false);
      expect(runtime.memory).toBeNull();
      expect(runtime.atomvmModule).toBeNull();
    });
  });

  describe('Behavioral Error Handling', () => {
    it('should fail-safe when SharedArrayBuffer is missing', async () => {
      const originalSAB = global.SharedArrayBuffer;
      delete global.SharedArrayBuffer;
      
      try {
        await expect(runtime.loadWASM()).rejects.toThrow(/SharedArrayBuffer not available/i);
        expect(runtime.state).toBe('Error');
      } finally {
        global.SharedArrayBuffer = originalSAB;
      }
    });
  });
});
