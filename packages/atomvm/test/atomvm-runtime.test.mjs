/** @vitest-environment jsdom */
import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { AtomVMRuntime } from '../src/atomvm-runtime.mjs';
import { TerminalUI } from '../src/terminal-ui.mjs';

describe('AtomVM browser runtime — observable state', () => {
  let runtime;
  let originalSharedArrayBuffer;

  beforeEach(() => {
    document.body.innerHTML = '<div id="terminal"></div>';
    originalSharedArrayBuffer = globalThis.SharedArrayBuffer;
    runtime = new AtomVMRuntime(new TerminalUI(), 'testmodule');
  });

  afterEach(() => {
    if (originalSharedArrayBuffer === undefined) delete globalThis.SharedArrayBuffer;
    else globalThis.SharedArrayBuffer = originalSharedArrayBuffer;
    document.body.innerHTML = '';
  });

  it('starts uninitialized and refuses execution before loading', async () => {
    expect(runtime.state).toBe('Uninitialized');
    expect(runtime.isLoaded()).toBe(false);
    await expect(runtime.executeBeam('/test.avm')).rejects.toThrow(/Runtime not ready/i);
  });

  it('destroys its own memory and runtime state', () => {
    runtime.state = 'Ready';
    runtime.memory = new Uint8Array(10);
    runtime.atomvmModule = null;

    runtime.destroy();

    expect(runtime.state).toBe('Destroyed');
    expect(runtime.isLoaded()).toBe(false);
    expect(runtime.memory).toBeNull();
    expect(runtime.atomvmModule).toBeNull();
  });

  it('moves to Error when SharedArrayBuffer is genuinely unavailable', async () => {
    delete globalThis.SharedArrayBuffer;
    await expect(runtime.loadWASM()).rejects.toThrow(/SharedArrayBuffer not available/i);
    expect(runtime.state).toBe('Error');
  });
});
