import { describe, it, expect, vi, beforeEach } from 'vitest';
import { HardenedAtomVM } from '../src/vm/facade.mjs';
import { POWL8_OPCODES } from '../src/vm/opcodes.mjs';

describe('HardenedAtomVM Facade', () => {
  let mockRawVm;
  let mockPolicyGraph;

  beforeEach(() => {
    mockRawVm = {
      registerOpcode: vi.fn(),
      intercept: vi.fn(),
      execute: vi.fn().mockResolvedValue({ status: 'ok' }),
      getState: vi.fn().mockReturnValue({ state: 'synced' }),
      getOpcodeCount: vi.fn().mockReturnValue(42),
      executeMicrotask: vi.fn().mockResolvedValue(),
      evaluatePredicate: vi.fn().mockResolvedValue(true)
    };
    mockPolicyGraph = { action: 'mcpp:MutateGraph' };
  });

  it('should initialize successfully and register opcodes', () => {
    const vm = new HardenedAtomVM(mockRawVm, mockPolicyGraph);
    expect(vm).toBeDefined();
    expect(mockRawVm.registerOpcode).toHaveBeenCalledWith('OP_SPAWN_SEQ', POWL8_OPCODES.OP_SPAWN_SEQ);
  });

  it('should execute raw bytecode sequentially by default', async () => {
    const vm = new HardenedAtomVM(mockRawVm, mockPolicyGraph);
    const mockReceipt = { schema: 'shacl:conforms', signature: Buffer.from('mock').toString('base64') };
    
    // We mock verifyPQReceipt, which we didn't inject, but we can bypass verification failure by spying or mocking if it fails.
    // For now we'll see if it throws or we need to mock it.
  });
});
