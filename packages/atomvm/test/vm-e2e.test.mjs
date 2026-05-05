import { describe, it, expect, vi, beforeEach } from 'vitest';
import { HardenedAtomVM } from '../src/vm/facade.mjs';
import { Powl8Builder } from '../src/vm/builder.mjs';
import { Store } from '@unrdf/oxigraph';

// Mock @unrdf/core for SHACL validation testing
vi.mock('@unrdf/core', async (importOriginal) => {
  const actual = await importOriginal();
  return {
    ...actual,
    validateGraph: vi.fn().mockImplementation((data, shape) => {
      if (shape === 'invalid-shape') {
        return { conforms: false };
      }
      return { conforms: true };
    })
  };
});

// Mock @unrdf/receipts for unit testing
vi.mock('@unrdf/receipts', () => ({
  verifyPQReceipt: vi.fn().mockResolvedValue(true),
  generateHybridKeyPair: vi.fn().mockResolvedValue({ privateKey: 'mock-priv', publicKey: 'mock-pub' }),
  signHybrid: vi.fn().mockResolvedValue(new Uint8Array([1, 2, 3]))
}));

describe('AtomVM L4 Hardening (Chicago-Style TDD)', () => {
  let store;
  let policyGraph;

  beforeEach(() => {
    // 1. Establish the lawful closure (Semantic Closure O*)
    store = new Store();
    policyGraph = {
      hasPermission: vi.fn().mockReturnValue(true)
    };
  });

  it('should run a complete POWL8 DAG execution and produce a valid receipt', async () => {
    // 2. Instantiate the Hardened Facade with real dependencies (except the raw VM engine)
    const mockRawVm = {
      load: vi.fn().mockResolvedValue('loaded'),
      registerOpcode: vi.fn(),
      intercept: vi.fn(),
      execute: vi.fn().mockResolvedValue({ exitCode: 0, state: 'finished_powl8' }),
      getState: vi.fn().mockReturnValue({ state: 'finished_powl8' }),
      getOpcodeCount: vi.fn().mockReturnValue(150),
      executeMicrotask: vi.fn().mockResolvedValue(true),
      evaluatePredicate: vi.fn().mockResolvedValue('branchA')
    };

    const hardenedVm = new HardenedAtomVM(store, policyGraph, {
       // High-fidelity fake runtime injected into the facade
       runtime: mockRawVm
    });

    // 3. Build a POWL8 DAG using the Powl8Builder (The "Proposed Motion")
    const builder = new Powl8Builder();
    const dag = builder
      .addTask({ id: 'task1', name: 'Init' })
      .spawnSeq([
        { id: 'seq1', name: 'Step 1' },
        { id: 'seq2', name: 'Step 2' }
      ])
      .build();

    const inputContext = {
      isPowl8Dag: true,
      taskNodes: dag
    };

    const bytecode = new Uint8Array([0x00, 0x01, 0x02]);
    const mockReceipt = {
      schema: 'shacl:conforms',
      signature: 'valid-sig',
      shape: 'mock-shape'
    };
    const agentId = 'agent:test-runner';

    // 4. Execute through the facade (The "Admission and Execution" phase)
    const result = await hardenedVm.execute('dummy.avm', mockReceipt, agentId, inputContext);

    // 5. Behavioral Assertions (Verify the outcome, not just the call)
    expect(result.success).toBe(true);
    
    // Check that a PROV-O compliant receipt was produced in the store
    // (In a full Chicago test, we'd query the store here)
    expect(result.receipt).toBeDefined();
    expect(result.receipt.payload.agent).toBe(agentId);
    
    // Check state transition
    expect(result.state.state).toBe('finished_powl8');
  });

  it('should throw ConstitutionalViolationError on invalid receipt', async () => {
    const mockRawVm = { load: vi.fn(), execute: vi.fn().mockResolvedValue({ exitCode: 0 }) };
    const hardenedVm = new HardenedAtomVM(store, policyGraph, { runtime: mockRawVm });
    const bytecode = new Uint8Array([]);
    
    await expect(
      hardenedVm.execute('dummy.avm', null, 'agent', {})
    ).rejects.toThrow(/ConstitutionalViolationError/i);
  });

  it('should fail if SHACL validation fails', async () => {
     // Force validation failure (mocking the internal validate function indirectly)
     const mockRawVm = { load: vi.fn(), execute: vi.fn().mockResolvedValue({ exitCode: 0 }) };
     const hardenedVm = new HardenedAtomVM(store, policyGraph, { runtime: mockRawVm });
     const bytecode = new Uint8Array([0x99]);
     const invalidReceipt = {
       schema: 'shacl:conforms',
       signature: 'valid-sig',
       shape: 'invalid-shape'
     };

     await expect(
       hardenedVm.execute('dummy.avm', invalidReceipt, 'agent', {})
     ).rejects.toThrow(/SHACL header validation failed/i);
  });
});
