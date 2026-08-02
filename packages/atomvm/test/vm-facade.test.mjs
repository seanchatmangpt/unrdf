import { describe, it, expect } from 'vitest';
import { Store } from '@unrdf/oxigraph';
import { HardenedAtomVM } from '../src/vm/facade.mjs';
import { AtomVMNodeRuntime } from '../src/node-runtime.mjs';

describe('HardenedAtomVM façade — Chicago boundaries', () => {
  it('constructs the real façade around a real RDF store and native runtime adapter', () => {
    const store = new Store();
    const vm = new HardenedAtomVM(store, { action: 'mcpp:MutateGraph' }, {
      atomvmBinary: process.env.ATOMVM_BIN,
      log: () => {},
      errorLog: () => {},
    });

    expect(vm.store).toBe(store);
    expect(vm.runtime).toBeInstanceOf(AtomVMNodeRuntime);
    expect(vm.bridge.store).toBe(store);
  });

  it('refuses execution before an authentic compilation receipt is admitted', async () => {
    const store = new Store();
    const vm = new HardenedAtomVM(store, {});

    await expect(vm.execute('/unadmitted/module.avm', null, 'agent:chicago', {}))
      .rejects.toThrow(/ConstitutionalViolationError/);
  });
});
