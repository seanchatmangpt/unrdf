import { describe, it, expect, beforeEach } from 'vitest';
import { HardenedAtomVM } from '../src/vm/facade.mjs';
import { createStore } from '@unrdf/oxigraph';

describe('Smart Law E2E Verification', () => {
  let store;
  let vm;

  const EX = 'http://example.org/';
  const RDF = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#';
  const XSD = 'http://www.w3.org/2001/XMLSchema#';
  const SH = 'http://www.w3.org/ns/shacl#';

  beforeEach(async () => {
    store = createStore();
    vm = new HardenedAtomVM(store);
  });

  it('should enforce a SHACL shape and block unlawful transactions', async () => {
    // a. Define a SHACL shape for a 'LawfulTransaction'
    // A LawfulTransaction must have exactly one amount of type xsd:integer
    const shape = {
      targetClass: `${EX}LawfulTransaction`,
      properties: [
        {
          path: `${EX}amount`,
          datatype: `${XSD}integer`,
          minCount: 1,
          maxCount: 1
        }
      ]
    };

    // b. Compile it to BEAM using the new SHACLToSPARQLCompiler
    // c. Execute it via HardenedAtomVM
    // Note: enforceRegulatoryShape handles JIT compilation to Erlang/BEAM
    const result = await vm.enforceRegulatoryShape('transaction_validator', shape);
    
    expect(result.success).toBe(true);
    expect(result.moduleName).toBe('transaction_validator');
    expect(result.source).toContain('-module(transaction_validator).');
    expect(result.source).toContain('check(Store) ->');

    // d. Verify that an 'UnlawfulTransaction' is correctly blocked
    
    // Add a Lawful Transaction
    store.add({
      subject: `${EX}tx_ok`,
      predicate: `${RDF}type`,
      object: `${EX}LawfulTransaction`
    });
    store.add({
      subject: `${EX}tx_ok`,
      predicate: `${EX}amount`,
      object: { value: '100', datatype: `${XSD}integer` }
    });

    // Add an Unlawful Transaction (missing amount)
    store.add({
      subject: `${EX}tx_bad`,
      predicate: `${RDF}type`,
      object: `${EX}LawfulTransaction`
    });
    // Missing amount property!

    // In a Smart Law context, "blocked" means the hooks engine 
    // identifies the violation when executing the regulatory hooks.
    const hookResult = await vm.bridge.executeHooks({
      store: store,
      t_ns: BigInt(Date.now() * 1000000),
      nodeId: 'test-node'
    });

    // Verify hook execution
    // Note: We expect the condition to FAIL for the whole store because tx_bad violates it
    // SHACL ASK usually returns false if ANY node of targetClass violates the shape.
    expect(hookResult.success).toBe(true);
    const validationHook = hookResult.conditions.find(c => c.hookName === 'transaction_validator');
    expect(validationHook).toBeDefined();
    
    // If the SHACL shape is violated, the condition should be false
    expect(validationHook.passed).toBe(false);
  });
});
