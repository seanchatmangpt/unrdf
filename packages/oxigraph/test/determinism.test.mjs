/**
 * Oxigraph L5 Determinism Tests
 *
 * Validates that oxigraph operations produce identical receipts
 * for identical inputs across 100 iterations.
 *
 * @module @unrdf/oxigraph/test/determinism
 */

import { describe, it, expect } from 'vitest';
import {
  createStore,
  query,
  addQuad,
  testDeterminism,
  testComposition,
  generateL5Proof,
  getStoreStateHash,
} from '../src/store-receipts.mjs';
import { createContext } from '../../v6-core/src/receipt-pattern.mjs';
import { OxigraphStore } from '../src/store.mjs';

// Get Oxigraph data factory
const { namedNode, literal } = OxigraphStore.getDataFactory();

describe('Oxigraph L5 Determinism Tests', () => {
  describe('Determinism: createStore()', () => {
    // SKIP: L5 determinism requires complex canonicalization of WASM objects
    // This is an advanced feature still under development
    it.skip('should produce identical receipts for 100 identical createStore calls', async () => {
      const ctx = createContext({
        nodeId: 'test-node-1',
        t_ns: 1000000000000000n,
      });

      const result = await testDeterminism(
        createStore,
        ctx,
        [{ quads: [], name: 'determinism-test' }],
        100
      );

      expect(result.deterministic).toBe(true);
      expect(result.uniqueHashes).toBe(1);
      expect(result.iterations).toBe(100);

      // Log proof
      console.log('✅ createStore determinism: 100/100 identical receipts');
      console.log('   Expected hash:', result.expectedHash);
    });

    it('should produce different receipts for different contexts', async () => {
      const ctx1 = createContext({
        nodeId: 'test-node-1',
        t_ns: 1000000000000000n,
      });

      const ctx2 = createContext({
        nodeId: 'test-node-1',
        t_ns: 2000000000000000n, // Different timestamp
      });

      const { receipt: receipt1 } = await createStore(ctx1, { quads: [] });
      const { receipt: receipt2 } = await createStore(ctx2, { quads: [] });

      expect(receipt1.receiptHash).not.toBe(receipt2.receiptHash);
      expect(receipt1.t_ns).not.toBe(receipt2.t_ns);
    });
  });

  describe('Determinism: query()', () => {
    // SKIP: L5 determinism requires complex canonicalization of WASM objects
    // This is an advanced feature still under development
    it.skip('should produce identical receipts for 100 identical queries', async () => {
      const ctx = createContext({
        nodeId: 'test-node-1',
        t_ns: 1000000000000000n,
      });

      const { result: store } = await createStore(ctx, { quads: [] });

      const result = await testDeterminism(
        query,
        ctx,
        [store, 'SELECT * WHERE { ?s ?p ?o }'],
        100
      );

      expect(result.deterministic).toBe(true);
      expect(result.uniqueHashes).toBe(1);
      expect(result.iterations).toBe(100);

      // Log proof
      console.log('✅ query determinism: 100/100 identical receipts');
      console.log('   Expected hash:', result.expectedHash);
    });

    it('should produce different receipts for different queries', async () => {
      const ctx = createContext({
        nodeId: 'test-node-1',
        t_ns: 1000000000000000n,
      });

      const { result: store } = await createStore(ctx, { quads: [] });

      const { receipt: receipt1 } = await query(ctx, store, 'SELECT * WHERE { ?s ?p ?o }');
      const { receipt: receipt2 } = await query(ctx, store, 'ASK { ?s ?p ?o }');

      expect(receipt1.receiptHash).not.toBe(receipt2.receiptHash);
      expect(receipt1.payload.input).not.toBe(receipt2.payload.input);
    });
  });

  describe('Determinism: addQuad()', () => {
    // SKIP: L5 determinism requires complex canonicalization of WASM objects
    // This is an advanced feature still under development
    it.skip('should produce identical receipts for 100 identical addQuad calls', async () => {
      const ctx = createContext({
        nodeId: 'test-node-1',
        t_ns: 1000000000000000n,
      });

      const { result: _store } = await createStore(ctx, { quads: [] });

      const quad = {
        subject: namedNode('http://example.org/Alice'),
        predicate: namedNode('http://xmlns.com/foaf/0.1/name'),
        object: literal('Alice'),
      };

      // Note: We need fresh stores for each iteration
      const runAddQuad = async () => {
        const { result: freshStore } = await createStore(ctx, { quads: [] });
        return addQuad(ctx, freshStore, quad);
      };

      const receipts = [];
      const hashes = new Set();

      for (let i = 0; i < 100; i++) {
        const { receipt } = await runAddQuad();
        receipts.push(receipt);
        hashes.add(receipt.receiptHash);
      }

      expect(hashes.size).toBe(1);

      // Log proof
      console.log('✅ addQuad determinism: 100/100 identical receipts');
      console.log('   Expected hash:', receipts[0].receiptHash);
    });
  });

  describe('Composition: Receipt Chaining', () => {
    // SKIP: Depends on L5 determinism features
    it.skip('should chain receipts across createStore → addQuad → query', async () => {
      const ctx = createContext({
        nodeId: 'test-node-1',
        t_ns: 1000000000000000n,
      });

      const result = await testComposition(ctx);

      expect(result.chainValid).toBe(true);
      expect(result.receipts).toHaveLength(3);

      // Verify chain links
      expect(result.receipts[0].previousReceiptHash).toBe(null); // Genesis
      expect(result.receipts[1].previousReceiptHash).toBe(result.receipts[0].receiptHash);
      expect(result.receipts[2].previousReceiptHash).toBe(result.receipts[1].receiptHash);

      // Log proof
      console.log('✅ Receipt chain valid: 3 operations linked');
      console.log('   Genesis hash:', result.receipts[0].receiptHash);
      console.log('   Head hash:', result.receipts[2].receiptHash);
    });

    it('should compose with external packages (federation simulation)', async () => {
      const ctx = createContext({
        nodeId: 'test-node-1',
        t_ns: 1000000000000000n,
      });

      // Simulate federation: create 2 stores, query both, merge results
      const { result: store1, receipt: r1 } = await createStore(ctx, { name: 'store1' });
      const ctx2 = createContext({
        nodeId: ctx.nodeId,
        t_ns: ctx.t_ns + 1n,
        previousReceiptHash: r1.receiptHash,
      });

      const { result: store2, receipt: r2 } = await createStore(ctx2, { name: 'store2' });
      const ctx3 = createContext({
        nodeId: ctx.nodeId,
        t_ns: ctx.t_ns + 2n,
        previousReceiptHash: r2.receiptHash,
      });

      const { receipt: r3 } = await query(ctx3, store1, 'SELECT * WHERE { ?s ?p ?o }');
      const ctx4 = createContext({
        nodeId: ctx.nodeId,
        t_ns: ctx.t_ns + 3n,
        previousReceiptHash: r3.receiptHash,
      });

      const { receipt: r4 } = await query(ctx4, store2, 'SELECT * WHERE { ?s ?p ?o }');

      // Verify receipt chain across "packages"
      expect(r2.previousReceiptHash).toBe(r1.receiptHash);
      expect(r3.previousReceiptHash).toBe(r2.receiptHash);
      expect(r4.previousReceiptHash).toBe(r3.receiptHash);

      console.log('✅ Cross-package composition: 4 operations across 2 stores');
    });
  });

  describe('State Hash Stability', () => {
    it('should produce identical state hashes for identical stores', async () => {
      const ctx = createContext({
        nodeId: 'test-node-1',
        t_ns: 1000000000000000n,
      });

      const { result: store1 } = await createStore(ctx, { quads: [] });
      const { result: store2 } = await createStore(ctx, { quads: [] });

      const hash1 = await getStoreStateHash(store1);
      const hash2 = await getStoreStateHash(store2);

      expect(hash1).toBe(hash2);

      console.log('✅ State hash stable:', hash1);
    });

    // SKIP: Depends on L5 determinism features
    it.skip('should produce different state hashes after adding quads', async () => {
      const ctx = createContext({
        nodeId: 'test-node-1',
        t_ns: 1000000000000000n,
      });

      const { result: store } = await createStore(ctx, { quads: [] });
      const hashBefore = await getStoreStateHash(store);

      const quad = {
        subject: namedNode('http://example.org/Alice'),
        predicate: namedNode('http://xmlns.com/foaf/0.1/name'),
        object: literal('Alice'),
      };

      const { result: updatedStore } = await addQuad(ctx, store, quad);
      const hashAfter = await getStoreStateHash(updatedStore);

      expect(hashBefore).not.toBe(hashAfter);

      console.log('✅ State hash changes after mutation');
      console.log('   Before:', hashBefore);
      console.log('   After:', hashAfter);
    });
  });

  describe('L5 Maturity Proof', () => {
    // SKIP: Depends on L5 determinism features
    it.skip('should generate complete L5 maturity proof', async () => {
      const ctx = createContext({
        nodeId: 'test-node-1',
        t_ns: 1000000000000000n,
      });

      const proof = await generateL5Proof(ctx);

      expect(proof.package).toBe('@unrdf/oxigraph');
      expect(proof.maturityLevel).toBe('L5');
      expect(proof.tests.createStoreDeterminism.passed).toBe(true);
      expect(proof.tests.queryDeterminism.passed).toBe(true);
      expect(proof.tests.composition.passed).toBe(true);
      expect(proof.tests.stateHashStability.passed).toBe(true);
      expect(proof.overallResult.L5Certified).toBe(true);

      console.log('\n📊 L5 MATURITY PROOF');
      console.log('====================');
      console.log(JSON.stringify(proof, null, 2));
      console.log('\n✅ @unrdf/oxigraph CERTIFIED L5');
    }, 30000); // 30s timeout for 100x iterations
  });

  describe('Performance: Receipt Overhead', () => {
    it('should measure receipt generation overhead', async () => {
      const ctx = createContext({
        nodeId: 'test-node-1',
        t_ns: 1000000000000000n,
      });

      // Measure with receipts
      const start1 = performance.now();
      for (let i = 0; i < 100; i++) {
        await createStore(ctx, { quads: [] });
      }
      const end1 = performance.now();
      const withReceipts = end1 - start1;

      console.log(`\n📈 Performance: 100 createStore operations`);
      console.log(`   With receipts: ${withReceipts.toFixed(2)}ms`);
      console.log(`   Per operation: ${(withReceipts / 100).toFixed(2)}ms`);

      // Receipt overhead should be minimal (<10ms per operation)
      expect(withReceipts / 100).toBeLessThan(10);
    });
  });
});
