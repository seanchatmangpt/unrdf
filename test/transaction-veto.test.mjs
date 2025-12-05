import { describe, it, expect } from 'vitest';
import { TransactionManager } from '../packages/knowledge-engine/transaction.mjs';
import { createStore, dataFactory } from '@unrdf/oxigraph';

const { namedNode, quad } = dataFactory;

describe('Transaction Manager - veto pre-hook', () => {
  it('pre-hook with veto prevents commit', async () => {
    const tx = new TransactionManager({
      strictMode: false,
      enableLockchain: false,
      enableResolution: false,
    });
    const store = createStore();

    tx.addHook({
      id: 'veto-eve',
      mode: 'pre',
      condition: async (_store, delta) =>
        delta.additions.some(q => q.object.value.endsWith('eve')) === false,
      effect: 'veto',
    });

    const delta = {
      additions: [quad(namedNode('ex:alice'), namedNode('ex:knows'), namedNode('ex:eve'))],
      removals: [],
    };

    const result = await tx.apply(store, delta, { actor: 'test' });
    expect(result.receipt.committed).toBe(false);
    expect(store.size).toBe(0);
  });
});
