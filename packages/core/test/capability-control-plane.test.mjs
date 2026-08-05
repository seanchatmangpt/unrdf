import test from 'node:test';
import assert from 'node:assert/strict';
import { createCapabilityLedger, Standing, Disposition } from '../src/capability-ledger.mjs';
import { createReceiptChain, compareReplay } from '../src/receipt-chain.mjs';
import { createExecutionPlan } from '../src/execution-plan.mjs';

test('capability ledger enforces crown requirements', () => {
  const ledger = createCapabilityLedger({ subject: 'test' });
  ledger.admit({ id: 'x', owner: 'team', contract: 'works', verifier: 'v', falsifier: 'f' });
  ledger.setDisposition('x', Disposition.PRESERVED, 'required');
  ledger.transition('x', Standing.PARTIAL_ALIVE, { command: 'node --test', exit: 0 });
  ledger.transition('x', Standing.ALIVE, { command: 'node --test', exit: 0 });
  assert.equal(ledger.crown().standing, Standing.ALIVE);
});

test('receipt chain detects replay equivalence', () => {
  const chain = createReceiptChain({ subject: 'x', source: 'abc' });
  chain.append({ action: 'build', result: 'success', outputs: { a: 1 } });
  assert.equal(chain.verify().valid, true);
  assert.equal(compareReplay({ a: 1 }, { a: 1 }).match, true);
});

test('execution plan is dependency ordered', async () => {
  const plan = createExecutionPlan();
  plan.add({ id: 'b', dependsOn: ['a'], run: ({ inputs }) => inputs.a + 1 });
  plan.add({ id: 'a', run: () => 1 });
  assert.deepEqual(await plan.execute(), { a: 1, b: 2 });
});
