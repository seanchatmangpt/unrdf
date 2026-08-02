import test from 'node:test';
import assert from 'node:assert/strict';
import {
  DownSignal,
  ExitClass,
  Proc,
  ProcAlias,
  ProcDirective,
  ProcLink,
  ProcMonitor,
  deepFreeze,
  immutableMessage,
} from '../src/otp/index.mjs';

async function eventually(predicate, { timeoutMs = 1500, intervalMs = 2 } = {}) {
  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    const value = await predicate();
    if (value) return value;
    await new Promise(resolve => setTimeout(resolve, intervalMs));
  }
  throw new Error(`condition not satisfied within ${timeoutMs} ms`);
}

test('Armstrong 1 — selective receive defers unmatched messages without reordering them', async () => {
  const urgentOnly = (_state, message) => message.type === 'URGENT';
  const proc = Proc.spawn([], (state, message) => {
    const next = Object.freeze([...state, message.type]);
    return message.type === 'URGENT'
      ? ProcDirective.receive(next, () => true)
      : next;
  }, { id: 'selective-receive', selector: urgentOnly });

  try {
    proc.tell(immutableMessage('NORMAL'));
    proc.tell(immutableMessage('URGENT'));
    await eventually(async () => (await proc.state()).length === 2);
    assert.deepEqual(await proc.state(), ['URGENT', 'NORMAL']);
  } finally {
    await proc.stop();
  }
});

test('Armstrong 2 — replies are protocol values and are not confused with server state', async () => {
  const proc = Proc.spawn({ count: 0 }, (state, message) => {
    if (message.type === 'INCREMENT') return deepFreeze({ count: state.count + 1 });
    if (message.type === 'GET') return ProcDirective.reply(state.count, state);
    throw new Error(`unknown message ${message.type}`);
  }, { id: 'explicit-replies' });

  try {
    proc.tell(immutableMessage('INCREMENT'));
    assert.equal(await proc.ask(immutableMessage('GET')), 1);
    assert.deepEqual(await proc.state(), { count: 1 });
  } finally {
    await proc.stop();
  }
});

test('Armstrong 3 — reduction budgets yield so one mailbox cannot monopolize the scheduler', async () => {
  const proc = Proc.spawn(0, state => state + 1, {
    id: 'fair-process',
    reductionLimit: 5,
  });

  try {
    for (let index = 0; index < 50; index += 1) proc.tell(index);
    const observedAfterTimerTurn = await new Promise(resolve => {
      setTimeout(() => resolve(proc.statistics().reductions), 0);
    });
    assert.ok(observedAfterTimerTurn >= 5 && observedAfterTimerTurn < 50);
    await eventually(async () => (await proc.state()) === 50);
    assert.ok(proc.statistics().yields >= 1);
  } finally {
    await proc.stop();
  }
});

test('Armstrong 4 — normal linked exits do not kill peers but abnormal exits do', async () => {
  const left = Proc.spawn(0, state => state, { id: 'link-left' });
  const normal = Proc.spawn(0, state => state, { id: 'link-normal' });
  const abnormal = Proc.spawn(0, state => state, { id: 'link-abnormal' });
  const normalLink = ProcLink.link(left, normal);

  await normal.stop('normal');
  await new Promise(resolve => setTimeout(resolve, 5));
  assert.equal(left.isRunning, true);
  normalLink.unlink();

  ProcLink.link(left, abnormal);
  abnormal.crash(new Error('boom'));
  await eventually(() => !left.isRunning);
  assert.equal(left.lastExit.kind, ExitClass.ERROR);
});

test('Armstrong 5 — monitors deliver immutable DOWN messages without sharing fate', async () => {
  const observer = Proc.spawn([], (state, message) => {
    if (message instanceof DownSignal) return Object.freeze([...state, message]);
    return state;
  }, { id: 'observer' });
  const target = Proc.spawn(0, state => state, { id: 'observed-target' });

  try {
    const monitor = ProcMonitor.monitorProcess(observer, target);
    await target.stop('normal');
    await eventually(async () => (await observer.state()).length === 1);
    const [down] = await observer.state();
    assert.equal(down.type, 'DOWN');
    assert.equal(down.ref, monitor.id);
    assert.equal(down.target, target.id);
    assert.equal(down.exit.kind, ExitClass.NORMAL);
    assert.equal(observer.isRunning, true);
  } finally {
    await observer.stop();
  }
});

test('Armstrong 6 — revocable aliases turn late replies into harmless dropped messages', async () => {
  const proc = Proc.spawn(0, state => state + 1, { id: 'alias-target' });
  const alias = new ProcAlias(proc, { id: 'reply-alias' });

  try {
    assert.equal(alias.tell(immutableMessage('INCREMENT')).status, 'ALIVE');
    await eventually(async () => (await proc.state()) === 1);
    assert.equal(alias.revoke(), true);
    assert.equal(alias.tell(immutableMessage('LATE_REPLY')).status, 'DROPPED');
    await new Promise(resolve => setTimeout(resolve, 5));
    assert.equal(await proc.state(), 1);
  } finally {
    await proc.stop();
  }
});

test('Armstrong 7 — per-sender sequence evidence preserves sender order', async () => {
  const proc = Proc.spawn([], (state, message, metadata) => Object.freeze([
    ...state,
    deepFreeze({ from: metadata.from, sequence: metadata.senderSequence, value: message.value }),
  ]), { id: 'sender-order' });

  try {
    proc.tellFrom('sender-a', deepFreeze({ value: 1 }));
    proc.tellFrom('sender-a', deepFreeze({ value: 2 }));
    proc.tellFrom('sender-b', deepFreeze({ value: 9 }));
    proc.tellFrom('sender-a', deepFreeze({ value: 3 }));
    await eventually(async () => (await proc.state()).length === 4);
    const state = await proc.state();
    assert.deepEqual(state.filter(item => item.from === 'sender-a').map(item => item.sequence), [1, 2, 3]);
    assert.deepEqual(state.filter(item => item.from === 'sender-a').map(item => item.value), [1, 2, 3]);
  } finally {
    await proc.stop();
  }
});

test('Armstrong 8 — crashes retain a bounded process report and dead sends become noproc evidence', async () => {
  const proc = Proc.spawn({ safe: true }, () => {
    throw new Error('invariant violated');
  }, { id: 'crash-report' });

  proc.tell(immutableMessage('BREAK'));
  const termination = await proc.awaitTermination();
  const report = proc.crashReport();
  assert.equal(termination.exit.kind, ExitClass.ERROR);
  assert.equal(report.reportType, 'otp.process.crash');
  assert.equal(report.processId, proc.id);
  assert.match(report.error.message, /invariant violated/);
  assert.equal(typeof report.currentMessageDigest, 'string');

  const dropped = proc.tryTell(immutableMessage('TOO_LATE'));
  assert.equal(dropped.status, 'DROPPED');
  assert.equal(dropped.error.kind, ExitClass.NOPROC);
});
