import test from 'node:test';
import assert from 'node:assert/strict';
import {
  Proc,
  ProcLib,
  ProcMonitor,
  ProcSys,
  SupervisorStrategy,
  immutableMessage,
} from '../src/otp/index.mjs';
import { VehicleMsg, linkedCoordinatorAndWorker } from '../books/atomvm-patterns/examples/part-3.mjs';
import {
  CounterMsg,
  counterHandler,
  fleetSupervisor,
  observe,
  retryFresh,
  sharedFatePair,
  supervisedStartup,
} from '../books/atomvm-patterns/examples/part-4.mjs';

async function eventually(predicate, { timeoutMs = 1500, intervalMs = 5 } = {}) {
  const deadline = Date.now() + timeoutMs;
  let lastError;
  while (Date.now() < deadline) {
    try {
      const value = await predicate();
      if (value) return value;
    } catch (error) {
      lastError = error;
    }
    await new Promise(resolve => setTimeout(resolve, intervalMs));
  }
  if (lastError) throw lastError;
  throw new Error(`condition not satisfied within ${timeoutMs} ms`);
}

async function stopAll(...targets) {
  await Promise.allSettled(targets.filter(Boolean).map(target => target.stop?.() ?? target.shutdown?.()));
}

test('Pattern 16 — Trap Exits', async () => {
  const { coordinator, worker } = linkedCoordinatorAndWorker();
  try {
    worker.tell(VehicleMsg.crash('sensor overflow'));
    await eventually(async () => (await ProcSys.getState(coordinator)).exits.length === 1);
    const state = await coordinator.ask(VehicleMsg.getState());
    assert.deepEqual(state.exits, ['sensor overflow']);
    assert.equal(coordinator.isRunning, true);
  } finally {
    await stopAll(coordinator, worker);
  }
});

test('Pattern 17 — Let It Crash', async () => {
  const supervisor = fleetSupervisor(SupervisorStrategy.ONE_FOR_ONE, { id: 'let-it-crash' });
  const ref = supervisor.supervise('counter', 0, counterHandler);
  ref.tell(CounterMsg.increment());
  assert.equal(await ref.ask(CounterMsg.get()), 1);
  const first = ref.proc();
  ref.tell(CounterMsg.crash());
  await eventually(() => ref.proc() !== first);
  assert.equal(await ref.ask(CounterMsg.get()), 0);
  await supervisor.shutdown();
});

test('Pattern 18 — Supervision Trees', async () => {
  const supervisor = fleetSupervisor(SupervisorStrategy.REST_FOR_ONE, { id: 'tree' });
  const a = supervisor.supervise('a', 0, counterHandler);
  const b = supervisor.supervise('b', 0, counterHandler);
  const c = supervisor.supervise('c', 0, counterHandler);
  const old = [a.proc(), b.proc(), c.proc()];
  b.tell(CounterMsg.crash());
  await eventually(() => b.proc() !== old[1] && c.proc() !== old[2]);
  assert.equal(a.proc(), old[0]);
  assert.notEqual(b.proc(), old[1]);
  assert.notEqual(c.proc(), old[2]);
  await supervisor.shutdown();
});

test('Pattern 19 — Restart Intensity as Circuit Breaker', async () => {
  const supervisor = fleetSupervisor(SupervisorStrategy.ONE_FOR_ONE, {
    id: 'intensity', maxRestarts: 1, windowMs: 1000,
  });
  const ref = supervisor.supervise('counter', 0, counterHandler);
  ref.tell(CounterMsg.crash());
  const firstRestart = await eventually(() => ref.generation === 1 && ref.proc());
  firstRestart.tell(CounterMsg.crash());
  await eventually(() => supervisor.isRunning === false);
  assert.equal(supervisor.fatalError.code, 'RESTART_INTENSITY_EXCEEDED');
});

test('Pattern 20 — Supervised Startup', async () => {
  const result = await supervisedStartup();
  assert.equal(result.isSuccess(), true);
  const proc = result.orElseThrow();
  assert.equal((await proc.ask(immutableMessage('GET'))).ready, true);
  await proc.stop();

  const failed = await ProcLib.startLink(
    0,
    async () => { throw new Error('database unavailable'); },
    state => state,
    50,
  );
  assert.equal(failed.isError(), true);
});

test('Pattern 21 — Links for Shared Fate', async () => {
  const { left, right } = sharedFatePair();
  left.tell(CounterMsg.crash());
  await eventually(() => !right.isRunning);
  assert.equal(left.isRunning, false);
  assert.equal(right.isRunning, false);
  assert.match(right.lastError.message, /intentional crash/);
});

test('Pattern 22 — Monitors for Observation', async () => {
  const target = Proc.spawn(0, counterHandler, { id: 'observed' });
  const events = [];
  const monitor = observe(target, events);
  await target.stop();
  assert.deepEqual(events, [{ status: 'stopped', reason: null }]);
  assert.equal(ProcMonitor.demonitor(monitor), false);
});

test('Pattern 23 — Retry with Fresh State', async () => {
  let attempts = 0;
  const result = await retryFresh(3, attempt => {
    attempts += 1;
    const attemptState = { attempt, token: Symbol(`attempt-${attempt}`) };
    if (attempt < 3) throw new Error(`transient-${attemptState.attempt}`);
    return attemptState.attempt;
  });
  assert.equal(result.orElseThrow(), 3);
  assert.equal(attempts, 3);
});
