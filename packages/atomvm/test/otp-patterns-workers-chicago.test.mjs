import test from 'node:test';
import assert from 'node:assert/strict';
import {
  ApplicationController,
  EventManager,
  Parallel,
  Proc,
  ProcRegistry,
  ProcSys,
  ProcTimer,
  Supervisor,
  SupervisorStrategy,
  deepFreeze,
  immutableMessage,
} from '../src/otp/index.mjs';
import { CounterMsg, counterHandler } from '../books/atomvm-patterns/examples/part-4.mjs';
import {
  VehicleEvent,
  assembleFleetPulse,
  createFleetEventBus,
  createHeartbeatTracker,
  createVehicleStateMachine,
  inspectProcess,
  queryFleetInParallel,
} from '../books/atomvm-patterns/examples/part-5.mjs';

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

test('Pattern 24 — State Machines for Complex Protocols', async () => {
  const machine = createVehicleStateMachine('V-24');
  machine.send(VehicleEvent.startTracking());
  await machine.call(VehicleEvent.pauseTracking());
  machine.send(VehicleEvent.startTracking());
  const data = await machine.call(VehicleEvent.beginMaintenance());
  assert.equal(await machine.state(), 'MAINTENANCE');
  assert.equal(data.tripCount, 2);
  await machine.stop();
});

test('Pattern 25 — Event Broadcasting', async () => {
  const events = createFleetEventBus('events-25');
  const received = [];
  const crashing = {
    handleEvent(event) {
      if (event.type === 'BOOM') throw new Error('handler crash');
      received.push(`A:${event.type}`);
    },
    terminate(reason) { received.push(`A:terminated:${reason?.message ?? 'normal'}`); },
  };
  const healthy = { handleEvent: event => received.push(`B:${event.type}`) };
  events.addHandler(crashing);
  events.addHandler(healthy);
  await events.syncNotify(immutableMessage('HELLO'));
  await events.syncNotify(immutableMessage('BOOM'));
  await events.syncNotify(immutableMessage('AFTER'));
  assert.deepEqual(received, [
    'A:HELLO', 'B:HELLO',
    'A:terminated:handler crash', 'B:BOOM',
    'B:AFTER',
  ]);
  assert.equal(events.handlerCount(), 1);
  await events.stop();
});

test('Pattern 26 — Timed Messages', async () => {
  const tracker = createHeartbeatTracker(Date.now() - 31_000);
  const message = immutableMessage('HEARTBEAT', { now: Date.now() });
  const interval = ProcTimer.sendInterval(10, tracker, message);
  try {
    await eventually(async () => (await ProcSys.getState(tracker)).heartbeats >= 2);
    interval.cancel();
    const state = await ProcSys.getState(tracker);
    assert.ok(state.alerts >= 2);
  } finally {
    interval.cancel();
    await tracker.stop();
  }
});

test('Pattern 27 — Fan-Out with Fail-Fast', async () => {
  const success = await queryFleetInParallel(['V1', 'V2', 'V3'], async id => {
    await new Promise(resolve => setTimeout(resolve, 5));
    return deepFreeze({ id, online: true });
  });
  assert.deepEqual(success.orElseThrow().map(item => item.id), ['V1', 'V2', 'V3']);

  const started = Date.now();
  const failure = await Parallel.all([
    async signal => {
      await new Promise((resolve, reject) => {
        const timer = setTimeout(resolve, 500);
        signal.addEventListener('abort', () => { clearTimeout(timer); reject(signal.reason); }, { once: true });
      });
      return 'slow';
    },
    async () => { throw new Error('unreachable vehicle'); },
  ]);
  assert.equal(failure.isError(), true);
  assert.ok(Date.now() - started < 200);
});

test('Pattern 28 — Process Introspection', async () => {
  const proc = Proc.spawn(0, counterHandler, { id: 'introspection' });
  ProcSys.suspend(proc);
  proc.tell(CounterMsg.increment());
  assert.equal(ProcSys.statistics(proc).queueDepth, 1);
  assert.equal((await ProcSys.getState(proc)), 0);
  ProcSys.resume(proc);
  await eventually(async () => (await ProcSys.getState(proc)) === 1);
  const report = await inspectProcess(proc);
  assert.equal(report.state, 1);
  assert.equal(report.stats.messagesOut, 1);
  assert.equal(report.receipts.length, 1);
  await proc.stop();
});

test('Pattern 29 — Assemble the Application', async () => {
  const { spec, state } = await assembleFleetPulse();
  state.eventBus.notify(immutableMessage('VEHICLE_ALERT', { message: 'GPS silent' }));
  await eventually(() => state.alerts.length === 1);
  assert.equal(ProcRegistry.whereis('V-1001'), state.tracker);
  assert.deepEqual(ApplicationController.whichApplications().map(app => app.name), ['fleet-pulse']);
  await ApplicationController.stop(spec.name);
  assert.equal(ProcRegistry.whereis('V-1001'), undefined);
});

test('Pattern 30 — Test the Boundary', async () => {
  const supervisor = Supervisor.create({
    id: 'boundary', strategy: SupervisorStrategy.ONE_FOR_ONE, maxRestarts: 3, windowMs: 1000,
  });
  const ref = supervisor.supervise('counter', 0, counterHandler);
  const bus = EventManager.start('boundary-events');
  const observed = [];
  bus.addHandler(event => observed.push(event.value));

  ref.tell(CounterMsg.increment());
  ref.tell(CounterMsg.increment());
  assert.equal(await ref.ask(CounterMsg.get()), 2);
  const old = ref.proc();
  ref.tell(CounterMsg.crash());
  await eventually(() => ref.proc() !== old);
  ref.tell(CounterMsg.increment());
  assert.equal(await ref.ask(CounterMsg.get()), 1);

  await bus.syncNotify(immutableMessage('DOMAIN_EVENT', { value: 'alive' }));
  assert.deepEqual(observed, ['alive']);
  assert.ok(supervisor.receipts().some(receipt => receipt.activity === 'child_restarted'));
  assert.ok(ProcSys.receipts(ref).some(receipt => receipt.status === 'ALIVE'));

  await bus.stop();
  await supervisor.shutdown();
});
