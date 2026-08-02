import test from 'node:test';
import assert from 'node:assert/strict';
import {
  ApplicationController,
  Proc,
  ProcRegistry,
  ProcSys,
  Result,
  deepFreeze,
  immutableMessage,
} from '../src/otp/index.mjs';
import {
  FuelPercent,
  Position,
  Telemetry,
  VehicleCommand,
  VehicleId,
  commandHandler,
  coordinatePipeline,
  initialVehicleState,
  telemetryHandler,
} from '../books/atomvm-patterns/examples/part-1.mjs';
import {
  Alerts,
  Telemetry as TelemetryLogic,
  TelemetryMsg,
  VehicleState,
  maintenancePipeline,
  processOrder,
  pureVehicleHandler,
} from '../books/atomvm-patterns/examples/part-2.mjs';
import {
  VehicleMsg,
  registerFleetCoordinator,
  spawnVehicle,
  stableVehicleRef,
} from '../books/atomvm-patterns/examples/part-3.mjs';
import { CounterMsg, counterHandler } from '../books/atomvm-patterns/examples/part-4.mjs';

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

test.afterEach(async () => {
  ProcRegistry.reset();
  await ApplicationController.reset();
});

test('Pattern 1 — Immutable Messages', () => {
  const message = Telemetry.gpsUpdate(VehicleId('V-1'), Position(34.1, -118.2, 1));
  assert.equal(Object.isFrozen(message), true);
  assert.equal(Object.isFrozen(message.position), true);
  assert.throws(() => { message.position.lat = 0; }, TypeError);
});

test('Pattern 2 — Sealed Message Protocols', () => {
  const state = initialVehicleState(VehicleId('V-2'));
  const next = commandHandler(state, VehicleCommand.emergencyStop(state.id));
  assert.equal(next.status, 'STOPPED');
  assert.throws(() => commandHandler(state, immutableMessage('REROUTE')), /unhandled vehicle command/);
});

test('Pattern 3 — State as Value', () => {
  const state = initialVehicleState(VehicleId('V-3'));
  const next = telemetryHandler(state, Telemetry.fuelLevel(state.id, FuelPercent(48)));
  assert.notEqual(next, state);
  assert.equal(state.fuel.value, 100);
  assert.equal(next.fuel.value, 48);
});

test('Pattern 4 — Result Railway', () => {
  const good = coordinatePipeline('[34.1478,-118.1445]');
  const bad = coordinatePipeline('[200,-118]');
  assert.equal(good.isSuccess(), true);
  assert.match(good.orElseThrow().label, /34\.148/);
  assert.equal(bad.isError(), true);
});

test('Pattern 5 — Domain Types Over Primitives', () => {
  assert.equal(VehicleId('V-5').value, 'V-5');
  assert.throws(() => VehicleId(''), /must not be blank/);
  assert.throws(() => Position(91, 0, 0), /Invalid latitude/);
  assert.throws(() => FuelPercent(101), /out of range/);
});

test('Pattern 6 — Pure State Handlers', () => {
  const initial = VehicleState({ fuelLevel: 0.75 });
  const next = pureVehicleHandler(initial, TelemetryMsg.gps(40.7128, -74.006, 1000));
  assert.deepEqual({ lat: next.lat, lng: next.lng, fuel: next.fuelLevel }, { lat: 40.7128, lng: -74.006, fuel: 0.75 });
  assert.equal(initial.lat, 0);
});

test('Pattern 7 — Compose by Purpose', () => {
  const initial = VehicleState({ fuelLevel: 0.75 });
  const gps = TelemetryLogic.applyGps(initial, TelemetryMsg.gps(51.5, -0.1, 500));
  const alert = Alerts.evaluate(gps, TelemetryMsg.alert('E-42', 'warning'));
  assert.equal(gps.fuelLevel, 0.75);
  assert.deepEqual(alert.activeAlerts, ['E-42']);
});

test('Pattern 8 — Railway Composition', () => {
  const fleet = new Map([['V-8', deepFreeze({ id: 'V-8' })]]);
  const inventory = new Set(['brake-pad']);
  const audit = [];
  const result = maintenancePipeline({ vehicleId: 'V-8', part: 'brake-pad' }, fleet, inventory, audit);
  assert.equal(result.isSuccess(), true);
  assert.equal(result.orElseThrow().id, 'WO-V-8-brake-pad');
  assert.equal(audit.length, 1);
});

test('Pattern 9 — Test Without a Framework', () => {
  const result = Result.of(() => 'order-42')
    .map(id => deepFreeze({ id, quantity: 10 }))
    .flatMap(order => order.quantity > 0 ? Result.ok(order) : Result.err('invalid quantity'));
  assert.equal(result.isSuccess(), true);
  assert.equal(result.orElseThrow().quantity, 10);
});

test('Pattern 10 — Skinny Left Margin', () => {
  const success = processOrder(deepFreeze({ order: null, error: null }), { id: 'O-10', quantity: 2 });
  const failure = processOrder(deepFreeze({ order: null, error: null }), { id: 'O-10', quantity: 0 });
  assert.equal(success.order.confirmed, true);
  assert.equal(failure.error, 'bad quantity');
});

test('Pattern 11 — Process as Boundary', async () => {
  const vehicle = spawnVehicle('vehicle-11');
  try {
    vehicle.tell(VehicleMsg.updatePosition(37.7749, -122.4194));
    const state = await vehicle.ask(VehicleMsg.getState());
    assert.deepEqual({ lat: state.lat, lng: state.lng }, { lat: 37.7749, lng: -122.4194 });
    assert.equal(ProcSys.statistics(vehicle).messagesOut, 2);
  } finally {
    await vehicle.stop();
  }
});

test('Pattern 12 — Tell, Don’t Block', async () => {
  const counter = Proc.spawn(0, counterHandler, { id: 'tell-counter' });
  try {
    for (let index = 0; index < 100; index += 1) counter.tell(CounterMsg.increment());
    await eventually(async () => (await ProcSys.getState(counter)) === 100);
    assert.equal((await ProcSys.getState(counter)), 100);
  } finally {
    await counter.stop();
  }
});

test('Pattern 13 — Ask with Timeout', async () => {
  const slow = Proc.spawn(0, async state => {
    await new Promise(resolve => setTimeout(resolve, 40));
    return state + 1;
  }, { id: 'slow' });
  try {
    await assert.rejects(() => slow.ask(immutableMessage('SLOW'), 5), error => error.code === 'ASK_TIMEOUT_REFUSED');
    await eventually(async () => (await ProcSys.getState(slow)) === 1);
  } finally {
    await slow.stop();
  }
});

test('Pattern 14 — Stable References', async () => {
  const ref = stableVehicleRef('vehicle-14');
  const first = ref.proc();
  ref.tell(VehicleMsg.updateSpeed(10));
  assert.equal((await ref.ask(VehicleMsg.getState())).kph, 10);
  const second = spawnVehicle('vehicle-14/1');
  ref.swap(second);
  ref.tell(VehicleMsg.updateSpeed(20));
  assert.notEqual(ref.proc(), first);
  assert.equal(ref.generation, 1);
  assert.equal((await ref.ask(VehicleMsg.getState())).kph, 20);
  await stopAll(first, second);
});

test('Pattern 15 — Named Processes', async () => {
  const coordinator = registerFleetCoordinator('fleet-coordinator');
  assert.equal(ProcRegistry.whereis('fleet-coordinator'), coordinator);
  assert.deepEqual(ProcRegistry.registered(), ['fleet-coordinator']);
  await coordinator.stop();
  assert.equal(ProcRegistry.whereis('fleet-coordinator'), undefined);
});
