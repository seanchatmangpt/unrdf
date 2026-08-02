import {
  ApplicationController,
  ApplicationSpec,
  EventManager,
  Parallel,
  Proc,
  ProcRegistry,
  ProcSys,
  ProcTimer,
  StateMachine,
  Supervisor,
  SupervisorStrategy,
  Transition,
  deepFreeze,
  immutableMessage,
} from '../../../src/otp/index.mjs';

export const VehicleEvent = Object.freeze({
  startTracking: () => immutableMessage('START_TRACKING'),
  pauseTracking: () => immutableMessage('PAUSE_TRACKING'),
  beginMaintenance: () => immutableMessage('BEGIN_MAINTENANCE'),
  completeMaintenance: () => immutableMessage('COMPLETE_MAINTENANCE'),
  decommission: () => immutableMessage('DECOMMISSION'),
});

export function vehicleTransition(state, event, data) {
  switch (state) {
    case 'IDLE':
      if (event.type === 'START_TRACKING') return Transition.nextState('TRACKING', deepFreeze({ ...data, tripCount: data.tripCount + 1 }));
      if (event.type === 'DECOMMISSION') return Transition.stop('decommissioned');
      return Transition.keepState(data);
    case 'TRACKING':
      if (event.type === 'PAUSE_TRACKING') return Transition.nextState('IDLE', data);
      if (event.type === 'BEGIN_MAINTENANCE') return Transition.nextState('MAINTENANCE', data);
      return Transition.keepState(data);
    case 'MAINTENANCE':
      if (event.type === 'COMPLETE_MAINTENANCE') return Transition.nextState('IDLE', data);
      return Transition.keepState(data);
    default:
      return Transition.keepState(data);
  }
}

export function createVehicleStateMachine(id = 'V-1001') {
  return StateMachine.create('IDLE', deepFreeze({ vehicleId: id, tripCount: 0 }), vehicleTransition, { id: `sm/${id}` });
}

export function createFleetEventBus(id = 'fleet-events') {
  return EventManager.start(id);
}

export function createHeartbeatTracker(now = Date.now()) {
  return Proc.spawn(
    deepFreeze({ lastGpsTimestamp: now, heartbeats: 0, alerts: 0 }),
    (state, message) => {
      switch (message.type) {
        case 'GPS_UPDATE': return deepFreeze({ ...state, lastGpsTimestamp: message.timestamp });
        case 'HEARTBEAT': return deepFreeze({
          ...state,
          heartbeats: state.heartbeats + 1,
          alerts: state.alerts + (message.now - state.lastGpsTimestamp > 30_000 ? 1 : 0),
        });
        default: return state;
      }
    },
    { id: 'heartbeat-tracker' },
  );
}

export async function queryFleetInParallel(vehicleIds, query) {
  return Parallel.all(vehicleIds.map(id => signal => query(id, signal)));
}

export async function inspectProcess(proc) {
  return deepFreeze({
    state: await ProcSys.getState(proc),
    stats: ProcSys.statistics(proc),
    receipts: ProcSys.receipts(proc),
  });
}

export async function assembleFleetPulse() {
  const eventBus = createFleetEventBus();
  const root = Supervisor.create({
    id: 'fleet-root',
    strategy: SupervisorStrategy.ONE_FOR_ONE,
    maxRestarts: 5,
    windowMs: 60_000,
  });
  const alerts = [];
  const alertRef = root.supervise(
    'alert-service',
    deepFreeze({ count: 0 }),
    (state, message) => {
      if (message.type !== 'PROCESS_ALERT') return state;
      alerts.push(message.message);
      return deepFreeze({ count: state.count + 1 });
    },
  );
  eventBus.addHandler(event => {
    if (event.type === 'VEHICLE_ALERT') {
      alertRef.tell(immutableMessage('PROCESS_ALERT', { message: event.message }));
    }
  });

  const tracker = createHeartbeatTracker();
  ProcRegistry.register('V-1001', tracker);
  const timer = ProcTimer.sendInterval(20, tracker, immutableMessage('HEARTBEAT', { now: Date.now() + 31_000 }));

  const spec = new ApplicationSpec({
    name: 'fleet-pulse',
    async start() { return { root, eventBus, tracker, timer, alerts, alertRef }; },
    async stop(state) {
      state.timer.cancel();
      ProcRegistry.unregister('V-1001');
      await state.eventBus.stop();
      await state.root.shutdown();
      await state.tracker.stop();
    },
  });
  const state = await ApplicationController.start(spec);
  return { spec, state };
}
