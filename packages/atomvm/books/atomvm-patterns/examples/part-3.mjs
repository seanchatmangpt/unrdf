import {
  ExitSignal,
  Proc,
  ProcLink,
  ProcRef,
  ProcRegistry,
  deepFreeze,
  immutableMessage,
} from '../../../src/otp/index.mjs';

export const VehicleMsg = Object.freeze({
  updatePosition: (lat, lng) => immutableMessage('UPDATE_POSITION', { lat, lng }),
  updateSpeed: kph => immutableMessage('UPDATE_SPEED', { kph }),
  getState: () => immutableMessage('GET_STATE'),
  crash: reason => immutableMessage('CRASH', { reason }),
});

export function VehicleState({ lat = 0, lng = 0, kph = 0, exits = [] } = {}) {
  return deepFreeze({ lat, lng, kph, exits: Object.freeze([...exits]) });
}

export function vehicleHandler(state, message) {
  if (message instanceof ExitSignal) {
    return VehicleState({ ...state, exits: [...state.exits, message.reason?.message ?? String(message.reason)] });
  }
  switch (message.type) {
    case 'UPDATE_POSITION': return VehicleState({ ...state, lat: message.lat, lng: message.lng });
    case 'UPDATE_SPEED': return VehicleState({ ...state, kph: message.kph });
    case 'GET_STATE': return state;
    case 'CRASH': throw new Error(message.reason);
    default: throw new TypeError(`unhandled vehicle message: ${message.type}`);
  }
}

export function spawnVehicle(id = 'vehicle') {
  return Proc.spawn(VehicleState(), vehicleHandler, { id });
}

export function stableVehicleRef(id = 'vehicle') {
  return new ProcRef(spawnVehicle(`${id}/0`), { id: `${id}/ref` });
}

export function registerFleetCoordinator(name = 'fleet-coordinator') {
  const coordinator = spawnVehicle(name);
  ProcRegistry.register(name, coordinator);
  return coordinator;
}

export function linkedCoordinatorAndWorker() {
  const coordinator = spawnVehicle('coordinator');
  coordinator.trapExits(true);
  const worker = ProcLink.spawnLink(coordinator, VehicleState(), vehicleHandler, { id: 'worker' });
  return deepFreeze({ coordinator, worker });
}
