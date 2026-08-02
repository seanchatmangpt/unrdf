import { Result, deepFreeze, immutableMessage } from '../../../src/otp/index.mjs';

export function VehicleId(value) {
  if (typeof value !== 'string' || value.trim() === '') throw new TypeError('VehicleId must not be blank');
  return deepFreeze({ type: 'VehicleId', value });
}

export function Position(lat, lng, timestamp) {
  if (!Number.isFinite(lat) || lat < -90 || lat > 90) throw new RangeError(`Invalid latitude: ${lat}`);
  if (!Number.isFinite(lng) || lng < -180 || lng > 180) throw new RangeError(`Invalid longitude: ${lng}`);
  return deepFreeze({ type: 'Position', lat, lng, timestamp });
}

export function Temperature(celsius) {
  if (!Number.isFinite(celsius)) throw new TypeError('temperature must be finite');
  return deepFreeze({
    type: 'Temperature',
    celsius,
    isCritical: () => celsius > 110,
    fahrenheit: () => celsius * 9 / 5 + 32,
  });
}

export function FuelPercent(value) {
  if (!Number.isFinite(value) || value < 0 || value > 100) throw new RangeError(`Fuel percent out of range: ${value}`);
  return deepFreeze({ type: 'FuelPercent', value, isLow: () => value < 15 });
}

export const Telemetry = Object.freeze({
  gpsUpdate: (id, position) => immutableMessage('GPS_UPDATE', { id, position }),
  engineStatus: (id, rpm, temperature) => immutableMessage('ENGINE_STATUS', { id, rpm, temperature }),
  fuelLevel: (id, fuel) => immutableMessage('FUEL_LEVEL', { id, fuel }),
});

export const VehicleCommand = Object.freeze({
  assignRoute: (id, route) => immutableMessage('ASSIGN_ROUTE', { id, route }),
  recallVehicle: (id, reason) => immutableMessage('RECALL_VEHICLE', { id, reason }),
  emergencyStop: id => immutableMessage('EMERGENCY_STOP', { id }),
  requestStatus: id => immutableMessage('REQUEST_STATUS', { id }),
});

export function initialVehicleState(id) {
  return vehicleState({
    id,
    position: Position(0, 0, 0),
    fuel: FuelPercent(100),
    engineRpm: 0,
    engineTemp: Temperature(20),
    status: 'IDLE',
    currentRoute: null,
    recallReason: null,
  });
}

export function vehicleState(value) {
  const next = {
    ...value,
    withPosition(position) { return vehicleState({ ...value, position }); },
    withFuel(fuel) { return vehicleState({ ...value, fuel }); },
    withEngine(engineRpm, engineTemp) { return vehicleState({ ...value, engineRpm, engineTemp }); },
    withStatus(status) { return vehicleState({ ...value, status }); },
    withRoute(currentRoute) { return vehicleState({ ...value, currentRoute }); },
    withRecallReason(recallReason) { return vehicleState({ ...value, recallReason }); },
  };
  return deepFreeze(next);
}

export function telemetryHandler(state, message) {
  switch (message.type) {
    case 'GPS_UPDATE': return state.withPosition(message.position);
    case 'ENGINE_STATUS': return state.withEngine(message.rpm, message.temperature);
    case 'FUEL_LEVEL': return state.withFuel(message.fuel);
    default: throw new TypeError(`unhandled telemetry message: ${message.type}`);
  }
}

export function commandHandler(state, command) {
  switch (command.type) {
    case 'ASSIGN_ROUTE': return state.withRoute(command.route).withStatus('EN_ROUTE');
    case 'RECALL_VEHICLE': return state.withStatus('RETURNING').withRecallReason(command.reason);
    case 'EMERGENCY_STOP': return state.withStatus('STOPPED');
    case 'REQUEST_STATUS': return state;
    default: throw new TypeError(`unhandled vehicle command: ${command.type}`);
  }
}

export function parseCoordinates(raw) {
  return Result.of(() => JSON.parse(raw)).flatMap(value => {
    if (!Array.isArray(value) || value.length !== 2) return Result.err('expected [lat,lng]');
    try { return Result.ok(Position(value[0], value[1], Date.now())); }
    catch (error) { return Result.err(error.message); }
  });
}

export function validateBounds(position) {
  return position.lat >= -90 && position.lat <= 90 && position.lng >= -180 && position.lng <= 180
    ? Result.ok(position)
    : Result.err('position out of bounds');
}

export function geocode(position) {
  return Result.ok(deepFreeze({ type: 'Address', label: `${position.lat.toFixed(3)},${position.lng.toFixed(3)}` }));
}

export function coordinatePipeline(raw) {
  return parseCoordinates(raw).flatMap(validateBounds).flatMap(geocode);
}
