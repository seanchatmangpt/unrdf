import { Result, deepFreeze, immutableMessage } from '../../../src/otp/index.mjs';

export const TelemetryMsg = Object.freeze({
  gps: (lat, lng, timestamp) => immutableMessage('GPS_UPDATE', { lat, lng, timestamp }),
  fuel: level => immutableMessage('FUEL_READING', { level }),
  alert: (code, severity) => immutableMessage('ENGINE_ALERT', { code, severity }),
});

export function VehicleState({ lat = 0, lng = 0, lastSeen = 0, fuelLevel = 1, activeAlerts = [], shutdown = false } = {}) {
  return deepFreeze({ lat, lng, lastSeen, fuelLevel, activeAlerts: Object.freeze([...activeAlerts]), shutdown });
}

export const Telemetry = Object.freeze({
  applyGps(state, message) {
    return VehicleState({ ...state, lat: message.lat, lng: message.lng, lastSeen: message.timestamp });
  },
  applyFuel(state, message) {
    return VehicleState({ ...state, fuelLevel: message.level });
  },
});

export const Alerts = Object.freeze({
  evaluate(state, message) {
    if (message.severity === 'critical') return VehicleState({ ...state, shutdown: true });
    return VehicleState({ ...state, activeAlerts: [...state.activeAlerts, message.code] });
  },
});

export function pureVehicleHandler(state, message) {
  switch (message.type) {
    case 'GPS_UPDATE': return Telemetry.applyGps(state, message);
    case 'FUEL_READING': return Telemetry.applyFuel(state, message);
    case 'ENGINE_ALERT': return Alerts.evaluate(state, message);
    default: throw new TypeError(`unhandled telemetry: ${message.type}`);
  }
}

export function validateMaintenance(request) {
  if (!request.vehicleId) throw new TypeError('vehicleId is required');
  return deepFreeze({ ...request, validated: true });
}

export function lookupVehicle(validRequest, fleet) {
  const vehicle = fleet.get(validRequest.vehicleId);
  if (!vehicle) throw new Error(`unknown vehicle ${validRequest.vehicleId}`);
  return deepFreeze({ request: validRequest, vehicle });
}

export function checkPartsAvailability(context, inventory) {
  if (!inventory.has(context.request.part)) throw new Error(`part unavailable: ${context.request.part}`);
  return deepFreeze({ ...context, part: context.request.part });
}

export function createWorkOrder(context) {
  return deepFreeze({
    id: `WO-${context.request.vehicleId}-${context.part}`,
    vehicleId: context.request.vehicleId,
    part: context.part,
  });
}

export function maintenancePipeline(request, fleet, inventory, audit) {
  return Result.of(() => validateMaintenance(request))
    .flatMap(valid => Result.of(() => lookupVehicle(valid, fleet)))
    .flatMap(context => Result.of(() => checkPartsAvailability(context, inventory)))
    .flatMap(context => Result.of(() => createWorkOrder(context)))
    .peek(order => audit.push(order));
}

export function processOrder(state, order) {
  return Result.of(() => {
    if (!order.id) throw new TypeError('order id is required');
    return order;
  }).flatMap(valid => valid.quantity > 0
    ? Result.ok(deepFreeze({ ...valid, confirmed: true }))
    : Result.err(new RangeError('bad quantity')))
    .fold(
      confirmed => deepFreeze({ ...state, order: confirmed, error: null }),
      error => deepFreeze({ ...state, error: error.message }),
    );
}
