import {
  CrashRecovery,
  Proc,
  ProcLib,
  ProcLink,
  ProcMonitor,
  Supervisor,
  SupervisorStrategy,
  deepFreeze,
  immutableMessage,
} from '../../../src/otp/index.mjs';

export const CounterMsg = Object.freeze({
  increment: () => immutableMessage('INCREMENT'),
  crash: () => immutableMessage('CRASH'),
  get: () => immutableMessage('GET'),
});

export function counterHandler(count, message) {
  switch (message.type) {
    case 'INCREMENT': return count + 1;
    case 'GET': return count;
    case 'CRASH': throw new Error('intentional crash');
    default: throw new TypeError(`unhandled counter message: ${message.type}`);
  }
}

export function fleetSupervisor(strategy = SupervisorStrategy.ONE_FOR_ONE, options = {}) {
  return Supervisor.create({
    id: options.id ?? 'fleet-root',
    strategy,
    maxRestarts: options.maxRestarts ?? 3,
    windowMs: options.windowMs ?? 60_000,
  });
}

export async function supervisedStartup(initial = deepFreeze({ ready: false })) {
  return ProcLib.startLink(
    initial,
    async (state, { initAck }) => {
      const initialized = deepFreeze({ ...state, ready: true });
      initAck();
      return initialized;
    },
    (state, message) => message.type === 'GET' ? state : state,
  );
}

export function sharedFatePair() {
  const left = Proc.spawn(0, counterHandler, { id: 'decoder' });
  const right = Proc.spawn(0, counterHandler, { id: 'websocket' });
  const link = ProcLink.link(left, right);
  return { left, right, link };
}

export function observe(target, events) {
  return ProcMonitor.monitor(target, reason => {
    events.push(deepFreeze({ status: reason ? 'crashed' : 'stopped', reason: reason?.message ?? null }));
  });
}

export async function retryFresh(maxAttempts, operation) {
  return CrashRecovery.retry(maxAttempts, operation);
}
