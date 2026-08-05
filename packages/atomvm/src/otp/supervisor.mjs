import { randomUUID } from 'node:crypto';
import { Proc, ProcRef, OtpRefusal, deepFreeze, digest } from './process.mjs';

function cloneInitial(value) {
  if (typeof value === 'function') return value();
  try { return structuredClone(value); } catch { return value; }
}

export const SupervisorStrategy = Object.freeze({
  ONE_FOR_ONE: 'one_for_one',
  ONE_FOR_ALL: 'one_for_all',
  REST_FOR_ONE: 'rest_for_one',
});

export const RestartType = Object.freeze({
  PERMANENT: 'permanent',
  TRANSIENT: 'transient',
  TEMPORARY: 'temporary',
});

function validatesStrategy(strategy) {
  if (!Object.values(SupervisorStrategy).includes(strategy)) {
    throw new OtpRefusal('INVALID_STRATEGY_REFUSED', `unsupported supervisor strategy: ${strategy}`);
  }
  return strategy;
}

function validatesRestartType(restart) {
  if (!Object.values(RestartType).includes(restart)) {
    throw new OtpRefusal('INVALID_RESTART_TYPE_REFUSED', `unsupported restart type: ${restart}`);
  }
  return restart;
}

export class Supervisor {
  #children = [];
  #restartTimes = [];
  #running = true;
  #fatalError = null;
  #receipts = [];
  #restarting = Promise.resolve();

  constructor({
    id = `supervisor-${randomUUID()}`,
    strategy = SupervisorStrategy.ONE_FOR_ONE,
    maxRestarts = 3,
    windowMs = 60_000,
  } = {}) {
    if (!Number.isInteger(maxRestarts) || maxRestarts < 0) throw new RangeError('maxRestarts must be >= 0');
    if (!Number.isFinite(windowMs) || windowMs <= 0) throw new RangeError('windowMs must be > 0');
    this.id = id;
    this.strategy = validatesStrategy(strategy);
    this.maxRestarts = maxRestarts;
    this.windowMs = windowMs;
  }

  static create(idOrOptions, strategy, maxRestarts, windowMs) {
    if (typeof idOrOptions === 'object') return new Supervisor(idOrOptions);
    return new Supervisor({ id: idOrOptions, strategy, maxRestarts, windowMs });
  }

  get isRunning() { return this.#running; }
  get fatalError() { return this.#fatalError; }

  supervise(id, initialState, handler, options = {}) {
    if (!this.#running) throw new OtpRefusal('SUPERVISOR_NOT_RUNNING_REFUSED', `${this.id} is not running`);
    if (this.#children.some(child => child.id === id)) {
      throw new OtpRefusal('DUPLICATE_CHILD_REFUSED', `child ${id} already exists`, { supervisorId: this.id, childId: id });
    }
    const spec = {
      id,
      initialState,
      handler,
      restart: validatesRestartType(options.restart ?? RestartType.PERMANENT),
      procOptions: { id: `${this.id}/${id}`, ...(options.procOptions ?? {}) },
      ref: null,
      generation: 0,
      removing: false,
    };
    const proc = this.#spawn(spec);
    spec.ref = new ProcRef(proc, { id: `${this.id}/${id}/ref` });
    this.#children.push(spec);
    return spec.ref;
  }

  getRef(id) {
    return this.#children.find(child => child.id === id)?.ref;
  }

  whichChildren() {
    return deepFreeze(this.#children.map((child, index) => ({
      id: child.id,
      index,
      generation: child.generation,
      restart: child.restart,
      processId: child.ref.proc().id,
      running: child.ref.proc().isRunning,
    })));
  }

  async restartChild(id, reason = new Error('manual restart')) {
    const child = this.#children.find(item => item.id === id);
    if (!child) throw new OtpRefusal('UNKNOWN_CHILD_REFUSED', `unknown child ${id}`);
    await this.#enqueueRestart(child, reason, true);
    return child.ref;
  }

  async terminateChild(id) {
    const index = this.#children.findIndex(child => child.id === id);
    if (index < 0) return false;
    const [child] = this.#children.splice(index, 1);
    child.removing = true;
    await child.ref.proc().stop('supervisor_terminate_child');
    this.#record('terminate_child', { childId: id, status: 'ALIVE' });
    return true;
  }

  async shutdown(reason = 'normal') {
    if (!this.#running) return;
    this.#running = false;
    await Promise.allSettled(this.#children.map(async child => {
      child.removing = true;
      await child.ref.proc().stop(reason);
    }));
    this.#record('shutdown', { reason, status: 'ALIVE' });
  }

  receipts() {
    return this.#receipts.map(receipt => receipt);
  }

  #spawn(spec) {
    const proc = Proc.spawn(cloneInitial(spec.initialState), spec.handler, spec.procOptions);
    proc.addTerminationCallback(reason => {
      if (!reason || spec.removing || !this.#running) return;
      void this.#enqueueRestart(spec, reason, false);
    });
    return proc;
  }

  #enqueueRestart(failedChild, reason, manual) {
    this.#restarting = this.#restarting.then(() => this.#handleFailure(failedChild, reason, manual));
    return this.#restarting;
  }

  async #handleFailure(failedChild, reason, manual) {
    if (!this.#running) return;
    if (!manual && !this.#shouldRestart(failedChild.restart, reason)) {
      this.#children = this.#children.filter(child => child !== failedChild);
      this.#record('child_not_restarted', {
        childId: failedChild.id,
        restart: failedChild.restart,
        reason: reason?.message,
        status: 'ALIVE',
      });
      return;
    }

    if (!manual && !this.#admitRestart(reason)) return;
    const failedIndex = this.#children.indexOf(failedChild);
    const affected = switchStrategy(this.strategy, this.#children, failedIndex);

    for (const child of affected) {
      if (child.ref.proc().isRunning) await child.ref.proc().stop('supervisor_restart');
    }
    for (const child of affected) {
      const next = this.#spawn(child);
      const previous = child.ref.swap(next);
      child.generation += 1;
      child.removing = false;
      this.#record('child_restarted', {
        failedChildId: failedChild.id,
        childId: child.id,
        previousProcessId: previous.id,
        processId: next.id,
        generation: child.generation,
        strategy: this.strategy,
        status: 'ALIVE',
      });
    }
  }

  #shouldRestart(restart, reason) {
    if (restart === RestartType.PERMANENT) return true;
    if (restart === RestartType.TEMPORARY) return false;
    return reason != null;
  }

  #admitRestart(reason) {
    const now = Date.now();
    this.#restartTimes = this.#restartTimes.filter(time => now - time <= this.windowMs);
    this.#restartTimes.push(now);
    if (this.#restartTimes.length <= this.maxRestarts) return true;

    this.#fatalError = new OtpRefusal(
      'RESTART_INTENSITY_EXCEEDED',
      `${this.id} exceeded ${this.maxRestarts} restarts in ${this.windowMs} ms`,
      { supervisorId: this.id, reason: reason?.message },
    );
    this.#running = false;
    for (const child of this.#children) {
      child.removing = true;
      if (child.ref.proc().isRunning) void child.ref.proc().stop('restart_intensity_exceeded');
    }
    this.#record('restart_intensity_exceeded', {
      status: 'BLOCKED',
      error: this.#fatalError.message,
    });
    return false;
  }

  #record(activity, payload) {
    const body = {
      receiptType: 'otp.supervisor.transition',
      supervisorId: this.id,
      activity,
      timestamp: new Date().toISOString(),
      ...payload,
    };
    const receipt = deepFreeze({ ...body, receiptDigest: digest(body) });
    this.#receipts.push(receipt);
    return receipt;
  }
}

function switchStrategy(strategy, children, failedIndex) {
  if (failedIndex < 0) return [];
  switch (strategy) {
    case SupervisorStrategy.ONE_FOR_ONE:
      return [children[failedIndex]];
    case SupervisorStrategy.ONE_FOR_ALL:
      return [...children];
    case SupervisorStrategy.REST_FOR_ONE:
      return children.slice(failedIndex);
    default:
      throw new OtpRefusal('INVALID_STRATEGY_REFUSED', `unsupported supervisor strategy: ${strategy}`);
  }
}

export class SupervisorTree extends Supervisor {
  constructor(id, restartStrategy = SupervisorStrategy.ONE_FOR_ONE, options = {}) {
    super({ id, strategy: restartStrategy, ...options });
    this.pending = [];
  }

  addChild(childId, start, childRestartStrategy = RestartType.PERMANENT) {
    if (typeof start !== 'function') throw new TypeError('child start must be a function');
    const restart = Object.values(RestartType).includes(childRestartStrategy)
      ? childRestartStrategy
      : RestartType.PERMANENT;
    this.pending.push({ childId, start, restart });
  }

  async start() {
    for (const pending of this.pending.splice(0)) {
      const start = pending.start;
      this.supervise(
        pending.childId,
        () => ({ starts: 0 }),
        async state => {
          await start();
          return { starts: state.starts + 1 };
        },
        { restart: pending.restart },
      );
      await this.getRef(pending.childId).ask(deepFreeze({ type: 'START' }));
    }
  }

  async restart(childId) {
    const ref = await this.restartChild(childId);
    await ref.ask(deepFreeze({ type: 'START' }));
    return ref;
  }
}
