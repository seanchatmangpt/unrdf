import { deepFreeze, digest, OtpRefusal } from './process.mjs';

export const StartType = Object.freeze({ NORMAL: 'normal', TAKEOVER: 'takeover', FAILOVER: 'failover' });
export const RunType = Object.freeze({ PERMANENT: 'permanent', TRANSIENT: 'transient', TEMPORARY: 'temporary' });

export class ApplicationSpec {
  constructor({ name, start, stop, dependencies = [], runType = RunType.PERMANENT }) {
    if (typeof name !== 'string' || name.length === 0) throw new TypeError('application name is required');
    if (typeof start !== 'function' || typeof stop !== 'function') {
      throw new TypeError('application start and stop callbacks are required');
    }
    this.name = name;
    this.start = start;
    this.stop = stop;
    this.dependencies = Object.freeze([...dependencies]);
    this.runType = runType;
    Object.freeze(this);
  }
}

export class ApplicationController {
  static #applications = new Map();
  static #receipts = [];

  static async start(specLike, startType = StartType.NORMAL) {
    const spec = specLike instanceof ApplicationSpec ? specLike : new ApplicationSpec(specLike);
    if (this.#applications.has(spec.name)) {
      throw new OtpRefusal('APPLICATION_ALREADY_STARTED_REFUSED', `${spec.name} is already started`);
    }
    for (const dependency of spec.dependencies) {
      if (!this.#applications.has(dependency)) {
        throw new OtpRefusal(
          'APPLICATION_DEPENDENCY_MISSING_REFUSED',
          `${spec.name} requires ${dependency}`,
          { application: spec.name, dependency },
        );
      }
    }

    const startedAt = new Date().toISOString();
    try {
      const state = await spec.start(startType);
      const application = { spec, state, startedAt };
      this.#applications.set(spec.name, application);
      this.#record('start', spec.name, 'ALIVE', { startType, stateDigest: digest(state) });
      return state;
    } catch (error) {
      this.#record('start', spec.name, 'BLOCKED', { error: error.message });
      throw error;
    }
  }

  static async stop(name, reason = 'normal') {
    const application = this.#applications.get(name);
    if (!application) return false;
    try {
      await application.spec.stop(application.state, reason);
      this.#applications.delete(name);
      this.#record('stop', name, 'ALIVE', { reason });
      return true;
    } catch (error) {
      this.#record('stop', name, 'BLOCKED', { reason, error: error.message });
      throw error;
    }
  }

  static whichApplications() {
    return deepFreeze([...this.#applications.entries()].map(([name, app]) => ({
      name,
      runType: app.spec.runType,
      startedAt: app.startedAt,
      dependencies: [...app.spec.dependencies],
    })).sort((left, right) => left.name.localeCompare(right.name)));
  }

  static receipts() {
    return this.#receipts.map(receipt => receipt);
  }

  static async reset() {
    for (const name of [...this.#applications.keys()].reverse()) await this.stop(name, 'reset');
    this.#applications.clear();
    this.#receipts = [];
  }

  static #record(activity, name, status, details) {
    const body = {
      receiptType: 'otp.application.lifecycle',
      activity,
      application: name,
      status,
      details,
      timestamp: new Date().toISOString(),
    };
    const receipt = deepFreeze({ ...body, receiptDigest: digest(body) });
    this.#receipts.push(receipt);
    return receipt;
  }
}
