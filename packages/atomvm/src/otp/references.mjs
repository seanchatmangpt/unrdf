import { randomUUID } from 'node:crypto';
import { Proc } from './proc.mjs';
import { OtpRefusal } from './values.mjs';

export class ProcRef {
  #delegate;
  #swapCallbacks = new Set();

  constructor(initial, { id = `ref-${randomUUID()}` } = {}) {
    if (!(initial instanceof Proc)) throw new TypeError('ProcRef requires a Proc');
    this.id = id;
    this.generation = 0;
    this.#delegate = initial;
  }

  tell(message) { return this.#delegate.tell(message); }
  ask(message, timeoutMs) { return this.#delegate.ask(message, timeoutMs); }
  stop(reason) { return this.#delegate.stop(reason); }
  proc() { return this.#delegate; }

  swap(next) {
    if (!(next instanceof Proc)) throw new TypeError('ProcRef.swap requires a Proc');
    const previous = this.#delegate;
    this.#delegate = next;
    this.generation += 1;
    for (const callback of [...this.#swapCallbacks]) callback(next, previous, this.generation);
    return previous;
  }

  onSwap(callback) {
    this.#swapCallbacks.add(callback);
    return () => this.#swapCallbacks.delete(callback);
  }
}

export class ProcRegistry {
  static #registry = new Map();

  static register(name, target) {
    if (typeof name !== 'string' || name.length === 0) throw new TypeError('registry name must be non-empty');
    if (!(target instanceof Proc) && !(target instanceof ProcRef)) {
      throw new TypeError('registry target must be Proc or ProcRef');
    }
    const current = this.#registry.get(name);
    if (current && this.whereis(name)) {
      throw new OtpRefusal('DUPLICATE_NAME_REFUSED', `process name ${name} is already registered`, { name });
    }

    const entry = { target, unobserve: null, unswap: null };
    const observe = proc => proc.addTerminationCallback((_reason, terminated) => {
      const active = target instanceof ProcRef ? target.proc() : target;
      if (active === terminated) this.unregister(name);
    });
    entry.unobserve = observe(target instanceof ProcRef ? target.proc() : target);
    if (target instanceof ProcRef) {
      entry.unswap = target.onSwap(next => {
        entry.unobserve?.();
        entry.unobserve = observe(next);
      });
    }
    this.#registry.set(name, entry);
    return target;
  }

  static whereis(name) {
    const entry = this.#registry.get(name);
    if (!entry) return undefined;
    const proc = entry.target instanceof ProcRef ? entry.target.proc() : entry.target;
    if (!proc.isRunning) {
      this.unregister(name);
      return undefined;
    }
    return entry.target;
  }

  static unregister(name) {
    const entry = this.#registry.get(name);
    if (!entry) return false;
    entry.unobserve?.();
    entry.unswap?.();
    return this.#registry.delete(name);
  }

  static registered() {
    for (const name of [...this.#registry.keys()]) this.whereis(name);
    return Object.freeze([...this.#registry.keys()].sort());
  }

  static reset() {
    for (const name of [...this.#registry.keys()]) this.unregister(name);
  }
}
