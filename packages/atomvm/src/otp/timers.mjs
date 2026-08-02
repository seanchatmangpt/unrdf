import { randomUUID } from 'node:crypto';
import { ProcRef } from './references.mjs';

export class TimerRef {
  #handle;
  #interval;
  #cancelled = false;

  constructor(handle, interval) {
    this.id = `timer-${randomUUID()}`;
    this.#handle = handle;
    this.#interval = interval;
  }

  cancel() {
    if (this.#cancelled) return false;
    this.#cancelled = true;
    if (this.#interval) clearInterval(this.#handle);
    else clearTimeout(this.#handle);
    return true;
  }

  get cancelled() { return this.#cancelled; }
}

export const ProcTimer = Object.freeze({
  sendAfter(delayMs, target, message) {
    const proc = target instanceof ProcRef ? target : target;
    const handle = setTimeout(() => {
      try { proc.tell(message); } catch { /* target lifecycle owns refusal */ }
    }, delayMs);
    return new TimerRef(handle, false);
  },
  sendInterval(periodMs, target, message) {
    const proc = target instanceof ProcRef ? target : target;
    const handle = setInterval(() => {
      try { proc.tell(message); } catch { /* target lifecycle owns refusal */ }
    }, periodMs);
    return new TimerRef(handle, true);
  },
  cancel(ref) { return ref.cancel(); },
});

export const ProcSys = Object.freeze({
  getState(target) {
    const proc = target instanceof ProcRef ? target.proc() : target;
    return proc.state();
  },
  suspend(target) {
    const proc = target instanceof ProcRef ? target.proc() : target;
    proc.suspend();
  },
  resume(target) {
    const proc = target instanceof ProcRef ? target.proc() : target;
    proc.resume();
  },
  statistics(target) {
    const proc = target instanceof ProcRef ? target.proc() : target;
    return proc.statistics();
  },
  receipts(target) {
    const proc = target instanceof ProcRef ? target.proc() : target;
    return proc.receipts();
  },
});
