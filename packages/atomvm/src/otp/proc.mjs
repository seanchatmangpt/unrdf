import { randomUUID } from 'node:crypto';
import { cloneInitial, deepFreeze, digest, ExitSignal, OtpRefusal } from './values.mjs';

export class Proc {
  #handler;
  #state;
  #mailbox = [];
  #running = true;
  #suspended = false;
  #scheduled = false;
  #processing = false;
  #trapExits = false;
  #lastError = null;
  #terminationCallbacks = new Set();
  #crashCallbacks = new Set();
  #stateWaiters = [];
  #terminationPromise;
  #resolveTermination;
  #receipts = [];
  #sequence = 0;
  #messagesIn = 0;
  #messagesOut = 0;

  constructor(initialState, handler, { id = `proc-${randomUUID()}`, receiptLimit = 4096 } = {}) {
    if (typeof handler !== 'function') throw new TypeError('handler must be a function');
    this.id = id;
    this.receiptLimit = receiptLimit;
    this.#handler = handler;
    this.#state = deepFreeze(cloneInitial(initialState));
    this.#terminationPromise = new Promise(resolve => { this.#resolveTermination = resolve; });
  }

  static spawn(initialState, handler, options) {
    return new Proc(initialState, handler, options);
  }

  get isRunning() { return this.#running; }
  get isSuspended() { return this.#suspended; }
  get lastError() { return this.#lastError; }

  tell(message) {
    this.#admit(message);
    this.#mailbox.push({ kind: 'tell', message });
    this.#messagesIn += 1;
    this.#schedule();
  }

  ask(message, timeoutMs = 5000) {
    this.#admit(message);
    if (!Number.isFinite(timeoutMs) || timeoutMs <= 0) {
      return Promise.reject(new OtpRefusal(
        'INVALID_TIMEOUT_REFUSED',
        'ask timeout must be a positive finite number',
        { timeoutMs },
      ));
    }

    this.#messagesIn += 1;
    return new Promise((resolve, reject) => {
      const envelope = { kind: 'ask', message, resolve, reject, timer: null };
      envelope.timer = setTimeout(() => {
        const index = this.#mailbox.indexOf(envelope);
        if (index >= 0) this.#mailbox.splice(index, 1);
        reject(new OtpRefusal(
          'ASK_TIMEOUT_REFUSED',
          `process ${this.id} did not reply within ${timeoutMs} ms`,
          { processId: this.id, timeoutMs },
        ));
      }, timeoutMs);
      this.#mailbox.push(envelope);
      this.#schedule();
    });
  }

  trapExits(flag = true) {
    this.#trapExits = Boolean(flag);
  }

  isTrappingExits() {
    return this.#trapExits;
  }

  deliverExitSignal(reason, from = null) {
    if (!this.#running) return;
    if (this.#trapExits) {
      this.tell(new ExitSignal(reason, from));
      return;
    }
    this.crash(reason instanceof Error ? reason : new Error(String(reason ?? 'linked process exited')));
  }

  crash(reason = new Error('process crashed')) {
    if (!this.#running) return;
    this.#lastError = reason instanceof Error ? reason : new Error(String(reason));
    this.#terminate(this.#lastError);
  }

  async stop(reason = 'normal') {
    if (!this.#running) return;
    this.#terminate(null, reason);
    await this.#terminationPromise;
  }

  awaitTermination() {
    return this.#terminationPromise;
  }

  addCrashCallback(callback) {
    if (typeof callback !== 'function') throw new TypeError('crash callback must be a function');
    this.#crashCallbacks.add(callback);
    return () => this.#crashCallbacks.delete(callback);
  }

  addTerminationCallback(callback) {
    if (typeof callback !== 'function') throw new TypeError('termination callback must be a function');
    this.#terminationCallbacks.add(callback);
    return () => this.#terminationCallbacks.delete(callback);
  }

  removeTerminationCallback(callback) {
    return this.#terminationCallbacks.delete(callback);
  }

  suspend() {
    if (!this.#running) throw new OtpRefusal('PROCESS_NOT_RUNNING_REFUSED', `process ${this.id} is not running`);
    this.#suspended = true;
  }

  resume() {
    if (!this.#running) throw new OtpRefusal('PROCESS_NOT_RUNNING_REFUSED', `process ${this.id} is not running`);
    this.#suspended = false;
    this.#schedule();
  }

  async state() {
    if (!this.#running && !this.#processing) return this.#state;
    if (!this.#processing) return this.#state;
    return new Promise((resolve, reject) => {
      this.#stateWaiters.push({ resolve, reject });
      this.#schedule();
    });
  }

  statistics() {
    return deepFreeze({
      processId: this.id,
      messagesIn: this.#messagesIn,
      messagesOut: this.#messagesOut,
      queueDepth: this.#mailbox.length,
      running: this.#running,
      suspended: this.#suspended,
      sequence: this.#sequence,
    });
  }

  receipts() {
    return this.#receipts.map(receipt => receipt);
  }

  #admit(message) {
    if (!this.#running) {
      throw new OtpRefusal('PROCESS_NOT_RUNNING_REFUSED', `process ${this.id} is not running`, { processId: this.id });
    }
    if (message && typeof message === 'object' && !Object.isFrozen(message)) {
      throw new OtpRefusal(
        'MUTABLE_MESSAGE_REFUSED',
        'object messages must be deeply frozen before crossing a process boundary',
        { processId: this.id, messageType: message.constructor?.name ?? 'Object' },
      );
    }
  }

  #schedule() {
    if (this.#scheduled || this.#processing || this.#suspended || !this.#running) return;
    this.#scheduled = true;
    queueMicrotask(() => {
      this.#scheduled = false;
      void this.#drain();
    });
  }

  async #drain() {
    if (this.#processing || this.#suspended || !this.#running) return;
    this.#processing = true;
    try {
      while (this.#running && !this.#suspended && this.#mailbox.length > 0) {
        const envelope = this.#mailbox.shift();
        const before = this.#state;
        const startedAt = new Date().toISOString();
        try {
          const next = await this.#handler(before, envelope.message);
          if (next === undefined) {
            throw new OtpRefusal('UNDEFINED_STATE_REFUSED', `handler for ${this.id} returned undefined`);
          }
          this.#state = deepFreeze(next);
          this.#messagesOut += 1;
          const receipt = this.#recordTransition({
            message: envelope.message,
            before,
            after: this.#state,
            status: 'ALIVE',
            startedAt,
          });
          if (envelope.timer) clearTimeout(envelope.timer);
          if (envelope.kind === 'ask') envelope.resolve(this.#state);
          this.#resolveStateWaiters();
          void receipt;
        } catch (error) {
          this.#messagesOut += 1;
          this.#recordTransition({
            message: envelope.message,
            before,
            after: before,
            status: envelope.kind === 'ask' ? 'REFUSED' : 'BLOCKED',
            error,
            startedAt,
          });
          if (envelope.timer) clearTimeout(envelope.timer);
          if (envelope.kind === 'ask') {
            envelope.reject(error);
            this.#resolveStateWaiters();
            continue;
          }
          this.#lastError = error instanceof Error ? error : new Error(String(error));
          this.#terminate(this.#lastError);
          break;
        }
      }
    } finally {
      this.#processing = false;
      this.#resolveStateWaiters();
      if (this.#mailbox.length > 0) this.#schedule();
    }
  }

  #recordTransition({ message, before, after, status, error = null, startedAt }) {
    const body = {
      receiptType: 'otp.process.transition',
      processId: this.id,
      sequence: ++this.#sequence,
      messageDigest: digest(message),
      beforeDigest: digest(before),
      afterDigest: digest(after),
      status,
      error: error ? { name: error.name, message: error.message, code: error.code } : null,
      startedAt,
      completedAt: new Date().toISOString(),
    };
    const receipt = deepFreeze({ ...body, receiptDigest: digest(body) });
    this.#receipts.push(receipt);
    if (this.#receipts.length > this.receiptLimit) this.#receipts.shift();
    return receipt;
  }

  #resolveStateWaiters() {
    if (this.#processing || this.#mailbox.length > 0) return;
    const waiters = this.#stateWaiters.splice(0);
    for (const waiter of waiters) waiter.resolve(this.#state);
  }

  #terminate(error, reason = null) {
    if (!this.#running) return;
    this.#running = false;
    this.#suspended = false;

    const pending = this.#mailbox.splice(0);
    for (const envelope of pending) {
      if (envelope.timer) clearTimeout(envelope.timer);
      if (envelope.kind === 'ask') {
        envelope.reject(new OtpRefusal(
          'PROCESS_TERMINATED_REFUSED',
          `process ${this.id} terminated before replying`,
          { processId: this.id, reason: error?.message ?? reason },
        ));
      }
    }

    const stateWaiters = this.#stateWaiters.splice(0);
    for (const waiter of stateWaiters) waiter.resolve(this.#state);

    if (error) {
      for (const callback of [...this.#crashCallbacks]) {
        try { callback(error, this); } catch { /* callbacks cannot mutate termination */ }
      }
    }
    for (const callback of [...this.#terminationCallbacks]) {
      try { callback(error, this); } catch { /* observation cannot mutate termination */ }
    }
    this.#resolveTermination(deepFreeze({ processId: this.id, error, reason, state: this.#state }));
  }
}
