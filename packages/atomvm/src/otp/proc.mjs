import { randomUUID } from 'node:crypto';
import {
  cloneInitial,
  deepFreeze,
  digest,
  ExitClass,
  ExitSignal,
  normalizeExit,
  OtpRefusal,
  ProcDirective,
  ProcessExit,
} from './values.mjs';

const DEFAULT_REDUCTION_LIMIT = 200;

function scheduleFairly(callback) {
  if (typeof setImmediate === 'function') return setImmediate(callback);
  return setTimeout(callback, 0);
}

function senderId(sender) {
  if (sender == null) return 'anonymous';
  if (typeof sender === 'string') return sender;
  if (typeof sender.id === 'string') return sender.id;
  return String(sender);
}

export class Proc {
  #handler;
  #state;
  #mailbox = [];
  #running = true;
  #suspended = false;
  #scheduled = false;
  #processing = false;
  #trapExits = false;
  #selector;
  #lastError = null;
  #lastExit = null;
  #crashReport = null;
  #currentEnvelope = null;
  #terminationCallbacks = new Set();
  #crashCallbacks = new Set();
  #stateWaiters = [];
  #terminationPromise;
  #resolveTermination;
  #receipts = [];
  #deliveryReceipts = [];
  #sequence = 0;
  #messagesIn = 0;
  #messagesOut = 0;
  #reductions = 0;
  #yields = 0;
  #senderSequences = new Map();

  constructor(initialState, handler, {
    id = `proc-${randomUUID()}`,
    receiptLimit = 4096,
    reductionLimit = DEFAULT_REDUCTION_LIMIT,
    selector = () => true,
  } = {}) {
    if (typeof handler !== 'function') throw new TypeError('handler must be a function');
    if (!Number.isInteger(reductionLimit) || reductionLimit <= 0) {
      throw new RangeError('reductionLimit must be a positive integer');
    }
    if (typeof selector !== 'function') throw new TypeError('selector must be a function');
    this.id = id;
    this.receiptLimit = receiptLimit;
    this.reductionLimit = reductionLimit;
    this.#handler = handler;
    this.#selector = selector;
    this.#state = deepFreeze(cloneInitial(initialState));
    this.#terminationPromise = new Promise(resolve => { this.#resolveTermination = resolve; });
  }

  static spawn(initialState, handler, options) {
    return new Proc(initialState, handler, options);
  }

  get isRunning() { return this.#running; }
  get isSuspended() { return this.#suspended; }
  get lastError() { return this.#lastError; }
  get lastExit() { return this.#lastExit; }

  tell(message) {
    return this.send(message);
  }

  tellFrom(from, message) {
    return this.send(message, { from });
  }

  send(message, { from = null } = {}) {
    this.#admit(message);
    const envelope = this.#envelope('tell', message, { from });
    this.#mailbox.push(envelope);
    this.#messagesIn += 1;
    const receipt = this.#recordDelivery(envelope, 'ALIVE');
    this.#schedule();
    return receipt;
  }

  tryTell(message, { from = null } = {}) {
    if (!this.#running) {
      const envelope = this.#envelope('tell', message, { from, admit: false });
      return this.#recordDelivery(envelope, 'DROPPED', ProcessExit.noproc('noproc', { processId: this.id }));
    }
    try {
      return this.send(message, { from });
    } catch (error) {
      const envelope = this.#envelope('tell', message, { from, admit: false });
      return this.#recordDelivery(envelope, 'REFUSED', error);
    }
  }

  ask(message, timeoutMs = 5000) {
    return this.askFrom(null, message, timeoutMs);
  }

  askFrom(from, message, timeoutMs = 5000) {
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
      const envelope = this.#envelope('ask', message, { from, resolve, reject });
      envelope.timer = setTimeout(() => {
        const index = this.#mailbox.indexOf(envelope);
        if (index >= 0) this.#mailbox.splice(index, 1);
        envelope.expired = true;
        reject(new OtpRefusal(
          'ASK_TIMEOUT_REFUSED',
          `process ${this.id} did not reply within ${timeoutMs} ms`,
          { processId: this.id, timeoutMs, correlationId: envelope.correlationId },
        ));
      }, timeoutMs);
      this.#mailbox.push(envelope);
      this.#recordDelivery(envelope, 'ALIVE');
      this.#schedule();
    });
  }

  setReceiveSelector(selector) {
    if (typeof selector !== 'function') throw new TypeError('selector must be a function');
    this.#selector = selector;
    this.#schedule();
  }

  trapExits(flag = true) {
    this.#trapExits = Boolean(flag);
  }

  isTrappingExits() {
    return this.#trapExits;
  }

  deliverExitSignal(reason, from = null) {
    if (!this.#running) return;
    const exit = normalizeExit(reason, { from, target: this.id });
    if (exit.kind === ExitClass.KILL) {
      this.#terminate(exit, exit.reason, exit);
      return;
    }
    if (this.#trapExits) {
      this.tellFrom(from, new ExitSignal(exit, from));
      return;
    }
    if (exit.kind === ExitClass.NORMAL) return;
    this.#terminate(exit, exit.reason, exit);
  }

  exit(reason = 'normal') {
    if (!this.#running) return;
    const exit = normalizeExit(reason, { processId: this.id });
    const error = exit.kind === ExitClass.NORMAL ? null : exit;
    this.#terminate(error, exit.reason, exit);
  }

  kill(reason = 'kill') {
    if (!this.#running) return;
    const exit = ProcessExit.kill(reason, { processId: this.id });
    this.#terminate(exit, exit.reason, exit);
  }

  crash(reason = new Error('process crashed')) {
    if (!this.#running) return;
    const exit = normalizeExit(reason, { processId: this.id });
    this.#lastError = reason instanceof Error ? reason : exit;
    this.#terminate(this.#lastError, exit.reason, exit);
  }

  async stop(reason = 'normal') {
    if (!this.#running) return;
    this.exit(reason);
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
      deliverableDepth: this.#deliverableCount(),
      running: this.#running,
      suspended: this.#suspended,
      sequence: this.#sequence,
      reductions: this.#reductions,
      yields: this.#yields,
      reductionLimit: this.reductionLimit,
      currentMessageDigest: this.#currentEnvelope ? digest(this.#currentEnvelope.message) : null,
      exitClass: this.#lastExit?.kind ?? null,
    });
  }

  snapshot() {
    return deepFreeze({
      processId: this.id,
      stateDigest: digest(this.#state),
      mailbox: this.#mailbox.map(envelope => ({
        kind: envelope.kind,
        from: envelope.from,
        senderSequence: envelope.senderSequence,
        correlationId: envelope.correlationId,
        messageType: envelope.message?.type ?? typeof envelope.message,
        messageDigest: digest(envelope.message),
        enqueuedAt: envelope.enqueuedAt,
      })),
      statistics: this.statistics(),
      crashReport: this.#crashReport,
    });
  }

  crashReport() {
    return this.#crashReport;
  }

  receipts() {
    return this.#receipts.map(receipt => receipt);
  }

  deliveryReceipts() {
    return this.#deliveryReceipts.map(receipt => receipt);
  }

  #envelope(kind, message, { from = null, resolve = null, reject = null, admit = true } = {}) {
    if (admit) this.#admit(message);
    const fromId = senderId(from);
    const senderSequence = (this.#senderSequences.get(fromId) ?? 0) + 1;
    this.#senderSequences.set(fromId, senderSequence);
    return {
      kind,
      message,
      from: fromId,
      senderSequence,
      correlationId: kind === 'ask' ? `call-${randomUUID()}` : null,
      enqueuedAt: new Date().toISOString(),
      resolve,
      reject,
      timer: null,
      expired: false,
    };
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

  #schedule({ fair = false } = {}) {
    if (this.#scheduled || this.#processing || this.#suspended || !this.#running) return;
    this.#scheduled = true;
    const run = () => {
      this.#scheduled = false;
      void this.#drain();
    };
    if (fair) scheduleFairly(run);
    else queueMicrotask(run);
  }

  #matches(envelope) {
    try {
      return this.#selector(this.#state, envelope.message, deepFreeze({
        kind: envelope.kind,
        from: envelope.from,
        senderSequence: envelope.senderSequence,
        correlationId: envelope.correlationId,
      })) === true;
    } catch (error) {
      this.crash(error);
      return false;
    }
  }

  #nextEnvelopeIndex() {
    for (let index = 0; index < this.#mailbox.length; index += 1) {
      if (this.#matches(this.#mailbox[index])) return index;
      if (!this.#running) return -1;
    }
    return -1;
  }

  #deliverableCount() {
    if (!this.#running) return 0;
    let count = 0;
    for (const envelope of this.#mailbox) if (this.#matches(envelope)) count += 1;
    return count;
  }

  async #drain() {
    if (this.#processing || this.#suspended || !this.#running) return;
    this.#processing = true;
    let turnReductions = 0;
    try {
      while (this.#running && !this.#suspended && this.#mailbox.length > 0) {
        const index = this.#nextEnvelopeIndex();
        if (index < 0) break;
        const [envelope] = this.#mailbox.splice(index, 1);
        this.#currentEnvelope = envelope;
        const before = this.#state;
        const startedAt = new Date().toISOString();
        try {
          const returned = await this.#handler(before, envelope.message, deepFreeze({
            from: envelope.from,
            senderSequence: envelope.senderSequence,
            correlationId: envelope.correlationId,
          }));
          const directive = returned instanceof ProcDirective ? returned : ProcDirective.continue(returned);
          if (directive.state === undefined) {
            throw new OtpRefusal('UNDEFINED_STATE_REFUSED', `handler for ${this.id} returned undefined`);
          }
          this.#state = deepFreeze(directive.state);
          if (directive.hasSelector) this.#selector = directive.selector;
          this.#messagesOut += 1;
          this.#reductions += 1;
          turnReductions += 1;
          this.#recordTransition({
            envelope,
            before,
            after: this.#state,
            status: 'ALIVE',
            startedAt,
          });
          if (envelope.timer) clearTimeout(envelope.timer);
          if (envelope.kind === 'ask' && !envelope.expired) {
            envelope.resolve(directive.hasReply ? directive.reply : this.#state);
          }
          this.#resolveStateWaiters();
        } catch (error) {
          this.#messagesOut += 1;
          this.#reductions += 1;
          turnReductions += 1;
          this.#recordTransition({
            envelope,
            before,
            after: before,
            status: 'BLOCKED',
            error,
            startedAt,
          });
          if (envelope.timer) clearTimeout(envelope.timer);
          if (envelope.kind === 'ask' && !envelope.expired) envelope.reject(error);
          this.#lastError = error instanceof Error ? error : new Error(String(error));
          this.#terminate(this.#lastError, this.#lastError.message, normalizeExit(this.#lastError, { processId: this.id }));
          break;
        } finally {
          this.#currentEnvelope = null;
        }
        if (turnReductions >= this.reductionLimit) break;
      }
    } finally {
      this.#processing = false;
      this.#resolveStateWaiters();
      if (this.#running && !this.#suspended && this.#mailbox.length > 0 && this.#nextEnvelopeIndex() >= 0) {
        const fair = turnReductions >= this.reductionLimit;
        if (fair) this.#yields += 1;
        this.#schedule({ fair });
      }
    }
  }

  #recordDelivery(envelope, status, error = null) {
    const body = {
      receiptType: 'otp.message.delivery',
      processId: this.id,
      from: envelope.from,
      senderSequence: envelope.senderSequence,
      correlationId: envelope.correlationId,
      messageDigest: digest(envelope.message),
      status,
      error: error ? { name: error.name, message: error.message, code: error.code, kind: error.kind } : null,
      timestamp: new Date().toISOString(),
    };
    const receipt = deepFreeze({ ...body, receiptDigest: digest(body) });
    this.#deliveryReceipts.push(receipt);
    if (this.#deliveryReceipts.length > this.receiptLimit) this.#deliveryReceipts.shift();
    return receipt;
  }

  #recordTransition({ envelope, before, after, status, error = null, startedAt }) {
    const body = {
      receiptType: 'otp.process.transition',
      processId: this.id,
      sequence: ++this.#sequence,
      from: envelope.from,
      senderSequence: envelope.senderSequence,
      correlationId: envelope.correlationId,
      messageDigest: digest(envelope.message),
      beforeDigest: digest(before),
      afterDigest: digest(after),
      status,
      error: error ? { name: error.name, message: error.message, code: error.code, kind: error.kind } : null,
      startedAt,
      completedAt: new Date().toISOString(),
    };
    const receipt = deepFreeze({ ...body, receiptDigest: digest(body) });
    this.#receipts.push(receipt);
    if (this.#receipts.length > this.receiptLimit) this.#receipts.shift();
    return receipt;
  }

  #resolveStateWaiters() {
    if (this.#processing) return;
    const waiters = this.#stateWaiters.splice(0);
    for (const waiter of waiters) waiter.resolve(this.#state);
  }

  #terminate(error, reason = null, exit = normalizeExit(error ?? reason, { processId: this.id })) {
    if (!this.#running) return;
    this.#running = false;
    this.#suspended = false;
    this.#lastExit = exit;
    if (error && !this.#lastError) this.#lastError = error;

    const pending = this.#mailbox.splice(0);
    for (const envelope of pending) {
      if (envelope.timer) clearTimeout(envelope.timer);
      if (envelope.kind === 'ask' && !envelope.expired) {
        envelope.reject(new OtpRefusal(
          'PROCESS_TERMINATED_REFUSED',
          `process ${this.id} terminated before replying`,
          { processId: this.id, reason: exit.message, exitClass: exit.kind },
        ));
      }
    }

    const stateWaiters = this.#stateWaiters.splice(0);
    for (const waiter of stateWaiters) waiter.resolve(this.#state);

    const termination = deepFreeze({
      processId: this.id,
      error,
      reason,
      exit,
      state: this.#state,
      statistics: this.statistics(),
      pendingMessageDigests: pending.map(envelope => digest(envelope.message)),
    });
    if (error) {
      this.#crashReport = deepFreeze({
        reportType: 'otp.process.crash',
        processId: this.id,
        exit,
        error: { name: error.name, message: error.message, code: error.code },
        currentMessageDigest: this.#currentEnvelope ? digest(this.#currentEnvelope.message) : null,
        stateDigest: digest(this.#state),
        mailboxDepth: pending.length,
        reductions: this.#reductions,
        timestamp: new Date().toISOString(),
      });
      for (const callback of [...this.#crashCallbacks]) {
        try { callback(error, this, termination); } catch { /* callbacks cannot mutate termination */ }
      }
    }
    for (const callback of [...this.#terminationCallbacks]) {
      try { callback(error, this, termination); } catch { /* observation cannot mutate termination */ }
    }
    this.#resolveTermination(termination);
  }
}
