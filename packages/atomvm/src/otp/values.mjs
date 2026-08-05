import { createHash } from 'node:crypto';

function canonical(value) {
  if (value === undefined) return 'null';
  if (typeof value === 'symbol') return JSON.stringify(value.description ?? 'symbol');
  if (typeof value === 'function') return JSON.stringify(`[Function:${value.name || 'anonymous'}]`);
  if (value === null || typeof value !== 'object') return JSON.stringify(value);
  if (value instanceof Error) {
    return canonical({ name: value.name, message: value.message, code: value.code, kind: value.kind });
  }
  if (Array.isArray(value)) return `[${value.map(canonical).join(',')}]`;
  const keys = Object.keys(value).sort();
  return `{${keys.map(key => `${JSON.stringify(key)}:${canonical(value[key])}`).join(',')}}`;
}

export function digest(value) {
  return createHash('sha256').update(canonical(value)).digest('hex');
}

export function deepFreeze(value, seen = new WeakSet()) {
  if (!value || typeof value !== 'object' || seen.has(value)) return value;
  seen.add(value);
  for (const child of Object.values(value)) deepFreeze(child, seen);
  return Object.freeze(value);
}

export function immutableMessage(type, fields = {}) {
  if (typeof type !== 'string' || type.length === 0) {
    throw new TypeError('message type must be a non-empty string');
  }
  let cloned;
  try { cloned = structuredClone(fields); } catch { cloned = { ...fields }; }
  return deepFreeze({ type, ...cloned });
}

export function cloneInitial(value) {
  if (typeof value === 'function') return value();
  try {
    return structuredClone(value);
  } catch {
    return value;
  }
}

export class OtpRefusal extends Error {
  constructor(code, message, details = {}) {
    super(message);
    this.name = 'OtpRefusal';
    this.code = code;
    this.details = deepFreeze({ ...details });
  }
}

export const ExitClass = Object.freeze({
  NORMAL: 'normal',
  SHUTDOWN: 'shutdown',
  ERROR: 'error',
  KILL: 'kill',
  NOPROC: 'noproc',
});

export class ProcessExit extends Error {
  constructor(kind, reason, details = {}) {
    const normalizedKind = Object.values(ExitClass).includes(kind) ? kind : ExitClass.ERROR;
    const normalizedReason = reason instanceof Error ? reason.message : String(reason ?? normalizedKind);
    super(normalizedReason);
    this.name = 'ProcessExit';
    this.code = `PROCESS_EXIT_${normalizedKind.toUpperCase()}`;
    this.kind = normalizedKind;
    this.reason = reason;
    this.details = deepFreeze({ ...details });
  }

  static normal(reason = 'normal', details) { return new ProcessExit(ExitClass.NORMAL, reason, details); }
  static shutdown(reason = 'shutdown', details) { return new ProcessExit(ExitClass.SHUTDOWN, reason, details); }
  static error(reason = 'error', details) { return new ProcessExit(ExitClass.ERROR, reason, details); }
  static kill(reason = 'kill', details) { return new ProcessExit(ExitClass.KILL, reason, details); }
  static noproc(reason = 'noproc', details) { return new ProcessExit(ExitClass.NOPROC, reason, details); }
}

export function normalizeExit(reason, details = {}) {
  if (reason instanceof ProcessExit) return reason;
  if (reason instanceof Error) return ProcessExit.error(reason, details);
  if (reason === 'normal' || reason == null) return ProcessExit.normal(reason ?? 'normal', details);
  if (reason === 'shutdown') return ProcessExit.shutdown(reason, details);
  if (reason === 'kill' || reason === 'killed') return ProcessExit.kill(reason, details);
  if (reason === 'noproc') return ProcessExit.noproc(reason, details);
  return ProcessExit.error(reason, details);
}

export class ExitSignal {
  constructor(reason, from = null) {
    const exit = normalizeExit(reason, { from });
    this.type = 'EXIT';
    this.exit = exit;
    this.reason = exit.reason;
    this.from = from;
    deepFreeze(this);
  }
}

export class DownSignal {
  constructor(ref, target, reason) {
    const exit = normalizeExit(reason, { target });
    this.type = 'DOWN';
    this.ref = ref;
    this.target = target;
    this.exit = exit;
    this.reason = exit.reason;
    deepFreeze(this);
  }
}

export class ProcDirective {
  constructor(state, { reply, hasReply = false, selector, hasSelector = false } = {}) {
    this.state = state;
    this.reply = reply;
    this.hasReply = hasReply;
    this.selector = selector;
    this.hasSelector = hasSelector;
    Object.freeze(this);
  }

  static continue(state) {
    return new ProcDirective(state);
  }

  static reply(reply, state) {
    return new ProcDirective(state, { reply, hasReply: true });
  }

  static receive(state, selector) {
    if (typeof selector !== 'function') throw new TypeError('receive selector must be a function');
    return new ProcDirective(state, { selector, hasSelector: true });
  }

  static replyAndReceive(reply, state, selector) {
    if (typeof selector !== 'function') throw new TypeError('receive selector must be a function');
    return new ProcDirective(state, { reply, hasReply: true, selector, hasSelector: true });
  }
}
