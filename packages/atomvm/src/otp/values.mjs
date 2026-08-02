import { createHash } from 'node:crypto';

function canonical(value) {
  if (value === undefined) return 'null';
  if (value === null || typeof value !== 'object') return JSON.stringify(value);
  if (value instanceof Error) {
    return canonical({ name: value.name, message: value.message, code: value.code });
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

export class ExitSignal {
  constructor(reason, from = null) {
    this.type = 'EXIT';
    this.reason = reason;
    this.from = from;
    deepFreeze(this);
  }
}
