import { createHash } from 'node:crypto';

const TYPE = '$unrdfType';

function encodeBytes(value) {
  if (value instanceof Uint8Array) return Buffer.from(value).toString('base64');
  if (Buffer.isBuffer(value)) return value.toString('base64');
  return null;
}

function normalize(value, seen) {
  if (value === null || typeof value === 'boolean' || typeof value === 'string') return value;
  if (typeof value === 'number') {
    if (Number.isFinite(value)) return value;
    return { [TYPE]: 'number', value: String(value) };
  }
  if (value === undefined) return { [TYPE]: 'undefined' };
  if (typeof value === 'bigint') return { [TYPE]: 'bigint', value: value.toString() };
  if (typeof value === 'symbol') return { [TYPE]: 'symbol', value: value.description ?? '' };
  if (typeof value === 'function') throw new TypeError('Functions cannot be canonicalized');
  if (seen.has(value)) throw new TypeError('Cycles cannot be canonicalized');
  seen.add(value);
  try {
    if (value instanceof Date) {
      if (Number.isNaN(value.getTime())) throw new TypeError('Invalid Date cannot be canonicalized');
      return { [TYPE]: 'date', value: value.toISOString() };
    }
    const bytes = encodeBytes(value);
    if (bytes !== null) return { [TYPE]: 'bytes', value: bytes };
    if (value instanceof Set) {
      const items = [...value].map(item => normalize(item, seen));
      items.sort((left, right) => JSON.stringify(left).localeCompare(JSON.stringify(right)));
      return { [TYPE]: 'set', value: items };
    }
    if (value instanceof Map) {
      const entries = [...value].map(([key, item]) => [normalize(key, seen), normalize(item, seen)]);
      entries.sort((left, right) => JSON.stringify(left[0]).localeCompare(JSON.stringify(right[0])));
      return { [TYPE]: 'map', value: entries };
    }
    if (Array.isArray(value)) return value.map(item => normalize(item, seen));
    const output = {};
    for (const key of Object.keys(value).sort()) output[key] = normalize(value[key], seen);
    return output;
  } finally {
    seen.delete(value);
  }
}

/**
 * Canonical JSON that preserves values regular JSON loses.
 */
export function canonicalJson(value) {
  return JSON.stringify(normalize(value, new WeakSet()));
}

export function deterministicDigest(value, algorithm = 'sha256') {
  return createHash(algorithm).update(canonicalJson(value)).digest('hex');
}

function decode(value) {
  if (Array.isArray(value)) return value.map(decode);
  if (!value || typeof value !== 'object') return value;
  if (TYPE in value) {
    switch (value[TYPE]) {
      case 'undefined': return undefined;
      case 'bigint': return BigInt(value.value);
      case 'date': return new Date(value.value);
      case 'bytes': return Uint8Array.from(Buffer.from(value.value, 'base64'));
      case 'set': return new Set(value.value.map(decode));
      case 'map': return new Map(value.value.map(([key, item]) => [decode(key), decode(item)]));
      case 'number': return Number(value.value);
      case 'symbol': return Symbol(value.value);
      default: throw new TypeError(`Unsupported canonical value type: ${value[TYPE]}`);
    }
  }
  return Object.fromEntries(Object.entries(value).map(([key, item]) => [key, decode(item)]));
}

export function parseCanonicalJson(text) {
  return decode(JSON.parse(text));
}

function assertSafeArtifactPath(path) {
  if (typeof path !== 'string' || !path || path.includes('\0')) throw new TypeError('Artifact path is required');
  if (path.startsWith('/') || path.startsWith('\\') || /^[A-Za-z]:[\\/]/.test(path)) {
    throw new TypeError('Artifact path must be relative');
  }
  const segments = path.replaceAll('\\', '/').split('/');
  if (segments.some(segment => segment === '..' || segment === '')) {
    throw new TypeError('Artifact path must be normalized and contained');
  }
  return segments.join('/');
}

export function createArtifactManifest(artifacts, { algorithm = 'sha256' } = {}) {
  if (!Array.isArray(artifacts)) throw new TypeError('artifacts must be an array');
  const seen = new Set();
  const entries = artifacts.map(artifact => {
    const path = assertSafeArtifactPath(artifact.path);
    if (seen.has(path)) throw new TypeError(`Duplicate artifact path: ${path}`);
    seen.add(path);
    const bytes = typeof artifact.content === 'string'
      ? Buffer.from(artifact.content, artifact.encoding ?? 'utf8')
      : Buffer.from(artifact.content ?? []);
    return Object.freeze({
      path,
      mediaType: String(artifact.mediaType ?? 'application/octet-stream'),
      size: bytes.length,
      algorithm,
      digest: createHash(algorithm).update(bytes).digest('hex'),
    });
  }).sort((left, right) => left.path.localeCompare(right.path));
  return Object.freeze(entries);
}

function createReceipt(previous, event) {
  const body = {
    sequence: previous ? previous.sequence + 1 : 0,
    previousDigest: previous?.digest ?? null,
    event,
  };
  return Object.freeze({ ...body, digest: deterministicDigest(body) });
}

function clone(value) {
  return parseCanonicalJson(canonicalJson(value));
}

const STATES = Object.freeze({ OPEN: 'OPEN', SEALED: 'SEALED' });

/**
 * Deterministic execution capsule builder.
 */
export class CapsuleBuilder {
  #state = STATES.OPEN;
  #receipts = [];
  #inputs = new Map();
  #tools = new Map();
  #changes = [];
  #artifacts = [];
  #identity;

  constructor(identity) {
    if (!identity?.subject || !identity?.source) {
      throw new TypeError('Capsule identity requires subject and source');
    }
    this.#identity = clone(identity);
    this.#record({ type: 'CAPSULE_OPENED', identityDigest: deterministicDigest(identity) });
  }

  get state() { return this.#state; }

  #assertOpen() {
    if (this.#state !== STATES.OPEN) throw new Error(`Capsule is ${this.#state}`);
  }

  #record(event) {
    this.#receipts.push(createReceipt(this.#receipts.at(-1), event));
  }

  addInput(name, value) {
    this.#assertOpen();
    if (!name || this.#inputs.has(name)) throw new TypeError(`Duplicate or invalid input: ${name}`);
    this.#inputs.set(String(name), clone(value));
    this.#record({ type: 'INPUT_ADDED', name: String(name), digest: deterministicDigest(value) });
    return this;
  }

  addTool(name, identity) {
    this.#assertOpen();
    if (!name || this.#tools.has(name)) throw new TypeError(`Duplicate or invalid tool: ${name}`);
    this.#tools.set(String(name), clone(identity));
    this.#record({ type: 'TOOL_ADDED', name: String(name), digest: deterministicDigest(identity) });
    return this;
  }

  recordChange(change) {
    this.#assertOpen();
    const normalized = clone(change);
    this.#changes.push(normalized);
    this.#record({ type: 'CHANGE_RECORDED', index: this.#changes.length - 1, digest: deterministicDigest(normalized) });
    return this;
  }

  addArtifact(artifact) {
    this.#assertOpen();
    const entry = createArtifactManifest([artifact])[0];
    if (this.#artifacts.some(item => item.path === entry.path)) {
      throw new TypeError(`Duplicate artifact path: ${entry.path}`);
    }
    this.#artifacts.push(entry);
    this.#record({ type: 'ARTIFACT_ADDED', path: entry.path, digest: entry.digest });
    return this;
  }

  seal({ standing = 'PARTIAL_ALIVE', result = null, exclusions = [] } = {}) {
    this.#assertOpen();
    const body = {
      schema: 'unrdf.capsule/v1',
      identity: this.#identity,
      inputs: Object.fromEntries([...this.#inputs].sort(([a], [b]) => a.localeCompare(b))),
      tools: Object.fromEntries([...this.#tools].sort(([a], [b]) => a.localeCompare(b))),
      changes: this.#changes,
      artifacts: [...this.#artifacts].sort((a, b) => a.path.localeCompare(b.path)),
      result: clone(result),
      standing,
      exclusions: [...new Set(exclusions.map(String))].sort(),
    };
    const receipt = createReceipt(this.#receipts.at(-1), {
      type: 'CAPSULE_SEALED',
      bodyDigest: deterministicDigest(body),
      standing,
    });
    this.#receipts.push(receipt);
    this.#state = STATES.SEALED;
    const capsule = {
      ...body,
      receipts: [...this.#receipts],
      rootDigest: receipt.digest,
    };
    return Object.freeze({ ...capsule, capsuleDigest: deterministicDigest(capsule) });
  }
}

export function createCapsule(identity) {
  return new CapsuleBuilder(identity);
}

export function verifyCapsule(capsule) {
  const errors = [];
  if (capsule?.schema !== 'unrdf.capsule/v1') errors.push('INVALID_SCHEMA');
  if (!Array.isArray(capsule?.receipts) || capsule.receipts.length < 2) errors.push('MISSING_RECEIPTS');
  let previous = null;
  for (const receipt of capsule?.receipts ?? []) {
    const { digest, ...body } = receipt;
    if (body.sequence !== (previous ? previous.sequence + 1 : 0)) errors.push(`SEQUENCE:${body.sequence}`);
    if (body.previousDigest !== (previous?.digest ?? null)) errors.push(`PREVIOUS_DIGEST:${body.sequence}`);
    if (deterministicDigest(body) !== digest) errors.push(`RECEIPT_DIGEST:${body.sequence}`);
    previous = receipt;
  }
  if (previous?.digest !== capsule?.rootDigest) errors.push('ROOT_DIGEST');
  const { capsuleDigest, ...body } = capsule ?? {};
  if (deterministicDigest(body) !== capsuleDigest) errors.push('CAPSULE_DIGEST');
  const duplicatePaths = (capsule?.artifacts ?? []).map(item => item.path)
    .filter((path, index, paths) => paths.indexOf(path) !== index);
  if (duplicatePaths.length) errors.push('DUPLICATE_ARTIFACT_PATH');
  return Object.freeze({ valid: errors.length === 0, errors });
}

function diffValues(expected, actual, path, differences) {
  if (canonicalJson(expected) === canonicalJson(actual)) return;
  const expectedObject = expected && typeof expected === 'object';
  const actualObject = actual && typeof actual === 'object';
  if (!expectedObject || !actualObject || Array.isArray(expected) !== Array.isArray(actual)) {
    differences.push({ path, expected, actual });
    return;
  }
  if (Array.isArray(expected)) {
    const length = Math.max(expected.length, actual.length);
    for (let index = 0; index < length; index += 1) {
      diffValues(expected[index], actual[index], `${path}/${index}`, differences);
    }
    return;
  }
  const keys = [...new Set([...Object.keys(expected), ...Object.keys(actual)])].sort();
  for (const key of keys) {
    diffValues(expected[key], actual[key], `${path}/${key.replaceAll('~', '~0').replaceAll('/', '~1')}`, differences);
  }
}

export function replayCapsule(expected, actual) {
  const expectedVerification = verifyCapsule(expected);
  const actualVerification = verifyCapsule(actual);
  if (!expectedVerification.valid || !actualVerification.valid) {
    return Object.freeze({
      state: 'REPLAY_INVALID',
      expectedErrors: expectedVerification.errors,
      actualErrors: actualVerification.errors,
      differences: [],
    });
  }
  const differences = [];
  for (const field of ['identity', 'inputs', 'tools', 'changes', 'artifacts', 'result', 'standing', 'exclusions']) {
    diffValues(expected[field], actual[field], `/${field}`, differences);
  }
  return Object.freeze({
    state: differences.length ? 'REPLAY_DIFFERENCE' : 'REPLAY_MATCH',
    differences,
    expectedDigest: expected.capsuleDigest,
    actualDigest: actual.capsuleDigest,
  });
}

export function serializeCapsule(capsule) {
  return `${canonicalJson(capsule)}\n`;
}

export function parseCapsule(text) {
  return parseCanonicalJson(text.trim());
}

export { STATES as CapsuleState };
