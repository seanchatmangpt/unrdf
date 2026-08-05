/** Deterministic KGC probe scheduling, hashing, merging, and replay primitives. */

import { createHash } from 'node:crypto';

export function canonicalize(value, active = new Set()) {
  if (value === null || ['string', 'boolean'].includes(typeof value)) return value;
  if (typeof value === 'number') {
    if (!Number.isFinite(value)) return { $number: String(value) };
    return Object.is(value, -0) ? 0 : value;
  }
  if (typeof value === 'bigint') return { $bigint: value.toString() };
  if (typeof value === 'undefined') return { $undefined: true };
  if (typeof value === 'function' || typeof value === 'symbol') throw new TypeError(`Cannot canonicalize ${typeof value}`);
  if (active.has(value)) throw new TypeError('Cannot canonicalize cyclic value');
  active.add(value);
  try {
    if (value instanceof Date) return { $date: value.toISOString() };
    if (value instanceof Uint8Array) return { $bytes: Buffer.from(value).toString('base64') };
    if (Array.isArray(value)) return value.map(item => canonicalize(item, active));
    if (value instanceof Set) {
      return { $set: [...value].map(item => canonicalize(item, active)).sort((a, b) => JSON.stringify(a).localeCompare(JSON.stringify(b))) };
    }
    if (value instanceof Map) {
      return { $map: [...value].map(([key, item]) => [canonicalize(key, active), canonicalize(item, active)]).sort((a, b) => JSON.stringify(a[0]).localeCompare(JSON.stringify(b[0]))) };
    }
    const result = {};
    for (const key of Object.keys(value).sort()) result[key] = canonicalize(value[key], active);
    return result;
  } finally {
    active.delete(value);
  }
}

export function canonicalJson(value) { return JSON.stringify(canonicalize(value)); }

export function digest(value, algorithm = 'sha256') {
  return createHash(algorithm).update(canonicalJson(value)).digest('hex');
}

export function deterministicId(namespace, value) {
  const hash = digest({ namespace, value });
  return `${namespace}-${hash.slice(0, 8)}-${hash.slice(8, 12)}-${hash.slice(12, 16)}-${hash.slice(16, 20)}-${hash.slice(20, 32)}`;
}

export function observationIdentity(observation) {
  return digest({
    agent: observation.agent,
    kind: observation.kind,
    severity: observation.severity,
    subject: observation.subject,
    predicate: observation.predicate ?? null,
    object: observation.object ?? null,
    evidence: observation.evidence ?? null,
  });
}

export function sortObservations(observations) {
  return [...observations].sort((left, right) => {
    const a = `${left.agent || ''}|${left.kind || ''}|${left.subject || ''}|${left.predicate || ''}|${left.object || ''}|${left.timestamp || ''}|${left.id || ''}`;
    const b = `${right.agent || ''}|${right.kind || ''}|${right.subject || ''}|${right.predicate || ''}|${right.object || ''}|${right.timestamp || ''}|${right.id || ''}`;
    return a.localeCompare(b);
  });
}

export function mergeObservations(shards, additions = [], options = {}) {
  const conflictPolicy = options.conflictPolicy || 'latest';
  const byIdentity = new Map();
  const conflicts = [];
  const sources = [];
  for (const [shardIndex, shard] of [...shards, { observations: additions }].entries()) {
    for (const observation of shard?.observations || []) {
      const identity = observationIdentity(observation);
      const existing = byIdentity.get(identity);
      if (!existing) {
        byIdentity.set(identity, observation);
        sources.push({ identity, shardIndex });
        continue;
      }
      if (canonicalJson(existing) === canonicalJson(observation)) continue;
      conflicts.push({ identity, left: existing, right: observation, shardIndex });
      if (conflictPolicy === 'error') throw new Error(`Conflicting observation ${identity}`);
      if (conflictPolicy === 'latest') {
        const leftTime = Date.parse(existing.timestamp || 0) || 0;
        const rightTime = Date.parse(observation.timestamp || 0) || 0;
        if (rightTime > leftTime || (rightTime === leftTime && canonicalJson(observation) > canonicalJson(existing))) byIdentity.set(identity, observation);
      }
      if (conflictPolicy === 'right') byIdentity.set(identity, observation);
    }
  }
  const observations = sortObservations(byIdentity.values());
  return { observations, conflicts, digest: digest(observations), sources };
}

export function createArtifactManifest(artifact) {
  const observations = sortObservations(artifact.observations || []);
  const sections = {
    observations: digest(observations),
    summary: digest(artifact.summary || {}),
    metadata: digest(artifact.metadata || {}),
    shards: digest({ count: artifact.shard_count || 0, hash: artifact.shard_hash || null }),
  };
  return {
    algorithm: 'sha256',
    observationCount: observations.length,
    sections,
    root: digest(sections),
  };
}

export function sealArtifact(artifact) {
  const normalized = { ...artifact, observations: sortObservations(artifact.observations || []) };
  return { ...normalized, integrity: createArtifactManifest(normalized) };
}

export function verifyArtifact(artifact) {
  const expected = createArtifactManifest(artifact);
  const actual = artifact?.integrity;
  const differences = [];
  if (!actual) differences.push({ path: 'integrity', expected, actual: null });
  else {
    if (actual.root !== expected.root) differences.push({ path: 'integrity.root', expected: expected.root, actual: actual.root });
    for (const [section, hash] of Object.entries(expected.sections)) {
      if (actual.sections?.[section] !== hash) differences.push({ path: `integrity.sections.${section}`, expected: hash, actual: actual.sections?.[section] });
    }
    if (actual.observationCount !== expected.observationCount) differences.push({ path: 'integrity.observationCount', expected: expected.observationCount, actual: actual.observationCount });
  }
  return { valid: differences.length === 0, differences, expected };
}

export function replayArtifacts(expected, actual) {
  const expectedSealed = sealArtifact({ ...expected, integrity: undefined });
  const actualSealed = sealArtifact({ ...actual, integrity: undefined });
  const same = expectedSealed.integrity.root === actualSealed.integrity.root;
  return {
    state: same ? 'REPLAY_MATCH' : 'REPLAY_DIFFERENCE',
    expectedRoot: expectedSealed.integrity.root,
    actualRoot: actualSealed.integrity.root,
    same,
  };
}

function abortError(message = 'Operation aborted') {
  const error = new Error(message);
  error.name = 'AbortError';
  error.code = 'ABORT_ERR';
  return error;
}

export async function withTimeout(task, timeoutMs, signal) {
  if (signal?.aborted) throw abortError(signal.reason?.message || 'Operation aborted');
  const controller = new AbortController();
  const onAbort = () => controller.abort(signal.reason || abortError());
  signal?.addEventListener('abort', onAbort, { once: true });
  let timer;
  try {
    return await Promise.race([
      Promise.resolve().then(() => task(controller.signal)),
      new Promise((_, reject) => {
        if (timeoutMs == null || !Number.isFinite(timeoutMs)) return;
        timer = setTimeout(() => {
          controller.abort();
          const error = new Error(`Operation timed out after ${timeoutMs}ms`);
          error.name = 'TimeoutError';
          error.code = 'ETIMEDOUT';
          reject(error);
        }, timeoutMs);
      }),
      signal ? new Promise((_, reject) => signal.addEventListener('abort', () => reject(abortError(signal.reason?.message || 'Operation aborted')), { once: true })) : new Promise(() => {}),
    ]);
  } finally {
    if (timer) clearTimeout(timer);
    signal?.removeEventListener('abort', onAbort);
  }
}

export async function runAgentPool(agentEntries, options = {}) {
  const concurrency = Math.max(1, options.concurrency ?? 4);
  const failFast = options.failFast === true;
  const results = new Array(agentEntries.length);
  const errors = [];
  let cursor = 0;
  let stopped = false;
  const workers = Array.from({ length: Math.min(concurrency, agentEntries.length) }, async () => {
    while (!stopped) {
      const index = cursor++;
      if (index >= agentEntries.length) return;
      const entry = agentEntries[index];
      const startedAt = options.now?.() ?? Date.now();
      try {
        const value = await withTimeout(signal => entry.run(signal), entry.timeoutMs ?? options.timeoutMs, options.signal);
        results[index] = { id: entry.id, status: 'fulfilled', value, durationMs: (options.now?.() ?? Date.now()) - startedAt };
      } catch (error) {
        const failure = { id: entry.id, status: 'rejected', error, durationMs: (options.now?.() ?? Date.now()) - startedAt };
        results[index] = failure;
        errors.push(failure);
        if (failFast) stopped = true;
      }
    }
  });
  await Promise.all(workers);
  return { results: results.filter(Boolean), errors, aborted: Boolean(options.signal?.aborted), stoppedEarly: stopped && cursor < agentEntries.length };
}
