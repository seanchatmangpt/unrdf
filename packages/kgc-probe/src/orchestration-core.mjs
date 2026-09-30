/** Deterministic KGC probe scheduling, hashing, merging, and replay primitives. */

import { createHash } from 'node:crypto';

/**
 * Convert a value into a JSON-safe, key-sorted canonical form so equal values serialize identically.
 * Non-JSON values are tagged ($number, $bigint, $undefined, $date, $bytes, $set, $map); functions,
 * symbols and cyclic structures throw a TypeError.
 * @param {*} value - Value to canonicalize
 * @param {Set<object>} [active] - Objects currently being visited (used for cycle detection)
 * @returns {*} Canonical JSON-serializable representation
 * @throws {TypeError} If the value is a function, symbol, or contains a cycle
 */
export function canonicalize(value, active = new Set()) {
  if (value === null || ['string', 'boolean'].includes(typeof value)) return value;
  if (typeof value === 'number') {
    if (!Number.isFinite(value)) return { $number: String(value) };
    return Object.is(value, -0) ? 0 : value;
  }
  if (typeof value === 'bigint') return { $bigint: value.toString() };
  if (typeof value === 'undefined') return { $undefined: true };
  if (typeof value === 'function' || typeof value === 'symbol')
    throw new TypeError(`Cannot canonicalize ${typeof value}`);
  if (active.has(value)) throw new TypeError('Cannot canonicalize cyclic value');
  active.add(value);
  try {
    if (value instanceof Date) return { $date: value.toISOString() };
    if (value instanceof Uint8Array) return { $bytes: Buffer.from(value).toString('base64') };
    if (Array.isArray(value)) return value.map(item => canonicalize(item, active));
    if (value instanceof Set) {
      return {
        $set: [...value]
          .map(item => canonicalize(item, active))
          .sort((a, b) => JSON.stringify(a).localeCompare(JSON.stringify(b))),
      };
    }
    if (value instanceof Map) {
      return {
        $map: [...value]
          .map(([key, item]) => [canonicalize(key, active), canonicalize(item, active)])
          .sort((a, b) => JSON.stringify(a[0]).localeCompare(JSON.stringify(b[0]))),
      };
    }
    const result = {};
    for (const key of Object.keys(value).sort()) result[key] = canonicalize(value[key], active);
    return result;
  } finally {
    active.delete(value);
  }
}

/**
 * Serialize a value to its canonical JSON string.
 * @param {*} value - Value to serialize
 * @returns {string} Canonical JSON text
 */
export function canonicalJson(value) {
  return JSON.stringify(canonicalize(value));
}

/**
 * Hash the canonical JSON form of a value.
 * @param {*} value - Value to hash
 * @param {string} [algorithm] - Node crypto hash algorithm name
 * @returns {string} Hex digest
 */
export function digest(value, algorithm = 'sha256') {
  return createHash(algorithm).update(canonicalJson(value)).digest('hex');
}

/**
 * Derive a UUID-shaped identifier, prefixed with the namespace, from the hash of (namespace, value).
 * @param {string} namespace - Namespace prefix for the identifier
 * @param {*} value - Value the identifier is derived from
 * @returns {string} Deterministic identifier of the form `namespace-xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx`
 */
export function deterministicId(namespace, value) {
  const hash = digest({ namespace, value });
  return `${namespace}-${hash.slice(0, 8)}-${hash.slice(8, 12)}-${hash.slice(12, 16)}-${hash.slice(16, 20)}-${hash.slice(20, 32)}`;
}

/**
 * Compute the identity hash of an observation from its agent, kind, severity, subject,
 * predicate, object and evidence (timestamp and id are excluded).
 * @param {Object} observation - Observation to identify
 * @returns {string} Hex digest identifying the observation
 */
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

/**
 * Return a copy of the observations sorted by agent, kind, subject, predicate, object, timestamp and id.
 * @param {Iterable<Object>} observations - Observations to sort (input is not mutated)
 * @returns {Object[]} New sorted array
 */
export function sortObservations(observations) {
  return [...observations].sort((left, right) => {
    const a = `${left.agent || ''}|${left.kind || ''}|${left.subject || ''}|${left.predicate || ''}|${left.object || ''}|${left.timestamp || ''}|${left.id || ''}`;
    const b = `${right.agent || ''}|${right.kind || ''}|${right.subject || ''}|${right.predicate || ''}|${right.object || ''}|${right.timestamp || ''}|${right.id || ''}`;
    return a.localeCompare(b);
  });
}

/**
 * Merge observations from several shards (plus optional additions), de-duplicating by identity and
 * resolving conflicting duplicates according to the conflict policy.
 * @param {Array<{observations: Object[]}>} shards - Shards holding observations
 * @param {Object[]} [additions] - Extra observations merged after the shards
 * @param {Object} [options] - Merge options
 * @param {string} [options.conflictPolicy] - 'latest' keeps the newest timestamp, 'right' keeps the
 *   later-seen observation, 'error' throws, any other value keeps the first seen (default 'latest')
 * @returns {{observations: Object[], conflicts: Object[], digest: string, sources: Object[]}} Merged result
 * @throws {Error} If a conflict is found and the policy is 'error'
 */
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
        if (
          rightTime > leftTime ||
          (rightTime === leftTime && canonicalJson(observation) > canonicalJson(existing))
        )
          byIdentity.set(identity, observation);
      }
      if (conflictPolicy === 'right') byIdentity.set(identity, observation);
    }
  }
  const observations = sortObservations(byIdentity.values());
  return { observations, conflicts, digest: digest(observations), sources };
}

/**
 * Build an integrity manifest (per-section digests and a root digest) for an artifact.
 * @param {Object} artifact - Artifact with observations, summary, metadata, shard_count and shard_hash
 * @returns {{algorithm: string, observationCount: number, sections: Object<string,string>, root: string}} Manifest
 */
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

/**
 * Return a copy of the artifact with sorted observations and an attached integrity manifest.
 * @param {Object} artifact - Artifact to seal
 * @returns {Object} New artifact including an `integrity` manifest
 */
export function sealArtifact(artifact) {
  const normalized = { ...artifact, observations: sortObservations(artifact.observations || []) };
  return { ...normalized, integrity: createArtifactManifest(normalized) };
}

/**
 * Recompute an artifact's manifest and compare it with its stored `integrity` field.
 * @param {Object} artifact - Artifact to verify
 * @returns {{valid: boolean, differences: Object[], expected: Object}} Verification result listing each mismatch
 */
export function verifyArtifact(artifact) {
  const expected = createArtifactManifest(artifact);
  const actual = artifact?.integrity;
  const differences = [];
  if (!actual) differences.push({ path: 'integrity', expected, actual: null });
  else {
    if (actual.root !== expected.root)
      differences.push({ path: 'integrity.root', expected: expected.root, actual: actual.root });
    for (const [section, hash] of Object.entries(expected.sections)) {
      if (actual.sections?.[section] !== hash)
        differences.push({
          path: `integrity.sections.${section}`,
          expected: hash,
          actual: actual.sections?.[section],
        });
    }
    if (actual.observationCount !== expected.observationCount)
      differences.push({
        path: 'integrity.observationCount',
        expected: expected.observationCount,
        actual: actual.observationCount,
      });
  }
  return { valid: differences.length === 0, differences, expected };
}

/**
 * Compare two artifacts by sealed integrity root, ignoring any stored integrity fields.
 * @param {Object} expected - Reference artifact
 * @param {Object} actual - Artifact produced by the replay
 * @returns {{state: string, expectedRoot: string, actualRoot: string, same: boolean}} 'REPLAY_MATCH' or 'REPLAY_DIFFERENCE' plus both roots
 */
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

/**
 * Create an Error marked as an abort (name 'AbortError', code 'ABORT_ERR').
 * @param {string} [message] - Error message
 * @returns {Error} Abort error
 */
function abortError(message = 'Operation aborted') {
  const error = new Error(message);
  error.name = 'AbortError';
  error.code = 'ABORT_ERR';
  return error;
}

/**
 * Run a task with an optional timeout and optional external abort signal. The task receives an
 * AbortSignal that is aborted on timeout or when the external signal aborts.
 * @param {(signal: AbortSignal) => *} task - Task to run
 * @param {number|null|undefined} timeoutMs - Timeout in milliseconds; null, undefined or non-finite disables it
 * @param {AbortSignal} [signal] - External abort signal
 * @returns {Promise<*>} The task's result
 * @throws {Error} AbortError if aborted, or TimeoutError (code 'ETIMEDOUT') if the timeout elapses first
 */
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
      signal
        ? new Promise((_, reject) =>
            signal.addEventListener(
              'abort',
              () => reject(abortError(signal.reason?.message || 'Operation aborted')),
              { once: true }
            )
          )
        : new Promise(() => {}),
    ]);
  } finally {
    if (timer) clearTimeout(timer);
    signal?.removeEventListener('abort', onAbort);
  }
}

/**
 * Run agent entries with bounded concurrency, capturing each outcome without throwing.
 * @param {Array<{id: string, run: (signal: AbortSignal) => *, timeoutMs?: number}>} agentEntries - Agents to run
 * @param {Object} [options] - Pool options
 * @param {number} [options.concurrency] - Maximum parallel agents (default 4, minimum 1)
 * @param {boolean} [options.failFast] - Stop starting new agents after the first failure
 * @param {number} [options.timeoutMs] - Default per-agent timeout
 * @param {AbortSignal} [options.signal] - External abort signal
 * @param {() => number} [options.now] - Clock function for durations (default Date.now)
 * @returns {Promise<{results: Object[], errors: Object[], aborted: boolean, stoppedEarly: boolean}>} Pool outcome
 */
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
        const value = await withTimeout(
          signal => entry.run(signal),
          entry.timeoutMs ?? options.timeoutMs,
          options.signal
        );
        results[index] = {
          id: entry.id,
          status: 'fulfilled',
          value,
          durationMs: (options.now?.() ?? Date.now()) - startedAt,
        };
      } catch (error) {
        const failure = {
          id: entry.id,
          status: 'rejected',
          error,
          durationMs: (options.now?.() ?? Date.now()) - startedAt,
        };
        results[index] = failure;
        errors.push(failure);
        if (failFast) stopped = true;
      }
    }
  });
  await Promise.all(workers);
  return {
    results: results.filter(Boolean),
    errors,
    aborted: Boolean(options.signal?.aborted),
    stoppedEarly: stopped && cursor < agentEntries.length,
  };
}
