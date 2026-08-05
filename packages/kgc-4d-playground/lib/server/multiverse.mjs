/**
 * Multiverse Server Utilities
 *
 * Fork/merge infrastructure for reality branching. Fork state is process-local;
 * callers that require durable forks must supply a durable registry above this layer.
 */

import { createHash } from 'node:crypto';
import { reconstructState, GRAPHS } from '@unrdf/kgc-4d';
import { dataFactory } from '@unrdf/oxigraph';
import {
  getUniverse as defaultGetUniverse,
  getGitBackbone as defaultGetGitBackbone,
} from '../../../../src/universe/universe.mjs';

export { defaultGetUniverse as getUniverse, defaultGetGitBackbone as getGitBackbone };

const forks = new Map();
let universeProvider = defaultGetUniverse;
let gitBackboneProvider = defaultGetGitBackbone;
let reconstructProvider = reconstructState;

/** Configure providers for an embedding host or an executable test capsule. */
export function configureMultiverse({ getUniverse, getGitBackbone, reconstructState: reconstruct } = {}) {
  if (getUniverse !== undefined && typeof getUniverse !== 'function') {
    throw new TypeError('getUniverse provider must be a function');
  }
  if (getGitBackbone !== undefined && typeof getGitBackbone !== 'function') {
    throw new TypeError('getGitBackbone provider must be a function');
  }
  if (reconstruct !== undefined && typeof reconstruct !== 'function') {
    throw new TypeError('reconstructState provider must be a function');
  }
  if (getUniverse) universeProvider = getUniverse;
  if (getGitBackbone) gitBackboneProvider = getGitBackbone;
  if (reconstruct) reconstructProvider = reconstruct;
}

/** Reset process-local forks and restore canonical providers. */
export function resetMultiverse() {
  forks.clear();
  universeProvider = defaultGetUniverse;
  gitBackboneProvider = defaultGetGitBackbone;
  reconstructProvider = reconstructState;
}

/** Create a new forked reality from a specific time. */
export async function createFork(forkId, fromTime) {
  if (typeof forkId !== 'string' || forkId.trim() === '') {
    throw new TypeError('forkId must be a non-empty string');
  }
  if (typeof fromTime !== 'bigint' || fromTime < 0n) {
    throw new TypeError('fromTime must be a non-negative BigInt');
  }
  if (forks.has(forkId)) {
    throw new Error(`Fork already exists: ${forkId}`);
  }

  const mainStore = await universeProvider();
  const git = gitBackboneProvider();
  const forkStore = await reconstructProvider(mainStore, git, fromTime);
  const quads = universeQuads(forkStore);
  const fork = {
    id: forkId,
    store: forkStore,
    createdAt: fromTime.toString(),
    createdAtIso: new Date(Number(fromTime / 1_000_000n)).toISOString(),
    events: [],
    baseQuadCount: quads.length,
    baseValues: snapshotPredicateValues(forkStore),
  };

  forks.set(forkId, fork);
  return {
    forkId,
    baseTime: fork.createdAt,
    baseTimeIso: fork.createdAtIso,
    quadCount: quads.length,
    status: 'active',
  };
}

/** Apply a validated delta to a forked Universe. */
export async function applyDeltaToFork(forkId, delta) {
  const fork = requireFork(forkId);
  try {
    const admitted = admitDelta(delta);
    applyDelta(fork.store, admitted);
    const event = {
      id: crypto.randomUUID(),
      type: admitted.type,
      delta: admitted,
      timestamp: new Date().toISOString(),
    };
    fork.events.push(event);
    return {
      status: 'ACK',
      eventId: event.id,
      forkId,
      eventCount: fork.events.length,
    };
  } catch (error) {
    return { status: 'REJECT', reason: error.message, forkId };
  }
}

/**
 * Merge a fork into the main Universe.
 *
 * strategy may be "auto", "manual", or an object:
 * { mode: "manual", resolutions: [{ subject, predicate, decision: "fork" | "main" }] }
 */
export async function mergeFork(forkId, strategy = 'auto') {
  const fork = requireFork(forkId);
  const admittedStrategy = admitMergeStrategy(strategy);
  const mainStore = await universeProvider();
  const conflicts = detectConflicts(mainStore, fork);

  if (admittedStrategy.mode === 'auto' && conflicts.length > 0) {
    return conflictResult(forkId, conflicts);
  }

  const resolutionResult = admitResolutions(admittedStrategy, conflicts);
  if (!resolutionResult.ok) {
    return {
      ...conflictResult(forkId, conflicts),
      message: resolutionResult.message,
      requiredResolutions: conflicts.map(({ subject, predicate }) => ({
        subject,
        predicate,
        decisions: ['fork', 'main'],
      })),
    };
  }

  const resolutionByKey = resolutionResult.resolutionByKey;
  const eventsToApply = fork.events.filter(event => {
    const key = deltaKey(event.delta);
    const resolution = resolutionByKey.get(key);
    return !resolution || resolution.decision === 'fork';
  });
  const affectedKeys = new Set(eventsToApply.map(event => deltaKey(event.delta)));
  const rollback = snapshotAffectedQuads(mainStore, affectedKeys);

  try {
    for (const event of eventsToApply) applyDelta(mainStore, event.delta);
  } catch (error) {
    restoreAffectedQuads(mainStore, rollback);
    return {
      status: 'error',
      forkId,
      message: `Merge actuation failed and was rolled back: ${error.message}`,
    };
  }

  const keptMain = [...resolutionByKey.values()].filter(item => item.decision === 'main');
  const receiptSubject = {
    forkId,
    baseTime: fork.createdAt,
    mode: admittedStrategy.mode,
    appliedEventIds: eventsToApply.map(event => event.id),
    keptMain: keptMain.map(({ subject, predicate }) => ({ subject, predicate })),
  };
  const receipt = {
    schema: 'urn:unrdf:multiverse-merge-receipt:v1',
    ...receiptSubject,
    digest: createHash('sha256').update(stableJson(receiptSubject)).digest('hex'),
  };

  forks.delete(forkId);
  return {
    status: 'success',
    forkId,
    mergedEvents: eventsToApply.length,
    keptMainConflicts: keptMain.length,
    receipt,
  };
}

export function getForkStatus(forkId) {
  const fork = forks.get(forkId);
  if (!fork) return null;
  return {
    forkId: fork.id,
    baseTime: fork.createdAt,
    baseTimeIso: fork.createdAtIso,
    baseQuadCount: fork.baseQuadCount,
    currentQuadCount: universeQuads(fork.store).length,
    eventCount: fork.events.length,
    events: fork.events,
    status: 'active',
  };
}

export function listForks() {
  return [...forks.keys()].map(getForkStatus);
}

export function destroyFork(forkId) {
  return { success: forks.delete(forkId), forkId };
}

function requireFork(forkId) {
  const fork = forks.get(forkId);
  if (!fork) throw new Error(`Fork not found: ${forkId}`);
  return fork;
}

function admitDelta(delta) {
  if (!delta || typeof delta !== 'object') throw new TypeError('delta must be an object');
  if (!['CREATE', 'UPDATE', 'DELETE'].includes(delta.type)) {
    throw new Error(`Unsupported delta type: ${delta.type}`);
  }
  if (typeof delta.subject !== 'string' || typeof delta.predicate !== 'string') {
    throw new TypeError('delta subject and predicate must be strings');
  }
  if ((delta.type === 'CREATE' || delta.type === 'UPDATE') && delta.newValue === undefined) {
    throw new TypeError(`${delta.type} requires newValue`);
  }
  if (delta.type === 'DELETE' && delta.value === undefined) {
    throw new TypeError('DELETE requires value');
  }
  return structuredClone(delta);
}

function admitMergeStrategy(strategy) {
  const normalized = typeof strategy === 'string' ? { mode: strategy, resolutions: [] } : strategy;
  if (!normalized || typeof normalized !== 'object') throw new TypeError('strategy must be a string or object');
  if (!['auto', 'manual'].includes(normalized.mode)) {
    throw new Error(`Unsupported merge strategy: ${normalized.mode}`);
  }
  if (normalized.resolutions !== undefined && !Array.isArray(normalized.resolutions)) {
    throw new TypeError('manual resolutions must be an array');
  }
  return { mode: normalized.mode, resolutions: normalized.resolutions || [] };
}

function admitResolutions(strategy, conflicts) {
  if (conflicts.length === 0) return { ok: true, resolutionByKey: new Map() };
  if (strategy.mode !== 'manual') return { ok: false, message: 'Conflicts require manual resolution.' };

  const conflictsByKey = new Map(conflicts.map(conflict => [conflict.key, conflict]));
  const resolutionByKey = new Map();
  for (const resolution of strategy.resolutions) {
    if (!resolution || typeof resolution !== 'object') return { ok: false, message: 'Each resolution must be an object.' };
    const key = predicateKey(resolution.subject, resolution.predicate);
    if (!conflictsByKey.has(key)) return { ok: false, message: `Resolution does not match a conflict: ${key}` };
    if (resolutionByKey.has(key)) return { ok: false, message: `Duplicate resolution: ${key}` };
    if (!['fork', 'main'].includes(resolution.decision)) {
      return { ok: false, message: `Resolution decision must be fork or main: ${key}` };
    }
    resolutionByKey.set(key, {
      subject: resolution.subject,
      predicate: resolution.predicate,
      decision: resolution.decision,
    });
  }

  const missing = conflicts.filter(conflict => !resolutionByKey.has(conflict.key));
  if (missing.length > 0) {
    return { ok: false, message: `Missing ${missing.length} conflict resolution(s).` };
  }
  return { ok: true, resolutionByKey };
}

function conflictResult(forkId, conflicts) {
  return {
    status: 'conflict',
    forkId,
    conflicts,
    message: `Found ${conflicts.length} conflict(s). Supply explicit manual resolutions.`,
  };
}

function detectConflicts(mainStore, fork) {
  const eventKeys = new Map();
  for (const event of fork.events) {
    const key = deltaKey(event.delta);
    if (!eventKeys.has(key)) eventKeys.set(key, []);
    eventKeys.get(key).push(event.id);
  }

  const conflicts = [];
  for (const [key, eventIds] of eventKeys) {
    const { subject, predicate } = parsePredicateKey(key);
    const baseValues = fork.baseValues.get(key) || [];
    const mainValues = valuesAt(mainStore, subject, predicate);
    const forkValues = valuesAt(fork.store, subject, predicate);
    if (!sameValues(mainValues, baseValues) && !sameValues(mainValues, forkValues)) {
      conflicts.push({ key, subject, predicate, baseValues, mainValues, forkValues, eventIds });
    }
  }
  return conflicts.sort((a, b) => a.key.localeCompare(b.key));
}

function snapshotPredicateValues(store) {
  const snapshot = new Map();
  for (const quad of universeQuads(store)) {
    const key = predicateKey(quad.subject.value, quad.predicate.value);
    if (!snapshot.has(key)) snapshot.set(key, []);
    snapshot.get(key).push(termDescriptor(quad.object));
  }
  for (const values of snapshot.values()) values.sort(compareDescriptors);
  return snapshot;
}

function valuesAt(store, subject, predicate) {
  const graph = dataFactory.namedNode(GRAPHS.UNIVERSE);
  return [...store.match(dataFactory.namedNode(subject), dataFactory.namedNode(predicate), null, graph)]
    .map(quad => termDescriptor(quad.object))
    .sort(compareDescriptors);
}

function termDescriptor(term) {
  return {
    termType: term.termType,
    value: term.value,
    datatype: term.datatype?.value || null,
    language: term.language || '',
  };
}

function compareDescriptors(left, right) {
  return stableJson(left).localeCompare(stableJson(right));
}

function sameValues(left, right) {
  return stableJson(left) === stableJson(right);
}

function predicateKey(subject, predicate) {
  return `${subject}\u0000${predicate}`;
}

function parsePredicateKey(key) {
  const [subject, predicate] = key.split('\u0000');
  return { subject, predicate };
}

function deltaKey(delta) {
  return predicateKey(delta.subject, delta.predicate);
}

function universeQuads(store) {
  return [...store.match(null, null, null, dataFactory.namedNode(GRAPHS.UNIVERSE))];
}

function snapshotAffectedQuads(store, affectedKeys) {
  const graph = dataFactory.namedNode(GRAPHS.UNIVERSE);
  const snapshot = new Map();
  for (const key of affectedKeys) {
    const { subject, predicate } = parsePredicateKey(key);
    snapshot.set(key, [...store.match(dataFactory.namedNode(subject), dataFactory.namedNode(predicate), null, graph)]);
  }
  return snapshot;
}

function restoreAffectedQuads(store, snapshot) {
  const graph = dataFactory.namedNode(GRAPHS.UNIVERSE);
  for (const [key, quads] of snapshot) {
    const { subject, predicate } = parsePredicateKey(key);
    for (const quad of [...store.match(dataFactory.namedNode(subject), dataFactory.namedNode(predicate), null, graph)]) {
      store.delete(quad);
    }
    for (const quad of quads) store.add(quad);
  }
}

function applyDelta(store, delta) {
  const graph = dataFactory.namedNode(GRAPHS.UNIVERSE);
  const subject = dataFactory.namedNode(delta.subject);
  const predicate = dataFactory.namedNode(delta.predicate);

  if (delta.type === 'UPDATE') {
    if (delta.oldValue !== undefined) {
      store.delete(dataFactory.quad(subject, predicate, createObject(delta.oldValue), graph));
    } else {
      for (const quad of [...store.match(subject, predicate, null, graph)]) store.delete(quad);
    }
  }

  if (delta.type === 'CREATE' || delta.type === 'UPDATE') {
    store.add(dataFactory.quad(subject, predicate, createObject(delta.newValue), graph));
  } else {
    store.delete(dataFactory.quad(subject, predicate, createObject(delta.value), graph));
  }
}

function createObject(value) {
  if (value && typeof value === 'object' && value.type === 'uri') {
    return dataFactory.namedNode(value.value);
  }
  if (value && typeof value === 'object' && value.type === 'literal') {
    if (value.language) return dataFactory.literal(value.value, value.language);
    if (value.datatype) return dataFactory.literal(value.value, dataFactory.namedNode(value.datatype));
    return dataFactory.literal(value.value);
  }
  return dataFactory.literal(typeof value === 'string' ? value : String(value));
}

function stableJson(value) {
  if (Array.isArray(value)) return `[${value.map(stableJson).join(',')}]`;
  if (value && typeof value === 'object') {
    return `{${Object.keys(value).sort().map(key => `${JSON.stringify(key)}:${stableJson(value[key])}`).join(',')}}`;
  }
  return JSON.stringify(value);
}
