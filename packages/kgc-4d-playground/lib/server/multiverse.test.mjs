import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import { OxigraphStore, dataFactory } from '@unrdf/oxigraph';
import { GRAPHS } from '@unrdf/kgc-4d';
import {
  applyDeltaToFork,
  configureMultiverse,
  createFork,
  getForkStatus,
  mergeFork,
  resetMultiverse,
} from './multiverse.mjs';

const SUBJECT = 'http://example.org/resource';
const PREDICATE = 'http://example.org/value';
const graph = dataFactory.namedNode(GRAPHS.UNIVERSE);

function cloneStore(store) {
  return new OxigraphStore([...store.match(null, null, null, null)]);
}

function setValue(store, value) {
  const subject = dataFactory.namedNode(SUBJECT);
  const predicate = dataFactory.namedNode(PREDICATE);
  for (const quad of [...store.match(subject, predicate, null, graph)]) store.delete(quad);
  store.add(dataFactory.quad(subject, predicate, dataFactory.literal(value), graph));
}

function getValue(store) {
  return [...store.match(
    dataFactory.namedNode(SUBJECT),
    dataFactory.namedNode(PREDICATE),
    null,
    graph
  )][0]?.object.value;
}

function forkUpdate(value) {
  return {
    type: 'UPDATE',
    subject: SUBJECT,
    predicate: PREDICATE,
    oldValue: 'base',
    newValue: value,
  };
}

describe('multiverse merge admission and actuation', () => {
  let mainStore;
  let baseline;

  beforeEach(() => {
    mainStore = new OxigraphStore();
    setValue(mainStore, 'base');
    baseline = cloneStore(mainStore);
    configureMultiverse({
      getUniverse: async () => mainStore,
      getGitBackbone: () => ({}),
      reconstructState: async () => cloneStore(baseline),
    });
  });

  afterEach(() => resetMultiverse());

  it('auto-merges when main still equals the fork base', async () => {
    await createFork('safe', 0n);
    expect((await applyDeltaToFork('safe', forkUpdate('fork'))).status).toBe('ACK');

    const result = await mergeFork('safe', 'auto');

    expect(result.status).toBe('success');
    expect(result.mergedEvents).toBe(1);
    expect(result.receipt.digest).toMatch(/^[a-f0-9]{64}$/);
    expect(getValue(mainStore)).toBe('fork');
    expect(getForkStatus('safe')).toBeNull();
  });

  it('detects a three-way conflict without mutating main', async () => {
    await createFork('conflict', 0n);
    await applyDeltaToFork('conflict', forkUpdate('fork'));
    setValue(mainStore, 'main');

    const result = await mergeFork('conflict', 'auto');

    expect(result.status).toBe('conflict');
    expect(result.conflicts).toHaveLength(1);
    expect(result.conflicts[0].baseValues[0].value).toBe('base');
    expect(result.conflicts[0].mainValues[0].value).toBe('main');
    expect(result.conflicts[0].forkValues[0].value).toBe('fork');
    expect(getValue(mainStore)).toBe('main');
    expect(getForkStatus('conflict')).not.toBeNull();
  });

  it('manual fork resolution applies the fork value', async () => {
    await createFork('manual-fork', 0n);
    await applyDeltaToFork('manual-fork', forkUpdate('fork'));
    setValue(mainStore, 'main');

    const result = await mergeFork('manual-fork', {
      mode: 'manual',
      resolutions: [{ subject: SUBJECT, predicate: PREDICATE, decision: 'fork' }],
    });

    expect(result.status).toBe('success');
    expect(getValue(mainStore)).toBe('fork');
  });

  it('manual main resolution preserves the admitted main value', async () => {
    await createFork('manual-main', 0n);
    await applyDeltaToFork('manual-main', forkUpdate('fork'));
    setValue(mainStore, 'main');

    const result = await mergeFork('manual-main', {
      mode: 'manual',
      resolutions: [{ subject: SUBJECT, predicate: PREDICATE, decision: 'main' }],
    });

    expect(result.status).toBe('success');
    expect(result.mergedEvents).toBe(0);
    expect(result.keptMainConflicts).toBe(1);
    expect(getValue(mainStore)).toBe('main');
  });

  it('refuses incomplete manual resolution before actuation', async () => {
    await createFork('manual-missing', 0n);
    await applyDeltaToFork('manual-missing', forkUpdate('fork'));
    setValue(mainStore, 'main');

    const result = await mergeFork('manual-missing', { mode: 'manual', resolutions: [] });

    expect(result.status).toBe('conflict');
    expect(result.requiredResolutions).toHaveLength(1);
    expect(getValue(mainStore)).toBe('main');
    expect(getForkStatus('manual-missing')).not.toBeNull();
  });
});
