import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp, rm, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { AtomVMProcessBroker, AtomVMProcessRefusal } from '../src/process-broker.mjs';

async function fixture(runner) {
  const root = await mkdtemp(join(tmpdir(), 'unrdf-atomvm-'));
  const binary = join(root, 'AtomVM');
  const app = join(root, 'probe.avm');
  await writeFile(binary, 'binary');
  await writeFile(app, 'application');
  const broker = new AtomVMProcessBroker({
    atomvmBinary: binary,
    runtimeRef: 'v0.6.6',
    swarms: { east: { avmPath: app, expectedMarker: 'atomvm_swarm_alive' } },
    runner,
  });
  return { broker, root };
}

const request = {
  intent: { operation: 'atomvm.execute' },
  target: { id: 'east' },
  route: ['west', 'central', 'east'],
};

test('executes only through the configured AtomVM process boundary', async () => {
  let observed;
  const { broker, root } = await fixture(async invocation => {
    observed = invocation;
    return { exitCode: 0, signal: null, stdout: '{atomvm_swarm_alive,ok}\n', stderr: '' };
  });
  try {
    const result = await broker.execute(request);
    assert.equal(result.runtime, 'AtomVM');
    assert.equal(result.runtimeRef, 'v0.6.6');
    assert.equal(result.targetId, 'east');
    assert.equal(result.markerObserved, true);
    assert.equal(observed.args.length, 1);
    assert.equal(observed.timeoutMs, 10_000);
  } finally {
    await rm(root, { recursive: true, force: true });
  }
});

test('refuses operations outside the broker admission contract', async () => {
  const { broker, root } = await fixture(async () => {
    throw new Error('runner must not execute');
  });
  try {
    await assert.rejects(
      () => broker.execute({ ...request, intent: { operation: 'rdf.delta.apply' } }),
      error => error instanceof AtomVMProcessRefusal && error.code === 'OPERATION_NOT_ADMITTED_REFUSED',
    );
  } finally {
    await rm(root, { recursive: true, force: true });
  }
});

test('refuses a zero-exit process that did not prove AtomVM execution', async () => {
  const { broker, root } = await fixture(async () => ({
    exitCode: 0,
    signal: null,
    stdout: 'generic process output\n',
    stderr: '',
  }));
  try {
    await assert.rejects(
      () => broker.execute(request),
      error => error instanceof AtomVMProcessRefusal && error.code === 'ATOMVM_MARKER_MISSING_REFUSED',
    );
  } finally {
    await rm(root, { recursive: true, force: true });
  }
});
