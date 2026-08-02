import test from 'node:test';
import assert from 'node:assert/strict';
import { AtomVMProcessBroker, AtomVMProcessRefusal } from '../src/process-broker.mjs';

const request = {
  intent: { operation: 'atomvm.execute' },
  target: { id: 'east' },
  route: ['west', 'central', 'east'],
};

test('refuses operations outside the real AtomVM broker admission contract', async () => {
  const broker = new AtomVMProcessBroker({ atomvmBinary: '/unavailable/AtomVM', swarms: {} });
  await assert.rejects(
    () => broker.execute({ ...request, intent: { operation: 'rdf.delta.apply' } }),
    error => error instanceof AtomVMProcessRefusal && error.code === 'OPERATION_NOT_ADMITTED_REFUSED',
  );
});

test('refuses execution when the real AtomVM binary is unavailable', async () => {
  const broker = new AtomVMProcessBroker({
    atomvmBinary: '/unavailable/AtomVM',
    swarms: { east: { avmPath: '/unavailable/probe.avm' } },
  });
  await assert.rejects(
    () => broker.execute(request),
    error => error instanceof AtomVMProcessRefusal && error.code === 'ATOMVM_BINARY_NOT_FOUND_REFUSED',
  );
});

test('executes the configured AtomVM process or observes the real unavailable-binary refusal', async () => {
  const configured = Boolean(process.env.ATOMVM_BIN && process.env.ATOMVM_APP);
  const broker = new AtomVMProcessBroker({
    atomvmBinary: configured ? process.env.ATOMVM_BIN : '/unavailable/AtomVM',
    runtimeRef: process.env.ATOMVM_SOURCE_REF ?? 'unknown',
    swarms: {
      east: {
        avmPath: configured ? process.env.ATOMVM_APP : '/unavailable/probe.avm',
        libraryPaths: configured && process.env.ATOMVM_LIB ? [process.env.ATOMVM_LIB] : [],
        expectedMarker: 'atomvm_swarm_alive',
      },
    },
  });

  if (!configured) {
    await assert.rejects(
      () => broker.execute(request),
      error => error instanceof AtomVMProcessRefusal && error.code === 'ATOMVM_BINARY_NOT_FOUND_REFUSED',
    );
    return;
  }

  const result = await broker.execute(request);
  assert.equal(result.runtime, 'AtomVM');
  assert.equal(result.targetId, 'east');
  assert.equal(result.exitCode, 0);
  assert.equal(result.markerObserved, true);
  assert.match(`${result.stdout}\n${result.stderr}`, /Return value: ok/);
});
