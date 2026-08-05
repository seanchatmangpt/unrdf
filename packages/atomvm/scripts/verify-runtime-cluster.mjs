#!/usr/bin/env node
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { mkdirSync, readFileSync, writeFileSync } from 'node:fs';
import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { AtomVMSwarmCluster, SwarmClusterRefusal } from '../src/swarm-cluster.mjs';
import { AtomVMProcessBroker } from '../src/process-broker.mjs';
import { evaluateInnovationCheckpoints, receiptToOcel } from '../src/innovation-checkpoints.mjs';

const __dirname = dirname(fileURLToPath(import.meta.url));
const packageRoot = resolve(__dirname, '..');

function requiredEnv(name) {
  const value = process.env[name];
  if (!value) throw new Error(`${name} is required`);
  return resolve(value);
}

function fileDigest(path) {
  return createHash('sha256').update(readFileSync(path)).digest('hex');
}

const atomvmBinary = requiredEnv('ATOMVM_BIN');
const avmPath = requiredEnv('ATOMVM_APP');
const atomvmLibrary = process.env.ATOMVM_LIB ? resolve(process.env.ATOMVM_LIB) : null;
const runtimeRef = process.env.ATOMVM_SOURCE_REF ?? 'unknown';
const artifactPath = resolve(
  process.env.ATOMVM_RECEIPT_PATH ?? resolve(packageRoot, 'artifacts/atomvm-runtime-cluster-receipt.json'),
);

let sequence = 0;
const cluster = new AtomVMSwarmCluster({
  clusterId: 'atomvm-runtime-cluster',
  idFactory: () => `runtime-${++sequence}`,
});

for (const id of ['west', 'central', 'east']) {
  cluster.admitSwarm({
    id,
    gatewayNode: `${id}-gateway`,
    cookieRef: `authority://atomvm/${id}`,
    endpoint: `atomvm://${id}`,
    metadata: { runtime: 'AtomVM', runtimeRef },
  });
}
cluster.connect('west', 'central');
cluster.connect('central', 'east');

const libraryPaths = atomvmLibrary ? [atomvmLibrary] : [];
const broker = new AtomVMProcessBroker({
  atomvmBinary,
  runtimeRef,
  swarms: Object.fromEntries(['west', 'central', 'east'].map(id => [id, {
    avmPath,
    libraryPaths,
    expectedMarker: 'atomvm_swarm_alive',
  }])),
});

const executions = [];
let finalIntent;
let finalReceipt;
for (const targetId of ['west', 'central', 'east']) {
  const intent = cluster.constructIntent({
    sourceId: 'west',
    targetId,
    operation: 'atomvm.execute',
    payload: { probe: 'swarm_probe', targetId },
  });
  const receipt = await cluster.actuate(intent, broker);

  assert.equal(receipt.status, 'ALIVE');
  assert.equal(receipt.result.runtime, 'AtomVM');
  assert.equal(receipt.result.targetId, targetId);
  assert.equal(receipt.result.markerObserved, true);
  assert.equal(cluster.verifyReceipt(receipt), true);
  assert.deepEqual(cluster.replay(receipt.receiptId), receipt);

  executions.push({ intent, receipt, ocel: receiptToOcel(receipt, intent) });
  finalIntent = intent;
  finalReceipt = receipt;
}

let negativeControlPassed = false;
try {
  await cluster.actuate(finalIntent);
} catch (error) {
  negativeControlPassed = error instanceof SwarmClusterRefusal && error.code === 'BROKER_REQUIRED_REFUSED';
}
assert.equal(negativeControlPassed, true);

const checkpointReport = evaluateInnovationCheckpoints({
  cluster,
  intent: finalIntent,
  receipt: finalReceipt,
  replayedReceipt: cluster.replay(finalReceipt.receiptId),
  brokerObserved: true,
  negativeControlPassed,
});
assert.equal(checkpointReport.status, 'ALIVE');
assert.equal(checkpointReport.passed, 10);

const body = {
  schema: 'https://unrdf.dev/atomvm/runtime-cluster-receipt/v1',
  status: 'ALIVE',
  runtime: {
    implementation: 'AtomVM',
    sourceRef: runtimeRef,
    binarySha256: fileDigest(atomvmBinary),
    applicationSha256: fileDigest(avmPath),
    librarySha256: atomvmLibrary ? fileDigest(atomvmLibrary) : null,
  },
  cluster: {
    id: cluster.clusterId,
    topologyNQuads: cluster.toNQuads(),
    snapshot: cluster.snapshot(),
  },
  executions,
  negativeControl: {
    unbrokeredActuationRefused: negativeControlPassed,
    refusalCode: 'BROKER_REQUIRED_REFUSED',
  },
  checkpoints: checkpointReport,
};
const receipt = {
  ...body,
  receiptDigest: createHash('sha256').update(JSON.stringify(body)).digest('hex'),
};

mkdirSync(dirname(artifactPath), { recursive: true });
writeFileSync(artifactPath, `${JSON.stringify(receipt, null, 2)}\n`);
console.log(JSON.stringify({
  status: receipt.status,
  runtimeRef,
  swarmsExecuted: executions.length,
  checkpointStanding: checkpointReport.status,
  receiptDigest: receipt.receiptDigest,
  artifactPath,
}, null, 2));
