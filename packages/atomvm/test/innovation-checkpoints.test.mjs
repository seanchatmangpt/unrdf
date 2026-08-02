import test from 'node:test';
import assert from 'node:assert/strict';
import { AtomVMSwarmCluster, SwarmClusterRefusal } from '../src/swarm-cluster.mjs';
import { evaluateInnovationCheckpoints, receiptToOcel } from '../src/innovation-checkpoints.mjs';

function fixture() {
  let id = 0;
  const cluster = new AtomVMSwarmCluster({
    clusterId: 'ten-gate-federation',
    clock: () => '2026-08-02T10:00:00.000Z',
    idFactory: () => `id-${++id}`,
  });
  for (const name of ['west', 'central', 'east']) {
    cluster.admitSwarm({
      id: name,
      gatewayNode: `${name}-gateway`,
      cookieRef: `secret://${name}`,
      endpoint: `atomvm://${name}`,
    });
  }
  cluster.connect('west', 'central');
  cluster.connect('central', 'east');
  return cluster;
}

test('all ten innovation checkpoints reach ALIVE on the receipted tracer bullet', async () => {
  const cluster = fixture();
  const intent = cluster.constructIntent({
    sourceId: 'west',
    targetId: 'east',
    operation: 'rdf.delta.apply',
    payload: { add: 3 },
  });
  let brokerObserved = false;
  const receipt = await cluster.actuate(intent, {
    execute: async ({ target, route }) => {
      brokerObserved = true;
      return { target: target.id, hops: route.length - 1 };
    },
  });
  let negativeControlPassed = false;
  try {
    await cluster.actuate(intent);
  } catch (error) {
    negativeControlPassed = error instanceof SwarmClusterRefusal && error.code === 'BROKER_REQUIRED_REFUSED';
  }
  const report = evaluateInnovationCheckpoints({
    cluster,
    intent,
    receipt,
    replayedReceipt: cluster.replay(receipt.receiptId),
    brokerObserved,
    negativeControlPassed,
  });

  assert.equal(report.status, 'ALIVE');
  assert.equal(report.passed, 10);
  assert.equal(report.total, 10);
  assert.equal(report.checkpoints.length, 10);
  assert.ok(report.checkpoints.every(checkpoint => checkpoint.status === 'ALIVE'));
  assert.match(report.reportDigest, /^[a-f0-9]{64}$/);
});

test('OCEL projection preserves distinct object identities and the execution event', async () => {
  const cluster = fixture();
  const intent = cluster.constructIntent({ sourceId: 'west', targetId: 'east', operation: 'ping', payload: null });
  const receipt = await cluster.actuate(intent, { execute: async () => 'pong' });
  const ocel = receiptToOcel(receipt, intent);

  assert.equal(ocel.events.length, 1);
  assert.equal(ocel.events[0].activity, 'ping');
  assert.equal(ocel.events[0].outcome, 'ALIVE');
  assert.equal(ocel.objects.length, 5);
  assert.equal(new Set(ocel.objects.map(object => object.id)).size, 5);
});

test('checkpoint report remains partial when broker authority evidence is absent', async () => {
  const cluster = fixture();
  const intent = cluster.constructIntent({ sourceId: 'west', targetId: 'east', operation: 'ping', payload: null });
  const receipt = await cluster.actuate(intent, { execute: async () => 'pong' });
  const report = evaluateInnovationCheckpoints({
    cluster,
    intent,
    receipt,
    replayedReceipt: cluster.replay(receipt.receiptId),
  });

  assert.equal(report.status, 'PARTIAL_ALIVE');
  assert.equal(report.checkpoints.find(checkpoint => checkpoint.id === 'dependency-rule').status, 'UNSUPPORTED');
});
