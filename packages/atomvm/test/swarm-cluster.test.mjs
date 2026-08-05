import test from 'node:test';
import assert from 'node:assert/strict';
import { AtomVMSwarmCluster, SwarmClusterRefusal } from '../src/swarm-cluster.mjs';

function fixture() {
  let id = 0;
  const cluster = new AtomVMSwarmCluster({
    clusterId: 'edge-federation',
    clock: () => '2026-08-02T10:00:00.000Z',
    idFactory: () => `id-${++id}`
  });
  for (const name of ['west', 'central', 'east']) {
    cluster.admitSwarm({
      id: name,
      gatewayNode: `${name}-gateway`,
      cookieRef: `secret://${name}`,
      endpoint: `atomvm://${name}`
    });
  }
  cluster.connect('west', 'central');
  cluster.connect('central', 'east');
  return cluster;
}

test('routes deterministically across multiple AtomVM swarms', () => {
  assert.deepEqual(fixture().route('west', 'east'), ['west', 'central', 'east']);
});

test('constructs, brokers, receipts, and replays an admitted operation', async () => {
  const cluster = fixture();
  const intent = cluster.constructIntent({
    sourceId: 'west',
    targetId: 'east',
    operation: 'rdf.delta.apply',
    payload: { add: 3 }
  });
  const receipt = await cluster.actuate(intent, {
    execute: async ({ target, route }) => ({ target: target.id, hops: route.length - 1 })
  });
  assert.equal(receipt.status, 'ALIVE');
  assert.equal(cluster.verifyReceipt(receipt), true);
  assert.deepEqual(cluster.replay(receipt.receiptId), receipt);
});

test('refuses unreceipted actuation', async () => {
  const cluster = fixture();
  const intent = cluster.constructIntent({
    sourceId: 'west',
    targetId: 'east',
    operation: 'ping',
    payload: null
  });
  await assert.rejects(
    () => cluster.actuate(intent),
    error => error instanceof SwarmClusterRefusal && error.code === 'BROKER_REQUIRED_REFUSED'
  );
});

test('projects the admitted topology as deterministic RDF', () => {
  const nquads = fixture().toNQuads();
  assert.match(nquads, /cluster\/edge-federation/);
  assert.match(nquads, /swarm\/central.*federatedWith.*swarm\/west/);
  assert.match(nquads, /swarm\/central.*federatedWith.*swarm\/east/);
});
