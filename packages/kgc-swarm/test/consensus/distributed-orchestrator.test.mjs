import { describe, expect, it, vi } from 'vitest';
import {
  ConsensusMode,
  DistributedOrchestrator,
} from '../../src/consensus/distributed-orchestrator.mjs';

function createNode(nodeId, port, peers = []) {
  return new DistributedOrchestrator({
    nodeId,
    host: '127.0.0.1',
    port,
    mode: ConsensusMode.CRDT,
    peers,
    membershipConfig: {
      gossipInterval: 60_000,
      failureTimeout: 60_000,
      suspectTimeout: 60_000,
    },
  });
}

describe('DistributedOrchestrator CRDT transport', () => {
  it('exchanges admitted LWW state through the configured network boundary', async () => {
    const nodeA = createNode('node-a', 9101, ['node-b']);
    const nodeB = createNode('node-b', 9102, ['node-a']);
    const nodes = new Map([
      ['node-a', nodeA],
      ['node-b', nodeB],
    ]);

    for (const node of nodes.values()) {
      node.setNetworkHandler((peerId, message) => nodes.get(peerId).handleNetworkMessage(message));
    }

    nodeA.crdtSet.add({ artifact: 'from-a' });
    nodeB.crdtSet.add({ artifact: 'from-b' });

    const receipt = await nodeA.syncCRDT();

    expect(receipt.attempted).toEqual(['node-b']);
    expect(receipt.merged).toEqual(['node-b']);
    expect(receipt.errors).toEqual([]);
    expect(nodeA.crdtSet.has({ artifact: 'from-a' })).toBe(true);
    expect(nodeA.crdtSet.has({ artifact: 'from-b' })).toBe(true);
  });

  it('returns CRDT local execution directly instead of timing out after an early event', async () => {
    const node = createNode('node-a', 9201);
    node.running = true;
    node.localOrchestrator = {
      run: vi.fn(async (seed, control) => ({ seed, control, standing: 'ALIVE' })),
    };

    const result = await node.run('seed', 'control');

    expect(result).toEqual({ seed: 'seed', control: 'control', standing: 'ALIVE' });
    expect(node.localOrchestrator.run).toHaveBeenCalledOnce();
  });

  it('refuses malformed peer state without mutating local state', async () => {
    const node = createNode('node-a', 9301, ['node-b']);
    node.crdtSet.add({ artifact: 'local' });
    node.setNetworkHandler(async () => ({
      type: 'crdt-state-response',
      from: 'node-b',
      state: { kind: 'untrusted-shape' },
    }));

    const receipt = await node.syncCRDT();

    expect(receipt.merged).toEqual([]);
    expect(receipt.errors).toHaveLength(1);
    expect(node.crdtSet.values()).toEqual([{ artifact: 'local' }]);
  });

  it('clears its CRDT synchronization timer on stop', async () => {
    vi.useFakeTimers();
    const node = createNode('node-a', 9401);

    await node.start();
    expect(node.crdtSyncTimer).not.toBeNull();
    await node.stop();
    expect(node.crdtSyncTimer).toBeNull();

    vi.useRealTimers();
  });
});
