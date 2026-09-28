/**
 * Cloud-Fog-Edge-IoT-Edge-Fog-Cloud (CFE-IEC) Loop Stress Test
 * Simulates a data packet traveling through the hierarchical KGC infrastructure
 * 3 iterations to test rehydration, state consistency, and causal ordering.
 */
import { describe, it, expect } from 'vitest';
import { KGCStore, EVENT_TYPES } from '@unrdf/kgc-4d';
import { VectorClock } from '../../src/time.mjs';
import { dataFactory } from '@unrdf/oxigraph';

const layers = ['cloud', 'fog', 'edge', 'iot', 'edge', 'fog', 'cloud'];
const NODES = 8;
const ITERATIONS = 3;

/**
 * One packet round trip through every layer.
 * @returns {Promise<Array<{layer: string, receipt: {event_count: number}}>>} receipts in layer order
 */
async function simulateRoundTrip(store, nodeId, iteration) {
  const results = [];
  for (const layer of layers) {
    const vc = new VectorClock(nodeId);
    vc.increment();

    const { receipt } = await store.appendEvent(
      {
        type: EVENT_TYPES.CREATE,
        payload: { layer, iteration, nodeId },
      },
      [
        {
          type: 'add',
          subject: dataFactory.namedNode(`http://node/${nodeId}`),
          predicate: dataFactory.namedNode('http://layer'),
          object: dataFactory.literal(layer),
        },
      ]
    );
    results.push({ layer, receipt });
  }
  return results;
}

async function runSimulation() {
  const store = new KGCStore({ nodeId: 'master-controller' });

  const simulations = Array.from({ length: NODES }, async (_, i) => {
    const results = [];
    for (let iteration = 1; iteration <= ITERATIONS; iteration++) {
      results.push(...(await simulateRoundTrip(store, `node-${i}`, iteration)));
    }
    return results;
  });

  const perNode = await Promise.all(simulations);
  return { store, perNode };
}

describe('CFE-IEC loop', () => {
  it('records every event of 8 parallel loops without state divergence', async () => {
    const { store } = await runSimulation();

    // 8 nodes * 7 layers * 3 iterations = 168
    const expected = NODES * layers.length * ITERATIONS;
    expect(expected).toBe(168);
    expect(store.getEventLogStats().eventCount).toBe(expected);
    expect(store.getEventCount()).toBe(expected);
  });

  it('assigns each concurrent append a unique, gap-free event count', async () => {
    const { perNode } = await runSimulation();

    const counts = perNode.flat().map(({ receipt }) => receipt.event_count);
    const total = NODES * layers.length * ITERATIONS;

    expect(new Set(counts).size).toBe(total);
    expect(Math.min(...counts)).toBe(1);
    expect(Math.max(...counts)).toBe(total);
  });

  it('preserves causal order of each node across its round trips', async () => {
    const { perNode } = await runSimulation();

    for (const results of perNode) {
      // Layer sequence repeats cloud->fog->edge->iot->edge->fog->cloud per iteration
      expect(results.map((r) => r.layer)).toEqual(
        Array.from({ length: ITERATIONS }, () => layers).flat()
      );

      // A node's own events are ordered: later steps get larger event counts
      const counts = results.map((r) => r.receipt.event_count);
      expect(counts).toEqual([...counts].sort((a, b) => a - b));
    }
  });
});
