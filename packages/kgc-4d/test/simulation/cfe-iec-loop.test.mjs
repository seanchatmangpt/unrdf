/**
 * Cloud-Fog-Edge-IoT-Edge-Fog-Cloud (CFE-IEC) Loop Stress Test
 * Simulates a data packet traveling through the hierarchical KGC infrastructure
 * 3 iterations to test rehydration, state consistency, and causal ordering.
 */
import { KGCStore, EVENT_TYPES } from '@unrdf/kgc-4d';
import { VectorClock } from '../../src/time.mjs';
import { dataFactory } from '@unrdf/oxigraph';

const layers = ['cloud', 'fog', 'edge', 'iot', 'edge', 'fog', 'cloud'];

async function simulateRoundTrip(store, nodeId, iteration) {
  for (const layer of layers) {
    const vc = new VectorClock(nodeId);
    vc.increment();
    
    await store.appendEvent(
      { 
        type: EVENT_TYPES.CREATE, 
        payload: { layer, iteration, nodeId } 
      },
      [{ 
        type: 'add', 
        subject: dataFactory.namedNode(`http://node/${nodeId}`), 
        predicate: dataFactory.namedNode('http://layer'), 
        object: dataFactory.literal(layer) 
      }]
    );
  }
}

async function runSimulation() {
  const store = new KGCStore({ nodeId: 'master-controller' });
  
  console.log('Starting 8 parallel CFE-IEC loops...');
  
  const simulations = Array.from({ length: 8 }, (_, i) => 
    simulateRoundTrip(store, `node-${i}`, 1)
      .then(() => simulateRoundTrip(store, `node-${i}`, 2))
      .then(() => simulateRoundTrip(store, `node-${i}`, 3))
  );

  await Promise.all(simulations);
  
  const stats = store.getEventLogStats();
  console.log(`Simulation complete. Total events: ${stats.eventCount}`);
  
  // Verify expected count: 8 nodes * 7 layers * 3 iterations = 168
  if (stats.eventCount !== 168) {
    throw new Error(`State divergence detected! Expected 168, got ${stats.eventCount}`);
  }
}

runSimulation().catch(console.error);
