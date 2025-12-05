/**
 * KGC-4D Universe Singleton - Server-Side State Management
 *
 * The Universe is the authoritative source of truth.
 * It holds the complete 4D Graph (O, t_ns, V, G) and enforces all invariants.
 *
 * Clients (Browsers) never hold the Universe - they receive Shards (projections).
 */

import { KGCStore, GitBackbone, GRAPHS, now, toISO, VectorClock } from '@unrdf/kgc-4d';
import { dataFactory } from '@unrdf/oxigraph';

// Singleton Universe instance
let universe = null;
let gitBackbone = null;

// Subscription registry for real-time updates
const subscriptions = new Map();

/**
 * Initialize or return the Universe singleton
 * @returns {Promise<KGCStore>}
 */
export async function getUniverse() {
  if (!universe) {
    universe = new KGCStore({ nodeId: 'universe-primary' });
    gitBackbone = new GitBackbone('./data/kgc-repo');

    // Seed with demo data if empty
    const universeGraph = dataFactory.namedNode(GRAPHS.UNIVERSE);
    const quads = [...universe.match(null, null, null, universeGraph)];

    if (quads.length === 0) {
      await seedDemoData(universe);
    }
  }

  return universe;
}

/**
 * Get the Git backbone instance
 * @returns {GitBackbone}
 */
export function getGitBackbone() {
  return gitBackbone;
}

/**
 * Seed the Universe with demo data for the playground
 */
async function seedDemoData(store) {
  const deltas = [];

  // Demo Project Alpha
  deltas.push({
    type: 'add',
    subject: dataFactory.namedNode('http://example.org/project/alpha'),
    predicate: dataFactory.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    object: dataFactory.namedNode('http://kgc.io/ontology/Project'),
  });
  deltas.push({
    type: 'add',
    subject: dataFactory.namedNode('http://example.org/project/alpha'),
    predicate: dataFactory.namedNode('http://kgc.io/ontology/name'),
    object: dataFactory.literal('Project Alpha'),
  });
  deltas.push({
    type: 'add',
    subject: dataFactory.namedNode('http://example.org/project/alpha'),
    predicate: dataFactory.namedNode('http://kgc.io/ontology/budget'),
    object: dataFactory.literal('25000', dataFactory.namedNode('http://www.w3.org/2001/XMLSchema#integer')),
  });
  deltas.push({
    type: 'add',
    subject: dataFactory.namedNode('http://example.org/project/alpha'),
    predicate: dataFactory.namedNode('http://kgc.io/ontology/status'),
    object: dataFactory.literal('active'),
  });

  // Demo Task 1
  deltas.push({
    type: 'add',
    subject: dataFactory.namedNode('http://example.org/task/1'),
    predicate: dataFactory.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    object: dataFactory.namedNode('http://kgc.io/ontology/Task'),
  });
  deltas.push({
    type: 'add',
    subject: dataFactory.namedNode('http://example.org/task/1'),
    predicate: dataFactory.namedNode('http://kgc.io/ontology/title'),
    object: dataFactory.literal('Implement Shard Projection'),
  });
  deltas.push({
    type: 'add',
    subject: dataFactory.namedNode('http://example.org/task/1'),
    predicate: dataFactory.namedNode('http://kgc.io/ontology/belongsTo'),
    object: dataFactory.namedNode('http://example.org/project/alpha'),
  });
  deltas.push({
    type: 'add',
    subject: dataFactory.namedNode('http://example.org/task/1'),
    predicate: dataFactory.namedNode('http://kgc.io/ontology/completed'),
    object: dataFactory.literal('false', dataFactory.namedNode('http://www.w3.org/2001/XMLSchema#boolean')),
  });

  // Demo Task 2
  deltas.push({
    type: 'add',
    subject: dataFactory.namedNode('http://example.org/task/2'),
    predicate: dataFactory.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    object: dataFactory.namedNode('http://kgc.io/ontology/Task'),
  });
  deltas.push({
    type: 'add',
    subject: dataFactory.namedNode('http://example.org/task/2'),
    predicate: dataFactory.namedNode('http://kgc.io/ontology/title'),
    object: dataFactory.literal('Build WebSocket Tether'),
  });
  deltas.push({
    type: 'add',
    subject: dataFactory.namedNode('http://example.org/task/2'),
    predicate: dataFactory.namedNode('http://kgc.io/ontology/belongsTo'),
    object: dataFactory.namedNode('http://example.org/project/alpha'),
  });
  deltas.push({
    type: 'add',
    subject: dataFactory.namedNode('http://example.org/task/2'),
    predicate: dataFactory.namedNode('http://kgc.io/ontology/completed'),
    object: dataFactory.literal('false', dataFactory.namedNode('http://www.w3.org/2001/XMLSchema#boolean')),
  });

  // Demo User
  deltas.push({
    type: 'add',
    subject: dataFactory.namedNode('http://example.org/user/alice'),
    predicate: dataFactory.namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
    object: dataFactory.namedNode('http://kgc.io/ontology/User'),
  });
  deltas.push({
    type: 'add',
    subject: dataFactory.namedNode('http://example.org/user/alice'),
    predicate: dataFactory.namedNode('http://kgc.io/ontology/name'),
    object: dataFactory.literal('Alice'),
  });
  deltas.push({
    type: 'add',
    subject: dataFactory.namedNode('http://example.org/user/alice'),
    predicate: dataFactory.namedNode('http://kgc.io/ontology/assignedTo'),
    object: dataFactory.namedNode('http://example.org/task/1'),
  });

  await store.appendEvent(
    {
      type: 'CREATE',
      payload: { description: 'Initial seed data for playground' },
    },
    deltas
  );

  console.log('[Universe] Seeded with demo data');
}

/**
 * Register a subscription for real-time updates
 * @param {string} subscriptionId - Unique subscription ID
 * @param {Object} query - Query pattern for filtering updates
 * @param {Function} callback - Callback for updates
 */
export function registerSubscription(subscriptionId, query, callback) {
  subscriptions.set(subscriptionId, { query, callback });
}

/**
 * Unregister a subscription
 * @param {string} subscriptionId
 */
export function unregisterSubscription(subscriptionId) {
  subscriptions.delete(subscriptionId);
}

/**
 * Broadcast update to all relevant subscribers
 * @param {Object} delta - The delta that was applied
 */
export function broadcastUpdate(delta) {
  for (const [id, { query, callback }] of subscriptions) {
    // Simple pattern matching - check if delta matches subscription query
    if (matchesQuery(delta, query)) {
      callback({
        type: 'DELTA',
        delta,
        t_ns: now().toString(),
      });
    }
  }
}

/**
 * Check if a delta matches a subscription query
 * @param {Object} delta
 * @param {Object} query
 * @returns {boolean}
 */
function matchesQuery(delta, query) {
  if (!query) return true;

  // Match by subject pattern
  if (query.subject && delta.subject !== query.subject) {
    return false;
  }

  // Match by predicate pattern
  if (query.predicate && delta.predicate !== query.predicate) {
    return false;
  }

  // Match by graph pattern
  if (query.graph && delta.graph !== query.graph) {
    return false;
  }

  return true;
}

export { GRAPHS, dataFactory, now, toISO, VectorClock };
