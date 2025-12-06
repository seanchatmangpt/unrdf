/**
 * Multiverse Server Utilities
 *
 * Fork/merge infrastructure for reality branching.
 * Forks are in-memory clones of the Universe at a specific time.
 * In production, would use Redis/DynamoDB for persistence.
 */

import { reconstructState, GRAPHS } from '@unrdf/kgc-4d';
import { dataFactory } from '@unrdf/oxigraph';
import { getUniverse, getGitBackbone } from './universe.mjs';

// In-memory fork registry (use Redis in production)
const forks = new Map();

/**
 * Create a new forked reality from a specific time
 *
 * @param {string} forkId - Unique fork identifier
 * @param {BigInt} fromTime - Timestamp to fork from
 * @returns {Promise<Object>} Fork metadata
 */
export async function createFork(forkId, fromTime) {
  const mainStore = await getUniverse();
  const git = getGitBackbone();

  // Reconstruct state at fork point
  const forkStore = await reconstructState(mainStore, git, fromTime);

  // Count quads
  const universeGraph = dataFactory.namedNode(GRAPHS.UNIVERSE);
  const quads = [...forkStore.match(null, null, null, universeGraph)];

  // Store fork metadata
  const fork = {
    id: forkId,
    store: forkStore,
    createdAt: fromTime.toString(),
    createdAtIso: new Date(Number(fromTime / 1000000n)).toISOString(),
    events: [], // Track events applied to this fork
    baseQuadCount: quads.length,
  };

  forks.set(forkId, fork);

  return {
    forkId,
    baseTime: fromTime.toString(),
    baseTimeIso: fork.createdAtIso,
    quadCount: quads.length,
    status: 'active',
  };
}

/**
 * Apply a delta to a forked Universe
 *
 * @param {string} forkId - Fork to modify
 * @param {Object} delta - Delta operation (CREATE, UPDATE, DELETE)
 * @returns {Promise<Object>} Result with ACK/REJECT
 */
export async function applyDeltaToFork(forkId, delta) {
  const fork = forks.get(forkId);
  if (!fork) {
    throw new Error(`Fork not found: ${forkId}`);
  }

  try {
    // Apply delta to fork store
    // (Simplified - in production would run knowledge hooks)
    const universeGraph = dataFactory.namedNode(GRAPHS.UNIVERSE);

    if (delta.type === 'CREATE' || delta.type === 'UPDATE') {
      const subject = dataFactory.namedNode(delta.subject);
      const predicate = dataFactory.namedNode(delta.predicate);

      // Remove old value if UPDATE
      if (delta.type === 'UPDATE' && delta.oldValue) {
        const oldObject = createObject(delta.oldValue);
        const oldQuad = dataFactory.quad(subject, predicate, oldObject, universeGraph);
        fork.store.delete(oldQuad);
      }

      // Add new value
      const newObject = createObject(delta.newValue);
      const newQuad = dataFactory.quad(subject, predicate, newObject, universeGraph);
      fork.store.add(newQuad);
    } else if (delta.type === 'DELETE') {
      const subject = dataFactory.namedNode(delta.subject);
      const predicate = dataFactory.namedNode(delta.predicate);
      const object = createObject(delta.value);
      const quad = dataFactory.quad(subject, predicate, object, universeGraph);
      fork.store.delete(quad);
    }

    // Track event
    const event = {
      id: crypto.randomUUID(),
      type: delta.type,
      delta,
      timestamp: new Date().toISOString(),
    };
    fork.events.push(event);

    return {
      status: 'ACK',
      eventId: event.id,
      forkId,
      eventCount: fork.events.length,
    };
  } catch (error) {
    return {
      status: 'REJECT',
      reason: error.message,
      forkId,
    };
  }
}

/**
 * Merge a fork back into the main Universe
 *
 * @param {string} forkId - Fork to merge
 * @param {string} strategy - 'auto' or 'manual'
 * @returns {Promise<Object>} Merge result with conflicts if any
 */
export async function mergeFork(forkId, strategy = 'auto') {
  const fork = forks.get(forkId);
  if (!fork) {
    throw new Error(`Fork not found: ${forkId}`);
  }

  const mainStore = await getUniverse();

  if (strategy === 'auto') {
    // Detect conflicts: events that modified same triples in main
    const conflicts = [];

    for (const event of fork.events) {
      const delta = event.delta;
      const subject = dataFactory.namedNode(delta.subject);
      const predicate = dataFactory.namedNode(delta.predicate);
      const universeGraph = dataFactory.namedNode(GRAPHS.UNIVERSE);

      // Check if main Universe has different value
      const mainQuads = [...mainStore.match(subject, predicate, null, universeGraph)];

      if (mainQuads.length > 0) {
        const forkQuads = [...fork.store.match(subject, predicate, null, universeGraph)];

        // Conflict if values differ
        if (mainQuads.length !== forkQuads.length ||
            !quadsEqual(mainQuads[0], forkQuads[0])) {
          conflicts.push({
            event,
            mainValue: mainQuads[0]?.object.value,
            forkValue: forkQuads[0]?.object.value,
          });
        }
      }
    }

    if (conflicts.length > 0) {
      return {
        status: 'conflict',
        conflicts,
        message: `Found ${conflicts.length} conflict(s). Use manual merge.`,
      };
    }

    // No conflicts - apply all fork events to main
    for (const event of fork.events) {
      await applyDeltaToMain(mainStore, event.delta);
    }

    // Destroy fork
    forks.delete(forkId);

    return {
      status: 'success',
      mergedEvents: fork.events.length,
      forkId,
    };
  }

  // Manual merge not implemented in Phase 2
  return {
    status: 'error',
    message: 'Manual merge not yet implemented',
  };
}

/**
 * Get fork status and metadata
 */
export function getForkStatus(forkId) {
  const fork = forks.get(forkId);
  if (!fork) {
    return null;
  }

  const universeGraph = dataFactory.namedNode(GRAPHS.UNIVERSE);
  const quads = [...fork.store.match(null, null, null, universeGraph)];

  return {
    forkId: fork.id,
    baseTime: fork.createdAt,
    baseTimeIso: fork.createdAtIso,
    baseQuadCount: fork.baseQuadCount,
    currentQuadCount: quads.length,
    eventCount: fork.events.length,
    events: fork.events,
    status: 'active',
  };
}

/**
 * List all active forks
 */
export function listForks() {
  return Array.from(forks.keys()).map((forkId) => getForkStatus(forkId));
}

/**
 * Destroy a fork without merging
 */
export function destroyFork(forkId) {
  const existed = forks.delete(forkId);
  return {
    success: existed,
    forkId,
  };
}

// Helper functions

function createObject(value) {
  if (typeof value === 'object' && value.type === 'uri') {
    return dataFactory.namedNode(value.value);
  } else if (typeof value === 'object' && value.type === 'literal') {
    return dataFactory.literal(value.value, value.datatype || value.language);
  } else if (typeof value === 'string') {
    return dataFactory.literal(value);
  } else {
    return dataFactory.literal(String(value));
  }
}

function quadsEqual(q1, q2) {
  if (!q1 || !q2) return false;
  return (
    q1.subject.value === q2.subject.value &&
    q1.predicate.value === q2.predicate.value &&
    q1.object.value === q2.object.value &&
    q1.graph.value === q2.graph.value
  );
}

async function applyDeltaToMain(store, delta) {
  const universeGraph = dataFactory.namedNode(GRAPHS.UNIVERSE);

  if (delta.type === 'CREATE' || delta.type === 'UPDATE') {
    const subject = dataFactory.namedNode(delta.subject);
    const predicate = dataFactory.namedNode(delta.predicate);

    if (delta.type === 'UPDATE' && delta.oldValue) {
      const oldObject = createObject(delta.oldValue);
      const oldQuad = dataFactory.quad(subject, predicate, oldObject, universeGraph);
      store.delete(oldQuad);
    }

    const newObject = createObject(delta.newValue);
    const newQuad = dataFactory.quad(subject, predicate, newObject, universeGraph);
    store.add(newQuad);
  } else if (delta.type === 'DELETE') {
    const subject = dataFactory.namedNode(delta.subject);
    const predicate = dataFactory.namedNode(delta.predicate);
    const object = createObject(delta.value);
    const quad = dataFactory.quad(subject, predicate, object, universeGraph);
    store.delete(quad);
  }
}
