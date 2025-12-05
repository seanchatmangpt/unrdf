/**
 * KGC Freeze - Universe Snapshot and Time-Travel Replay
 * Uses hash-wasm for BLAKE3 (ARD-mandated: fastest WASM implementation)
 */

import { blake3 } from 'hash-wasm';
import { dataFactory } from '@unrdf/oxigraph';
import { now, toISO, fromISO } from './time.mjs';
import { GRAPHS, EVENT_TYPES, PREDICATES } from './constants.mjs';

/**
 * Freeze the current universe state: dump, hash, commit to Git, record in EventLog
 *
 * @example
 * import { freezeUniverse } from './freeze.mjs';
 * import { KGCStore } from './store.mjs';
 * import { GitBackbone } from './git.mjs';
 * const store = new KGCStore();
 * const git = new GitBackbone('/tmp/freeze-test');
 * const result = await freezeUniverse(store, git);
 * console.assert(result.id, 'Returns receipt with id');
 */
export async function freezeUniverse(store, gitBackbone) {
  try {
    // 1. Get only Universe graph quads and serialize to N-Quads
    const universeGraph = dataFactory.namedNode(GRAPHS.UNIVERSE);
    const universeQuads = [...store.match(null, null, null, universeGraph)];

    // Sort for canonical ordering (deterministic hash)
    // Use RDF spec compliant S-P-O component comparison (NOT localeCompare - varies by env)
    universeQuads.sort((a, b) => {
      // Compare subject first
      const sCompare = a.subject.value < b.subject.value ? -1 :
                       a.subject.value > b.subject.value ? 1 : 0;
      if (sCompare !== 0) return sCompare;

      // Then predicate
      const pCompare = a.predicate.value < b.predicate.value ? -1 :
                       a.predicate.value > b.predicate.value ? 1 : 0;
      if (pCompare !== 0) return pCompare;

      // Finally object
      return a.object.value < b.object.value ? -1 :
             a.object.value > b.object.value ? 1 : 0;
    });

    // Serialize to N-Quads format
    const nquads = universeQuads.map(q => {
      const s = q.subject.termType === 'NamedNode'
        ? `<${q.subject.value}>`
        : `_:${q.subject.value}`;
      const p = `<${q.predicate.value}>`;
      let o;
      if (q.object.termType === 'NamedNode') {
        o = `<${q.object.value}>`;
      } else if (q.object.termType === 'BlankNode') {
        o = `_:${q.object.value}`;
      } else {
        // Literal
        const escaped = q.object.value.replace(/\\/g, '\\\\').replace(/"/g, '\\"');
        if (q.object.datatype && q.object.datatype.value !== 'http://www.w3.org/2001/XMLSchema#string') {
          o = `"${escaped}"^^<${q.object.datatype.value}>`;
        } else if (q.object.language) {
          o = `"${escaped}"@${q.object.language}`;
        } else {
          o = `"${escaped}"`;
        }
      }
      const g = `<${q.graph.value}>`;
      return `${s} ${p} ${o} ${g} .`;
    }).join('\n');

    // 2. Hash the universe state (BLAKE3 via hash-wasm - ARD mandated)
    const universeHash = await blake3(nquads);

    // 3. Commit to Git (use wall-clock time for message)
    const wallClockISO = new Date().toISOString();
    const gitRef = await gitBackbone.commitSnapshot(
      nquads,
      `Universe freeze at ${wallClockISO}`
    );

    // 4. Create SNAPSHOT event and append to log
    const { receipt } = await store.appendEvent({
      type: EVENT_TYPES.SNAPSHOT,
      payload: {
        universe_hash: universeHash,
        nquad_count: nquads.split('\n').filter(l => l.trim()).length,
      },
      git_ref: gitRef,  // Store git_ref as separate predicate (CRITICAL for reconstructState lookup)
    }, []);

    // 5. Update latest snapshot pointer in System graph (O(1) lookup optimization - Flaw 6 fix)
    const systemGraph = dataFactory.namedNode(GRAPHS.SYSTEM);
    const configSubj = dataFactory.namedNode('http://kgc.io/system/config');
    const latestSnapshotPred = dataFactory.namedNode('http://kgc.io/latestSnapshot');
    const latestSnapshotTimePred = dataFactory.namedNode('http://kgc.io/latestSnapshotTime');

    // Remove old pointers
    const oldSnapshotPointers = [...store.match(configSubj, latestSnapshotPred, null, systemGraph)];
    const oldTimePointers = [...store.match(configSubj, latestSnapshotTimePred, null, systemGraph)];
    for (const q of oldSnapshotPointers) store.delete(q);
    for (const q of oldTimePointers) store.delete(q);

    // Add new pointer to latest snapshot
    store.add(dataFactory.quad(
      configSubj,
      latestSnapshotPred,
      dataFactory.namedNode(`http://kgc.io/event/${receipt.id}`),
      systemGraph
    ));
    store.add(dataFactory.quad(
      configSubj,
      latestSnapshotTimePred,
      dataFactory.literal(receipt.t_ns, dataFactory.namedNode('http://www.w3.org/2001/XMLSchema#integer')),
      systemGraph
    ));

    return {
      id: receipt.id,
      t_ns: receipt.t_ns,
      timestamp_iso: receipt.timestamp_iso,
      universe_hash: universeHash,
      git_ref: gitRef,
      event_count: receipt.event_count,
    };
  } catch (error) {
    throw new Error(`Failed to freeze universe: ${error.message}`);
  }
}

/**
 * Helper: Convert serialized delta back to quad
 * @param {Object} delta - Serialized delta from event payload
 * @param {string} graphUri - Target graph URI
 * @returns {Object} RDF quad
 */
function deltaToQuad(delta, graphUri) {
  // Reconstruct subject
  const subject = delta.subjectType === 'BlankNode'
    ? dataFactory.blankNode(delta.subject)
    : dataFactory.namedNode(delta.subject);

  // Reconstruct predicate
  const predicate = dataFactory.namedNode(delta.predicate);

  // Reconstruct object
  let object;
  if (delta.object.type === 'Literal') {
    if (delta.object.language) {
      object = dataFactory.literal(delta.object.value, delta.object.language);
    } else if (delta.object.datatype) {
      object = dataFactory.literal(delta.object.value, dataFactory.namedNode(delta.object.datatype));
    } else {
      object = dataFactory.literal(delta.object.value);
    }
  } else if (delta.object.type === 'BlankNode') {
    object = dataFactory.blankNode(delta.object.value);
  } else {
    object = dataFactory.namedNode(delta.object.value);
  }

  return dataFactory.quad(subject, predicate, object, dataFactory.namedNode(graphUri));
}

/**
 * Reconstruct universe state at a specific time by loading nearest snapshot and replaying events
 * Implements FULL nanosecond-precision time travel (Flaw 2 fix)
 */
export async function reconstructState(store, gitBackbone, targetTime) {
  try {
    const eventLogGraph = dataFactory.namedNode(GRAPHS.EVENT_LOG);
    const systemGraph = dataFactory.namedNode(GRAPHS.SYSTEM);
    const typePredi = dataFactory.namedNode('http://kgc.io/type');
    const tNsPredi = dataFactory.namedNode('http://kgc.io/t_ns');
    const gitRefPredi = dataFactory.namedNode('http://kgc.io/git_ref');
    const payloadPredi = dataFactory.namedNode(PREDICATES.PAYLOAD);

    // 1. Try O(1) lookup via cached snapshot pointer first (Flaw 6 optimization)
    const configSubj = dataFactory.namedNode('http://kgc.io/system/config');
    const latestSnapshotPred = dataFactory.namedNode('http://kgc.io/latestSnapshot');
    const cachedPointers = [...store.match(configSubj, latestSnapshotPred, null, systemGraph)];

    let bestSnapshot = null;
    let bestTime = 0n;

    if (cachedPointers.length > 0) {
      // Check if cached snapshot is usable (before target time)
      const cachedSubject = dataFactory.namedNode(cachedPointers[0].object.value);
      const cachedTimeQuads = [...store.match(cachedSubject, tNsPredi, null, eventLogGraph)];
      const cachedGitRefQuads = [...store.match(cachedSubject, gitRefPredi, null, eventLogGraph)];

      if (cachedTimeQuads.length > 0 && cachedGitRefQuads.length > 0) {
        const cachedTime = BigInt(cachedTimeQuads[0].object.value);
        if (cachedTime <= targetTime) {
          bestTime = cachedTime;
          bestSnapshot = {
            subject: cachedSubject,
            t_ns: cachedTime,
            git_ref: cachedGitRefQuads[0].object.value
          };
        }
      }
    }

    // 2. If no cached snapshot or it's after target time, scan for best snapshot
    if (!bestSnapshot) {
      // Try to find all events (not just filtered by type, since snapshot storage may vary)
      const allEventTimeQuadsForEdgeCase = [...store.match(null, tNsPredi, null, eventLogGraph)];

      if (allEventTimeQuadsForEdgeCase.length === 0) {
        // Edge case 1: No events exist yet - return empty store (genesis)
        const TempStore = store.constructor;
        return new TempStore();
      }

      // Find events with SNAPSHOT type indicator
      const snapshotQuads = [
        ...store.match(null, typePredi, dataFactory.literal('SNAPSHOT'), eventLogGraph)
      ];

      if (snapshotQuads.length === 0) {
        // Edge case 2: No snapshots created yet - check if targetTime is before all events
        let earliestTime = null;
        for (const timeQuad of allEventTimeQuadsForEdgeCase) {
          const time = BigInt(timeQuad.object.value);
          if (earliestTime === null || time < earliestTime) {
            earliestTime = time;
          }
        }

        if (earliestTime !== null && targetTime < earliestTime) {
          // Target time is before any events - return empty store (genesis)
          const TempStore = store.constructor;
          return new TempStore();
        }

        throw new Error(`No snapshot found before time ${targetTime} (earliest event: ${earliestTime})`);
      }

      for (const quad of snapshotQuads) {
        const subject = quad.subject;
        const timeQuads = [...store.match(subject, tNsPredi, null, eventLogGraph)];
        const gitRefQuads = [...store.match(subject, gitRefPredi, null, eventLogGraph)];

        if (timeQuads.length > 0 && gitRefQuads.length > 0) {
          const time = BigInt(timeQuads[0].object.value);
          if (time <= targetTime && time > bestTime) {
            bestTime = time;
            bestSnapshot = {
              subject,
              t_ns: time,
              git_ref: gitRefQuads[0].object.value
            };
          }
        }
      }
    }

    if (!bestSnapshot) {
      throw new Error(`No snapshot found before time ${targetTime}`);
    }

    const snapshotTime = bestSnapshot.t_ns;
    const snapshotGitRef = bestSnapshot.git_ref;

    // 3. Load snapshot from Git
    const snapshotNQuads = await gitBackbone.readSnapshot(snapshotGitRef);

    // 4. Create temporary store from snapshot
    const TempStore = store.constructor;
    const tempStore = new TempStore();
    await tempStore.load(snapshotNQuads, {
      format: 'application/n-quads',
      graph: GRAPHS.UNIVERSE,
    });

    // 5. Find ALL events between snapshot time and target time (Flaw 2 fix - CRITICAL)
    const allEventTimeQuads = [...store.match(null, tNsPredi, null, eventLogGraph)];

    // Filter events in the replay window: snapshotTime < eventTime <= targetTime
    const eventsToReplay = [];
    for (const timeQuad of allEventTimeQuads) {
      const eventTime = BigInt(timeQuad.object.value);
      if (eventTime > snapshotTime && eventTime <= targetTime) {
        eventsToReplay.push({
          subject: timeQuad.subject,
          t_ns: eventTime
        });
      }
    }

    // Sort by time (ascending) for correct replay order
    eventsToReplay.sort((a, b) => {
      if (a.t_ns < b.t_ns) return -1;
      if (a.t_ns > b.t_ns) return 1;
      return 0;
    });

    // 6. Replay each event's deltas to reconstruct exact state at targetTime
    for (const event of eventsToReplay) {
      const payloadQuads = [...store.match(event.subject, payloadPredi, null, eventLogGraph)];

      if (payloadQuads.length > 0) {
        try {
          const payload = JSON.parse(payloadQuads[0].object.value);
          const deltas = payload.deltas || [];

          // Apply each delta to the temporary store
          for (const delta of deltas) {
            const quad = deltaToQuad(delta, GRAPHS.UNIVERSE);

            if (delta.type === 'add') {
              tempStore.add(quad);
            } else if (delta.type === 'delete') {
              tempStore.delete(quad);
            }
          }
        } catch {
          // Payload parsing failed - skip this event (might be SNAPSHOT with no deltas)
          continue;
        }
      }
    }

    return tempStore;
  } catch (error) {
    throw new Error(`Failed to reconstruct state: ${error.message}`);
  }
}

/**
 * Verify a frozen universe by comparing hash and git commit
 */
export async function verifyReceipt(receipt, gitBackbone, store) {
  try {
    // 1. Load snapshot from Git
    const nquads = await gitBackbone.readSnapshot(receipt.git_ref);

    // 2. Recompute hash (BLAKE3 via hash-wasm - ARD mandated)
    const recomputedHash = await blake3(nquads);

    // 3. Compare hashes
    if (recomputedHash !== receipt.universe_hash) {
      return {
        valid: false,
        reason: `Hash mismatch: expected ${receipt.universe_hash}, got ${recomputedHash}`,
      };
    }

    return {
      valid: true,
      receipt_id: receipt.id,
      timestamp: receipt.timestamp_iso,
      universe_hash: receipt.universe_hash,
      git_commit: receipt.git_ref,
    };
  } catch (error) {
    return {
      valid: false,
      reason: `Verification failed: ${error.message}`,
    };
  }
}
