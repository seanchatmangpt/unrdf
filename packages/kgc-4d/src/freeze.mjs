/**
 * KGC Freeze - Universe Snapshot and Time-Travel Replay
 */

import { blake3 } from '@noble/hashes/blake3';
import { bytesToHex } from '@noble/hashes/utils';
import { dataFactory } from '@unrdf/oxigraph';
import { now, toISO, fromISO } from './time.mjs';
import { GRAPHS, EVENT_TYPES, PREDICATES } from './constants.mjs';

/**
 * Freeze the current universe state: dump, hash, commit to Git, record in EventLog
 */
export async function freezeUniverse(store, gitBackbone) {
  try {
    // 1. Get only Universe graph quads and serialize to N-Quads
    const universeGraph = dataFactory.namedNode(GRAPHS.UNIVERSE);
    const universeQuads = [...store.match(null, null, null, universeGraph)];

    // Sort for canonical ordering (deterministic hash)
    universeQuads.sort((a, b) => {
      const as = a.subject.value + a.predicate.value + a.object.value;
      const bs = b.subject.value + b.predicate.value + b.object.value;
      return as.localeCompare(bs);
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

    // 2. Hash the universe state (BLAKE3)
    const universeHash = bytesToHex(blake3(nquads));

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
        git_ref: gitRef,
        nquad_count: nquads.split('\n').filter(l => l.trim()).length,
      },
    }, []);

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
 * Reconstruct universe state at a specific time by loading nearest snapshot and replaying events
 */
export async function reconstructState(store, gitBackbone, targetTime) {
  try {
    // 1. Find nearest SNAPSHOT event before targetTime
    // Query all events in EventLog and find SNAPSHOT events
    const eventLogGraph = dataFactory.namedNode(GRAPHS.EVENT_LOG);
    const typePredi = dataFactory.namedNode('http://kgc.io/type');
    const tNsPredi = dataFactory.namedNode('http://kgc.io/t_ns');
    const gitRefPredi = dataFactory.namedNode('http://kgc.io/git_ref');

    // Get all snapshot quads from event log
    const snapshotQuads = [
      ...store.match(null, typePredi, dataFactory.literal('SNAPSHOT'), eventLogGraph)
    ];

    if (snapshotQuads.length === 0) {
      throw new Error(`No snapshot found before time ${targetTime}`);
    }

    // Find the snapshot subject with matching time
    let bestSnapshot = null;
    let bestTime = 0n;

    for (const quad of snapshotQuads) {
      const subject = quad.subject;
      // Get the t_ns for this event
      const timeQuads = [...store.match(subject, tNsPredi, null, eventLogGraph)];
      const gitRefQuads = [...store.match(subject, gitRefPredi, null, eventLogGraph)];

      if (timeQuads.length > 0 && gitRefQuads.length > 0) {
        const timeStr = timeQuads[0].object.value;
        const time = BigInt(timeStr);

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

    if (!bestSnapshot) {
      throw new Error(`No snapshot found before time ${targetTime}`);
    }

    const snapshotTime = bestSnapshot.t_ns;
    const snapshotGitRef = bestSnapshot.git_ref;

    // 2. Load snapshot from Git
    const snapshotNQuads = await gitBackbone.readSnapshot(snapshotGitRef);

    // 3. Create temporary store from snapshot
    const TempStore = store.constructor;
    const tempStore = new TempStore();
    await tempStore.load(snapshotNQuads, {
      format: 'application/n-quads',
      graph: GRAPHS.UNIVERSE,
    });

    // 4. For MVP, return the snapshot state at target time
    // Full delta replay would require deserializing and applying events (future enhancement)
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

    // 2. Recompute hash
    const recomputedHash = bytesToHex(blake3(nquads));

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
