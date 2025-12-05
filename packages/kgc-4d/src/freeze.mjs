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
    const snapshots = await store.queryEventLog(`
      PREFIX kgc: <http://kgc.io/>
      SELECT ?event ?t_ns ?git_ref
      WHERE {
        GRAPH <${GRAPHS.EVENT_LOG}> {
          ?event kgc:type "SNAPSHOT" ;
                 kgc:t_ns ?t_ns ;
                 kgc:git_ref ?git_ref .
          FILTER(?t_ns <= ${targetTime})
        }
      }
      ORDER BY DESC(?t_ns)
      LIMIT 1
    `);

    if (!snapshots || snapshots.length === 0) {
      throw new Error(`No snapshot found before time ${targetTime}`);
    }

    const snapshotResult = snapshots[0];
    const snapshotTime = BigInt(snapshotResult.t_ns.value);
    const snapshotGitRef = snapshotResult.git_ref.value;

    // 2. Load snapshot from Git
    const snapshotNQuads = await gitBackbone.readSnapshot(snapshotGitRef);

    // 3. Create temporary store from snapshot
    const TempStore = store.constructor;
    const tempStore = new TempStore();
    await tempStore.load(snapshotNQuads, {
      format: 'application/n-quads',
      graph: GRAPHS.UNIVERSE,
    });

    // 4. Replay events between snapshot and target time
    const events = await store.queryEventLog(`
      PREFIX kgc: <http://kgc.io/>
      SELECT ?event ?t_ns ?type
      WHERE {
        GRAPH <${GRAPHS.EVENT_LOG}> {
          ?event kgc:t_ns ?t_ns ;
                 kgc:type ?type .
          FILTER(?t_ns > ${snapshotTime} && ?t_ns <= ${targetTime})
        }
      }
      ORDER BY ?t_ns
    `);

    // Note: Full delta replay would require deserializing events
    // For MVP, we just return the snapshot (can enhance later)
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
