/**
 * Run Capsule - Deterministic Δ_run objects for Claude executions
 *
 * Every Claude run becomes a first-class capsule:
 * Δ_run = ⟨ΔO, ΔΠ, ΔΛ, ΔQ, tool_trace, artifacts⟩
 *
 * - ΔO: Changes to ontology/universe state
 * - ΔΠ: Changes to projections (views)
 * - ΔΛ: Changes to laws/constraints
 * - ΔQ: Changes to invariants
 * - tool_trace: Normalized tool call sequence
 * - artifacts: Produced files/outputs
 *
 * @module @unrdf/kgc-claude/run-capsule
 */

import { blake3 } from 'hash-wasm';
import { z } from 'zod';
import { dataFactory } from '@unrdf/oxigraph';
import { now, toISO, VectorClock } from '@unrdf/kgc-4d';
import { GRAPHS, PREDICATES, RUN_STATUS } from './constants.mjs';

/**
 * Tool call schema - normalized tool invocation
 */
const ToolCallSchema = z.object({
  id: z.string(),
  name: z.string(),
  input: z.object({}).passthrough().optional(),
  output: z.unknown().optional(),
  startTime: z.union([z.bigint(), z.number()]),
  endTime: z.union([z.bigint(), z.number()]).optional(),
  status: z.enum(['pending', 'success', 'error']),
  error: z.string().optional(),
}).passthrough();

/**
 * Artifact schema - produced file/output
 */
const ArtifactSchema = z.object({
  id: z.string(),
  type: z.enum(['file', 'edit', 'create', 'delete', 'command']),
  path: z.string().optional(),
  contentHash: z.string().optional(),
  metadata: z.object({}).passthrough().optional(),
}).passthrough();

/**
 * Delta schema - state change
 */
const DeltaSchema = z.object({
  type: z.enum(['add', 'delete', 'modify']),
  target: z.string(),
  before: z.unknown().optional(),
  after: z.unknown().optional(),
  hash: z.string().optional(),
}).passthrough();

/**
 * Run capsule schema - complete Δ_run
 */
export const RunCapsuleSchema = z.object({
  id: z.string().uuid(),
  parentRunId: z.string().uuid().optional(),
  t_ns: z.bigint(),
  timestamp_iso: z.string(),
  status: z.enum(['pending', 'running', 'completed', 'failed', 'denied', 'rolled_back']),

  // The four deltas
  deltaO: z.array(DeltaSchema).default([]),
  deltaPi: z.array(DeltaSchema).default([]),
  deltaLambda: z.array(DeltaSchema).default([]),
  deltaQ: z.array(DeltaSchema).default([]),

  // Tool trace and artifacts
  toolTrace: z.array(ToolCallSchema).default([]),
  artifacts: z.array(ArtifactSchema).default([]),

  // Cryptographic proofs
  runHash: z.string(),
  previousRunHash: z.string().nullable(),
  vectorClock: z.unknown().optional(),

  // Admission result
  admitted: z.boolean().default(false),
  denialReason: z.string().optional(),
});

/**
 * @typedef {z.infer<typeof RunCapsuleSchema>} RunCapsule
 */

/**
 * @typedef {z.infer<typeof ToolCallSchema>} ToolCall
 */

/**
 * @typedef {z.infer<typeof ArtifactSchema>} Artifact
 */

/**
 * @typedef {z.infer<typeof DeltaSchema>} Delta
 */

/**
 * Generate UUID v4
 * @returns {string}
 */
function generateUUID() {
  if (typeof crypto !== 'undefined' && crypto.randomUUID) {
    return crypto.randomUUID();
  }
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
    const r = (Math.random() * 16) | 0;
    const v = c === 'x' ? r : (r & 0x3) | 0x8;
    return v.toString(16);
  });
}

/**
 * Deterministic serialization for hashing
 * @param {any} obj
 * @returns {string}
 */
function deterministicSerialize(obj) {
  if (obj === null || obj === undefined) return 'null';
  if (typeof obj === 'bigint') return obj.toString();
  if (typeof obj !== 'object') return JSON.stringify(obj);
  if (Array.isArray(obj)) {
    return `[${obj.map(deterministicSerialize).join(',')}]`;
  }
  const sortedKeys = Object.keys(obj).sort();
  const pairs = sortedKeys.map((k) => `${JSON.stringify(k)}:${deterministicSerialize(obj[k])}`);
  return `{${pairs.join(',')}}`;
}

/**
 * Compute BLAKE3 hash of run capsule contents
 * @param {Object} contents - Run contents to hash
 * @returns {Promise<string>} 64-char hex hash
 */
async function computeRunHash(contents) {
  const serialized = deterministicSerialize(contents);
  return blake3(serialized);
}

/**
 * Create a new run capsule builder
 *
 * @param {Object} [options]
 * @param {string} [options.parentRunId] - Parent run ID for nested runs
 * @param {string} [options.previousRunHash] - Previous run hash for chaining
 * @param {Object} [options.vectorClock] - Vector clock for causality
 * @returns {RunCapsuleBuilder}
 *
 * @example
 * const builder = createRunCapsule();
 * builder.addToolCall({ name: 'Read', input: { file: 'foo.txt' } });
 * builder.addDeltaO({ type: 'add', target: 'http://example.org/file', after: { content: '...' } });
 * const capsule = await builder.seal();
 */
export function createRunCapsule(options = {}) {
  const id = generateUUID();
  const t_ns = now();

  /** @type {ToolCall[]} */
  const toolTrace = [];

  /** @type {Artifact[]} */
  const artifacts = [];

  /** @type {Delta[]} */
  const deltaO = [];
  const deltaPi = [];
  const deltaLambda = [];
  const deltaQ = [];

  let status = RUN_STATUS.PENDING;
  let denialReason;
  let admitted = false;

  return {
    id,
    t_ns,

    /**
     * Add a tool call to the trace
     * @param {Object} call
     * @param {string} call.name - Tool name
     * @param {Object} call.input - Tool input
     * @returns {string} Tool call ID
     */
    addToolCall(call) {
      const toolCall = {
        id: generateUUID(),
        name: call.name,
        input: call.input || {},
        startTime: now(),
        status: 'pending',
      };
      toolTrace.push(toolCall);
      return toolCall.id;
    },

    /**
     * Complete a tool call
     * @param {string} callId - Tool call ID
     * @param {Object} result
     * @param {any} [result.output] - Tool output
     * @param {string} [result.error] - Error message if failed
     */
    completeToolCall(callId, result) {
      const call = toolTrace.find((c) => c.id === callId);
      if (call) {
        call.endTime = now();
        if (result.error) {
          call.status = 'error';
          call.error = result.error;
        } else {
          call.status = 'success';
          call.output = result.output;
        }
      }
    },

    /**
     * Add an artifact
     * @param {Object} artifact
     * @param {string} artifact.type - Artifact type
     * @param {string} [artifact.path] - File path
     * @param {string} [artifact.contentHash] - Content hash
     * @param {Object} [artifact.metadata] - Additional metadata
     * @returns {string} Artifact ID
     */
    addArtifact(artifact) {
      const art = {
        id: generateUUID(),
        type: artifact.type,
        path: artifact.path,
        contentHash: artifact.contentHash,
        metadata: artifact.metadata,
      };
      artifacts.push(art);
      return art.id;
    },

    /**
     * Add a delta to ΔO (ontology/universe changes)
     * @param {Delta} delta
     */
    addDeltaO(delta) {
      deltaO.push(delta);
    },

    /**
     * Add a delta to ΔΠ (projection changes)
     * @param {Delta} delta
     */
    addDeltaPi(delta) {
      deltaPi.push(delta);
    },

    /**
     * Add a delta to ΔΛ (law/constraint changes)
     * @param {Delta} delta
     */
    addDeltaLambda(delta) {
      deltaLambda.push(delta);
    },

    /**
     * Add a delta to ΔQ (invariant changes)
     * @param {Delta} delta
     */
    addDeltaQ(delta) {
      deltaQ.push(delta);
    },

    /**
     * Get current metrics for autonomy checking
     * @returns {{ deltaSize: number, toolOps: number, filesTouched: number }}
     */
    getMetrics() {
      const filesTouched = new Set(artifacts.filter((a) => a.path).map((a) => a.path));
      return {
        deltaSize: deltaO.length + deltaPi.length + deltaLambda.length + deltaQ.length,
        toolOps: toolTrace.length,
        filesTouched: filesTouched.size,
      };
    },

    /**
     * Mark run as denied (failed admission)
     * @param {string} reason - Denial reason
     */
    deny(reason) {
      status = RUN_STATUS.DENIED;
      denialReason = reason;
      admitted = false;
    },

    /**
     * Seal and finalize the run capsule
     * @returns {Promise<RunCapsule>} Complete run capsule with hash
     */
    async seal() {
      if (status === RUN_STATUS.PENDING) {
        status = RUN_STATUS.COMPLETED;
        admitted = true;
      }

      // Compute hash of capsule contents
      const hashContents = {
        id,
        parentRunId: options.parentRunId,
        deltaO,
        deltaPi,
        deltaLambda,
        deltaQ,
        toolTrace,
        artifacts,
        t_ns: t_ns.toString(),
      };

      const runHash = await computeRunHash(hashContents);

      const capsule = {
        id,
        parentRunId: options.parentRunId,
        t_ns,
        timestamp_iso: toISO(t_ns),
        status,
        deltaO,
        deltaPi,
        deltaLambda,
        deltaQ,
        toolTrace,
        artifacts,
        runHash,
        previousRunHash: options.previousRunHash || null,
        vectorClock: options.vectorClock,
        admitted,
        denialReason,
      };

      return RunCapsuleSchema.parse(capsule);
    },
  };
}

/**
 * @typedef {ReturnType<typeof createRunCapsule>} RunCapsuleBuilder
 */

/**
 * Check if a run capsule should be admitted
 * Admission rule: admit(Δ_run) ⟺ preserve(Q) ∧ Δ_run ∉ H
 *
 * @param {RunCapsule} capsule - Run capsule to check
 * @param {Object} context
 * @param {Set<string>} [context.history] - Set of previously admitted run hashes
 * @param {Function} [context.preserveQ] - Function to check invariant preservation
 * @returns {{ admitted: boolean, reason?: string }}
 */
export function checkAdmission(capsule, context = {}) {
  const { history = new Set(), preserveQ = () => true } = context;

  // Check if already in history (duplicate)
  if (history.has(capsule.runHash)) {
    return { admitted: false, reason: 'Duplicate run (already in history)' };
  }

  // Check invariant preservation
  if (!preserveQ(capsule)) {
    return { admitted: false, reason: 'Invariant Q not preserved' };
  }

  return { admitted: true };
}

/**
 * Persist a run capsule to the KGC store
 *
 * @param {Object} store - KGCStore instance
 * @param {RunCapsule} capsule - Run capsule to persist
 * @returns {Promise<{ receipt: Object }>}
 */
export async function persistRunCapsule(store, capsule) {
  const runUri = dataFactory.namedNode(`http://kgc.io/run/${capsule.id}`);
  const graph = dataFactory.namedNode(GRAPHS.RUN_CAPSULES);

  const deltas = [
    // Run ID
    {
      type: 'add',
      subject: runUri,
      predicate: dataFactory.namedNode(PREDICATES.RUN_ID),
      object: dataFactory.literal(capsule.id),
    },
    // Status
    {
      type: 'add',
      subject: runUri,
      predicate: dataFactory.namedNode(PREDICATES.RUN_STATUS),
      object: dataFactory.literal(capsule.status),
    },
    // Run hash
    {
      type: 'add',
      subject: runUri,
      predicate: dataFactory.namedNode(PREDICATES.RUN_HASH),
      object: dataFactory.literal(capsule.runHash),
    },
    // Tool trace (JSON)
    {
      type: 'add',
      subject: runUri,
      predicate: dataFactory.namedNode(PREDICATES.TOOL_TRACE),
      object: dataFactory.literal(JSON.stringify(capsule.toolTrace)),
    },
    // Artifacts (JSON)
    {
      type: 'add',
      subject: runUri,
      predicate: dataFactory.namedNode(PREDICATES.ARTIFACTS),
      object: dataFactory.literal(JSON.stringify(capsule.artifacts)),
    },
    // Deltas (JSON)
    {
      type: 'add',
      subject: runUri,
      predicate: dataFactory.namedNode(PREDICATES.DELTA_O),
      object: dataFactory.literal(JSON.stringify(capsule.deltaO)),
    },
    {
      type: 'add',
      subject: runUri,
      predicate: dataFactory.namedNode(PREDICATES.DELTA_PI),
      object: dataFactory.literal(JSON.stringify(capsule.deltaPi)),
    },
    {
      type: 'add',
      subject: runUri,
      predicate: dataFactory.namedNode(PREDICATES.DELTA_LAMBDA),
      object: dataFactory.literal(JSON.stringify(capsule.deltaLambda)),
    },
    {
      type: 'add',
      subject: runUri,
      predicate: dataFactory.namedNode(PREDICATES.DELTA_Q),
      object: dataFactory.literal(JSON.stringify(capsule.deltaQ)),
    },
  ];

  // Add parent run link if exists
  if (capsule.parentRunId) {
    deltas.push({
      type: 'add',
      subject: runUri,
      predicate: dataFactory.namedNode(PREDICATES.PARENT_RUN),
      object: dataFactory.namedNode(`http://kgc.io/run/${capsule.parentRunId}`),
    });
  }

  return store.appendEvent(
    {
      type: 'RUN_CAPSULE',
      payload: {
        runId: capsule.id,
        runHash: capsule.runHash,
        status: capsule.status,
        admitted: capsule.admitted,
        denialReason: capsule.denialReason,
      },
    },
    deltas,
  );
}

/**
 * Replay a run capsule from persisted state
 *
 * @param {Object} store - KGCStore instance
 * @param {string} runId - Run ID to replay
 * @returns {Promise<RunCapsule|null>}
 */
export async function replayRunCapsule(store, runId) {
  const runUri = `http://kgc.io/run/${runId}`;
  const sparql = `
    PREFIX kgcc: <http://kgc.io/claude/>
    SELECT ?status ?runHash ?toolTrace ?artifacts ?deltaO ?deltaPi ?deltaLambda ?deltaQ
    WHERE {
      GRAPH <${GRAPHS.RUN_CAPSULES}> {
        <${runUri}> kgcc:runStatus ?status ;
                    kgcc:runHash ?runHash ;
                    kgcc:toolTrace ?toolTrace ;
                    kgcc:artifacts ?artifacts ;
                    kgcc:deltaO ?deltaO ;
                    kgcc:deltaPi ?deltaPi ;
                    kgcc:deltaLambda ?deltaLambda ;
                    kgcc:deltaQ ?deltaQ .
      }
    }
  `;

  const results = await store.query(sparql);
  if (results.length === 0) return null;

  const row = results[0];
  return {
    id: runId,
    t_ns: now(), // Replayed timestamp
    timestamp_iso: toISO(now()),
    status: row.status.value,
    deltaO: JSON.parse(row.deltaO.value),
    deltaPi: JSON.parse(row.deltaPi.value),
    deltaLambda: JSON.parse(row.deltaLambda.value),
    deltaQ: JSON.parse(row.deltaQ.value),
    toolTrace: JSON.parse(row.toolTrace.value),
    artifacts: JSON.parse(row.artifacts.value),
    runHash: row.runHash.value,
    previousRunHash: null,
    admitted: true,
  };
}
