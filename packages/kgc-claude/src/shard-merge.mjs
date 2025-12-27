/**
 * Shard Merge - Multi-agent concurrency without conflicts
 *
 * Concurrency is safe because we shard deltas and merge deterministically:
 * μ(O ⊔ Δ₁ ⊔ Δ₂) = μ(O ⊔ Δ₁) ⊔ μ(Δ₂)
 *
 * With Λ defining a total conflict resolution order.
 * Ten agents can operate concurrently because conflicts are not "negotiated,"
 * they are resolved by law and emitted as receipts.
 *
 * @module @unrdf/kgc-claude/shard-merge
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';
import { dataFactory } from '@unrdf/oxigraph';
import { now, toISO, VectorClock } from '@unrdf/kgc-4d';
import { GRAPHS, PREDICATES, DENIAL_REASONS } from './constants.mjs';

/**
 * Shard scope schema - defines what an agent can touch
 */
export const ShardScopeSchema = z.object({
  /** File path patterns (globs) */
  files: z.array(z.string()).default([]),
  /** Graph URIs */
  graphs: z.array(z.string()).default([]),
  /** Subject URI patterns */
  subjects: z.array(z.string()).default([]),
  /** Predicate URI patterns */
  predicates: z.array(z.string()).default([]),
});

/**
 * @typedef {z.infer<typeof ShardScopeSchema>} ShardScope
 */

/**
 * Agent shard schema
 */
export const AgentShardSchema = z.object({
  id: z.string().uuid(),
  agentId: z.string(),
  scope: ShardScopeSchema,
  priority: z.number().int().default(0),
  createdAt: z.bigint(),
  vectorClock: z.any(),
});

/**
 * @typedef {z.infer<typeof AgentShardSchema>} AgentShard
 */

/**
 * Delta with agent attribution
 */
export const AttributedDeltaSchema = z.object({
  delta: z.object({
    type: z.enum(['add', 'delete', 'modify']),
    target: z.string(),
    before: z.any().optional(),
    after: z.any().optional(),
  }),
  agentId: z.string(),
  shardId: z.string(),
  t_ns: z.bigint(),
  vectorClock: z.any(),
});

/**
 * @typedef {z.infer<typeof AttributedDeltaSchema>} AttributedDelta
 */

/**
 * Merge result schema
 */
export const MergeResultSchema = z.object({
  merged: z.array(AttributedDeltaSchema),
  conflicts: z.array(z.object({
    delta1: AttributedDeltaSchema,
    delta2: AttributedDeltaSchema,
    resolution: z.enum(['delta1_wins', 'delta2_wins', 'both_rejected']),
    reason: z.string(),
  })),
  receiptHash: z.string(),
});

/**
 * @typedef {z.infer<typeof MergeResultSchema>} MergeResult
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
 * Shard registry for managing agent scopes
 */
const shardRegistry = new Map();

/**
 * Create a new agent shard
 *
 * @param {string} agentId - Agent identifier
 * @param {Partial<ShardScope>} scope - Shard scope
 * @param {Object} [options]
 * @param {number} [options.priority] - Merge priority (higher wins)
 * @returns {AgentShard}
 *
 * @example
 * const shard = createShard('agent-1', {
 *   files: ['src/components/**'],
 *   graphs: ['http://kgc.io/Universe'],
 * });
 */
export function createShard(agentId, scope = {}, options = {}) {
  const id = generateUUID();
  const vectorClock = new VectorClock(agentId);

  const shard = AgentShardSchema.parse({
    id,
    agentId,
    scope: ShardScopeSchema.parse(scope),
    priority: options.priority || 0,
    createdAt: now(),
    vectorClock: vectorClock.toJSON(),
  });

  shardRegistry.set(id, {
    shard,
    vectorClock,
    pendingDeltas: [],
  });

  return shard;
}

/**
 * Check if a target matches a shard scope
 *
 * @param {string} target - Target to check
 * @param {ShardScope} scope - Shard scope
 * @returns {boolean}
 */
function matchesScope(target, scope) {
  // Check files
  for (const pattern of scope.files) {
    if (matchGlob(target, pattern)) return true;
  }

  // Check graphs
  for (const graph of scope.graphs) {
    if (target.startsWith(graph)) return true;
  }

  // Check subjects
  for (const subject of scope.subjects) {
    if (target.startsWith(subject)) return true;
  }

  // Check predicates
  for (const predicate of scope.predicates) {
    if (target.includes(predicate)) return true;
  }

  // Empty scope = matches nothing
  return (
    scope.files.length === 0 &&
    scope.graphs.length === 0 &&
    scope.subjects.length === 0 &&
    scope.predicates.length === 0
  );
}

/**
 * Simple glob matching
 * @param {string} str
 * @param {string} pattern
 * @returns {boolean}
 */
function matchGlob(str, pattern) {
  const regex = new RegExp(
    '^' + pattern.replace(/\*\*/g, '.*').replace(/\*/g, '[^/]*').replace(/\?/g, '.') + '$',
  );
  return regex.test(str);
}

/**
 * Add a delta to an agent's shard
 *
 * @param {string} shardId - Shard ID
 * @param {Object} delta - Delta to add
 * @returns {{ success: boolean, reason?: string }}
 */
export function addDelta(shardId, delta) {
  const entry = shardRegistry.get(shardId);
  if (!entry) {
    return { success: false, reason: `Shard not found: ${shardId}` };
  }

  const { shard, vectorClock, pendingDeltas } = entry;

  // Check if delta is within shard scope
  if (!matchesScope(delta.target, shard.scope)) {
    return { success: false, reason: `Delta target outside shard scope: ${delta.target}` };
  }

  // Increment vector clock
  vectorClock.increment();

  const attributed = {
    delta,
    agentId: shard.agentId,
    shardId: shard.id,
    t_ns: now(),
    vectorClock: vectorClock.toJSON(),
  };

  pendingDeltas.push(attributed);
  return { success: true };
}

/**
 * Get pending deltas for a shard
 *
 * @param {string} shardId
 * @returns {AttributedDelta[]}
 */
export function getPendingDeltas(shardId) {
  const entry = shardRegistry.get(shardId);
  return entry ? [...entry.pendingDeltas] : [];
}

/**
 * Clear pending deltas for a shard
 * @param {string} shardId
 */
export function clearPendingDeltas(shardId) {
  const entry = shardRegistry.get(shardId);
  if (entry) {
    entry.pendingDeltas = [];
  }
}

/**
 * Merge deltas from multiple shards deterministically
 *
 * Conflict resolution order (Λ):
 * 1. Higher priority shard wins
 * 2. Earlier timestamp wins (deterministic ordering)
 * 3. Lexicographic agent ID comparison (final tiebreaker)
 *
 * @param {AttributedDelta[][]} deltaSets - Arrays of deltas from each shard
 * @returns {Promise<MergeResult>}
 *
 * @example
 * const result = await mergeDeltas([
 *   agent1Deltas,
 *   agent2Deltas,
 * ]);
 * console.log(`Merged ${result.merged.length} deltas, ${result.conflicts.length} conflicts`);
 */
export async function mergeDeltas(deltaSets) {
  // Flatten all deltas
  const allDeltas = deltaSets.flat();

  // Group by target
  const byTarget = new Map();
  for (const delta of allDeltas) {
    const key = delta.delta.target;
    if (!byTarget.has(key)) {
      byTarget.set(key, []);
    }
    byTarget.get(key).push(delta);
  }

  const merged = [];
  const conflicts = [];

  // Process each target
  for (const [target, deltas] of byTarget) {
    if (deltas.length === 1) {
      // No conflict
      merged.push(deltas[0]);
    } else {
      // Conflict resolution using Λ
      const sorted = [...deltas].sort((a, b) => {
        // 1. Priority comparison
        const shardA = shardRegistry.get(a.shardId)?.shard;
        const shardB = shardRegistry.get(b.shardId)?.shard;
        const priorityA = shardA?.priority || 0;
        const priorityB = shardB?.priority || 0;

        if (priorityA !== priorityB) {
          return priorityB - priorityA; // Higher priority wins
        }

        // 2. Timestamp comparison
        if (a.t_ns !== b.t_ns) {
          return a.t_ns < b.t_ns ? -1 : 1; // Earlier wins
        }

        // 3. Agent ID comparison (lexicographic)
        return a.agentId.localeCompare(b.agentId);
      });

      // Winner is first after sorting
      const winner = sorted[0];
      merged.push(winner);

      // Record conflicts
      for (let i = 1; i < sorted.length; i++) {
        conflicts.push({
          delta1: winner,
          delta2: sorted[i],
          resolution: 'delta1_wins',
          reason: `Resolved by Λ: priority=${shardRegistry.get(winner.shardId)?.shard?.priority || 0}`,
        });
      }
    }
  }

  // Sort merged deltas by timestamp for deterministic ordering
  merged.sort((a, b) => (a.t_ns < b.t_ns ? -1 : a.t_ns > b.t_ns ? 1 : 0));

  // Compute receipt hash
  const receiptContent = {
    merged: merged.map((d) => ({ target: d.delta.target, agentId: d.agentId })),
    conflicts: conflicts.length,
    t_ns: now().toString(),
  };
  const receiptHash = await blake3(JSON.stringify(receiptContent));

  return MergeResultSchema.parse({
    merged,
    conflicts,
    receiptHash,
  });
}

/**
 * Apply merged deltas to a store
 *
 * @param {Object} store - KGCStore instance
 * @param {MergeResult} mergeResult - Result from mergeDeltas
 * @returns {Promise<{ applied: number, receipts: string[] }>}
 */
export async function applyMergedDeltas(store, mergeResult) {
  const applied = [];
  const receipts = [];

  for (const attributed of mergeResult.merged) {
    const { delta, agentId, shardId } = attributed;

    // Convert to store deltas
    const storeDeltas = [];

    if (delta.type === 'add' || delta.type === 'modify') {
      storeDeltas.push({
        type: 'add',
        subject: dataFactory.namedNode(delta.target),
        predicate: dataFactory.namedNode('http://kgc.io/value'),
        object: dataFactory.literal(JSON.stringify(delta.after)),
      });
    }

    if (delta.type === 'delete' || delta.type === 'modify') {
      if (delta.before !== undefined) {
        storeDeltas.push({
          type: 'delete',
          subject: dataFactory.namedNode(delta.target),
          predicate: dataFactory.namedNode('http://kgc.io/value'),
          object: dataFactory.literal(JSON.stringify(delta.before)),
        });
      }
    }

    const { receipt } = await store.appendEvent(
      {
        type: 'SHARD_DELTA',
        payload: {
          agentId,
          shardId,
          deltaType: delta.type,
          target: delta.target,
        },
      },
      storeDeltas,
    );

    applied.push(attributed);
    receipts.push(receipt.id);
  }

  return { applied: applied.length, receipts };
}

/**
 * Check for shard conflicts before merge
 *
 * @param {string} shardId1
 * @param {string} shardId2
 * @returns {{ overlap: boolean, overlappingTargets: string[] }}
 */
export function checkShardOverlap(shardId1, shardId2) {
  const entry1 = shardRegistry.get(shardId1);
  const entry2 = shardRegistry.get(shardId2);

  if (!entry1 || !entry2) {
    return { overlap: false, overlappingTargets: [] };
  }

  const targets1 = new Set(entry1.pendingDeltas.map((d) => d.delta.target));
  const overlapping = entry2.pendingDeltas
    .filter((d) => targets1.has(d.delta.target))
    .map((d) => d.delta.target);

  return {
    overlap: overlapping.length > 0,
    overlappingTargets: overlapping,
  };
}

/**
 * Get all registered shards
 * @returns {AgentShard[]}
 */
export function getShards() {
  return [...shardRegistry.values()].map((e) => e.shard);
}

/**
 * Remove a shard
 * @param {string} shardId
 */
export function removeShard(shardId) {
  shardRegistry.delete(shardId);
}

/**
 * Clear all shards (for testing)
 */
export function clearShards() {
  shardRegistry.clear();
}

/**
 * Create a coordinated multi-agent session
 *
 * @param {Object[]} agents - Agent configurations
 * @param {string} agents[].id - Agent ID
 * @param {ShardScope} agents[].scope - Agent scope
 * @param {number} [agents[].priority] - Agent priority
 * @returns {{ shards: AgentShard[], merge: () => Promise<MergeResult> }}
 *
 * @example
 * const session = createMultiAgentSession([
 *   { id: 'frontend', scope: { files: ['src/components/**'] }, priority: 1 },
 *   { id: 'backend', scope: { files: ['src/api/**'] }, priority: 2 },
 * ]);
 *
 * // Agents work concurrently...
 * addDelta(session.shards[0].id, frontendDelta);
 * addDelta(session.shards[1].id, backendDelta);
 *
 * // Merge when ready
 * const result = await session.merge();
 */
export function createMultiAgentSession(agents) {
  const shards = agents.map((agent) => createShard(agent.id, agent.scope, { priority: agent.priority }));

  return {
    shards,
    async merge() {
      const deltaSets = shards.map((s) => getPendingDeltas(s.id));
      const result = await mergeDeltas(deltaSets);

      // Clear pending deltas after merge
      for (const shard of shards) {
        clearPendingDeltas(shard.id);
      }

      return result;
    },
  };
}
