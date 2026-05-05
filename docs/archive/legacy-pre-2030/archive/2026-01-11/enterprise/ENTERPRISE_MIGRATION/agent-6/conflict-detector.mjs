/**
 * Conflict Detection for Capsule Deltas
 *
 * Detects conflicts between delta operations and determines
 * if deltas can be safely reordered.
 *
 * @module agent-6/conflict-detector
 */

import { computeImpactSet } from './impact-set.mjs';

/**
 * @typedef {import('./impact-set.mjs').Delta} Delta
 * @typedef {import('./impact-set.mjs').ImpactSet} ImpactSet
 */

/**
 * @typedef {'WRITE_WRITE' | 'READ_WRITE' | 'DELETE_ANY' | 'NONE'} ConflictType
 */

/**
 * @typedef {Object} ConflictResult
 * @property {ConflictType} type - Type of conflict detected
 * @property {boolean} hasConflict - Whether a conflict exists
 * @property {string[]} conflictingPaths - Paths where conflicts occur
 * @property {string} description - Human-readable description
 */

/**
 * Detect WRITE-WRITE conflicts (both deltas write to same resource)
 * @param {ImpactSet} impactA - Impact set of delta A
 * @param {ImpactSet} impactB - Impact set of delta B
 * @returns {string[]} Conflicting paths
 */
function detectWriteWriteConflict(impactA, impactB) {
  const conflicts = [];

  for (const write of impactA.writes) {
    if (impactB.writes.has(write)) {
      conflicts.push(write);
    }
  }

  return conflicts;
}

/**
 * Detect READ-WRITE conflicts (one reads what the other writes)
 * @param {ImpactSet} impactA - Impact set of delta A
 * @param {ImpactSet} impactB - Impact set of delta B
 * @returns {string[]} Conflicting paths
 */
function detectReadWriteConflict(impactA, impactB) {
  const conflicts = [];

  // A reads what B writes
  for (const read of impactA.reads) {
    if (impactB.writes.has(read)) {
      conflicts.push(read);
    }
  }

  // B reads what A writes
  for (const read of impactB.reads) {
    if (impactA.writes.has(read)) {
      conflicts.push(read);
    }
  }

  return [...new Set(conflicts)]; // Deduplicate
}

/**
 * Detect DELETE-ANY conflicts (delete conflicts with any access)
 * @param {ImpactSet} impactA - Impact set of delta A
 * @param {ImpactSet} impactB - Impact set of delta B
 * @returns {string[]} Conflicting paths
 */
function detectDeleteConflict(impactA, impactB) {
  const conflicts = [];

  // A deletes what B accesses
  for (const del of impactA.deletes) {
    if (impactB.reads.has(del) || impactB.writes.has(del) || impactB.deletes.has(del)) {
      conflicts.push(del);
    }
  }

  // B deletes what A accesses
  for (const del of impactB.deletes) {
    if (impactA.reads.has(del) || impactA.writes.has(del)) {
      conflicts.push(del);
    }
  }

  return [...new Set(conflicts)]; // Deduplicate
}

/**
 * Detect conflicts between two deltas
 *
 * Checks for:
 * - WRITE_WRITE: Both deltas write to the same resource
 * - READ_WRITE: One delta reads what the other writes
 * - DELETE_ANY: One delta deletes what the other accesses
 * - NONE: No conflicts detected
 *
 * @param {Delta} deltaA - First delta
 * @param {Delta} deltaB - Second delta
 * @returns {ConflictResult} Conflict detection result
 * @throws {Error} If deltas are invalid
 *
 * @example
 * detectConflict(
 *   { type: 'UPDATE', path: '/users/123', fields: ['name'] },
 *   { type: 'UPDATE', path: '/users/123', fields: ['email'] }
 * )
 * // Returns: { type: 'NONE', hasConflict: false, conflictingPaths: [], ... }
 *
 * @example
 * detectConflict(
 *   { type: 'UPDATE', path: '/users/123', fields: ['name'] },
 *   { type: 'DELETE', path: '/users/123' }
 * )
 * // Returns: { type: 'DELETE_ANY', hasConflict: true, conflictingPaths: ['/users/123'], ... }
 */
export function detectConflict(deltaA, deltaB) {
  if (!deltaA || !deltaB) {
    throw new Error('Both deltaA and deltaB must be provided');
  }

  // Compute impact sets
  const impactA = computeImpactSet(deltaA);
  const impactB = computeImpactSet(deltaB);

  // Check for DELETE conflicts first (highest priority)
  const deleteConflicts = detectDeleteConflict(impactA, impactB);
  if (deleteConflicts.length > 0) {
    return {
      type: 'DELETE_ANY',
      hasConflict: true,
      conflictingPaths: deleteConflicts,
      description: `Delete conflict on paths: ${deleteConflicts.join(', ')}`
    };
  }

  // Check for WRITE-WRITE conflicts
  const writeWriteConflicts = detectWriteWriteConflict(impactA, impactB);
  if (writeWriteConflicts.length > 0) {
    return {
      type: 'WRITE_WRITE',
      hasConflict: true,
      conflictingPaths: writeWriteConflicts,
      description: `Write-write conflict on paths: ${writeWriteConflicts.join(', ')}`
    };
  }

  // Check for READ-WRITE conflicts
  const readWriteConflicts = detectReadWriteConflict(impactA, impactB);
  if (readWriteConflicts.length > 0) {
    return {
      type: 'READ_WRITE',
      hasConflict: true,
      conflictingPaths: readWriteConflicts,
      description: `Read-write conflict on paths: ${readWriteConflicts.join(', ')}`
    };
  }

  // No conflicts detected
  return {
    type: 'NONE',
    hasConflict: false,
    conflictingPaths: [],
    description: 'No conflicts detected'
  };
}

/**
 * Check if two deltas can be safely reordered
 *
 * Two deltas can be reordered if:
 * 1. They have no conflicts, OR
 * 2. They only have READ-READ dependencies (safe to parallelize)
 *
 * WRITE-WRITE, READ-WRITE, and DELETE conflicts prevent reordering.
 *
 * @param {Delta} deltaA - First delta
 * @param {Delta} deltaB - Second delta
 * @returns {boolean} True if deltas can be safely reordered
 *
 * @example
 * canReorder(
 *   { type: 'READ', path: '/users/123' },
 *   { type: 'READ', path: '/users/456' }
 * )
 * // Returns: true
 *
 * @example
 * canReorder(
 *   { type: 'UPDATE', path: '/users/123', fields: ['name'] },
 *   { type: 'READ', path: '/users/123' }
 * )
 * // Returns: false (READ-WRITE conflict)
 */
export function canReorder(deltaA, deltaB) {
  const conflict = detectConflict(deltaA, deltaB);

  // No conflict = safe to reorder
  if (!conflict.hasConflict) {
    return true;
  }

  // Any conflict prevents reordering
  return false;
}

/**
 * Batch conflict detection for multiple deltas
 * @param {Delta[]} deltas - Array of deltas to check
 * @returns {Object} Conflict matrix with all pairwise conflicts
 */
export function detectBatchConflicts(deltas) {
  if (!Array.isArray(deltas)) {
    throw new Error('Deltas must be an array');
  }

  const conflicts = [];

  for (let i = 0; i < deltas.length; i++) {
    for (let j = i + 1; j < deltas.length; j++) {
      const conflict = detectConflict(deltas[i], deltas[j]);
      if (conflict.hasConflict) {
        conflicts.push({
          indexA: i,
          indexB: j,
          deltaA: deltas[i],
          deltaB: deltas[j],
          conflict
        });
      }
    }
  }

  return {
    totalPairs: (deltas.length * (deltas.length - 1)) / 2,
    conflictCount: conflicts.length,
    conflicts,
    hasAnyConflict: conflicts.length > 0
  };
}

/**
 * Check if a delta is independent (has no conflicts with any in a set)
 * @param {Delta} delta - Delta to check
 * @param {Delta[]} deltaSet - Set of deltas to check against
 * @returns {boolean} True if delta is independent
 */
export function isIndependent(delta, deltaSet) {
  if (!Array.isArray(deltaSet)) {
    throw new Error('deltaSet must be an array');
  }

  for (const other of deltaSet) {
    const conflict = detectConflict(delta, other);
    if (conflict.hasConflict) {
      return false;
    }
  }

  return true;
}

/**
 * Find all deltas that conflict with a given delta
 * @param {Delta} delta - Delta to check
 * @param {Delta[]} deltaSet - Set of deltas to check against
 * @returns {Array<{delta: Delta, conflict: ConflictResult}>} Conflicting deltas
 */
export function findConflictingDeltas(delta, deltaSet) {
  if (!Array.isArray(deltaSet)) {
    throw new Error('deltaSet must be an array');
  }

  const conflicting = [];

  for (const other of deltaSet) {
    const conflict = detectConflict(delta, other);
    if (conflict.hasConflict) {
      conflicting.push({ delta: other, conflict });
    }
  }

  return conflicting;
}

/**
 * Partition deltas into conflict-free groups
 * @param {Delta[]} deltas - Deltas to partition
 * @returns {Delta[][]} Array of conflict-free groups
 */
export function partitionConflictFree(deltas) {
  if (!Array.isArray(deltas)) {
    throw new Error('Deltas must be an array');
  }

  const groups = [];

  for (const delta of deltas) {
    let placed = false;

    // Try to place in existing group
    for (const group of groups) {
      if (isIndependent(delta, group)) {
        group.push(delta);
        placed = true;
        break;
      }
    }

    // Create new group if couldn't place
    if (!placed) {
      groups.push([delta]);
    }
  }

  return groups;
}

/**
 * Compute dependency graph for deltas
 * @param {Delta[]} deltas - Deltas to analyze
 * @returns {Map<number, Set<number>>} Adjacency list (index -> depends on indices)
 */
export function computeDependencyGraph(deltas) {
  if (!Array.isArray(deltas)) {
    throw new Error('Deltas must be an array');
  }

  const graph = new Map();

  for (let i = 0; i < deltas.length; i++) {
    graph.set(i, new Set());
  }

  // Add edges for conflicts (i depends on j if they conflict and j comes first)
  for (let i = 0; i < deltas.length; i++) {
    for (let j = 0; j < i; j++) {
      const conflict = detectConflict(deltas[j], deltas[i]);
      if (conflict.hasConflict) {
        graph.get(i).add(j);
      }
    }
  }

  return graph;
}

/**
 * Check if delta sequence is serializable (has valid ordering)
 * @param {Delta[]} deltas - Ordered sequence of deltas
 * @returns {boolean} True if sequence is serializable
 */
export function isSerializable(deltas) {
  if (!Array.isArray(deltas)) {
    throw new Error('Deltas must be an array');
  }

  // Check for cycles in dependency graph
  const graph = computeDependencyGraph(deltas);

  // Simple cycle detection using DFS
  const visited = new Set();
  const recursionStack = new Set();

  function hasCycle(node) {
    visited.add(node);
    recursionStack.add(node);

    const neighbors = graph.get(node) || new Set();
    for (const neighbor of neighbors) {
      if (!visited.has(neighbor)) {
        if (hasCycle(neighbor)) return true;
      } else if (recursionStack.has(neighbor)) {
        return true;
      }
    }

    recursionStack.delete(node);
    return false;
  }

  for (let i = 0; i < deltas.length; i++) {
    if (!visited.has(i)) {
      if (hasCycle(i)) return false;
    }
  }

  return true;
}
