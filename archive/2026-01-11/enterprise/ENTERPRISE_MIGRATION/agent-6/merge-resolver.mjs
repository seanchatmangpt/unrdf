/**
 * Merge Conflict Resolution for Delta Operations
 *
 * Resolves conflicts between deltas using various strategies.
 * Supports automatic and manual resolution modes.
 *
 * @module agent-6/merge-resolver
 */

import { detectConflict } from './conflict-detector.mjs';
import { computeImpactSet } from './impact-set.mjs';

/**
 * @typedef {import('./conflict-detector.mjs').Delta} Delta
 * @typedef {import('./conflict-detector.mjs').ConflictResult} ConflictResult
 */

/**
 * @typedef {'OURS' | 'THEIRS' | 'UNION' | 'INTERSECTION' | 'MANUAL' | 'LAST_WRITE_WINS' | 'FIRST_WRITE_WINS'} MergeStrategy
 */

/**
 * @typedef {Object} MergeResult
 * @property {boolean} resolved - Whether conflict was resolved
 * @property {Delta} [delta] - Resolved delta
 * @property {MergeStrategy} strategy - Strategy used
 * @property {string} [error] - Error if resolution failed
 * @property {Object} [metadata] - Additional resolution metadata
 */

/**
 * Resolve conflict using OURS strategy (prefer first delta)
 * @param {Delta} deltaA - First delta (ours)
 * @param {Delta} deltaB - Second delta (theirs)
 * @returns {MergeResult} Resolution result
 */
function resolveOurs(deltaA, deltaB) {
  return {
    resolved: true,
    delta: { ...deltaA },
    strategy: 'OURS',
    metadata: {
      chosen: 'deltaA',
      discarded: 'deltaB',
      reasoning: 'OURS strategy - preferring first delta'
    }
  };
}

/**
 * Resolve conflict using THEIRS strategy (prefer second delta)
 * @param {Delta} deltaA - First delta (ours)
 * @param {Delta} deltaB - Second delta (theirs)
 * @returns {MergeResult} Resolution result
 */
function resolveTheirs(deltaA, deltaB) {
  return {
    resolved: true,
    delta: { ...deltaB },
    strategy: 'THEIRS',
    metadata: {
      chosen: 'deltaB',
      discarded: 'deltaA',
      reasoning: 'THEIRS strategy - preferring second delta'
    }
  };
}

/**
 * Resolve conflict using UNION strategy (merge both changes)
 * @param {Delta} deltaA - First delta
 * @param {Delta} deltaB - Second delta
 * @returns {MergeResult} Resolution result
 */
function resolveUnion(deltaA, deltaB) {
  // Check if deltas are compatible for union
  if (deltaA.type !== deltaB.type) {
    return {
      resolved: false,
      strategy: 'UNION',
      error: 'Cannot union deltas with different types'
    };
  }

  if (deltaA.path !== deltaB.path) {
    return {
      resolved: false,
      strategy: 'UNION',
      error: 'Cannot union deltas with different paths'
    };
  }

  const merged = {
    type: deltaA.type,
    path: deltaA.path
  };

  // Merge fields
  if (deltaA.fields || deltaB.fields) {
    const fieldsA = new Set(deltaA.fields || []);
    const fieldsB = new Set(deltaB.fields || []);
    merged.fields = [...new Set([...fieldsA, ...fieldsB])];
  }

  // Merge data (object merge)
  if (deltaA.data || deltaB.data) {
    merged.data = {
      ...(deltaA.data || {}),
      ...(deltaB.data || {})
    };
  }

  // Merge nested deltas
  if (deltaA.deltas || deltaB.deltas) {
    merged.deltas = [
      ...(deltaA.deltas || []),
      ...(deltaB.deltas || [])
    ];
  }

  return {
    resolved: true,
    delta: merged,
    strategy: 'UNION',
    metadata: {
      mergedFields: merged.fields?.length || 0,
      reasoning: 'UNION strategy - merged all changes from both deltas'
    }
  };
}

/**
 * Resolve conflict using INTERSECTION strategy (only keep common changes)
 * @param {Delta} deltaA - First delta
 * @param {Delta} deltaB - Second delta
 * @returns {MergeResult} Resolution result
 */
function resolveIntersection(deltaA, deltaB) {
  if (deltaA.type !== deltaB.type || deltaA.path !== deltaB.path) {
    return {
      resolved: false,
      strategy: 'INTERSECTION',
      error: 'Cannot intersect deltas with different types or paths'
    };
  }

  const merged = {
    type: deltaA.type,
    path: deltaA.path
  };

  // Intersect fields
  if (deltaA.fields && deltaB.fields) {
    const fieldsA = new Set(deltaA.fields);
    const fieldsB = new Set(deltaB.fields);
    const intersection = [...fieldsA].filter(f => fieldsB.has(f));

    if (intersection.length === 0) {
      return {
        resolved: false,
        strategy: 'INTERSECTION',
        error: 'No common fields between deltas'
      };
    }

    merged.fields = intersection;
  }

  // Intersect data (keep only matching values)
  if (deltaA.data && deltaB.data) {
    const dataIntersection = {};
    for (const key in deltaA.data) {
      if (key in deltaB.data &&
          JSON.stringify(deltaA.data[key]) === JSON.stringify(deltaB.data[key])) {
        dataIntersection[key] = deltaA.data[key];
      }
    }

    if (Object.keys(dataIntersection).length === 0) {
      return {
        resolved: false,
        strategy: 'INTERSECTION',
        error: 'No common data values between deltas'
      };
    }

    merged.data = dataIntersection;
  }

  return {
    resolved: true,
    delta: merged,
    strategy: 'INTERSECTION',
    metadata: {
      commonFields: merged.fields?.length || 0,
      reasoning: 'INTERSECTION strategy - kept only common changes'
    }
  };
}

/**
 * Resolve conflict using LAST_WRITE_WINS strategy (prefer most recent)
 * @param {Delta} deltaA - First delta
 * @param {Delta} deltaB - Second delta
 * @returns {MergeResult} Resolution result
 */
function resolveLastWriteWins(deltaA, deltaB) {
  // If both have timestamps, use them
  if (deltaA.timestamp && deltaB.timestamp) {
    const timeA = new Date(deltaA.timestamp).getTime();
    const timeB = new Date(deltaB.timestamp).getTime();

    const chosen = timeB > timeA ? deltaB : deltaA;
    return {
      resolved: true,
      delta: { ...chosen },
      strategy: 'LAST_WRITE_WINS',
      metadata: {
        chosen: timeB > timeA ? 'deltaB' : 'deltaA',
        timestampA: deltaA.timestamp,
        timestampB: deltaB.timestamp,
        reasoning: 'LAST_WRITE_WINS strategy - chose delta with latest timestamp'
      }
    };
  }

  // No timestamps - default to THEIRS (assumes B is more recent)
  return {
    resolved: true,
    delta: { ...deltaB },
    strategy: 'LAST_WRITE_WINS',
    metadata: {
      chosen: 'deltaB',
      reasoning: 'No timestamps available - defaulting to second delta (assumed more recent)'
    }
  };
}

/**
 * Resolve conflict using FIRST_WRITE_WINS strategy (prefer earliest)
 * @param {Delta} deltaA - First delta
 * @param {Delta} deltaB - Second delta
 * @returns {MergeResult} Resolution result
 */
function resolveFirstWriteWins(deltaA, deltaB) {
  if (deltaA.timestamp && deltaB.timestamp) {
    const timeA = new Date(deltaA.timestamp).getTime();
    const timeB = new Date(deltaB.timestamp).getTime();

    const chosen = timeA < timeB ? deltaA : deltaB;
    return {
      resolved: true,
      delta: { ...chosen },
      strategy: 'FIRST_WRITE_WINS',
      metadata: {
        chosen: timeA < timeB ? 'deltaA' : 'deltaB',
        timestampA: deltaA.timestamp,
        timestampB: deltaB.timestamp,
        reasoning: 'FIRST_WRITE_WINS strategy - chose delta with earliest timestamp'
      }
    };
  }

  // No timestamps - default to OURS
  return {
    resolved: true,
    delta: { ...deltaA },
    strategy: 'FIRST_WRITE_WINS',
    metadata: {
      chosen: 'deltaA',
      reasoning: 'No timestamps available - defaulting to first delta'
    }
  };
}

/**
 * Resolve conflict between two deltas using specified strategy
 *
 * Strategies:
 * - OURS: Keep first delta, discard second
 * - THEIRS: Keep second delta, discard first
 * - UNION: Merge both deltas (combine all changes)
 * - INTERSECTION: Keep only common changes
 * - LAST_WRITE_WINS: Prefer delta with latest timestamp
 * - FIRST_WRITE_WINS: Prefer delta with earliest timestamp
 * - MANUAL: Require manual intervention
 *
 * @param {Delta} deltaA - First delta
 * @param {Delta} deltaB - Second delta
 * @param {MergeStrategy} strategy - Resolution strategy
 * @returns {MergeResult} Resolution result
 * @throws {Error} If strategy is MANUAL or deltas are invalid
 *
 * @example
 * resolveConflict(
 *   { type: 'UPDATE', path: '/users/123', fields: ['name'] },
 *   { type: 'UPDATE', path: '/users/123', fields: ['email'] },
 *   'UNION'
 * )
 * // Returns: { resolved: true, delta: { ..., fields: ['name', 'email'] }, ... }
 */
export function resolveConflict(deltaA, deltaB, strategy) {
  if (!deltaA || typeof deltaA !== 'object') {
    throw new Error('deltaA must be a non-null object');
  }

  if (!deltaB || typeof deltaB !== 'object') {
    throw new Error('deltaB must be a non-null object');
  }

  if (!strategy || typeof strategy !== 'string') {
    throw new Error('strategy must be a non-empty string');
  }

  const upperStrategy = strategy.toUpperCase();

  // Check if there's actually a conflict
  const conflict = detectConflict(deltaA, deltaB);
  if (!conflict.hasConflict) {
    return {
      resolved: true,
      delta: null, // No merge needed - deltas are independent
      strategy: upperStrategy,
      metadata: {
        reasoning: 'No conflict detected - deltas are independent',
        conflictResult: conflict
      }
    };
  }

  // Apply strategy
  switch (upperStrategy) {
    case 'OURS':
      return resolveOurs(deltaA, deltaB);

    case 'THEIRS':
      return resolveTheirs(deltaA, deltaB);

    case 'UNION':
      return resolveUnion(deltaA, deltaB);

    case 'INTERSECTION':
      return resolveIntersection(deltaA, deltaB);

    case 'LAST_WRITE_WINS':
    case 'LWW':
      return resolveLastWriteWins(deltaA, deltaB);

    case 'FIRST_WRITE_WINS':
    case 'FWW':
      return resolveFirstWriteWins(deltaA, deltaB);

    case 'MANUAL':
      throw new Error('Manual resolution required - cannot resolve automatically');

    default:
      throw new Error(`Unknown resolution strategy: ${strategy}`);
  }
}

/**
 * Batch resolve conflicts for multiple delta pairs
 * @param {Array<[Delta, Delta]>} deltaPairs - Pairs of conflicting deltas
 * @param {MergeStrategy} strategy - Resolution strategy
 * @returns {MergeResult[]} Array of resolution results
 */
export function resolveBatchConflicts(deltaPairs, strategy) {
  if (!Array.isArray(deltaPairs)) {
    throw new Error('deltaPairs must be an array');
  }

  return deltaPairs.map(([deltaA, deltaB], index) => {
    try {
      return {
        index,
        ...resolveConflict(deltaA, deltaB, strategy)
      };
    } catch (error) {
      return {
        index,
        resolved: false,
        strategy,
        error: error.message
      };
    }
  });
}

/**
 * Merge a sequence of deltas using conflict resolution
 * @param {Delta[]} deltas - Deltas to merge
 * @param {MergeStrategy} strategy - Resolution strategy
 * @returns {Object} Merge result with final delta
 */
export function mergeSequence(deltas, strategy) {
  if (!Array.isArray(deltas) || deltas.length === 0) {
    throw new Error('deltas must be a non-empty array');
  }

  if (deltas.length === 1) {
    return {
      success: true,
      finalDelta: deltas[0],
      mergeCount: 0,
      conflicts: []
    };
  }

  let current = deltas[0];
  const conflicts = [];

  for (let i = 1; i < deltas.length; i++) {
    const conflict = detectConflict(current, deltas[i]);

    if (conflict.hasConflict) {
      conflicts.push({
        index: i,
        conflict,
        resolution: null
      });

      try {
        const resolution = resolveConflict(current, deltas[i], strategy);
        conflicts[conflicts.length - 1].resolution = resolution;

        if (resolution.resolved && resolution.delta) {
          current = resolution.delta;
        } else if (resolution.resolved && resolution.delta === null) {
          // Deltas are independent, just continue with current
          continue;
        } else {
          return {
            success: false,
            error: `Failed to resolve conflict at index ${i}`,
            finalDelta: current,
            mergeCount: i - 1,
            conflicts
          };
        }
      } catch (error) {
        return {
          success: false,
          error: error.message,
          finalDelta: current,
          mergeCount: i - 1,
          conflicts
        };
      }
    } else {
      // No conflict - could potentially merge if same type/path
      const mergeAttempt = resolveUnion(current, deltas[i]);
      if (mergeAttempt.resolved) {
        current = mergeAttempt.delta;
      }
    }
  }

  return {
    success: true,
    finalDelta: current,
    mergeCount: deltas.length - 1,
    conflicts
  };
}

/**
 * Create a three-way merge of deltas (base, ours, theirs)
 * @param {Delta} base - Common ancestor delta
 * @param {Delta} ours - Our changes
 * @param {Delta} theirs - Their changes
 * @param {MergeStrategy} strategy - Resolution strategy for conflicts
 * @returns {MergeResult} Three-way merge result
 */
export function threeWayMerge(base, ours, theirs, strategy = 'UNION') {
  if (!base || !ours || !theirs) {
    throw new Error('base, ours, and theirs must all be provided');
  }

  // Check if ours and theirs made same changes
  const ourChanges = computeImpactSet(ours);
  const theirChanges = computeImpactSet(theirs);

  // Detect conflicts
  const conflict = detectConflict(ours, theirs);

  if (!conflict.hasConflict) {
    // No conflict - can merge both
    const merged = resolveUnion(ours, theirs);
    return {
      ...merged,
      metadata: {
        ...merged.metadata,
        mergeType: 'THREE_WAY',
        baseProvided: true,
        conflictDetected: false
      }
    };
  }

  // Has conflict - use strategy
  const resolution = resolveConflict(ours, theirs, strategy);
  return {
    ...resolution,
    metadata: {
      ...resolution.metadata,
      mergeType: 'THREE_WAY',
      baseProvided: true,
      conflictDetected: true,
      conflictType: conflict.type
    }
  };
}

/**
 * Suggest best resolution strategy based on conflict type
 * @param {Delta} deltaA - First delta
 * @param {Delta} deltaB - Second delta
 * @returns {Object} Strategy recommendation
 */
export function suggestStrategy(deltaA, deltaB) {
  const conflict = detectConflict(deltaA, deltaB);

  if (!conflict.hasConflict) {
    return {
      strategy: 'UNION',
      confidence: 'HIGH',
      reasoning: 'No conflict detected - safe to merge both changes'
    };
  }

  switch (conflict.type) {
    case 'DELETE_ANY':
      return {
        strategy: 'MANUAL',
        confidence: 'HIGH',
        reasoning: 'Delete conflicts require manual review to determine correct outcome'
      };

    case 'WRITE_WRITE':
      return {
        strategy: 'LAST_WRITE_WINS',
        confidence: 'MEDIUM',
        reasoning: 'Write-write conflict - timestamp-based resolution recommended'
      };

    case 'READ_WRITE':
      return {
        strategy: 'THEIRS',
        confidence: 'LOW',
        reasoning: 'Read-write dependency - prefer writer (second delta)'
      };

    default:
      return {
        strategy: 'MANUAL',
        confidence: 'LOW',
        reasoning: 'Unknown conflict type - manual review recommended'
      };
  }
}

/**
 * Validate that a resolved delta doesn't create new conflicts
 * @param {Delta} resolved - Resolved delta
 * @param {Delta[]} existing - Existing deltas
 * @returns {Object} Validation result
 */
export function validateResolution(resolved, existing) {
  if (!Array.isArray(existing)) {
    throw new Error('existing must be an array');
  }

  const newConflicts = [];

  for (const delta of existing) {
    const conflict = detectConflict(resolved, delta);
    if (conflict.hasConflict) {
      newConflicts.push({ delta, conflict });
    }
  }

  return {
    valid: newConflicts.length === 0,
    conflictCount: newConflicts.length,
    conflicts: newConflicts
  };
}
