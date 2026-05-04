/**
 * @fileoverview CRDT (Conflict-free Replicated Data Types) for KGC Runtime
 * Implements LWW-Register and OR-Set for semantic content merging
 *
 * Pattern: Pure functions + vector clocks + deterministic merge
 */

import { z } from 'zod';

// ============================================================================
// Schemas
// ============================================================================

/**
 * Timestamp schema - Lamport timestamp for causality
 */
const TimestampSchema = z.object({
  counter: z.number().int().nonnegative(),
  actor: z.string(),
});

/**
 * LWW-Register schema - Last Write Wins register
 */
const LWWRegisterSchema = z.object({
  value: z.any(),
  timestamp: TimestampSchema,
});

/**
 * OR-Set element schema
 */
const ORSetElementSchema = z.object({
  value: z.any(),
  addTimestamp: TimestampSchema,
  removeTimestamp: TimestampSchema.optional().nullable(),
});

/**
 * OR-Set schema - Observed-Remove set
 */
const ORSetSchema = z.object({
  elements: z.array(ORSetElementSchema),
});

/**
 * @typedef {z.infer<typeof LWWRegisterSchema>} LWWRegister
 */

/**
 * @typedef {z.infer<typeof ORSetSchema>} ORSet
 */

/**
 * @typedef {z.infer<typeof TimestampSchema>} Timestamp
 */

// ============================================================================
// CRDT Operations
// ============================================================================

/**
 * Create a Lamport timestamp
 *
 * @param {number} counter - Logical clock counter
 * @param {string} actor - Actor ID
 * @returns {Timestamp} Lamport timestamp
 */
export function createTimestamp(counter, actor) {
  return TimestampSchema.parse({ counter, actor });
}

/**
 * Compare two timestamps (returns -1, 0, 1)
 *
 * @param {Timestamp} t1 - First timestamp
 * @param {Timestamp} t2 - Second timestamp
 * @returns {number} Comparison result
 */
export function compareTimestamps(t1, t2) {
  if (t1.counter !== t2.counter) {
    return t1.counter - t2.counter;
  }
  return t1.actor.localeCompare(t2.actor);
}

// ============================================================================
// LWW-Register (Last Write Wins)
// ============================================================================

/**
 * Create LWW-Register
 *
 * @param {any} value - Initial value
 * @param {Timestamp} timestamp - Initial timestamp
 * @returns {LWWRegister} LWW-Register
 */
export function createLWWRegister(value, timestamp) {
  return LWWRegisterSchema.parse({ value, timestamp });
}

/**
 * Update LWW-Register value
 *
 * @param {LWWRegister} register - Register to update
 * @param {any} newValue - New value
 * @param {Timestamp} timestamp - Update timestamp
 * @returns {LWWRegister} Updated register
 */
export function updateLWWRegister(register, newValue, timestamp) {
  const validated = LWWRegisterSchema.parse(register);

  // Only update if new timestamp is later
  if (compareTimestamps(timestamp, validated.timestamp) > 0) {
    return createLWWRegister(newValue, timestamp);
  }

  return validated;
}

/**
 * Merge two LWW-Registers (last write wins)
 *
 * @param {LWWRegister} r1 - First register
 * @param {LWWRegister} r2 - Second register
 * @returns {LWWRegister} Merged register
 */
export function mergeLWWRegisters(r1, r2) {
  const reg1 = LWWRegisterSchema.parse(r1);
  const reg2 = LWWRegisterSchema.parse(r2);

  // Compare timestamps - later wins
  const cmp = compareTimestamps(reg1.timestamp, reg2.timestamp);

  if (cmp > 0) {
    return reg1;
  } else if (cmp < 0) {
    return reg2;
  } else {
    // Same timestamp - use deterministic tiebreaker (lexicographic on value)
    const v1Str = JSON.stringify(reg1.value);
    const v2Str = JSON.stringify(reg2.value);
    return v1Str.localeCompare(v2Str) > 0 ? reg1 : reg2;
  }
}

// ============================================================================
// OR-Set (Observed-Remove Set)
// ============================================================================

/**
 * Create empty OR-Set
 *
 * @returns {ORSet} Empty OR-Set
 */
export function createORSet() {
  return ORSetSchema.parse({ elements: [] });
}

/**
 * Add element to OR-Set
 *
 * @param {ORSet} set - OR-Set to modify
 * @param {any} value - Value to add
 * @param {Timestamp} timestamp - Add timestamp
 * @returns {ORSet} Updated OR-Set
 */
export function addToORSet(set, value, timestamp) {
  const validated = ORSetSchema.parse(set);

  // Add new element with timestamp
  const newElement = ORSetElementSchema.parse({
    value,
    addTimestamp: timestamp,
  });

  return ORSetSchema.parse({
    elements: [...validated.elements, newElement],
  });
}

/**
 * Remove element from OR-Set
 *
 * @param {ORSet} set - OR-Set to modify
 * @param {any} value - Value to remove
 * @param {Timestamp} timestamp - Remove timestamp
 * @returns {ORSet} Updated OR-Set
 */
export function removeFromORSet(set, value, timestamp) {
  const validated = ORSetSchema.parse(set);

  // Mark elements as removed if they match value
  const updatedElements = validated.elements.map(elem => {
    const valueMatch = JSON.stringify(elem.value) === JSON.stringify(value);
    if (valueMatch && !elem.removeTimestamp) {
      return { ...elem, removeTimestamp: timestamp };
    }
    return elem;
  });

  return ORSetSchema.parse({ elements: updatedElements });
}

/**
 * Get current values in OR-Set (not removed)
 *
 * @param {ORSet} set - OR-Set
 * @returns {any[]} Active values
 */
export function getORSetValues(set) {
  const validated = ORSetSchema.parse(set);

  // Return values that are not removed
  return validated.elements
    .filter(elem => !elem.removeTimestamp)
    .map(elem => elem.value);
}

/**
 * Merge two OR-Sets
 *
 * @param {ORSet} s1 - First OR-Set
 * @param {ORSet} s2 - Second OR-Set
 * @returns {ORSet} Merged OR-Set
 */
export function mergeORSets(s1, s2) {
  const set1 = ORSetSchema.parse(s1);
  const set2 = ORSetSchema.parse(s2);

  // Combine all elements
  const allElements = [...set1.elements, ...set2.elements];

  // Group by value
  const valueMap = new Map();

  for (const elem of allElements) {
    const key = JSON.stringify(elem.value);

    if (!valueMap.has(key)) {
      valueMap.set(key, []);
    }

    valueMap.get(key).push(elem);
  }

  // Merge each group
  const mergedElements = [];

  for (const [, elems] of valueMap) {
    // Keep all unique add timestamps
    const addTimestamps = new Map();
    for (const elem of elems) {
      const key = `${elem.addTimestamp.counter}_${elem.addTimestamp.actor}`;
      if (!addTimestamps.has(key)) {
        addTimestamps.set(key, elem);
      }
    }

    // Find latest remove timestamp
    let latestRemove = null;
    for (const elem of elems) {
      if (elem.removeTimestamp) {
        if (!latestRemove || compareTimestamps(elem.removeTimestamp, latestRemove) > 0) {
          latestRemove = elem.removeTimestamp;
        }
      }
    }

    // Keep elements where add timestamp > remove timestamp
    for (const [, elem] of addTimestamps) {
      if (!latestRemove || compareTimestamps(elem.addTimestamp, latestRemove) > 0) {
        mergedElements.push({
          value: elem.value,
          addTimestamp: elem.addTimestamp,
          removeTimestamp: latestRemove,
        });
      } else {
        // Element was removed after being added
        mergedElements.push({
          value: elem.value,
          addTimestamp: elem.addTimestamp,
          removeTimestamp: latestRemove,
        });
      }
    }
  }

  return ORSetSchema.parse({ elements: mergedElements });
}

// ============================================================================
// Three-way Merge with Common Ancestor
// ============================================================================

/**
 * Three-way merge for LWW-Registers
 *
 * @param {LWWRegister} ancestor - Common ancestor state
 * @param {LWWRegister} left - Left branch
 * @param {LWWRegister} right - Right branch
 * @returns {LWWRegister} Merged state
 */
export function threeWayMergeLWW(ancestor, left, right) {
  // If both branches modified, use LWW merge
  const ancestorValidated = LWWRegisterSchema.parse(ancestor);
  const leftValidated = LWWRegisterSchema.parse(left);
  const rightValidated = LWWRegisterSchema.parse(right);

  // Check if either branch is unchanged from ancestor
  const leftUnchanged = compareTimestamps(leftValidated.timestamp, ancestorValidated.timestamp) === 0;
  const rightUnchanged = compareTimestamps(rightValidated.timestamp, ancestorValidated.timestamp) === 0;

  if (leftUnchanged && rightUnchanged) {
    // No changes - return ancestor
    return ancestorValidated;
  } else if (leftUnchanged) {
    // Only right changed
    return rightValidated;
  } else if (rightUnchanged) {
    // Only left changed
    return leftValidated;
  } else {
    // Both changed - use LWW merge
    return mergeLWWRegisters(leftValidated, rightValidated);
  }
}

/**
 * Three-way merge for OR-Sets
 *
 * @param {ORSet} ancestor - Common ancestor state
 * @param {ORSet} left - Left branch
 * @param {ORSet} right - Right branch
 * @returns {ORSet} Merged state
 */
export function threeWayMergeORSet(ancestor, left, right) {
  // Merge left and right branches
  const merged = mergeORSets(left, right);

  return merged;
}

// ============================================================================
// Exports
// ============================================================================

export default {
  createTimestamp,
  compareTimestamps,
  createLWWRegister,
  updateLWWRegister,
  mergeLWWRegisters,
  threeWayMergeLWW,
  createORSet,
  addToORSet,
  removeFromORSet,
  getORSetValues,
  mergeORSets,
  threeWayMergeORSet,
};
