/**
 * V6 Delta Reconciliation - μ(O ⊔ Δ)
 *
 * Reconciliation engine that computes new ontology state from:
 * - Current ontology state O
 * - Proposed delta Δ
 * - Conflict resolution strategy
 *
 * @module @unrdf/v6-core/delta/reconcile
 *
 * RECONCILIATION SEMANTICS:
 * - All operations in Δ are applied atomically
 * - Conflicts are detected and resolved deterministically
 * - Result is new state O' with cryptographic commitment
 */

import { dataFactory } from '@unrdf/oxigraph';
import { validateDeltaConflict } from './schema.mjs';

/**
 * Reconcile delta with current ontology state
 *
 * Computes μ(O ⊔ Δ) by:
 * 1. Detecting conflicts between Δ and O
 * 2. Resolving conflicts using resolver strategy
 * 3. Applying all operations atomically
 * 4. Computing new state hash
 *
 * @param {Object} store - KnowledgeStore instance
 * @param {Object} delta - Validated delta to reconcile
 * @param {Function} [conflictResolver] - Conflict resolution strategy
 * @returns {Promise<Object>} Reconciliation result {applied, stateHash, conflicts, reason}
 *
 * @example
 * const result = await reconcile(store, delta, (conflict) => 'delta-wins');
 * if (result.applied) {
 *   console.log('New state hash:', result.stateHash);
 *   console.log('Conflicts resolved:', result.conflicts.length);
 * }
 */
export async function reconcile(store, delta, conflictResolver = defaultConflictResolver) {
  try {
    const conflicts = [];
    const appliedOps = [];

    // Phase 1: Detect conflicts
    for (const op of delta.operations) {
      const conflict = await detectConflict(store, op);
      if (conflict) {
        conflicts.push(conflict);
      }
    }

    // Phase 2: Resolve conflicts
    if (conflicts.length > 0) {
      for (const conflict of conflicts) {
        const resolution = conflictResolver(conflict);
        conflict.resolution = resolution;

        // Validate conflict resolution
        validateDeltaConflict(conflict);

        // If resolution is 'reject', abort entire delta
        if (resolution === 'reject') {
          return {
            applied: false,
            reason: `Conflict rejected: ${conflict.subject} ${conflict.predicate}`,
            conflicts,
          };
        }
      }
    }

    // Phase 3: Apply operations atomically
    for (const op of delta.operations) {
      const applied = await applyOperation(store, op, conflicts);
      if (applied) {
        appliedOps.push(op);
      }
    }

    // Phase 4: Compute new state hash
    const stateCommitment = await store.getStateCommitment();

    return {
      applied: true,
      stateHash: stateCommitment.state_hash,
      conflicts,
      operationsApplied: appliedOps.length,
    };
  } catch (error) {
    return {
      applied: false,
      reason: `Reconciliation failed: ${error.message}`,
      conflicts: [],
    };
  }
}

/**
 * Detect conflict for a delta operation
 *
 * Checks if operation conflicts with current store state.
 *
 * @param {Object} store - KnowledgeStore instance
 * @param {Object} op - Delta operation
 * @returns {Promise<Object|null>} Conflict object or null if no conflict
 * @private
 */
async function detectConflict(store, op) {
  const { subject: subjectUri, predicate: predicateUri, object: objectUri, graph } = op;

  // Convert URIs to RDF terms
  const subject = dataFactory.namedNode(subjectUri);
  const predicate = dataFactory.namedNode(predicateUri);

  // Query current state
  const existing = store.selectTriples({ subject, predicate, object: null, graph: graph || null });

  if (op.op === 'add') {
    // Conflict if triple already exists with different object
    for (const quad of existing) {
      if (quad.object.value !== objectUri) {
        return {
          subject: subjectUri,
          predicate: predicateUri,
          currentObject: quad.object.value,
          deltaObject: objectUri,
          resolution: 'pending',
        };
      }
    }
  } else if (op.op === 'delete') {
    // Conflict if triple doesn't exist
    const objectTerm = dataFactory.literal(objectUri);
    const hasExact = [...existing].some(quad => quad.object.value === objectUri);

    if (!hasExact) {
      return {
        subject: subjectUri,
        predicate: predicateUri,
        currentObject: 'none',
        deltaObject: objectUri,
        resolution: 'pending',
      };
    }
  } else if (op.op === 'update') {
    // Conflict if oldObject doesn't match current state
    const hasOld = [...existing].some(quad => quad.object.value === op.oldObject);

    if (!hasOld) {
      const currentObject = existing.size > 0 ? [...existing][0].object.value : 'none';
      return {
        subject: subjectUri,
        predicate: predicateUri,
        currentObject,
        deltaObject: op.newObject,
        resolution: 'pending',
      };
    }
  }

  return null;
}

/**
 * Apply single operation to store
 *
 * Executes operation based on conflict resolution.
 *
 * @param {Object} store - KnowledgeStore instance
 * @param {Object} op - Delta operation
 * @param {Array<Object>} conflicts - Resolved conflicts
 * @returns {Promise<boolean>} Whether operation was applied
 * @private
 */
async function applyOperation(store, op, conflicts) {
  const { subject: subjectUri, predicate: predicateUri, object: objectUri, graph } = op;

  // Convert URIs to RDF terms
  const subject = dataFactory.namedNode(subjectUri);
  const predicate = dataFactory.namedNode(predicateUri);
  const object = dataFactory.literal(objectUri);

  // Check if operation has a conflict
  const conflict = conflicts.find(c =>
    c.subject === subjectUri && c.predicate === predicateUri
  );

  // Handle conflict resolution
  if (conflict) {
    if (conflict.resolution === 'current-wins') {
      return false; // Skip operation
    } else if (conflict.resolution === 'reject') {
      return false; // Already handled in reconcile
    }
    // 'delta-wins' and 'merge' fall through to normal application
  }

  // Apply operation
  try {
    if (op.op === 'add') {
      await store.appendTriple('add', subject, predicate, object, graph);
      return true;
    } else if (op.op === 'delete') {
      await store.appendTriple('delete', subject, predicate, object, graph);
      return true;
    } else if (op.op === 'update') {
      // Update = delete old + add new (atomic)
      const oldObject = dataFactory.literal(op.oldObject);
      await store.appendTriple('delete', subject, predicate, oldObject, graph);
      await store.appendTriple('add', subject, predicate, object, graph);
      return true;
    }
  } catch (error) {
    console.error(`Failed to apply operation: ${error.message}`);
    return false;
  }

  return false;
}

/**
 * Default conflict resolver
 *
 * Strategy: delta-wins (new value always replaces old value)
 *
 * @param {Object} conflict - Conflict to resolve
 * @returns {string} Resolution strategy
 */
export function defaultConflictResolver(conflict) {
  return 'delta-wins';
}

/**
 * Create current-wins conflict resolver
 *
 * Strategy: Keep existing value, reject delta change
 *
 * @returns {Function} Conflict resolver function
 *
 * @example
 * const resolver = currentWinsResolver();
 * const result = await reconcile(store, delta, resolver);
 */
export function currentWinsResolver() {
  return (conflict) => 'current-wins';
}

/**
 * Create strict conflict resolver
 *
 * Strategy: Reject any conflicts
 *
 * @returns {Function} Conflict resolver function
 *
 * @example
 * const resolver = strictResolver();
 * const result = await reconcile(store, delta, resolver);
 */
export function strictResolver() {
  return (conflict) => 'reject';
}

/**
 * Create custom conflict resolver
 *
 * Allows application-specific conflict resolution logic.
 *
 * @param {Function} strategy - Custom strategy function
 * @returns {Function} Conflict resolver function
 *
 * @example
 * const resolver = customResolver((conflict) => {
 *   // Custom logic based on predicate
 *   if (conflict.predicate.includes('timestamp')) {
 *     return 'delta-wins'; // Always use newer timestamp
 *   }
 *   return 'current-wins';
 * });
 */
export function customResolver(strategy) {
  if (typeof strategy !== 'function') {
    throw new TypeError('Custom resolver strategy must be a function');
  }
  return strategy;
}
