/**
 * V6 Delta Contract - Public API
 *
 * The ONLY legal change carrier in UNRDF v6.
 * All ontology mutations flow through Δ (Delta).
 *
 * @module @unrdf/v6-core/delta
 *
 * CORE INVARIANTS:
 * 1. Δ is the ONLY way to mutate O (Ontology state)
 * 2. All APIs that "mutate" become: propose Δ → μ(O ⊔ Δ) → receipt → atomic A
 * 3. No partial applications: all-or-none
 * 4. Every Δ produces a receipt (success or denial)
 *
 * USAGE:
 * ```javascript
 * import { DeltaGate, reconcile, WorkflowAdapter } from '@unrdf/v6-core/delta';
 *
 * // 1. Create adapter for your domain
 * const adapter = new WorkflowAdapter();
 * const delta = adapter.taskTransition('task-1', 'enabled', 'executing');
 *
 * // 2. Propose delta through gate
 * const gate = new DeltaGate({ policies: myPolicies });
 * const receipt = await gate.proposeDelta(delta, store);
 *
 * // 3. Check receipt
 * if (receipt.applied) {
 *   console.log('Success:', receipt.stateHash);
 * } else {
 *   console.error('Rejected:', receipt.reason);
 * }
 * ```
 */

// =============================================================================
// Schema Exports
// =============================================================================

export {
  DeltaSchema,
  DeltaOperationSchema,
  DeltaSourceSchema,
  DeltaAdmissibilitySchema,
  DeltaReceiptSchema,
  DeltaConflictSchema,
  validateDelta,
  validateDeltaOperation,
  validateDeltaReceipt,
  validateDeltaConflict,
} from './schema.mjs';

// =============================================================================
// Gate Exports
// =============================================================================

export { DeltaGate } from './gate.mjs';

// =============================================================================
// Reconciliation Exports
// =============================================================================

export {
  reconcile,
  defaultConflictResolver,
  currentWinsResolver,
  strictResolver,
  customResolver,
} from './reconcile.mjs';

// =============================================================================
// Adapter Exports
// =============================================================================

export {
  WorkflowAdapter,
  createWorkflowAdapter,
} from './adapters/workflow-adapter.mjs';

export {
  ResourceAdapter,
  createResourceAdapter,
} from './adapters/resource-adapter.mjs';

export {
  GraphQLAdapter,
  createGraphQLAdapter,
} from './adapters/graphql-adapter.mjs';

// =============================================================================
// Convenience Factory Functions
// =============================================================================

/**
 * Create complete Delta system
 *
 * Factory function that creates DeltaGate with all adapters.
 *
 * @param {Object} [options] - Configuration options
 * @param {Object} [options.policies] - Policy enforcement rules
 * @param {boolean} [options.strict] - Strict mode (reject on any conflict)
 * @param {Function} [options.conflictResolver] - Custom conflict resolution
 * @param {Object} [options.workflowOptions] - WorkflowAdapter options
 * @param {Object} [options.resourceOptions] - ResourceAdapter options
 * @param {Object} [options.graphqlOptions] - GraphQLAdapter options
 * @returns {Object} Complete delta system {gate, adapters}
 *
 * @example
 * const system = createDeltaSystem({
 *   policies: myPolicies,
 *   strict: true
 * });
 *
 * const delta = system.adapters.workflow.taskTransition('task-1', 'enabled', 'executing');
 * const receipt = await system.gate.proposeDelta(delta, store);
 */
export function createDeltaSystem(options = {}) {
  const gate = new DeltaGate({
    policies: options.policies,
    strict: options.strict,
    conflictResolver: options.conflictResolver,
  });

  const adapters = {
    workflow: createWorkflowAdapter(options.workflowOptions),
    resource: createResourceAdapter(options.resourceOptions),
    graphql: createGraphQLAdapter(options.graphqlOptions),
  };

  return { gate, adapters };
}

/**
 * Create delta from operation shorthand
 *
 * Convenience function for creating simple deltas.
 *
 * @param {string} op - Operation type ('add', 'delete', 'update')
 * @param {string} subject - Subject URI
 * @param {string} predicate - Predicate URI
 * @param {string} object - Object value or new value
 * @param {Object} [options] - Additional options
 * @param {string} [options.oldObject] - Old value for update operations
 * @param {string} [options.graph] - Graph URI
 * @param {string} [options.package] - Source package
 * @param {string} [options.actor] - Source actor
 * @returns {Object} Delta
 *
 * @example
 * const delta = createDelta('add',
 *   'http://ex.org/subject',
 *   'http://ex.org/predicate',
 *   'value',
 *   { package: '@unrdf/app' }
 * );
 */
export function createDelta(op, subject, predicate, object, options = {}) {
  const operation = { op, subject, predicate, object };

  if (op === 'update') {
    if (!options.oldObject) {
      throw new Error('Update operation requires oldObject in options');
    }
    operation.oldObject = options.oldObject;
    operation.newObject = object;
    delete operation.object;
  }

  if (options.graph) {
    operation.graph = options.graph;
  }

  const delta = {
    id: generateUUID(),
    timestamp_iso: new Date().toISOString(),
    t_ns: BigInt(Date.now()) * 1_000_000n,
    operations: [operation],
    source: {
      package: options.package || '@unrdf/v6-core',
      actor: options.actor,
      context: options.context,
    },
  };

  // Import validateDelta from schema exports
  const { validateDelta: validate } = await import('./schema.mjs');
  return validate(delta);
}

/**
 * Generate UUID (browser/Node.js compatible)
 *
 * @returns {string} UUID v4
 * @private
 */
function generateUUID() {
  if (typeof crypto !== 'undefined' && crypto.randomUUID) {
    return crypto.randomUUID();
  }
  try {
    const crypto = require('crypto');
    return crypto.randomUUID();
  } catch {
    return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
      const r = (Math.random() * 16) | 0;
      const v = c === 'x' ? r : (r & 0x3) | 0x8;
      return v.toString(16);
    });
  }
}
