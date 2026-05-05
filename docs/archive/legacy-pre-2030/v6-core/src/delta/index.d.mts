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
export function createDeltaSystem(options?: {
    policies?: any;
    strict?: boolean;
    conflictResolver?: Function;
    workflowOptions?: any;
    resourceOptions?: any;
    graphqlOptions?: any;
}): any;
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
export function createDelta(op: string, subject: string, predicate: string, object: string, options?: {
    oldObject?: string;
    graph?: string;
    package?: string;
    actor?: string;
}): any;
/**
 * Legacy: createDeltaProposal()
 * Creates a delta proposal with legacy from/to fields
 *
 * @param {string} from - Source state identifier
 * @param {string} to - Target state identifier
 * @param {Array} [operations=[]] - Delta operations
 * @returns {Object} Delta proposal
 */
export function createDeltaProposal(from: string, to: string, operations?: any[]): any;
/**
 * Legacy: applyDelta()
 * Applies a delta to the store (compatibility stub)
 *
 * @param {Object} store - RDF store
 * @param {Object} delta - Delta to apply
 * @returns {Promise<Object>} Result
 */
export function applyDelta(store: any, delta: any): Promise<any>;
export { DeltaGate } from "./gate.mjs";
/**
 * Legacy: DeltaProposalSchema
 * Lenient schema for backward compatibility with v5 API
 * Accepts legacy format without strict validation
 */
export const DeltaProposalSchema: z.ZodObject<{
    id: z.ZodString;
    from: z.ZodOptional<z.ZodString>;
    to: z.ZodOptional<z.ZodString>;
    operations: z.ZodOptional<z.ZodArray<z.ZodAny>>;
    timestamp: z.ZodOptional<z.ZodString>;
    timestamp_iso: z.ZodOptional<z.ZodString>;
    t_ns: z.ZodOptional<z.ZodBigInt>;
    source: z.ZodOptional<z.ZodAny>;
}, z.core.$strip>;
export namespace adapters {
    export { WorkflowAdapter };
    export { ResourceAdapter };
    export { GraphQLAdapter };
    export { MemoryAdapter };
}
import { z } from 'zod';
import { WorkflowAdapter } from './adapters/workflow-adapter.mjs';
import { ResourceAdapter } from './adapters/resource-adapter.mjs';
import { GraphQLAdapter } from './adapters/graphql-adapter.mjs';
import { MemoryAdapter } from './adapters/index.mjs';
export { DeltaSchema, DeltaOperationSchema, DeltaSourceSchema, DeltaAdmissibilitySchema, DeltaReceiptSchema, DeltaConflictSchema, validateDelta, validateDeltaOperation, validateDeltaReceipt, validateDeltaConflict } from "./schema.mjs";
export { reconcile, defaultConflictResolver, currentWinsResolver, strictResolver, customResolver } from "./reconcile.mjs";
export { DeltaStore, createDeltaStore, readDeltaFromFile, getDefaultStore, resetDefaultStore, DeltaStatus, StoredDeltaSchema } from "./store.mjs";
export { WorkflowAdapter, createWorkflowAdapter } from "./adapters/workflow-adapter.mjs";
export { ResourceAdapter, createResourceAdapter } from "./adapters/resource-adapter.mjs";
export { GraphQLAdapter, createGraphQLAdapter } from "./adapters/graphql-adapter.mjs";
export { MemoryAdapter, DeltaAdapter } from "./adapters/index.mjs";
